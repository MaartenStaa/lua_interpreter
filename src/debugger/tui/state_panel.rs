use std::{
    collections::HashSet,
    time::{Duration, Instant},
};

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use ratatui::{
    Frame,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    text::{Line, Span},
    widgets::{Block, Borders},
};

use crate::debugger::protocol::{FrameKind, VmState};

use super::KeyResult;

const HIGHLIGHT_DURATION: Duration = Duration::from_millis(600);

fn change_color(intensity: f32) -> Color {
    if intensity <= 0.0 {
        return Color::White;
    }
    let r = (0xFF as f32 * (1.0 - intensity * 0.3)) as u8;
    let g = 0xFF;
    let b = (0xFF as f32 * (1.0 - intensity * 0.7)) as u8;
    Color::Rgb(r, g, b)
}

fn change_intensity(changed_at: Instant) -> f32 {
    let elapsed = changed_at.elapsed();
    if elapsed >= HIGHLIGHT_DURATION {
        return 0.0;
    }
    1.0 - (elapsed.as_secs_f32() / HIGHLIGHT_DURATION.as_secs_f32())
}

// --- Scrollable list panel helper ---

struct ListState {
    focus: usize,
    scroll: usize,
}

impl ListState {
    fn new() -> Self {
        Self {
            focus: 0,
            scroll: 0,
        }
    }

    fn handle_key(&mut self, key: KeyEvent, len: usize) -> KeyResult {
        match key {
            KeyEvent {
                code: KeyCode::Char('j') | KeyCode::Down,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                if len > 0 {
                    self.focus = (self.focus + 1).min(len - 1);
                }
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('k') | KeyCode::Up,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                self.focus = self.focus.saturating_sub(1);
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('d'),
                modifiers: KeyModifiers::CONTROL,
                ..
            } => {
                if len > 0 {
                    self.focus = (self.focus + 10).min(len - 1);
                }
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('u'),
                modifiers: KeyModifiers::CONTROL,
                ..
            } => {
                self.focus = self.focus.saturating_sub(10);
                KeyResult::Consumed
            }
            _ => KeyResult::Ignored,
        }
    }

    fn ensure_visible(&mut self, visible_height: usize) {
        if self.focus < self.scroll {
            self.scroll = self.focus;
        } else if self.focus >= self.scroll + visible_height {
            self.scroll = self.focus - visible_height + 1;
        }
    }
}

// --- Call Stack Panel ---

pub(crate) struct CallStackPanel {
    list: ListState,
    entries: Vec<CallStackEntry>,
}

struct CallStackEntry {
    label: String,
    is_top: bool,
}

impl CallStackPanel {
    fn new() -> Self {
        Self {
            list: ListState::new(),
            entries: Vec::new(),
        }
    }

    pub(crate) fn update(&mut self, state: &VmState) {
        self.entries = state
            .call_stack
            .iter()
            .enumerate()
            .rev()
            .map(|(i, frame)| {
                let is_top = i == state.call_stack.len() - 1;
                let marker = if is_top { ">" } else { " " };
                let label = match &frame.kind {
                    FrameKind::Lua { chunk, ip } => {
                        let name = &state.chunks[*chunk as usize].name;
                        format!("{marker} #{i} {name}:{ip}")
                    }
                    FrameKind::Native { name } => {
                        format!("{marker} #{i} [native] {name}")
                    }
                };
                CallStackEntry { label, is_top }
            })
            .collect();

        if self.list.focus >= self.entries.len() {
            self.list.focus = self.entries.len().saturating_sub(1);
        }
    }

    pub(crate) fn handle_key(&mut self, key: KeyEvent) -> KeyResult {
        self.list.handle_key(key, self.entries.len())
    }

    pub(crate) fn render(&self, frame: &mut Frame, area: Rect, focused: bool) {
        let block = panel_block(focused, " Call stack ");
        let inner = block.inner(area);
        let visible_height = inner.height as usize;

        let mut list = self.list.clone_for_render();
        list.ensure_visible(visible_height);

        let lines: Vec<Line> = self
            .entries
            .iter()
            .enumerate()
            .skip(list.scroll)
            .take(visible_height)
            .map(|(i, entry)| {
                let mut style = if entry.is_top {
                    Style::default().fg(Color::Yellow)
                } else {
                    Style::default().fg(Color::White)
                };
                if focused && i == self.list.focus {
                    style = style.bg(Color::Rgb(0x36, 0x3a, 0x4f));
                }
                Line::from(Span::styled(&entry.label, style))
            })
            .collect();

        super::render_lines(frame, area, block, &lines);
    }
}

// --- Locals Panel ---

pub(crate) struct LocalsPanel {
    list: ListState,
    entries: Vec<LocalEntry>,
    changed: HashSet<String>,
    changed_at: Option<Instant>,
}

struct LocalEntry {
    name: String,
    display: String,
}

impl LocalsPanel {
    fn new() -> Self {
        Self {
            list: ListState::new(),
            entries: Vec::new(),
            changed: HashSet::new(),
            changed_at: None,
        }
    }

    pub(crate) fn update(&mut self, state: &VmState) {
        let old_entries = std::mem::take(&mut self.entries);

        if let Some(top_frame) = state.call_stack.last()
            && let FrameKind::Lua { chunk, ip } = &top_frame.kind
        {
            let chunk_snap = &state.chunks[*chunk as usize];
            self.entries = chunk_snap
                .local_vars
                .iter()
                .filter(|var| *ip >= var.start_ip && *ip <= var.end_ip)
                .map(|var| {
                    let reg = var.register as usize;
                    let abs_idx = top_frame.frame_pointer + reg;
                    let value = state
                        .stack
                        .get(abs_idx)
                        .cloned()
                        .unwrap_or(crate::value::LuaValue::NIL);
                    LocalEntry {
                        name: var.name.clone(),
                        display: format!("{value}"),
                    }
                })
                .collect();
        }

        let mut new_changed = HashSet::new();
        for entry in &self.entries {
            let old_display = old_entries
                .iter()
                .find(|e| e.name == entry.name)
                .map(|e| &e.display);
            if old_display != Some(&entry.display) {
                new_changed.insert(entry.name.clone());
            }
        }
        if !new_changed.is_empty() {
            self.changed = new_changed;
            self.changed_at = Some(Instant::now());
        }

        if self.list.focus >= self.entries.len() {
            self.list.focus = self.entries.len().saturating_sub(1);
        }
    }

    pub(crate) fn handle_key(&mut self, key: KeyEvent) -> KeyResult {
        self.list.handle_key(key, self.entries.len())
    }

    pub(crate) fn render(&self, frame: &mut Frame, area: Rect, focused: bool) {
        let block = panel_block(focused, " Locals ");
        let inner = block.inner(area);
        let visible_height = inner.height as usize;

        let mut list = self.list.clone_for_render();
        list.ensure_visible(visible_height);

        let intensity = self.changed_at.map_or(0.0, change_intensity);

        let lines: Vec<Line> = self
            .entries
            .iter()
            .enumerate()
            .skip(list.scroll)
            .take(visible_height)
            .map(|(i, entry)| {
                let is_changed = intensity > 0.0 && self.changed.contains(&entry.name);
                let value_fg = if is_changed {
                    change_color(intensity)
                } else {
                    Color::White
                };

                let mut bg = None;
                if focused && i == self.list.focus {
                    bg = Some(Color::Rgb(0x36, 0x3a, 0x4f));
                }

                Line::from(vec![
                    styled_span(format!("  {} ", entry.name), Color::Yellow, bg),
                    styled_span("= ".to_string(), Color::DarkGray, bg),
                    styled_span(entry.display.clone(), value_fg, bg),
                ])
            })
            .collect();

        super::render_lines(frame, area, block, &lines);
    }
}

// --- Registers Panel ---

pub(crate) struct RegistersPanel {
    list: ListState,
    entries: Vec<String>,
    changed: HashSet<usize>,
    changed_at: Option<Instant>,
}

impl RegistersPanel {
    fn new() -> Self {
        Self {
            list: ListState::new(),
            entries: Vec::new(),
            changed: HashSet::new(),
            changed_at: None,
        }
    }

    pub(crate) fn update(&mut self, state: &VmState) {
        let old_entries = std::mem::take(&mut self.entries);

        if let Some(top_frame) = state.call_stack.last() {
            let end = state
                .stack
                .len()
                .min(top_frame.frame_pointer + top_frame.max_registers as usize);
            self.entries = (top_frame.frame_pointer..end)
                .map(|i| format!("{}", state.stack[i]))
                .collect();
        }

        let mut new_changed = HashSet::new();
        for (i, entry) in self.entries.iter().enumerate() {
            if old_entries.get(i) != Some(entry) {
                new_changed.insert(i);
            }
        }
        if !new_changed.is_empty() {
            self.changed = new_changed;
            self.changed_at = Some(Instant::now());
        }

        if self.list.focus >= self.entries.len() {
            self.list.focus = self.entries.len().saturating_sub(1);
        }
    }

    pub(crate) fn handle_key(&mut self, key: KeyEvent) -> KeyResult {
        self.list.handle_key(key, self.entries.len())
    }

    pub(crate) fn render(&self, frame: &mut Frame, area: Rect, focused: bool) {
        let block = panel_block(focused, " Registers ");
        let inner = block.inner(area);
        let visible_height = inner.height as usize;

        let mut list = self.list.clone_for_render();
        list.ensure_visible(visible_height);

        let intensity = self.changed_at.map_or(0.0, change_intensity);

        let lines: Vec<Line> = self
            .entries
            .iter()
            .enumerate()
            .skip(list.scroll)
            .take(visible_height)
            .map(|(i, display)| {
                let is_changed = intensity > 0.0 && self.changed.contains(&i);
                let value_fg = if is_changed {
                    change_color(intensity)
                } else {
                    Color::White
                };

                let mut bg = None;
                if focused && i == self.list.focus {
                    bg = Some(Color::Rgb(0x36, 0x3a, 0x4f));
                }

                Line::from(vec![
                    styled_span(format!("  R{i:<3}"), Color::DarkGray, bg),
                    styled_span(display.clone(), value_fg, bg),
                ])
            })
            .collect();

        super::render_lines(frame, area, block, &lines);
    }
}

// --- StatePanels container ---

pub(crate) struct StatePanels {
    pub call_stack: CallStackPanel,
    pub locals: LocalsPanel,
    pub registers: RegistersPanel,
}

impl StatePanels {
    pub(crate) fn new() -> Self {
        Self {
            call_stack: CallStackPanel::new(),
            locals: LocalsPanel::new(),
            registers: RegistersPanel::new(),
        }
    }

    pub(crate) fn update(&mut self, state: &VmState) {
        self.call_stack.update(state);
        self.locals.update(state);
        self.registers.update(state);
    }

    pub(crate) fn render(&self, frame: &mut Frame, area: Rect, active_panel: usize) {
        let rows = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Percentage(25),
                Constraint::Percentage(25),
                Constraint::Percentage(50),
            ])
            .split(area);

        self.call_stack
            .render(frame, rows[0], active_panel == super::PANEL_CALL_STACK);
        self.locals
            .render(frame, rows[1], active_panel == super::PANEL_LOCALS);
        self.registers
            .render(frame, rows[2], active_panel == super::PANEL_REGISTERS);
    }
}

// --- Helpers ---

impl ListState {
    fn clone_for_render(&self) -> ListState {
        ListState {
            focus: self.focus,
            scroll: self.scroll,
        }
    }
}

fn panel_block(focused: bool, title: &str) -> Block<'_> {
    let border_style = if focused {
        Style::default().fg(Color::Cyan)
    } else {
        Style::default().fg(Color::DarkGray)
    };
    Block::default()
        .borders(Borders::ALL)
        .border_style(border_style)
        .title(title)
}

fn styled_span(text: String, fg: Color, bg: Option<Color>) -> Span<'static> {
    let style = Style::default().fg(fg);
    let style = match bg {
        Some(c) => style.bg(c),
        None => style,
    };
    Span::styled(text, style)
}
