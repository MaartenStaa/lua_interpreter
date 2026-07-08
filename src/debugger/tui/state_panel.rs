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

use crate::{
    debugger::protocol::{FrameKind, VmState},
    value::{LuaObject, LuaValue},
};

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

// --- Scrollable list state ---

#[derive(Debug)]
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

    fn move_focus(&mut self, delta: isize, len: usize) -> bool {
        if len == 0 {
            return false;
        }
        let old = self.focus;
        if delta > 0 {
            self.focus = (self.focus + delta as usize).min(len - 1);
        } else {
            self.focus = self.focus.saturating_sub((-delta) as usize);
        }
        self.focus != old
    }

    fn ensure_visible(&mut self, visible_height: usize) {
        if visible_height == 0 {
            return;
        }
        if self.focus < self.scroll {
            self.scroll = self.focus;
        } else if self.focus >= self.scroll + visible_height {
            self.scroll = self.focus - visible_height + 1;
        }
    }

    fn clamp(&mut self, len: usize) {
        if len == 0 {
            self.focus = 0;
        } else if self.focus >= len {
            self.focus = len - 1;
        }
    }
}

// --- Tree view for expandable values ---

type TreePath = Vec<String>;

#[derive(Debug, Clone)]
struct TreeRow {
    path: TreePath,
    depth: usize,
    label: String,
    display: String,
    expandable: bool,
    expanded: bool,
}

#[derive(Debug)]
struct TreeView {
    expanded: HashSet<TreePath>,
}

impl TreeView {
    fn new() -> Self {
        Self {
            expanded: HashSet::new(),
        }
    }

    fn toggle(&mut self, path: &TreePath) {
        if self.expanded.contains(path) {
            self.expanded.retain(|p| !p.starts_with(path));
        } else {
            self.expanded.insert(path.clone());
        }
    }

    fn prune_stale(&mut self, valid_roots: &HashSet<String>) {
        self.expanded
            .retain(|path| path.first().is_some_and(|root| valid_roots.contains(root)));
    }

    fn flatten_value(
        &self,
        label: String,
        value: &LuaValue,
        path: &TreePath,
        depth: usize,
        rows: &mut Vec<TreeRow>,
    ) {
        let expandable = is_expandable(value);
        let expanded = expandable && self.expanded.contains(path);

        let display = if expandable {
            format_table_summary(value)
        } else {
            format!("{value}")
        };

        rows.push(TreeRow {
            path: path.clone(),
            depth,
            label,
            display,
            expandable,
            expanded,
        });

        if expanded {
            self.flatten_table_children(value, path, depth + 1, rows);
        }
    }

    fn flatten_table_children(
        &self,
        value: &LuaValue,
        parent_path: &TreePath,
        depth: usize,
        rows: &mut Vec<TreeRow>,
    ) {
        if !value.is_object() {
            return;
        }
        let obj_lock = unsafe { value.as_object() };
        let Ok(obj) = obj_lock.read() else {
            return;
        };
        let LuaObject::Table(table) = &*obj else {
            return;
        };

        for (key, val) in table.iter() {
            if val.is_nil() {
                continue;
            }
            let key_str = format_key(key);
            let mut child_path = parent_path.clone();
            child_path.push(key_str.clone());

            self.flatten_value(key_str, val, &child_path, depth, rows);
        }
    }
}

fn is_expandable(value: &LuaValue) -> bool {
    if !value.is_object() {
        return false;
    }
    let obj_lock = unsafe { value.as_object() };
    let Ok(obj) = obj_lock.read() else {
        return false;
    };
    matches!(&*obj, LuaObject::Table(t) if !t.is_empty())
}

fn format_table_summary(value: &LuaValue) -> String {
    if !value.is_object() {
        return format!("{value}");
    }
    let obj_lock = unsafe { value.as_object() };
    let Ok(obj) = obj_lock.read() else {
        return format!("{value}");
    };
    match &*obj {
        LuaObject::Table(table) => {
            let count = table.iter().filter(|(_, v)| !v.is_nil()).count();
            format!("table ({count} fields)")
        }
        _ => format!("{value}"),
    }
}

fn format_key(key: &LuaValue) -> String {
    if key.is_string() {
        String::from_utf8_lossy(unsafe { key.as_string() }).into_owned()
    } else {
        format!("[{key}]")
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

        self.list.clamp(self.entries.len());
    }

    pub(crate) fn handle_key(&mut self, key: KeyEvent) -> KeyResult {
        let len = self.entries.len();
        match key {
            KeyEvent {
                code: KeyCode::Char('j') | KeyCode::Down,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                self.list.move_focus(1, len);
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('k') | KeyCode::Up,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                self.list.move_focus(-1, len);
                KeyResult::Consumed
            }
            _ => KeyResult::Ignored,
        }
    }

    pub(crate) fn render(&self, frame: &mut Frame, area: Rect, focused: bool) {
        let block = panel_block(focused, " Call stack ");
        let inner = block.inner(area);
        let visible_height = inner.height as usize;

        let mut list = ListState {
            focus: self.list.focus,
            scroll: self.list.scroll,
        };
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

#[derive(Debug)]
pub(crate) struct LocalsPanel {
    list: ListState,
    tree: TreeView,
    rows: Vec<TreeRow>,
    entries: Vec<(String, LuaValue)>,
    changed: HashSet<String>,
    changed_at: Option<Instant>,
    prev_displays: Vec<(String, String)>,
}

impl LocalsPanel {
    fn new() -> Self {
        Self {
            list: ListState::new(),
            tree: TreeView::new(),
            rows: Vec::new(),
            entries: Vec::new(),
            changed: HashSet::new(),
            changed_at: None,
            prev_displays: Vec::new(),
        }
    }

    fn rebuild_rows(&mut self) {
        self.rows.clear();
        for (name, value) in &self.entries {
            self.tree
                .flatten_value(name.clone(), value, &vec![name.clone()], 0, &mut self.rows);
        }
        self.list.clamp(self.rows.len());
    }

    pub(crate) fn update(&mut self, state: &VmState) {
        let mut entries: Vec<(String, LuaValue)> = Vec::new();

        if let Some(top_frame) = state.call_stack.last()
            && let FrameKind::Lua { chunk, ip } = &top_frame.kind
        {
            let chunk_snap = &state.chunks[*chunk as usize];
            let nil = LuaValue::NIL;
            for var in &chunk_snap.local_vars {
                if *ip >= var.start_ip && *ip <= var.end_ip {
                    let abs_idx = top_frame.frame_pointer + var.register as usize;
                    let value = state.stack.get(abs_idx).unwrap_or(&nil);
                    let value = if value.is_upvalue() {
                        unsafe { value.as_upvalue() }.read().unwrap().0.clone()
                    } else {
                        value.clone()
                    };
                    entries.push((var.name.clone(), value));
                }
            }
        }

        // Detect changes
        let mut new_changed = HashSet::new();
        for (name, value) in &entries {
            let display = format!("{value}");
            let old = self
                .prev_displays
                .iter()
                .find(|(n, _)| n == name)
                .map(|(_, d)| d.as_str());
            if old != Some(&display) {
                new_changed.insert(name.clone());

                // If the type changed (e.g. table -> nil), collapse expansion
                let was_table = self
                    .rows
                    .iter()
                    .any(|r| r.path == vec![name.clone()] && r.expandable);
                let is_table = is_expandable(value);
                if was_table && !is_table {
                    self.tree.expanded.retain(|p| p.first() != Some(name));
                }
            }
        }
        if !new_changed.is_empty() {
            self.changed = new_changed;
            self.changed_at = Some(Instant::now());
        }

        // Prune expansion paths for locals that no longer exist
        let valid_roots: HashSet<String> = entries.iter().map(|(n, _)| n.clone()).collect();
        self.tree.prune_stale(&valid_roots);

        // Store displays for next diff
        self.prev_displays = entries
            .iter()
            .map(|(n, v)| (n.clone(), format!("{v}")))
            .collect();

        self.entries = entries;
        self.rebuild_rows();
    }

    pub(crate) fn handle_key(&mut self, key: KeyEvent) -> KeyResult {
        let len = self.rows.len();
        match key {
            KeyEvent {
                code: KeyCode::Char('j') | KeyCode::Down,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                self.list.move_focus(1, len);
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('k') | KeyCode::Up,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                self.list.move_focus(-1, len);
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('d'),
                modifiers: KeyModifiers::CONTROL,
                ..
            } => {
                self.list.move_focus(10, len);
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('u'),
                modifiers: KeyModifiers::CONTROL,
                ..
            } => {
                self.list.move_focus(-10, len);
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Enter,
                ..
            } => {
                if let Some(row) = self.rows.get(self.list.focus)
                    && row.expandable
                {
                    let path = row.path.clone();
                    self.tree.toggle(&path);
                    self.rebuild_rows();
                    KeyResult::Consumed
                } else {
                    KeyResult::Ignored
                }
            }
            _ => KeyResult::Ignored,
        }
    }

    pub(crate) fn render(&self, frame: &mut Frame, area: Rect, focused: bool) {
        let block = panel_block(focused, " Locals ");
        let inner = block.inner(area);
        let visible_height = inner.height as usize;

        let mut list = ListState {
            focus: self.list.focus,
            scroll: self.list.scroll,
        };
        list.ensure_visible(visible_height);

        let intensity = self.changed_at.map_or(0.0, change_intensity);

        let lines: Vec<Line> = self
            .rows
            .iter()
            .enumerate()
            .skip(list.scroll)
            .take(visible_height)
            .flat_map(|(i, row)| {
                let root = row.path.first().map_or("", String::as_str);
                let is_changed = intensity > 0.0 && self.changed.contains(root);
                let value_fg = if is_changed {
                    change_color(intensity)
                } else {
                    Color::White
                };

                let bg = if focused && i == self.list.focus {
                    Some(Color::Rgb(0x36, 0x3a, 0x4f))
                } else {
                    None
                };

                render_tree_row(row, value_fg, bg)
            })
            .collect();

        super::render_lines(frame, area, block, &lines);
    }
}

// --- Registers Panel ---

pub(crate) struct RegistersPanel {
    list: ListState,
    tree: TreeView,
    rows: Vec<TreeRow>,
    entries: Vec<(String, LuaValue)>,
    changed: HashSet<usize>,
    changed_at: Option<Instant>,
    prev_displays: Vec<String>,
}

impl RegistersPanel {
    fn new() -> Self {
        Self {
            list: ListState::new(),
            tree: TreeView::new(),
            rows: Vec::new(),
            entries: Vec::new(),
            changed: HashSet::new(),
            changed_at: None,
            prev_displays: Vec::new(),
        }
    }

    fn rebuild_rows(&mut self) {
        self.rows.clear();
        for (name, value) in &self.entries {
            self.tree
                .flatten_value(name.clone(), value, &vec![name.clone()], 0, &mut self.rows);
        }
        self.list.clamp(self.rows.len());
    }

    pub(crate) fn update(&mut self, state: &VmState) {
        let mut entries: Vec<(String, LuaValue)> = Vec::new();

        if let Some(top_frame) = state.call_stack.last() {
            let end = state
                .stack
                .len()
                .min(top_frame.frame_pointer + top_frame.max_registers as usize);
            for (i, abs_idx) in (top_frame.frame_pointer..end).enumerate() {
                let value = &state.stack[abs_idx];
                let value = if value.is_upvalue() {
                    unsafe { value.as_upvalue() }.read().unwrap().0.clone()
                } else {
                    value.clone()
                };
                entries.push((format!("R{i}"), value));
            }
        }

        // Detect changes
        let mut new_changed = HashSet::new();
        for (i, (_, value)) in entries.iter().enumerate() {
            let display = format!("{value}");
            if self.prev_displays.get(i) != Some(&display) {
                new_changed.insert(i);

                let reg_name = format!("R{i}");
                let was_table = self
                    .rows
                    .iter()
                    .any(|r| r.path == vec![reg_name.clone()] && r.expandable);
                let is_table = is_expandable(value);
                if was_table && !is_table {
                    self.tree.expanded.retain(|p| p.first() != Some(&reg_name));
                }
            }
        }
        if !new_changed.is_empty() {
            self.changed = new_changed;
            self.changed_at = Some(Instant::now());
        }

        let valid_roots: HashSet<String> = entries.iter().map(|(n, _)| n.clone()).collect();
        self.tree.prune_stale(&valid_roots);

        self.prev_displays = entries.iter().map(|(_, v)| format!("{v}")).collect();

        self.entries = entries;
        self.rebuild_rows();
    }

    pub(crate) fn handle_key(&mut self, key: KeyEvent) -> KeyResult {
        let len = self.rows.len();
        match key {
            KeyEvent {
                code: KeyCode::Char('j') | KeyCode::Down,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                self.list.move_focus(1, len);
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('k') | KeyCode::Up,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                self.list.move_focus(-1, len);
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('d'),
                modifiers: KeyModifiers::CONTROL,
                ..
            } => {
                self.list.move_focus(10, len);
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('u'),
                modifiers: KeyModifiers::CONTROL,
                ..
            } => {
                self.list.move_focus(-10, len);
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Enter,
                ..
            } => {
                if let Some(row) = self.rows.get(self.list.focus)
                    && row.expandable
                {
                    let path = row.path.clone();
                    self.tree.toggle(&path);
                    self.rebuild_rows();
                    KeyResult::Consumed
                } else {
                    KeyResult::Ignored
                }
            }
            _ => KeyResult::Ignored,
        }
    }

    pub(crate) fn render(&self, frame: &mut Frame, area: Rect, focused: bool) {
        let block = panel_block(focused, " Registers ");
        let inner = block.inner(area);
        let visible_height = inner.height as usize;

        let mut list = ListState {
            focus: self.list.focus,
            scroll: self.list.scroll,
        };
        list.ensure_visible(visible_height);

        let intensity = self.changed_at.map_or(0.0, change_intensity);

        let lines: Vec<Line> = self
            .rows
            .iter()
            .enumerate()
            .skip(list.scroll)
            .take(visible_height)
            .flat_map(|(i, row)| {
                let root_idx: usize = row
                    .path
                    .first()
                    .and_then(|r| r.strip_prefix('R'))
                    .and_then(|n| n.parse().ok())
                    .unwrap_or(usize::MAX);
                let is_changed = intensity > 0.0 && self.changed.contains(&root_idx);
                let value_fg = if is_changed {
                    change_color(intensity)
                } else {
                    Color::White
                };

                let bg = if focused && i == self.list.focus {
                    Some(Color::Rgb(0x36, 0x3a, 0x4f))
                } else {
                    None
                };

                render_tree_row(row, value_fg, bg)
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

// --- Rendering helpers ---

fn render_tree_row<'a>(row: &TreeRow, value_fg: Color, bg: Option<Color>) -> Vec<Line<'a>> {
    let indent = "  ".repeat(row.depth);
    let toggle = if row.expandable {
        if row.expanded {
            "\u{25bc} "
        } else {
            "\u{25b6} "
        }
    } else {
        "  "
    };

    let result = vec![Line::from(vec![
        styled_span(format!("  {indent}{toggle}"), Color::DarkGray, bg),
        styled_span(format!("{} ", row.label), Color::Yellow, bg),
        styled_span("= ".to_string(), Color::DarkGray, bg),
        styled_span(row.display.clone(), value_fg, bg),
    ])];

    // if let Some(value) = &row.value {
    //     let debug_text = format!("{:#?}", value);
    //     for l in debug_text.lines() {
    //         result.push(Line::from(styled_span(
    //             format!("  {indent}  {l}"),
    //             Color::DarkGray,
    //             bg,
    //         )));
    //     }
    // }

    result
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
