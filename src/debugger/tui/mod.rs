mod bytecode_fmt;
mod code_panel;
mod colors;
mod help;
mod state_panel;

use std::{
    sync::mpsc::{Receiver, Sender},
    time::Duration,
};

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use ratatui::{
    Frame,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    text::{Line, Span},
    widgets::{Block, Paragraph, Widget},
};

use super::protocol::{DebugCommand, DebugEvent, VmState};

pub(crate) fn render_lines(frame: &mut Frame, area: Rect, block: Block<'_>, lines: &[Line<'_>]) {
    let inner = block.inner(area);
    let buf = frame.buffer_mut();
    block.render(area, buf);

    let paragraph = Paragraph::new(lines.to_vec());
    frame.render_widget(paragraph, inner);
}

pub(crate) enum KeyResult {
    Consumed,
    Command(DebugCommand),
    Ignored,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RunState {
    Running,
    Finished,
    Error,
}

struct App {
    vm_state: Option<VmState>,
    code_panel: code_panel::CodePanel,
    state_panels: state_panel::StatePanels,
    active_panel: usize,
    status: String,
    show_help: bool,
    quit: bool,
    run_state: RunState,
}

const PANEL_CODE: usize = 0;
const PANEL_CALL_STACK: usize = 1;
const PANEL_LOCALS: usize = 2;
const PANEL_REGISTERS: usize = 3;
const NUM_PANELS: usize = 4;

impl App {
    fn new() -> Self {
        Self {
            vm_state: None,
            code_panel: code_panel::CodePanel::new(),
            state_panels: state_panel::StatePanels::new(),
            active_panel: PANEL_CODE,
            status: "Waiting for VM...".to_string(),
            show_help: false,
            quit: false,
            run_state: RunState::Running,
        }
    }

    fn update_state(&mut self, state: VmState) {
        self.code_panel.update(&state);
        self.state_panels.update(&state);

        let chunk = &state.chunks[state.current_chunk as usize];
        let filename = chunk.filename.as_deref().unwrap_or("<unknown>");
        let line_info = self
            .code_panel
            .current_source_line()
            .map(|l| format!(":{l}"))
            .unwrap_or_default();
        self.status = format!(
            "Paused at {filename}{line_info} (chunk={}, ip={})",
            state.current_chunk, state.current_ip
        );
        self.vm_state = Some(state);
    }

    fn render(&mut self, frame: &mut Frame) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Min(5), Constraint::Length(1)])
            .split(frame.area());

        let main_area = chunks[0];
        let status_area = chunks[1];

        let main_columns = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(70), Constraint::Percentage(30)])
            .split(main_area);

        self.code_panel
            .render(frame, main_columns[0], self.active_panel == PANEL_CODE);
        self.state_panels
            .render(frame, main_columns[1], self.active_panel);
        self.render_status_bar(frame, status_area);

        if self.show_help {
            help::render_help_overlay(frame);
        }
    }

    fn render_status_bar(&self, frame: &mut Frame, area: Rect) {
        let mode_label = self.code_panel.mode_label();

        let keys = match self.run_state {
            RunState::Running => "s:step n:next o:out c:cont q:quit",
            RunState::Finished | RunState::Error => "q:quit",
        };

        let line = Line::from(vec![
            Span::styled(
                format!(" {}", self.status),
                Style::default().bg(Color::DarkGray).fg(Color::White),
            ),
            Span::styled("  ", Style::default().bg(Color::DarkGray)),
            Span::styled(
                format!("[{mode_label}]"),
                Style::default().bg(Color::DarkGray).fg(Color::Cyan),
            ),
            Span::styled("  ", Style::default().bg(Color::DarkGray)),
            Span::styled(keys, Style::default().bg(Color::DarkGray).fg(Color::White)),
            Span::styled("  ", Style::default().bg(Color::DarkGray)),
            Span::styled(
                "?:help",
                Style::default().bg(Color::DarkGray).fg(Color::White),
            ),
        ]);

        let buf = frame.buffer_mut();
        let mut x = area.x;
        for span in &line.spans {
            for ch in span.content.chars() {
                if x >= area.right() {
                    break;
                }
                buf[(x, area.y)].set_char(ch).set_style(span.style);
                x += 1;
            }
        }
        let fill_style = Style::default().bg(Color::DarkGray);
        while x < area.right() {
            buf[(x, area.y)].set_char(' ').set_style(fill_style);
            x += 1;
        }
    }

    fn cycle_focus(&mut self, forward: bool) {
        if forward {
            self.active_panel = (self.active_panel + 1) % NUM_PANELS;
        } else {
            let prev = (self.active_panel + NUM_PANELS - 1) % NUM_PANELS;
            self.active_panel = prev;
            if prev == PANEL_CODE {
                self.code_panel.focus_rightmost();
            }
        }
    }
}

pub(crate) fn run_tui(
    cmd_tx: &Sender<DebugCommand>,
    evt_rx: &Receiver<DebugEvent>,
) -> std::io::Result<()> {
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let _ = ratatui::try_restore();
        hook(info);
    }));

    let mut terminal = ratatui::init();
    let mut app = App::new();

    let result = run_event_loop(&mut terminal, &mut app, cmd_tx, evt_rx);

    ratatui::restore();
    result
}

fn run_event_loop(
    terminal: &mut ratatui::DefaultTerminal,
    app: &mut App,
    cmd_tx: &Sender<DebugCommand>,
    evt_rx: &Receiver<DebugEvent>,
) -> std::io::Result<()> {
    loop {
        while let Ok(evt) = evt_rx.try_recv() {
            match evt {
                DebugEvent::Paused(state) => {
                    app.update_state(*state);
                }
                DebugEvent::Completed => {
                    app.status = "Execution completed. Press q to exit.".to_string();
                    app.run_state = RunState::Finished;
                }
                DebugEvent::Error(msg) => {
                    app.status = format!("Error: {msg} — Press q to exit.");
                    app.run_state = RunState::Error;
                }
            }
        }

        if app.quit {
            break;
        }

        terminal.draw(|frame| app.render(frame))?;

        if event::poll(Duration::from_millis(50))?
            && let Event::Key(key) = event::read()?
        {
            if app.show_help {
                if matches!(key.code, KeyCode::Esc | KeyCode::Char('?')) {
                    app.show_help = false;
                }
                continue;
            }

            if app.run_state != RunState::Running {
                if matches!(key.code, KeyCode::Char('q') | KeyCode::Esc) {
                    app.quit = true;
                }
                continue;
            }

            // Global keys first
            match key {
                KeyEvent {
                    code: KeyCode::Char('s'),
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    let _ = cmd_tx.send(DebugCommand::Step);
                    continue;
                }
                KeyEvent {
                    code: KeyCode::Char('n'),
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    let _ = cmd_tx.send(DebugCommand::StepOver);
                    continue;
                }
                KeyEvent {
                    code: KeyCode::Char('o'),
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    let _ = cmd_tx.send(DebugCommand::StepOut);
                    continue;
                }
                KeyEvent {
                    code: KeyCode::Char('c'),
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    let _ = cmd_tx.send(DebugCommand::Continue);
                    continue;
                }
                KeyEvent {
                    code: KeyCode::Char('q'),
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    let _ = cmd_tx.send(DebugCommand::Quit);
                    app.quit = true;
                    continue;
                }
                KeyEvent {
                    code: KeyCode::Char('?'),
                    ..
                } => {
                    app.show_help = true;
                    continue;
                }
                _ => {}
            }

            // Delegate to focused panel
            let result = match app.active_panel {
                PANEL_CODE => app.code_panel.handle_key(key),
                PANEL_CALL_STACK => app.state_panels.call_stack.handle_key(key),
                PANEL_LOCALS => app.state_panels.locals.handle_key(key),
                PANEL_REGISTERS => app.state_panels.registers.handle_key(key),
                _ => KeyResult::Ignored,
            };

            match result {
                KeyResult::Command(cmd) => {
                    let _ = cmd_tx.send(cmd);
                }
                KeyResult::Ignored => {
                    // h/l not consumed by panel → cycle App focus
                    match key {
                        KeyEvent {
                            code: KeyCode::Char('l') | KeyCode::Right,
                            modifiers: KeyModifiers::NONE,
                            ..
                        } => app.cycle_focus(true),
                        KeyEvent {
                            code: KeyCode::Char('h') | KeyCode::Left,
                            modifiers: KeyModifiers::NONE,
                            ..
                        } => app.cycle_focus(false),
                        _ => {}
                    }
                }
                KeyResult::Consumed => {}
            }
        }
    }

    Ok(())
}
