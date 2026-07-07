mod bytecode;
mod help;
mod source;
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
    widgets::{Block, Borders, Paragraph, Widget},
};

use super::protocol::{Breakpoint, DebugCommand, DebugEvent, StateSnapshot};

fn render_lines(frame: &mut Frame, area: Rect, block: Block<'_>, lines: &[Line<'_>]) {
    let inner = block.inner(area);
    let buf = frame.buffer_mut();
    block.render(area, buf);

    let paragraph = Paragraph::new(lines.to_vec());
    frame.render_widget(paragraph, inner);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PanelMode {
    Source,
    Bytecode,
    Split,
    Aligned,
}

impl PanelMode {
    fn cycle(self) -> Self {
        match self {
            Self::Source => Self::Bytecode,
            Self::Bytecode => Self::Split,
            Self::Split => Self::Aligned,
            Self::Aligned => Self::Source,
        }
    }

    fn label(self) -> &'static str {
        match self {
            Self::Source => "Source",
            Self::Bytecode => "Bytecode",
            Self::Split => "Source | Bytecode",
            Self::Aligned => "Aligned",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ActivePanel {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RunState {
    Running,
    Finished,
    Error,
}

pub(crate) struct App {
    snapshot: Option<StateSnapshot>,
    panel_mode: PanelMode,
    active_panel: ActivePanel,
    left_scroll: usize,
    right_scroll: usize,
    status: String,
    show_help: bool,
    quit: bool,
    run_state: RunState,
}

impl App {
    fn new() -> Self {
        Self {
            snapshot: None,
            panel_mode: PanelMode::Source,
            active_panel: ActivePanel::Left,
            left_scroll: 0,
            right_scroll: 0,
            status: "Waiting for VM...".to_string(),
            show_help: false,
            quit: false,
            run_state: RunState::Running,
        }
    }

    fn update_snapshot(&mut self, snapshot: StateSnapshot) {
        if let Some(current_line) = Self::current_source_line(&snapshot) {
            self.left_scroll = current_line.saturating_sub(5);
        }

        match self.panel_mode {
            PanelMode::Bytecode | PanelMode::Aligned => {
                self.left_scroll = snapshot.current_ip.saturating_sub(5);
            }
            PanelMode::Split | PanelMode::Source => {}
        }

        let filename = snapshot.filename.as_deref().unwrap_or("<unknown>");
        let line_info = Self::current_source_line(&snapshot)
            .map(|l| format!(":{l}"))
            .unwrap_or_default();
        self.status = format!(
            "Paused at {filename}{line_info} (chunk={}, ip={})",
            snapshot.current_chunk, snapshot.current_ip
        );
        self.snapshot = Some(snapshot);
    }

    fn current_source_line(snapshot: &StateSnapshot) -> Option<usize> {
        snapshot
            .source_map
            .ip_to_line
            .get(&snapshot.current_ip)
            .copied()
    }

    fn active_scroll_mut(&mut self) -> &mut usize {
        match self.active_panel {
            ActivePanel::Left => &mut self.left_scroll,
            ActivePanel::Right => &mut self.right_scroll,
        }
    }

    fn scroll_down(&mut self, amount: usize) {
        let scroll = self.active_scroll_mut();
        *scroll = scroll.saturating_add(amount);
    }

    fn scroll_up(&mut self, amount: usize) {
        let scroll = self.active_scroll_mut();
        *scroll = scroll.saturating_sub(amount);
    }

    fn render(&self, frame: &mut Frame) {
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

        self.render_left_panel(frame, main_columns[0]);
        self.render_right_panel(frame, main_columns[1]);
        self.render_status_bar(frame, status_area);

        if self.show_help {
            help::render_help_overlay(frame);
        }
    }

    fn render_left_panel(&self, frame: &mut Frame, area: Rect) {
        let Some(snapshot) = &self.snapshot else {
            let block = Block::default().borders(Borders::ALL).title(" Waiting... ");
            render_lines(
                frame,
                area,
                block,
                &[Line::from("Waiting for VM to pause...")],
            );
            return;
        };

        match self.panel_mode {
            PanelMode::Source => {
                source::render_source_panel(
                    frame,
                    area,
                    snapshot,
                    self.left_scroll,
                    self.active_panel == ActivePanel::Left,
                );
            }
            PanelMode::Bytecode => {
                bytecode::render_bytecode_panel(
                    frame,
                    area,
                    snapshot,
                    self.left_scroll,
                    self.active_panel == ActivePanel::Left,
                );
            }
            PanelMode::Split => {
                let split = Layout::default()
                    .direction(Direction::Horizontal)
                    .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
                    .split(area);
                source::render_source_panel(
                    frame,
                    split[0],
                    snapshot,
                    self.left_scroll,
                    self.active_panel == ActivePanel::Left,
                );
                bytecode::render_bytecode_panel(frame, split[1], snapshot, self.left_scroll, false);
            }
            PanelMode::Aligned => {
                let split = Layout::default()
                    .direction(Direction::Horizontal)
                    .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
                    .split(area);
                source::render_aligned_source_panel(
                    frame,
                    split[0],
                    snapshot,
                    self.left_scroll,
                    self.active_panel == ActivePanel::Left,
                );
                bytecode::render_bytecode_panel(frame, split[1], snapshot, self.left_scroll, false);
            }
        }
    }

    fn render_right_panel(&self, frame: &mut Frame, area: Rect) {
        state_panel::render_state_panel(
            frame,
            area,
            self.snapshot.as_ref(),
            self.right_scroll,
            self.active_panel == ActivePanel::Right,
        );
    }

    fn render_status_bar(&self, frame: &mut Frame, area: Rect) {
        let mode_label = self.panel_mode.label();

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
                DebugEvent::Paused(snapshot) => {
                    app.update_snapshot(*snapshot);
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

            match key {
                KeyEvent {
                    code: KeyCode::Char('s'),
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    let _ = cmd_tx.send(DebugCommand::Step);
                }
                KeyEvent {
                    code: KeyCode::Char('n'),
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    let _ = cmd_tx.send(DebugCommand::StepOver);
                }
                KeyEvent {
                    code: KeyCode::Char('o'),
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    let _ = cmd_tx.send(DebugCommand::StepOut);
                }
                KeyEvent {
                    code: KeyCode::Char('c'),
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    let _ = cmd_tx.send(DebugCommand::Continue);
                }
                KeyEvent {
                    code: KeyCode::Char('q'),
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    let _ = cmd_tx.send(DebugCommand::Quit);
                    app.quit = true;
                }
                KeyEvent {
                    code: KeyCode::Tab, ..
                } => {
                    app.panel_mode = app.panel_mode.cycle();
                }
                KeyEvent {
                    code: KeyCode::Char('?'),
                    ..
                } => {
                    app.show_help = true;
                }
                KeyEvent {
                    code: KeyCode::Char('j') | KeyCode::Down,
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    app.scroll_down(1);
                }
                KeyEvent {
                    code: KeyCode::Char('k') | KeyCode::Up,
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    app.scroll_up(1);
                }
                KeyEvent {
                    code: KeyCode::Char('h') | KeyCode::Left,
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    app.active_panel = ActivePanel::Left;
                }
                KeyEvent {
                    code: KeyCode::Char('l') | KeyCode::Right,
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    app.active_panel = ActivePanel::Right;
                }
                KeyEvent {
                    code: KeyCode::Char('d'),
                    modifiers: KeyModifiers::CONTROL,
                    ..
                } => {
                    app.scroll_down(15);
                }
                KeyEvent {
                    code: KeyCode::Char('u'),
                    modifiers: KeyModifiers::CONTROL,
                    ..
                } => {
                    app.scroll_up(15);
                }
                KeyEvent {
                    code: KeyCode::Char('b'),
                    modifiers: KeyModifiers::NONE,
                    ..
                } => {
                    toggle_breakpoint(app, cmd_tx);
                }
                _ => {}
            }
        }
    }

    Ok(())
}

fn toggle_breakpoint(app: &App, cmd_tx: &Sender<DebugCommand>) {
    let Some(snapshot) = &app.snapshot else {
        return;
    };
    let Some(filename) = &snapshot.filename else {
        return;
    };

    let Some(current_line) = snapshot
        .source_map
        .ip_to_line
        .get(&snapshot.current_ip)
        .copied()
    else {
        return;
    };

    let bp = Breakpoint::SourceLine {
        filename: filename.clone(),
        line: current_line,
    };

    if snapshot.breakpoints.contains(&bp) {
        let _ = cmd_tx.send(DebugCommand::RemoveBreakpoint(bp));
    } else {
        let _ = cmd_tx.send(DebugCommand::AddBreakpoint(bp));
    }
}
