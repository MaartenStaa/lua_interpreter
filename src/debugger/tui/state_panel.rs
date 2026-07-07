use ratatui::{
    Frame,
    layout::{Constraint, Layout, Rect},
    style::{Color, Style},
    text::{Line, Span},
    widgets::{Block, Borders},
};

use crate::debugger::protocol::{CallFrameKindSnapshot, StateSnapshot};

use super::ChangeTracker;

fn change_color(tracker: &ChangeTracker) -> Color {
    let t = tracker.intensity();
    if t <= 0.0 {
        return Color::White;
    }
    let r = (0xFF as f32 * (1.0 - t * 0.3)) as u8;
    let g = (0xFF as f32 * (1.0 - t * 0.0)) as u8;
    let b = (0xFF as f32 * (1.0 - t * 0.7)) as u8;
    Color::Rgb(r, g, b)
}

pub(super) fn render_state_panel(
    frame: &mut Frame,
    area: Rect,
    snapshot: Option<&StateSnapshot>,
    _scroll: usize,
    focused: u8,
    changes: &ChangeTracker,
) {
    let rows = Layout::default()
        .direction(ratatui::layout::Direction::Vertical)
        .constraints([
            Constraint::Percentage(25),
            Constraint::Percentage(25),
            Constraint::Percentage(50),
        ])
        .split(area);

    let call_stack_area = rows[0];
    let locals_area = rows[1];
    let stack_area = rows[2];

    render_call_stack_panel(frame, call_stack_area, snapshot, focused == 2);
    render_locals_panel(frame, locals_area, snapshot, changes, focused == 3);
    render_stack_panel(frame, stack_area, snapshot, changes, focused == 4);
}

fn panel(focused: bool, title: &str) -> Block<'_> {
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

fn render_call_stack_panel(
    frame: &mut Frame,
    area: Rect,
    snapshot: Option<&StateSnapshot>,
    focused: bool,
) {
    let block = panel(focused, " Call stack ");
    let Some(snapshot) = snapshot else {
        super::render_lines(frame, area, block, &[]);
        return;
    };

    let mut lines = Vec::new();
    for (i, frame_snap) in snapshot.call_stack.iter().enumerate().rev() {
        let is_top = i == snapshot.call_stack.len() - 1;
        let marker = if is_top { ">" } else { " " };

        let label = match &frame_snap.kind {
            CallFrameKindSnapshot::Lua { chunk_name, ip, .. } => {
                format!("{marker} #{i} {chunk_name}:{ip}")
            }
            CallFrameKindSnapshot::Native { name } => {
                format!("{marker} #{i} [native] {name}")
            }
        };

        let style = if is_top {
            Style::default().fg(Color::Yellow)
        } else {
            Style::default().fg(Color::White)
        };

        lines.push(Line::from(Span::styled(label, style)));
    }

    super::render_lines(frame, area, block, &lines);
}

fn render_locals_panel(
    frame: &mut Frame,
    area: Rect,
    snapshot: Option<&StateSnapshot>,
    changes: &ChangeTracker,
    focused: bool,
) {
    let block = panel(focused, " Locals ");
    let Some(snapshot) = snapshot else {
        super::render_lines(frame, area, block, &[]);
        return;
    };

    let mut lines = Vec::new();

    if let Some(top_frame) = snapshot.call_stack.last() {
        for local in &top_frame.locals {
            let local_has_changed = changes.is_local_changed(&local.name);
            let value_fg = if local_has_changed {
                change_color(changes)
            } else {
                Color::White
            };
            let value_style = Style::default().fg(value_fg);

            lines.push(Line::from(vec![
                Span::styled(
                    format!("  {} ", local.name),
                    Style::default().fg(Color::Yellow),
                ),
                Span::styled("= ", Style::default().fg(Color::DarkGray)),
                Span::styled(format!("{}", local.value), value_style),
            ]));
        }
    }

    super::render_lines(frame, area, block, &lines);
}

fn render_stack_panel(
    frame: &mut Frame,
    area: Rect,
    snapshot: Option<&StateSnapshot>,
    changes: &ChangeTracker,
    focused: bool,
) {
    let block = panel(focused, " Registers ");
    let Some(snapshot) = snapshot else {
        super::render_lines(frame, area, block, &[]);
        return;
    };

    let mut lines = Vec::new();

    for (i, val) in snapshot.frame_stack.iter().enumerate() {
        let register_has_changed = changes.is_register_changed(i);
        let value_fg = if register_has_changed {
            change_color(changes)
        } else {
            Color::White
        };

        lines.push(Line::from(vec![
            Span::styled(format!("  R{i:<3}"), Style::default().fg(Color::DarkGray)),
            Span::styled(format!("{val}"), Style::default().fg(value_fg)),
        ]));
    }

    super::render_lines(frame, area, block, &lines);
}
