use ratatui::{
    Frame,
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders},
};

use crate::debugger::protocol::{CallFrameKindSnapshot, StateSnapshot};

pub(super) fn render_state_panel(
    frame: &mut Frame,
    area: Rect,
    snapshot: Option<&StateSnapshot>,
    scroll: usize,
    focused: bool,
) {
    let border_style = if focused {
        Style::default().fg(Color::Cyan)
    } else {
        Style::default().fg(Color::DarkGray)
    };

    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(border_style)
        .title(" State ");

    let Some(snapshot) = snapshot else {
        super::render_lines(frame, area, block, &[]);
        return;
    };

    let mut lines: Vec<Line> = Vec::new();

    lines.push(Line::from(Span::styled(
        "Call Stack",
        Style::default()
            .fg(Color::Cyan)
            .add_modifier(Modifier::BOLD),
    )));

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

    lines.push(Line::from(""));

    if let Some(top_frame) = snapshot.call_stack.last()
        && !top_frame.locals.is_empty()
    {
        lines.push(Line::from(Span::styled(
            "Locals",
            Style::default()
                .fg(Color::Cyan)
                .add_modifier(Modifier::BOLD),
        )));

        for local in &top_frame.locals {
            lines.push(Line::from(vec![
                Span::styled(
                    format!("  {} ", local.name),
                    Style::default().fg(Color::Yellow),
                ),
                Span::styled("= ", Style::default().fg(Color::DarkGray)),
                Span::styled(
                    format!("{}", local.value),
                    Style::default().fg(Color::White),
                ),
            ]));
        }

        lines.push(Line::from(""));
    }

    lines.push(Line::from(Span::styled(
        "Registers",
        Style::default()
            .fg(Color::Cyan)
            .add_modifier(Modifier::BOLD),
    )));

    for (i, val) in snapshot.frame_stack.iter().enumerate() {
        lines.push(Line::from(vec![
            Span::styled(format!("  R{i:<3}"), Style::default().fg(Color::DarkGray)),
            Span::styled(format!("{val}"), Style::default().fg(Color::White)),
        ]));
    }

    let display_lines: Vec<Line> = lines.into_iter().skip(scroll).collect();

    super::render_lines(frame, area, block, &display_lines);
}
