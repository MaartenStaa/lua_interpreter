use ratatui::{
    Frame,
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders},
};

use crate::debugger::protocol::StateSnapshot;

use super::AlignedRow;

pub(super) fn render_bytecode_panel(
    frame: &mut Frame,
    area: Rect,
    snapshot: &StateSnapshot,
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
        .title(" Bytecode ");

    let inner = block.inner(area);
    let visible_height = inner.height as usize;

    let lines: Vec<Line> = snapshot
        .bytecode_lines
        .iter()
        .skip(scroll)
        .take(visible_height)
        .map(|bl| {
            let is_current = bl.ip == snapshot.current_ip;
            let bg = if is_current {
                Color::Rgb(50, 50, 15)
            } else {
                Color::Reset
            };

            let marker = if is_current { ">" } else { " " };

            Line::from(vec![
                Span::styled(
                    marker,
                    Style::default()
                        .fg(Color::Yellow)
                        .bg(bg)
                        .add_modifier(if is_current {
                            Modifier::BOLD
                        } else {
                            Modifier::empty()
                        }),
                ),
                Span::styled(
                    format!(" {} ", bl.address),
                    Style::default().fg(Color::DarkGray).bg(bg),
                ),
                Span::styled(
                    format!("{} ", bl.raw),
                    Style::default().fg(Color::Rgb(100, 100, 100)).bg(bg),
                ),
                Span::styled(
                    format!("{:<18}", bl.opcode_name),
                    Style::default()
                        .fg(Color::Blue)
                        .bg(bg)
                        .add_modifier(if is_current {
                            Modifier::BOLD
                        } else {
                            Modifier::empty()
                        }),
                ),
                Span::styled(
                    bl.operands.clone(),
                    Style::default().fg(Color::Yellow).bg(bg),
                ),
            ])
        })
        .collect();

    super::render_lines(frame, area, block, &lines);
}

pub(super) fn render_aligned_bytecode_panel(
    frame: &mut Frame,
    area: Rect,
    snapshot: &StateSnapshot,
    rows: &[AlignedRow<'_>],
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
        .title(" Bytecode (aligned) ");

    let inner = block.inner(area);
    let visible_height = inner.height as usize;

    let lines: Vec<Line> = rows
        .iter()
        .skip(scroll)
        .take(visible_height)
        .map(|row| {
            let bytecode = match row {
                AlignedRow::SourceWithBytecode { bytecode, .. }
                | AlignedRow::BytecodeOnly { bytecode } => Some(*bytecode),
                AlignedRow::SourceOnly { .. } => None,
            };

            let Some(bl) = bytecode else {
                return Line::from("");
            };

            let is_current = bl.ip == snapshot.current_ip;
            let bg = if is_current {
                Color::Rgb(50, 50, 15)
            } else {
                Color::Reset
            };

            let marker = if is_current { ">" } else { " " };

            Line::from(vec![
                Span::styled(
                    marker,
                    Style::default()
                        .fg(Color::Yellow)
                        .bg(bg)
                        .add_modifier(if is_current {
                            Modifier::BOLD
                        } else {
                            Modifier::empty()
                        }),
                ),
                Span::styled(
                    format!(" {} ", bl.address),
                    Style::default().fg(Color::DarkGray).bg(bg),
                ),
                Span::styled(
                    format!("{} ", bl.raw),
                    Style::default().fg(Color::Rgb(100, 100, 100)).bg(bg),
                ),
                Span::styled(
                    format!("{:<18}", bl.opcode_name),
                    Style::default()
                        .fg(Color::Blue)
                        .bg(bg)
                        .add_modifier(if is_current {
                            Modifier::BOLD
                        } else {
                            Modifier::empty()
                        }),
                ),
                Span::styled(
                    bl.operands.clone(),
                    Style::default().fg(Color::Yellow).bg(bg),
                ),
            ])
        })
        .collect();

    super::render_lines(frame, area, block, &lines);
}
