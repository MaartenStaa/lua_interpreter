use ratatui::{
    Frame,
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph},
};

use crate::debugger::protocol::StateSnapshot;

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

            let marker = if is_current { ">" } else { " " };
            let marker_style = Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD);

            let text_style = if is_current {
                Style::default()
                    .bg(Color::Rgb(60, 60, 20))
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default()
            };

            Line::from(vec![
                Span::styled(marker, marker_style),
                Span::styled(format!(" {}", bl.text), text_style),
            ])
        })
        .collect();

    frame.render_widget(Paragraph::new(lines).block(block), area);
}
