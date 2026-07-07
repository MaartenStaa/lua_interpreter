use ratatui::{
    Frame,
    layout::{Constraint, Flex, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Clear, Paragraph},
};

pub(super) fn render_help_overlay(frame: &mut Frame) {
    let area = centered_rect(60, 70, frame.area());

    frame.render_widget(Clear, area);

    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::Cyan))
        .title(" Keyboard Shortcuts ")
        .title_bottom(" Esc or ? to close ");

    let section_style = Style::default()
        .fg(Color::Cyan)
        .add_modifier(Modifier::BOLD);
    let key_style = Style::default()
        .fg(Color::Yellow)
        .add_modifier(Modifier::BOLD);
    let desc_style = Style::default().fg(Color::White);
    let lines = vec![
        Line::from(""),
        Line::from(Span::styled("  Execution", section_style)),
        Line::from(""),
        help_line("  s", "Step one instruction", key_style, desc_style),
        help_line("  n", "Step over (next)", key_style, desc_style),
        help_line("  o", "Step out", key_style, desc_style),
        help_line("  c", "Continue to next breakpoint", key_style, desc_style),
        help_line("  q", "Quit debugger", key_style, desc_style),
        Line::from(""),
        Line::from(Span::styled("  Navigation", section_style)),
        Line::from(""),
        help_line("  j / Down", "Scroll down", key_style, desc_style),
        help_line("  k / Up", "Scroll up", key_style, desc_style),
        help_line("  h / Left", "Focus left panel", key_style, desc_style),
        help_line("  l / Right", "Focus right panel", key_style, desc_style),
        help_line("  Ctrl+d", "Scroll down half page", key_style, desc_style),
        help_line("  Ctrl+u", "Scroll up half page", key_style, desc_style),
        Line::from(""),
        Line::from(Span::styled("  View", section_style)),
        Line::from(""),
        help_line(
            "  Tab",
            "Cycle: Source / Bytecode / Split / Aligned",
            key_style,
            desc_style,
        ),
        help_line(
            "  b",
            "Toggle breakpoint on current line",
            key_style,
            desc_style,
        ),
        help_line(
            "  Enter",
            "Expand/collapse table in locals/registers",
            key_style,
            desc_style,
        ),
        help_line("  ?", "Show/hide this help", key_style, desc_style),
    ];

    frame.render_widget(Paragraph::new(lines).block(block), area);
}

fn help_line<'a>(key: &'a str, desc: &'a str, key_style: Style, desc_style: Style) -> Line<'a> {
    Line::from(vec![
        Span::styled(format!("{key:<14}"), key_style),
        Span::styled(desc, desc_style),
    ])
}

fn centered_rect(percent_x: u16, percent_y: u16, area: Rect) -> Rect {
    let vertical = Layout::vertical([Constraint::Percentage(percent_y)]).flex(Flex::Center);
    let horizontal = Layout::horizontal([Constraint::Percentage(percent_x)]).flex(Flex::Center);
    let [area] = vertical.areas(area);
    let [area] = horizontal.areas(area);
    area
}
