use ratatui::{
    Frame,
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph},
};

use crate::debugger::protocol::StateSnapshot;

pub(super) fn render_source_panel(
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

    let title = snapshot
        .filename
        .as_deref()
        .unwrap_or("Source");

    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(border_style)
        .title(format!(" {title} "));

    let inner = block.inner(area);
    let visible_height = inner.height as usize;

    let source_lines: Vec<&str> = snapshot.source.lines().collect();

    let highlight_range = snapshot.current_span.map(|(start, end)| {
        let mut start_line = 0;
        let mut start_col = 0;
        let mut end_line = 0;
        let mut end_col = 0;
        let mut offset = 0;

        for (i, line) in source_lines.iter().enumerate() {
            let line_end = offset + line.len() + 1; // +1 for newline
            if offset <= start && start < line_end {
                start_line = i;
                start_col = start - offset;
            }
            if offset <= end && end <= line_end {
                end_line = i;
                end_col = end - offset;
            }
            offset = line_end;
        }

        (start_line, start_col, end_line, end_col)
    });

    let breakpoint_lines: Vec<usize> = snapshot
        .breakpoints
        .iter()
        .filter_map(|bp| {
            if let crate::debugger::protocol::Breakpoint::SourceLine { line, .. } = bp {
                Some(*line)
            } else {
                None
            }
        })
        .collect();

    let lines: Vec<Line> = source_lines
        .iter()
        .enumerate()
        .skip(scroll)
        .take(visible_height)
        .map(|(line_idx, line_text)| {
            let line_num = line_idx + 1;
            let has_breakpoint = breakpoint_lines.contains(&line_num);

            let bp_marker = if has_breakpoint { "●" } else { " " };
            let bp_style = Style::default().fg(Color::Red);

            let gutter = format!("{line_num:>4} ");
            let gutter_style = Style::default().fg(Color::DarkGray);

            if let Some((sl, sc, el, ec)) = highlight_range {
                if line_idx >= sl && line_idx <= el {
                    let highlight_style = Style::default()
                        .bg(Color::Rgb(60, 60, 20))
                        .add_modifier(Modifier::BOLD);

                    let col_start = if line_idx == sl { sc } else { 0 };
                    let col_end = if line_idx == el {
                        ec.min(line_text.len())
                    } else {
                        line_text.len()
                    };

                    let before = &line_text[..col_start.min(line_text.len())];
                    let highlighted = &line_text
                        [col_start.min(line_text.len())..col_end.min(line_text.len())];
                    let after = &line_text[col_end.min(line_text.len())..];

                    return Line::from(vec![
                        Span::styled(bp_marker, bp_style),
                        Span::styled(gutter, gutter_style),
                        Span::raw(before),
                        Span::styled(highlighted, highlight_style),
                        Span::raw(after),
                    ]);
                }
            }

            Line::from(vec![
                Span::styled(bp_marker, bp_style),
                Span::styled(gutter, gutter_style),
                Span::raw(*line_text),
            ])
        })
        .collect();

    frame.render_widget(Paragraph::new(lines).block(block), area);
}
