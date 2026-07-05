use ratatui::{
    Frame,
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph},
};

use crate::debugger::protocol::{HighlightKind, HighlightSpan, StateSnapshot};

use super::pad_line;

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

    let title = snapshot.filename.as_deref().unwrap_or("Source");

    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(border_style)
        .title(format!(" {title} "));

    let inner = block.inner(area);
    let visible_height = inner.height as usize;
    let panel_width = inner.width as usize;

    let source_lines: Vec<&str> = snapshot.source.lines().collect();
    let line_offsets = compute_line_offsets(&snapshot.source);

    let highlight_range = snapshot.current_span.map(|(start, end)| {
        let start_line = offset_to_line(&line_offsets, start);
        let start_col = start - line_offsets[start_line];
        let end_line = offset_to_line(&line_offsets, end);
        let end_col = end - line_offsets[end_line];
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

    let mut lines: Vec<Line> = source_lines
        .iter()
        .enumerate()
        .skip(scroll)
        .take(visible_height)
        .map(|(line_idx, line_text)| {
            let line_num = line_idx + 1;
            let has_breakpoint = breakpoint_lines.contains(&line_num);

            let bp_marker = if has_breakpoint { "*" } else { " " };
            let bp_style = Style::default().fg(Color::Red);
            let gutter = format!("{line_num:>4} ");
            let gutter_style = Style::default().fg(Color::DarkGray);

            let line_start_offset = line_offsets[line_idx];
            let line_end_offset = line_start_offset + line_text.len();

            let is_in_expr_highlight =
                highlight_range.is_some_and(|(sl, _, el, _)| line_idx >= sl && line_idx <= el);

            let expr_col_start = highlight_range.and_then(|(sl, sc, _, _)| {
                if line_idx == sl {
                    Some(sc)
                } else if is_in_expr_highlight {
                    Some(0)
                } else {
                    None
                }
            });
            let expr_col_end = highlight_range.and_then(|(_, _, el, ec)| {
                if line_idx == el {
                    Some(ec.min(line_text.len()))
                } else if is_in_expr_highlight {
                    Some(line_text.len())
                } else {
                    None
                }
            });

            let mut spans = vec![
                Span::styled(bp_marker, bp_style),
                Span::styled(gutter, gutter_style),
            ];

            spans.extend(highlight_line(
                line_text,
                line_start_offset,
                line_end_offset,
                &snapshot.highlight_spans,
                expr_col_start,
                expr_col_end,
            ));

            pad_line(Line::from(spans), panel_width)
        })
        .collect();

    while lines.len() < visible_height {
        lines.push(pad_line(Line::from(""), panel_width));
    }

    frame.render_widget(Paragraph::new(lines).block(block), area);
}

fn highlight_line<'a>(
    line: &'a str,
    line_start: usize,
    line_end: usize,
    highlights: &[HighlightSpan],
    expr_col_start: Option<usize>,
    expr_col_end: Option<usize>,
) -> Vec<Span<'a>> {
    if line.is_empty() {
        return vec![];
    }

    let relevant: Vec<&HighlightSpan> = highlights
        .iter()
        .filter(|h| h.end > line_start && h.start < line_end)
        .collect();

    if relevant.is_empty() {
        return apply_expr_highlight_multi(
            line,
            Style::default().fg(Color::White),
            0,
            expr_col_start,
            expr_col_end,
        );
    }

    let mut spans = Vec::new();
    let mut col = 0;

    for hl in &relevant {
        let hl_col_start = hl.start.saturating_sub(line_start);
        let hl_col_end = hl.end.saturating_sub(line_start).min(line.len());

        if hl_col_start > col {
            let gap = &line[col..hl_col_start];
            spans.extend(apply_expr_highlight_multi(
                gap,
                Style::default().fg(Color::White),
                col,
                expr_col_start,
                expr_col_end,
            ));
        }

        if hl_col_start < hl_col_end && hl_col_start < line.len() {
            let token_text = &line[hl_col_start..hl_col_end.min(line.len())];
            let fg = highlight_color(hl.kind);
            let style = Style::default().fg(fg);
            let style = if hl.kind == HighlightKind::Keyword {
                style.add_modifier(Modifier::BOLD)
            } else {
                style
            };
            spans.extend(apply_expr_highlight_multi(
                token_text,
                style,
                hl_col_start,
                expr_col_start,
                expr_col_end,
            ));
            col = hl_col_end;
        }
    }

    if col < line.len() {
        let rest = &line[col..];
        spans.extend(apply_expr_highlight_multi(
            rest,
            Style::default().fg(Color::White),
            col,
            expr_col_start,
            expr_col_end,
        ));
    }

    spans
}

fn apply_expr_highlight_multi(
    text: &str,
    base_style: Style,
    text_col_start: usize,
    expr_col_start: Option<usize>,
    expr_col_end: Option<usize>,
) -> Vec<Span<'_>> {
    let (Some(es), Some(ee)) = (expr_col_start, expr_col_end) else {
        return vec![Span::styled(text, base_style)];
    };

    let text_col_end = text_col_start + text.len();

    if text_col_end <= es || text_col_start >= ee {
        return vec![Span::styled(text, base_style)];
    }

    if text_col_start >= es && text_col_end <= ee {
        return vec![Span::styled(
            text,
            base_style
                .bg(Color::Rgb(70, 70, 15))
                .add_modifier(Modifier::BOLD),
        )];
    }

    let mut spans = Vec::new();
    let hl_start_in_text = es.saturating_sub(text_col_start).min(text.len());
    let hl_end_in_text = ee.saturating_sub(text_col_start).min(text.len());

    if hl_start_in_text > 0 {
        spans.push(Span::styled(&text[..hl_start_in_text], base_style));
    }
    if hl_start_in_text < hl_end_in_text {
        spans.push(Span::styled(
            &text[hl_start_in_text..hl_end_in_text],
            base_style
                .bg(Color::Rgb(70, 70, 15))
                .add_modifier(Modifier::BOLD),
        ));
    }
    if hl_end_in_text < text.len() {
        spans.push(Span::styled(&text[hl_end_in_text..], base_style));
    }

    spans
}

fn highlight_color(kind: HighlightKind) -> Color {
    match kind {
        HighlightKind::Keyword => Color::Red,
        HighlightKind::Literal => Color::Magenta,
        HighlightKind::String => Color::Green,
        HighlightKind::Comment => Color::DarkGray,
        HighlightKind::Operator => Color::White,
        HighlightKind::Identifier => Color::White,
    }
}

fn compute_line_offsets(source: &str) -> Vec<usize> {
    let mut offsets = vec![0];
    for (i, ch) in source.char_indices() {
        if ch == '\n' {
            offsets.push(i + 1);
        }
    }
    offsets
}

fn offset_to_line(line_offsets: &[usize], offset: usize) -> usize {
    match line_offsets.binary_search(&offset) {
        Ok(line) => line,
        Err(line) => line.saturating_sub(1),
    }
}
