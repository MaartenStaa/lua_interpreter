use std::borrow::Cow;

use ratatui::{
    Frame,
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders},
};

use crate::debugger::protocol::{HighlightSpan, StateSnapshot};

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

    let lines: Vec<Line> = source_lines
        .iter()
        .enumerate()
        .skip(scroll)
        .take(visible_height)
        .map(|(line_idx, line_text)| {
            let line_num = line_idx + 1;
            let has_breakpoint = breakpoint_lines.contains(&line_num);

            let bp_marker = if has_breakpoint { "*" } else { " " };
            let bp_style = Style::default().fg(Color::Red);
            let gutter = format!("{line_num:>4} │ ");
            let gutter_style = Style::default().fg(Color::DarkGray);

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
                line_idx,
                &snapshot.source_spans,
                expr_col_start,
                expr_col_end,
            ));

            Line::from(spans)
        })
        .collect();

    super::render_lines(frame, area, block, &lines);
}

const HIGHLIGHT_KIND_PRIORITY: &[&str] = &[
    "variable.member",
    "function.method.call",
    "function.call",
    "function.builtin",
    "function",
    "variable.parameter",
    "variable.builtin",
    "variable",
    "constant.builtin",
    "constant",
    "module.builtin",
    "module",
    "label",
    "string.regexp",
    "string.escape",
    "string",
    "boolean",
    "number.float",
    "number",
    "attribute",
    "property",
    "constructor",
    "operator",
    "keyword.operator",
    "keyword.function",
    "keyword.repeat",
    "repeat",
    "keyword.return",
    "keyword.conditional",
    "conditional",
    "keyword",
    "punctuation.delimiter",
    "punctuation.bracket",
];

fn highlight_line<'a>(
    line: &'a str,
    line_idx: usize,
    highlights: &[HighlightSpan],
    expr_col_start: Option<usize>,
    expr_col_end: Option<usize>,
) -> Vec<Span<'a>> {
    if line.is_empty() {
        return vec![];
    }

    let relevant: Vec<&HighlightSpan> = highlights
        .iter()
        .filter(|h| h.start.row <= line_idx && h.end.row >= line_idx)
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

    let mut iter = relevant.iter().peekable();
    while let Some(mut hl) = iter.next() {
        let hl_col_start = hl.start.column;
        let hl_col_end = hl.end.column;

        // Sometimes the highlights query returns the same node more than once. Check if the next
        // highlight has the exact same span. If so, select whichever highlight is more
        // specific (i.e. more components).
        while let Some(next) = iter.peek()
            && next.start == hl.start
            && next.end == hl.end
        {
            let specificity = hl.kind.matches('.').count();
            let next_specificity = next.kind.matches('.').count();

            if next_specificity > specificity {
                hl = *next;
            } else if next_specificity == specificity && next.kind != hl.kind {
                let priority = HIGHLIGHT_KIND_PRIORITY
                    .iter()
                    .position(|&k| k == hl.kind)
                    .unwrap_or(HIGHLIGHT_KIND_PRIORITY.len());
                let next_priority = HIGHLIGHT_KIND_PRIORITY
                    .iter()
                    .position(|&k| k == next.kind)
                    .unwrap_or(HIGHLIGHT_KIND_PRIORITY.len());

                if next_priority < priority {
                    hl = *next;
                }
            }

            // Consume it, so we don't encounter it again.
            iter.next();
        }

        if hl_col_start > col && hl.start.row == line_idx {
            assert!(
                hl_col_start <= line.len(),
                "Highlight start column {} is out of bounds for line {} (length {})\nline: {line}\nrelevant_spans: {relevant:#?}",
                hl_col_start,
                line_idx,
                line.len()
            );

            spans.extend(apply_expr_highlight_multi(
                &line[col..hl_col_start],
                Style::default().fg(Color::White),
                col,
                expr_col_start,
                expr_col_end,
            ));
        }

        if hl_col_start < hl_col_end && hl_col_start < line.len() {
            let token_text = &line[hl_col_start..hl_col_end.min(line.len())];
            spans.extend(apply_expr_highlight_multi(
                token_text,
                highlight_style(&hl.kind),
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

fn patch_tabs<'a>(text: impl Into<Cow<'a, str>>) -> Cow<'a, str> {
    let text = text.into();
    if text.contains('\t') {
        let mut patched = String::with_capacity(text.len());
        for b in text.as_bytes() {
            match *b {
                b'\t' => patched.push_str("  "),
                _ => patched.push(*b as char),
            }
        }
        Cow::Owned(patched)
    } else {
        text
    }
}

fn apply_expr_highlight_multi(
    text: &str,
    base_style: Style,
    text_col_start: usize,
    expr_col_start: Option<usize>,
    expr_col_end: Option<usize>,
) -> Vec<Span<'_>> {
    let (Some(es), Some(ee)) = (expr_col_start, expr_col_end) else {
        return vec![Span::styled(patch_tabs(text), base_style)];
    };

    let text_col_end = text_col_start + text.len();
    if text_col_end <= es || text_col_start >= ee {
        return vec![Span::styled(patch_tabs(text), base_style)];
    }

    if text_col_start >= es && text_col_end <= ee {
        return vec![Span::styled(
            patch_tabs(text),
            base_style
                .bg(Color::Rgb(70, 70, 15))
                .add_modifier(Modifier::BOLD),
        )];
    }

    let mut spans = Vec::new();
    let hl_start_in_text = es.saturating_sub(text_col_start).min(text.len());
    let hl_end_in_text = ee.saturating_sub(text_col_start).min(text.len());

    if hl_start_in_text > 0 {
        spans.push(Span::styled(
            patch_tabs(&text[..hl_start_in_text]),
            base_style,
        ));
    }
    if hl_start_in_text < hl_end_in_text {
        spans.push(Span::styled(
            patch_tabs(&text[hl_start_in_text..hl_end_in_text]),
            base_style
                .bg(Color::Rgb(70, 70, 15))
                .add_modifier(Modifier::BOLD),
        ));
    }
    if hl_end_in_text < text.len() {
        spans.push(Span::styled(
            patch_tabs(&text[hl_end_in_text..]),
            base_style,
        ));
    }

    spans
}

#[allow(clippy::match_same_arms)]
#[allow(dead_code)]
fn highlight_style(kind: &str) -> Style {
    // Catppuccin macchiatto palette:
    // https://github.com/catppuccin/nvim/blob/main/lua/catppuccin/palettes/macchiato.lua
    const COLOR_ROSEWATER: Color = Color::Rgb(0xf4, 0xdb, 0xd6);
    const COLOR_FLAMINGO: Color = Color::Rgb(0xf0, 0xc6, 0xc6);
    const COLOR_PINK: Color = Color::Rgb(0xf5, 0xbd, 0xe6);
    const COLOR_MAUVE: Color = Color::Rgb(0xc6, 0xa0, 0xf6);
    const COLOR_RED: Color = Color::Rgb(0xed, 0x87, 0x96);
    const COLOR_MAROON: Color = Color::Rgb(0xee, 0x99, 0xa0);
    const COLOR_PEACH: Color = Color::Rgb(0xf5, 0xa9, 0x7f);
    const COLOR_YELLOW: Color = Color::Rgb(0xee, 0xd4, 0x9f);
    const COLOR_GREEN: Color = Color::Rgb(0xa6, 0xda, 0x95);
    const COLOR_TEAL: Color = Color::Rgb(0x8b, 0xd5, 0xca);
    const COLOR_SKY: Color = Color::Rgb(0x91, 0xd7, 0xe3);
    const COLOR_SAPPHIRE: Color = Color::Rgb(0x7d, 0xc4, 0xe4);
    const COLOR_BLUE: Color = Color::Rgb(0x8a, 0xad, 0xf4);
    const COLOR_LAVENDER: Color = Color::Rgb(0xb7, 0xbd, 0xf8);
    const COLOR_TEXT: Color = Color::Rgb(0xca, 0xd3, 0xf5);
    const COLOR_SUBTEXT1: Color = Color::Rgb(0xb8, 0xc0, 0xe0);
    const COLOR_SUBTEXT0: Color = Color::Rgb(0xa5, 0xad, 0xcb);
    const COLOR_OVERLAY2: Color = Color::Rgb(0x93, 0x9a, 0xb7);
    const COLOR_OVERLAY1: Color = Color::Rgb(0x80, 0x87, 0xa2);
    const COLOR_OVERLAY0: Color = Color::Rgb(0x6e, 0x73, 0x8d);
    const COLOR_SURFACE2: Color = Color::Rgb(0x5b, 0x60, 0x78);
    const COLOR_SURFACE1: Color = Color::Rgb(0x49, 0x4d, 0x64);
    const COLOR_SURFACE0: Color = Color::Rgb(0x36, 0x3a, 0x4f);
    const COLOR_BASE: Color = Color::Rgb(0x24, 0x27, 0x3a);
    const COLOR_MANTLE: Color = Color::Rgb(0x1e, 0x20, 0x30);
    const COLOR_CRUST: Color = Color::Rgb(0x18, 0x19, 0x26);

    let mut style = Style::default();
    style = match kind {
        // Identifiers
        // Any variable name that does not have another highlight.
        "variable" => style.fg(COLOR_TEXT),
        // Variable names that are defined by the languages, like this or self.
        "variable.builtin" => style.fg(COLOR_RED),
        // For parameters of a function.
        "variable.parameter" | "parameter" | "variable.parameter.builtin" => style.fg(COLOR_MAROON),
        // For fields.
        "variable.member" | "field" => style.fg(COLOR_LAVENDER),

        // For constants
        "constant" => style.fg(COLOR_PEACH),
        // For constant that are built in the language: nil in Lua.
        "constant.builtin" => style.fg(COLOR_PEACH),

        // For identifiers referring to modules and namespaces.
        "module" | "module.builtin" => style.fg(COLOR_YELLOW).add_modifier(Modifier::ITALIC),

        // For labels: label: in C and :label: in Lua.
        "label" => style.fg(COLOR_SAPPHIRE),

        // Literals
        // For strings.
        "string" => style.fg(COLOR_GREEN),
        // For escape characters within a string.
        "string.escape" => style.fg(COLOR_PINK),
        // For regexes.
        "string.regexp" => style.fg(COLOR_PINK),

        // For booleans.
        // For all numbers
        // For floats.
        "boolean" | "number" | "number.float" => style.fg(COLOR_PEACH),

        // Attribute annotations
        "attribute" => style.fg(COLOR_PEACH),
        // For fields, like accessing `bar` property on `foo.bar`.
        "property" => style.fg(COLOR_LAVENDER),

        // Functions
        // For function (calls and definitions).
        "function" | "function.call" => style.fg(COLOR_BLUE),
        // For builtin functions: table.insert in Lua.
        "function.builtin" => style.fg(COLOR_PEACH), /*, style = O.styles.functions or {}*/
        // For method calls.
        "function.method.call" | "method.call" => style.fg(COLOR_BLUE),

        // For constructor calls and definitions: = { } in Lua, and Java constructors.
        "constructor" => style.fg(COLOR_YELLOW),
        // For any operator: +, but also -> and * in C.
        "operator" => style.fg(COLOR_SKY),

        // Keywords
        "keyword" | "keyword.coroutine" => style.fg(COLOR_PINK).add_modifier(Modifier::ITALIC),

        // For keywords used to define a function.
        // For new keyword operator
        "keyword.function" | "keyword.operator" => style.fg(COLOR_MAUVE),
        // For keywords related to loops.
        "keyword.repeat" | "repeat" => style.fg(COLOR_MAUVE),
        "keyword.return" => style.fg(COLOR_MAUVE),

        // For keywords related to conditionnals.
        "keyword.conditional" | "conditional" => {
            style.fg(COLOR_MAUVE).add_modifier(Modifier::ITALIC)
        }

        // Punctuation
        // For delimiters (e.g. `;` / `.` / `,`).
        // For brackets and parenthesis.
        "punctuation.delimiter" | "punctuation.bracket" => style.fg(COLOR_OVERLAY2),

        // Comment
        "comment" | "comment.documentation" => {
            style.fg(COLOR_OVERLAY2).add_modifier(Modifier::ITALIC)
        }

        other => {
            unreachable!("Unknown highlight kind: {other}");
        }
    };

    style
}

pub(super) fn render_aligned_source_panel(
    frame: &mut Frame,
    area: Rect,
    snapshot: &StateSnapshot,
    rows: &[super::AlignedRow<'_>],
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
        .title(format!(" {title} (aligned) "));

    let inner = block.inner(area);
    let visible_height = inner.height as usize;

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

    let gutter_style = Style::default().fg(Color::DarkGray);

    let lines: Vec<Line> = rows
        .iter()
        .skip(scroll)
        .take(visible_height)
        .map(|row| {
            let source_line_idx = match row {
                super::AlignedRow::SourceWithBytecode {
                    source_line_idx, ..
                }
                | super::AlignedRow::SourceOnly { source_line_idx } => Some(*source_line_idx),
                super::AlignedRow::BytecodeOnly { .. } => None,
            };

            let Some(line_idx) = source_line_idx else {
                return Line::from(Span::styled("      │", gutter_style));
            };

            let line_num = line_idx + 1;
            let line_text = source_lines.get(line_idx).copied().unwrap_or("");

            let has_breakpoint = breakpoint_lines.contains(&line_num);
            let bp_marker = if has_breakpoint { "*" } else { " " };
            let bp_style = Style::default().fg(Color::Red);
            let gutter = format!("{line_num:>4} │ ");

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
                line_idx,
                &snapshot.source_spans,
                expr_col_start,
                expr_col_end,
            ));
            Line::from(spans)
        })
        .collect();

    super::render_lines(frame, area, block, &lines);
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
