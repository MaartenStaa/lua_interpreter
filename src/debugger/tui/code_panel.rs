use std::borrow::Cow;
use std::collections::HashMap;

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use ratatui::{
    Frame,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders},
};
use tree_sitter::StreamingIterator;

use crate::debugger::protocol::{Breakpoint, ChunkSnapshot, VmState};
use crate::debugger::source_map::{SourceMap, build_source_map};
use crate::vm::ChunkIndex;

use super::KeyResult;
use super::bytecode_fmt::{self, BytecodeLine};

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
enum SubFocus {
    Left,
    Right,
}

const SCROLL_MARGIN: usize = 4;

pub(crate) struct CodePanel {
    mode: PanelMode,
    sub_focus: SubFocus,
    scroll: usize,
    focus_line: usize,
    last_visible_height: usize,
    // Cached per-chunk data
    cached_chunk: Option<ChunkIndex>,
    source_map: SourceMap,
    bytecode_lines: Vec<BytecodeLine>,
    highlight_spans: Vec<HighlightSpan>,
    source: String,
    filename: Option<String>,
    // Current instruction state
    current_chunk: ChunkIndex,
    current_ip: usize,
    current_span: Option<(usize, usize)>,
    breakpoints: Vec<crate::debugger::protocol::Breakpoint>,
}

#[derive(Debug, Clone)]
pub(crate) struct HighlightSpan {
    pub start: tree_sitter::Point,
    pub end: tree_sitter::Point,
    pub kind: String,
}

impl CodePanel {
    pub(crate) fn new() -> Self {
        Self {
            mode: PanelMode::Source,
            sub_focus: SubFocus::Left,
            scroll: 0,
            focus_line: 0,
            last_visible_height: 20,
            cached_chunk: None,
            source_map: SourceMap {
                line_to_ips: HashMap::new(),
                ip_to_line: HashMap::new(),
            },
            bytecode_lines: Vec::new(),
            highlight_spans: Vec::new(),
            source: String::new(),
            filename: None,
            current_chunk: 0,
            current_ip: 0,
            current_span: None,
            breakpoints: Vec::new(),
        }
    }

    pub(crate) fn mode_label(&self) -> &str {
        self.mode.label()
    }

    pub(crate) fn current_source_line(&self) -> Option<usize> {
        self.source_map.ip_to_line.get(&self.current_ip).copied()
    }

    pub(crate) fn update(&mut self, state: &VmState) {
        self.current_chunk = state.current_chunk;
        self.current_ip = state.current_ip;
        self.breakpoints.clone_from(&state.breakpoints);

        let chunk = &state.chunks[state.current_chunk as usize];
        self.current_span = chunk
            .instruction_spans
            .get(&state.current_ip)
            .map(|s| (s.start, s.end));

        if self.cached_chunk != Some(state.current_chunk) {
            self.rebuild_cache(chunk, state.current_chunk);
        }

        let exec_row = self.execution_row();
        self.ensure_row_visible(exec_row);
    }

    fn execution_row(&self) -> usize {
        match self.mode {
            PanelMode::Source | PanelMode::Split => {
                self.current_source_line().unwrap_or(0).saturating_sub(1)
            }
            PanelMode::Bytecode => self.current_ip,
            PanelMode::Aligned => self.current_ip,
        }
    }

    fn total_rows(&self) -> usize {
        match self.mode {
            PanelMode::Source | PanelMode::Split => self.source.lines().count(),
            PanelMode::Bytecode => self.bytecode_lines.len(),
            PanelMode::Aligned => {
                let source_count = self.source.lines().count();
                source_count + self.bytecode_lines.len()
            }
        }
    }

    fn ensure_row_visible(&mut self, row: usize) {
        let h = self.last_visible_height;
        if h == 0 {
            return;
        }
        if row < self.scroll + SCROLL_MARGIN {
            self.scroll = row.saturating_sub(SCROLL_MARGIN);
        } else if row >= self.scroll + h - SCROLL_MARGIN {
            self.scroll = row.saturating_sub(h - SCROLL_MARGIN - 1);
        }
    }

    fn move_focus(&mut self, delta: isize) {
        let max = self.total_rows().saturating_sub(1);
        if delta > 0 {
            self.focus_line = (self.focus_line + delta as usize).min(max);
        } else {
            self.focus_line = self.focus_line.saturating_sub((-delta) as usize);
        }
        self.ensure_row_visible(self.focus_line);
    }

    fn rebuild_cache(&mut self, chunk: &ChunkSnapshot, chunk_index: ChunkIndex) {
        self.source = String::from_utf8_lossy(&chunk.source).into_owned();
        self.filename.clone_from(&chunk.filename);
        self.source_map =
            build_source_map(&chunk.instructions, &chunk.instruction_spans, &chunk.source);
        self.bytecode_lines =
            bytecode_fmt::build_bytecode_lines(chunk, chunk_index, &self.source_map.ip_to_line);
        self.highlight_spans = build_highlight_spans(&chunk.source);
        self.cached_chunk = Some(chunk_index);
    }

    fn has_split(&self) -> bool {
        matches!(self.mode, PanelMode::Split | PanelMode::Aligned)
    }

    pub(crate) fn focus_rightmost(&mut self) {
        if self.has_split() {
            self.sub_focus = SubFocus::Right;
        }
    }

    pub(crate) fn handle_key(&mut self, key: KeyEvent) -> KeyResult {
        match key {
            KeyEvent {
                code: KeyCode::Char('l') | KeyCode::Right,
                modifiers: KeyModifiers::NONE,
                ..
            } if self.has_split() && self.sub_focus == SubFocus::Left => {
                self.sub_focus = SubFocus::Right;
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('h') | KeyCode::Left,
                modifiers: KeyModifiers::NONE,
                ..
            } if self.has_split() && self.sub_focus == SubFocus::Right => {
                self.sub_focus = SubFocus::Left;
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Tab, ..
            } => {
                self.mode = self.mode.cycle();
                self.sub_focus = SubFocus::Left;
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('j') | KeyCode::Down,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                self.move_focus(1);
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('k') | KeyCode::Up,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                self.move_focus(-1);
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('d'),
                modifiers: KeyModifiers::CONTROL,
                ..
            } => {
                let half = (self.last_visible_height / 2).max(1);
                self.move_focus(half as isize);
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('u'),
                modifiers: KeyModifiers::CONTROL,
                ..
            } => {
                let half = (self.last_visible_height / 2).max(1);
                self.move_focus(-(half as isize));
                KeyResult::Consumed
            }
            KeyEvent {
                code: KeyCode::Char('b'),
                modifiers: KeyModifiers::NONE,
                ..
            } => self.toggle_breakpoint(),
            _ => KeyResult::Ignored,
        }
    }

    fn focused_source_line(&self) -> Option<usize> {
        match self.mode {
            PanelMode::Source | PanelMode::Split => Some(self.focus_line + 1),
            PanelMode::Bytecode => self
                .bytecode_lines
                .get(self.focus_line)
                .and_then(|bl| bl.source_line),
            PanelMode::Aligned => {
                let rows = self.build_aligned_rows();
                rows.get(self.focus_line).and_then(|row| match row {
                    AlignedRow::SourceWithBytecode {
                        source_line_idx, ..
                    }
                    | AlignedRow::SourceOnly { source_line_idx } => Some(*source_line_idx + 1),
                    AlignedRow::BytecodeOnly { bytecode } => bytecode.source_line,
                })
            }
        }
    }

    fn toggle_breakpoint(&self) -> KeyResult {
        let Some(filename) = &self.filename else {
            return KeyResult::Ignored;
        };
        let Some(line) = self.focused_source_line() else {
            return KeyResult::Ignored;
        };

        let bp = Breakpoint::SourceLine {
            filename: filename.clone(),
            line,
        };

        if self.breakpoints.contains(&bp) {
            KeyResult::Command(super::super::protocol::DebugCommand::RemoveBreakpoint(bp))
        } else {
            KeyResult::Command(super::super::protocol::DebugCommand::AddBreakpoint(bp))
        }
    }

    pub(crate) fn render(&mut self, frame: &mut Frame, area: Rect, focused: bool) {
        if self.source.is_empty() {
            let block = Block::default().borders(Borders::ALL).title(" Waiting... ");
            super::render_lines(
                frame,
                area,
                block,
                &[Line::from("Waiting for VM to pause...")],
            );
            return;
        }

        match self.mode {
            PanelMode::Source => {
                self.render_source(frame, area, focused);
            }
            PanelMode::Bytecode => {
                self.render_bytecode(frame, area, focused);
            }
            PanelMode::Split => {
                let split = Layout::default()
                    .direction(Direction::Horizontal)
                    .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
                    .split(area);
                let left_focused = focused && self.sub_focus == SubFocus::Left;
                let right_focused = focused && self.sub_focus == SubFocus::Right;
                self.render_source(frame, split[0], left_focused);
                self.render_bytecode(frame, split[1], right_focused);
            }
            PanelMode::Aligned => {
                let split = Layout::default()
                    .direction(Direction::Horizontal)
                    .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
                    .split(area);
                let left_focused = focused && self.sub_focus == SubFocus::Left;
                let right_focused = focused && self.sub_focus == SubFocus::Right;
                let block_tmp = Block::default().borders(Borders::ALL);
                self.last_visible_height = block_tmp.inner(split[0]).height as usize;
                let rows = self.build_aligned_rows();
                self.render_aligned_source_immut(frame, split[0], &rows, left_focused);
                self.render_aligned_bytecode(frame, split[1], &rows, right_focused);
            }
        }
    }

    // --- Source panel rendering ---

    fn render_source(&mut self, frame: &mut Frame, area: Rect, focused: bool) {
        let border_style = if focused {
            Style::default().fg(Color::Cyan)
        } else {
            Style::default().fg(Color::DarkGray)
        };

        let title = self.filename.as_deref().unwrap_or("Source");
        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(border_style)
            .title(format!(" {title} "));

        let inner = block.inner(area);
        let visible_height = inner.height as usize;
        self.last_visible_height = visible_height;

        let source_lines: Vec<&str> = self.source.lines().collect();
        let line_offsets = compute_line_offsets(&self.source);
        let highlight_range = self.compute_highlight_range(&line_offsets);

        let lines: Vec<Line> = source_lines
            .iter()
            .enumerate()
            .skip(self.scroll)
            .take(visible_height)
            .map(|(line_idx, line_text)| {
                let mut line = self.build_source_line(&line_offsets, highlight_range, line_idx, line_text);
                if focused && line_idx == self.focus_line {
                    apply_focus_bg(&mut line);
                }
                line
            })
            .collect();

        super::render_lines(frame, area, block, &lines);
    }

    fn build_source_line<'a>(
        &'a self,
        _line_offsets: &[usize],
        highlight_range: Option<(usize, usize, usize, usize)>,
        line_idx: usize,
        line_text: &'a str,
    ) -> Line<'a> {
        let line_num = line_idx + 1;
        let has_breakpoint = self.breakpoints.iter().any(|bp| {
            matches!(bp, crate::debugger::protocol::Breakpoint::SourceLine { line, .. } if *line == line_num)
        });

        let bp_marker = if has_breakpoint { "*" } else { " " };
        let bp_style = Style::default().fg(Color::Red);
        let gutter = format!("{line_num:>4} \u{2502} ");
        let gutter_style = Style::default().fg(Color::DarkGray);

        let is_in_expr =
            highlight_range.is_some_and(|(sl, _, el, _)| line_idx >= sl && line_idx <= el);

        let expr_col_start = highlight_range.and_then(|(sl, sc, _, _)| {
            if line_idx == sl {
                Some(sc)
            } else if is_in_expr {
                Some(0)
            } else {
                None
            }
        });
        let expr_col_end = highlight_range.and_then(|(_, _, el, ec)| {
            if line_idx == el {
                Some(ec.min(line_text.len()))
            } else if is_in_expr {
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
            &self.highlight_spans,
            expr_col_start,
            expr_col_end,
        ));
        Line::from(spans)
    }

    fn compute_highlight_range(
        &self,
        line_offsets: &[usize],
    ) -> Option<(usize, usize, usize, usize)> {
        self.current_span.map(|(start, end)| {
            let start_line = offset_to_line(line_offsets, start);
            let start_col = start - line_offsets[start_line];
            let end_line = offset_to_line(line_offsets, end);
            let end_col = end - line_offsets[end_line];
            (start_line, start_col, end_line, end_col)
        })
    }

    // --- Bytecode panel rendering ---

    fn render_bytecode(&mut self, frame: &mut Frame, area: Rect, focused: bool) {
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
        self.last_visible_height = visible_height;

        let lines: Vec<Line> = self
            .bytecode_lines
            .iter()
            .enumerate()
            .skip(self.scroll)
            .take(visible_height)
            .map(|(idx, bl)| {
                let mut line = render_bytecode_line(bl, self.current_ip);
                if focused && idx == self.focus_line {
                    apply_focus_bg(&mut line);
                }
                line
            })
            .collect();

        super::render_lines(frame, area, block, &lines);
    }

    // --- Aligned mode ---

    fn build_aligned_rows(&self) -> Vec<AlignedRow<'_>> {
        let source_line_count = self.source.lines().count();
        let mut rows = Vec::new();
        let mut bc_index = 0;

        for line_num in 1..=source_line_count {
            let line_idx = line_num - 1;
            let mut i = 0;
            let any_bc = loop {
                let bc = self.bytecode_lines.get(bc_index);
                if let Some(bc) = bc {
                    match bc.source_line {
                        Some(line) if line == line_num && i == 0 => {
                            rows.push(AlignedRow::SourceWithBytecode {
                                source_line_idx: line_idx,
                                bytecode: bc,
                            });
                            bc_index += 1;
                        }
                        Some(line) if line == line_num => {
                            rows.push(AlignedRow::BytecodeOnly { bytecode: bc });
                            bc_index += 1;
                        }
                        Some(_) => break i > 0,
                        None => {
                            rows.push(if i == 0 {
                                AlignedRow::SourceWithBytecode {
                                    source_line_idx: line_idx,
                                    bytecode: bc,
                                }
                            } else {
                                AlignedRow::BytecodeOnly { bytecode: bc }
                            });
                            bc_index += 1;
                        }
                    }
                } else {
                    break i > 0;
                }
                i += 1;
            };

            if !any_bc {
                rows.push(AlignedRow::SourceOnly {
                    source_line_idx: line_idx,
                });
            }
        }

        rows
    }

    fn render_aligned_source_immut(
        &self,
        frame: &mut Frame,
        area: Rect,
        rows: &[AlignedRow<'_>],
        focused: bool,
    ) {
        let border_style = if focused {
            Style::default().fg(Color::Cyan)
        } else {
            Style::default().fg(Color::DarkGray)
        };

        let title = self.filename.as_deref().unwrap_or("Source");
        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(border_style)
            .title(format!(" {title} (aligned) "));

        let inner = block.inner(area);
        let visible_height = inner.height as usize;

        let source_lines: Vec<&str> = self.source.lines().collect();
        let line_offsets = compute_line_offsets(&self.source);
        let highlight_range = self.compute_highlight_range(&line_offsets);
        let gutter_style = Style::default().fg(Color::DarkGray);

        let lines: Vec<Line> = rows
            .iter()
            .enumerate()
            .skip(self.scroll)
            .take(visible_height)
            .map(|(row_idx, row)| {
                let source_line_idx = match row {
                    AlignedRow::SourceWithBytecode {
                        source_line_idx, ..
                    }
                    | AlignedRow::SourceOnly { source_line_idx } => Some(*source_line_idx),
                    AlignedRow::BytecodeOnly { .. } => None,
                };

                let Some(line_idx) = source_line_idx else {
                    let mut line = Line::from(Span::styled("      \u{2502}", gutter_style));
                    if focused && row_idx == self.focus_line {
                        apply_focus_bg(&mut line);
                    }
                    return line;
                };

                let line_text = source_lines.get(line_idx).copied().unwrap_or("");
                let mut line =
                    self.build_source_line(&line_offsets, highlight_range, line_idx, line_text);
                if focused && row_idx == self.focus_line {
                    apply_focus_bg(&mut line);
                }
                line
            })
            .collect();

        super::render_lines(frame, area, block, &lines);
    }

    fn render_aligned_bytecode(
        &self,
        frame: &mut Frame,
        area: Rect,
        rows: &[AlignedRow<'_>],
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
            .enumerate()
            .skip(self.scroll)
            .take(visible_height)
            .map(|(row_idx, row)| {
                let bytecode = match row {
                    AlignedRow::SourceWithBytecode { bytecode, .. }
                    | AlignedRow::BytecodeOnly { bytecode } => Some(*bytecode),
                    AlignedRow::SourceOnly { .. } => None,
                };

                let mut line = match bytecode {
                    Some(bl) => render_bytecode_line(bl, self.current_ip),
                    None => Line::from(""),
                };
                if focused && row_idx == self.focus_line {
                    apply_focus_bg(&mut line);
                }
                line
            })
            .collect();

        super::render_lines(frame, area, block, &lines);
    }
}

enum AlignedRow<'a> {
    SourceWithBytecode {
        source_line_idx: usize,
        bytecode: &'a BytecodeLine,
    },
    SourceOnly {
        source_line_idx: usize,
    },
    BytecodeOnly {
        bytecode: &'a BytecodeLine,
    },
}

fn apply_focus_bg(line: &mut Line<'_>) {
    for span in &mut line.spans {
        if span.style.bg.is_none() {
            span.style = span.style.bg(Color::Rgb(0x36, 0x3a, 0x4f));
        }
    }
}

fn render_bytecode_line(bl: &BytecodeLine, current_ip: usize) -> Line<'_> {
    let is_current = bl.ip == current_ip;
    let bg = if is_current {
        Color::Rgb(50, 50, 15)
    } else {
        Color::Reset
    };
    let bold = if is_current {
        Modifier::BOLD
    } else {
        Modifier::empty()
    };
    let marker = if is_current { ">" } else { " " };

    Line::from(vec![
        Span::styled(
            marker,
            Style::default().fg(Color::Yellow).bg(bg).add_modifier(bold),
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
            Style::default().fg(Color::Blue).bg(bg).add_modifier(bold),
        ),
        Span::styled(
            bl.operands.clone(),
            Style::default().fg(Color::Yellow).bg(bg),
        ),
    ])
}

// --- Source highlighting ---

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
        return apply_expr_highlight(
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
            iter.next();
        }

        if hl_col_start > col && hl.start.row == line_idx {
            assert!(
                hl_col_start <= line.len(),
                "Highlight start column {} out of bounds for line {} (len {})",
                hl_col_start,
                line_idx,
                line.len()
            );
            spans.extend(apply_expr_highlight(
                &line[col..hl_col_start],
                Style::default().fg(Color::White),
                col,
                expr_col_start,
                expr_col_end,
            ));
        }

        if hl_col_start < hl_col_end && hl_col_start < line.len() {
            let token_text = &line[hl_col_start..hl_col_end.min(line.len())];
            spans.extend(apply_expr_highlight(
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
        spans.extend(apply_expr_highlight(
            &line[col..],
            Style::default().fg(Color::White),
            col,
            expr_col_start,
            expr_col_end,
        ));
    }

    spans
}

fn patch_tabs(text: &str) -> Cow<'_, str> {
    if text.contains('\t') {
        Cow::Owned(text.replace('\t', "  "))
    } else {
        Cow::Borrowed(text)
    }
}

fn apply_expr_highlight(
    text: &str,
    base_style: Style,
    text_col_start: usize,
    expr_start: Option<usize>,
    expr_end: Option<usize>,
) -> Vec<Span<'_>> {
    let (Some(es), Some(ee)) = (expr_start, expr_end) else {
        return vec![Span::styled(patch_tabs(text), base_style)];
    };

    let text_col_end = text_col_start + text.len();
    if text_col_end <= es || text_col_start >= ee {
        return vec![Span::styled(patch_tabs(text), base_style)];
    }

    let hl_style = base_style
        .bg(Color::Rgb(70, 70, 15))
        .add_modifier(Modifier::BOLD);

    if text_col_start >= es && text_col_end <= ee {
        return vec![Span::styled(patch_tabs(text), hl_style)];
    }

    let mut spans = Vec::new();
    let hl_start = es.saturating_sub(text_col_start).min(text.len());
    let hl_end = ee.saturating_sub(text_col_start).min(text.len());

    if hl_start > 0 {
        spans.push(Span::styled(patch_tabs(&text[..hl_start]), base_style));
    }
    if hl_start < hl_end {
        spans.push(Span::styled(patch_tabs(&text[hl_start..hl_end]), hl_style));
    }
    if hl_end < text.len() {
        spans.push(Span::styled(patch_tabs(&text[hl_end..]), base_style));
    }

    spans
}

#[allow(clippy::match_same_arms)]
fn highlight_style(kind: &str) -> Style {
    const COLOR_PINK: Color = Color::Rgb(0xf5, 0xbd, 0xe6);
    const COLOR_MAUVE: Color = Color::Rgb(0xc6, 0xa0, 0xf6);
    const COLOR_RED: Color = Color::Rgb(0xed, 0x87, 0x96);
    const COLOR_MAROON: Color = Color::Rgb(0xee, 0x99, 0xa0);
    const COLOR_PEACH: Color = Color::Rgb(0xf5, 0xa9, 0x7f);
    const COLOR_YELLOW: Color = Color::Rgb(0xee, 0xd4, 0x9f);
    const COLOR_GREEN: Color = Color::Rgb(0xa6, 0xda, 0x95);
    const COLOR_SKY: Color = Color::Rgb(0x91, 0xd7, 0xe3);
    const COLOR_SAPPHIRE: Color = Color::Rgb(0x7d, 0xc4, 0xe4);
    const COLOR_BLUE: Color = Color::Rgb(0x8a, 0xad, 0xf4);
    const COLOR_LAVENDER: Color = Color::Rgb(0xb7, 0xbd, 0xf8);
    const COLOR_TEXT: Color = Color::Rgb(0xca, 0xd3, 0xf5);
    const COLOR_OVERLAY2: Color = Color::Rgb(0x93, 0x9a, 0xb7);

    let style = Style::default();
    match kind {
        "variable" => style.fg(COLOR_TEXT),
        "variable.builtin" => style.fg(COLOR_RED),
        "variable.parameter" | "parameter" | "variable.parameter.builtin" => style.fg(COLOR_MAROON),
        "variable.member" | "field" => style.fg(COLOR_LAVENDER),
        "constant" | "constant.builtin" => style.fg(COLOR_PEACH),
        "module" | "module.builtin" => style.fg(COLOR_YELLOW).add_modifier(Modifier::ITALIC),
        "label" => style.fg(COLOR_SAPPHIRE),
        "string" => style.fg(COLOR_GREEN),
        "string.escape" | "string.regexp" => style.fg(COLOR_PINK),
        "boolean" | "number" | "number.float" => style.fg(COLOR_PEACH),
        "attribute" => style.fg(COLOR_PEACH),
        "property" => style.fg(COLOR_LAVENDER),
        "function" | "function.call" | "function.method.call" | "method.call" => {
            style.fg(COLOR_BLUE)
        }
        "function.builtin" => style.fg(COLOR_PEACH),
        "constructor" => style.fg(COLOR_YELLOW),
        "operator" => style.fg(COLOR_SKY),
        "keyword" | "keyword.coroutine" => style.fg(COLOR_PINK).add_modifier(Modifier::ITALIC),
        "keyword.function" | "keyword.operator" | "keyword.repeat" | "repeat"
        | "keyword.return" => style.fg(COLOR_MAUVE),
        "keyword.conditional" | "conditional" => {
            style.fg(COLOR_MAUVE).add_modifier(Modifier::ITALIC)
        }
        "punctuation.delimiter" | "punctuation.bracket" => style.fg(COLOR_OVERLAY2),
        "comment" | "comment.documentation" => {
            style.fg(COLOR_OVERLAY2).add_modifier(Modifier::ITALIC)
        }
        _ => style.fg(COLOR_TEXT),
    }
}

fn build_highlight_spans(source: &[u8]) -> Vec<HighlightSpan> {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_lua::LANGUAGE.into())
        .expect("Failed to set Lua language");

    let ast = parser.parse(source, None).expect("Failed to parse source");
    let root = ast.root_node();

    let query = tree_sitter::Query::new(
        &tree_sitter_lua::LANGUAGE.into(),
        include_str!("../../../queries/highlights.scm"),
    )
    .expect("Failed to create query");

    let mut cursor = tree_sitter::QueryCursor::new();
    let mut matches = cursor.matches(&query, root, source);
    let capture_names = query.capture_names();

    let mut spans = Vec::new();
    while let Some(m) = matches.next() {
        for capture in m.captures {
            spans.push(HighlightSpan {
                start: capture.node.start_position(),
                end: capture.node.end_position(),
                kind: capture_names[capture.index as usize].to_string(),
            });
        }
    }

    spans.sort_by_key(|s| s.start);
    spans
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

fn offset_to_line(offsets: &[usize], offset: usize) -> usize {
    match offsets.binary_search(&offset) {
        Ok(line) => line,
        Err(line) => line.saturating_sub(1),
    }
}
