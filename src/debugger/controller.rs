use std::sync::{
    Mutex,
    mpsc::{Receiver, Sender},
};

use crate::{
    chunk::Chunk,
    error::lua_error,
    instruction::{Instruction, OpCode},
    state::LuaState,
    value::{LuaConst, LuaValue},
    vm::{ChunkIndex, ConstIndex, call_frame::CallFrameKind},
};

use crate::{lexer::Lexer, token::TokenKind};

use super::{
    breakpoint::BreakpointSet,
    protocol::{
        Breakpoint, BytecodeLine, CallFrameKindSnapshot, CallFrameSnapshot, DebugCommand,
        DebugEvent, DisplayValue, HighlightKind, HighlightSpan, LocalVar, StateSnapshot,
    },
    source_map::build_source_map,
};

#[derive(Debug, Clone, Copy)]
pub(crate) enum RunMode {
    Paused,
    Step,
    StepOver { target_depth: usize },
    StepOut { target_depth: usize },
    Continue,
}

pub(crate) struct DebuggerController {
    cmd_rx: Mutex<Receiver<DebugCommand>>,
    evt_tx: Sender<DebugEvent>,
    pub(crate) breakpoints: BreakpointSet,
    mode: RunMode,
}

impl DebuggerController {
    pub(crate) fn new(
        cmd_rx: Receiver<DebugCommand>,
        evt_tx: Sender<DebugEvent>,
        initial_breakpoints: Vec<Breakpoint>,
        break_on_start: bool,
        chunks: &[Chunk],
    ) -> Self {
        let mut breakpoints = BreakpointSet::new();
        for bp in initial_breakpoints {
            breakpoints.add(bp, chunks);
        }

        let mode = if break_on_start || breakpoints.specs().is_empty() {
            RunMode::Paused
        } else {
            RunMode::Continue
        };

        Self {
            cmd_rx: Mutex::new(cmd_rx),
            evt_tx,
            breakpoints,
            mode,
        }
    }

    pub(crate) fn on_instruction(
        &mut self,
        state: &LuaState,
        chunk_index: ChunkIndex,
        ip: usize,
    ) -> crate::Result<()> {
        let should_pause = match self.mode {
            RunMode::Paused => true,
            RunMode::Step => true,
            RunMode::StepOver { target_depth } => {
                state.call_stack_index <= target_depth
            }
            RunMode::StepOut { target_depth } => {
                state.call_stack_index < target_depth
            }
            RunMode::Continue => self.breakpoints.is_hit(chunk_index, ip),
        };

        if !should_pause {
            return Ok(());
        }

        let snapshot = self.build_snapshot(state, chunk_index, ip);
        let _ = self.evt_tx.send(DebugEvent::Paused(snapshot));

        self.wait_for_command(state)
    }

    pub(crate) fn send_completed(&self) {
        let _ = self.evt_tx.send(DebugEvent::Completed);
    }

    pub(crate) fn send_error(&self, msg: String) {
        let _ = self.evt_tx.send(DebugEvent::Error(msg));
    }

    fn wait_for_command(&mut self, state: &LuaState) -> crate::Result<()> {
        let cmd_rx = self.cmd_rx.lock().unwrap();
        loop {
            match cmd_rx.recv() {
                Ok(DebugCommand::Step) => {
                    self.mode = RunMode::Step;
                    return Ok(());
                }
                Ok(DebugCommand::StepOver) => {
                    self.mode = RunMode::StepOver {
                        target_depth: state.call_stack_index,
                    };
                    return Ok(());
                }
                Ok(DebugCommand::StepOut) => {
                    self.mode = RunMode::StepOut {
                        target_depth: state.call_stack_index,
                    };
                    return Ok(());
                }
                Ok(DebugCommand::Continue) => {
                    self.mode = RunMode::Continue;
                    return Ok(());
                }
                Ok(DebugCommand::AddBreakpoint(bp)) => {
                    let global = state.global.read().unwrap();
                    self.breakpoints.add(bp, &global.chunks);
                    let snapshot = self.build_snapshot_with_global(state, &global);
                    let _ = self.evt_tx.send(DebugEvent::Paused(snapshot));
                }
                Ok(DebugCommand::RemoveBreakpoint(bp)) => {
                    self.breakpoints.remove(&bp);
                    let global = state.global.read().unwrap();
                    let snapshot = self.build_snapshot_with_global(state, &global);
                    let _ = self.evt_tx.send(DebugEvent::Paused(snapshot));
                }
                Ok(DebugCommand::Quit) => {
                    return Err(lua_error!("debugger: execution terminated by user"));
                }
                Err(_) => {
                    return Err(lua_error!("debugger: TUI disconnected"));
                }
            }
        }
    }

    fn build_snapshot(&self, state: &LuaState, chunk_index: ChunkIndex, ip: usize) -> StateSnapshot {
        let global = state.global.read().unwrap();
        self.build_snapshot_inner(state, &global, chunk_index, ip)
    }

    fn build_snapshot_with_global(
        &self,
        state: &LuaState,
        global: &crate::state::GlobalState,
    ) -> StateSnapshot {
        let CallFrameKind::Lua { chunk: chunk_index, ip } = state.call_frame().kind else {
            unreachable!();
        };
        self.build_snapshot_inner(state, global, chunk_index, ip)
    }

    fn build_snapshot_inner(
        &self,
        state: &LuaState,
        global: &crate::state::GlobalState,
        chunk_index: ChunkIndex,
        ip: usize,
    ) -> StateSnapshot {
        let chunk = &global.chunks[chunk_index as usize];

        let source = String::from_utf8_lossy(&chunk.source).into_owned();
        let filename = chunk
            .filename
            .as_ref()
            .and_then(|p| p.file_name())
            .and_then(|n| n.to_str())
            .map(String::from);

        let current_span = chunk
            .instruction_spans
            .get(&ip)
            .map(|s| (s.start, s.end));

        let source_map = build_source_map(chunk);
        let bytecode_lines = self.build_bytecode_lines(chunk, chunk_index, &source_map);
        let call_stack = self.build_call_stack(state, &global.chunks);
        let frame_stack = self.build_frame_stack(state);
        let highlight_spans = build_highlight_spans(&chunk.name, &chunk.source);

        StateSnapshot {
            current_chunk: chunk_index,
            current_ip: ip,
            source,
            filename,
            bytecode_lines,
            current_span,
            call_stack,
            frame_stack,
            breakpoints: self.breakpoints.specs().to_vec(),
            source_map,
            highlight_spans,
        }
    }

    fn build_bytecode_lines(
        &self,
        chunk: &Chunk,
        chunk_index: ChunkIndex,
        source_map: &super::protocol::SourceMap,
    ) -> Vec<BytecodeLine> {
        chunk
            .instructions
            .iter()
            .enumerate()
            .map(|(ip, instr)| {
                let address = format!("{chunk_index}:{ip:04}");
                let raw = format!(
                    "0x{:02x} {:#010x}",
                    instr.opcode().raw(),
                    instr.raw()
                );
                let opcode_name = opcode_display_name(instr.opcode());
                let operands = format_operands(chunk, *instr);
                BytecodeLine {
                    ip,
                    address,
                    raw,
                    opcode_name,
                    operands,
                    source_line: source_map.ip_to_line.get(&ip).copied(),
                }
            })
            .collect()
    }

    fn build_call_stack(
        &self,
        state: &LuaState,
        chunks: &[Chunk],
    ) -> Vec<CallFrameSnapshot> {
        (0..state.call_stack_index)
            .map(|i| {
                let frame = &state.call_stack[i];
                let kind = match &frame.kind {
                    CallFrameKind::Lua { chunk, ip } => {
                        let c = &chunks[*chunk as usize];
                        CallFrameKindSnapshot::Lua {
                            chunk_index: *chunk,
                            chunk_name: String::from_utf8_lossy(&c.name).into_owned(),
                            ip: *ip,
                            filename: c
                                .filename
                                .as_ref()
                                .and_then(|p| p.file_name())
                                .and_then(|n| n.to_str())
                                .map(String::from),
                        }
                    }
                    CallFrameKind::Native { name } => CallFrameKindSnapshot::Native {
                        name: (*name).to_string(),
                    },
                };

                let locals = match &frame.kind {
                    CallFrameKind::Lua { chunk, ip } => {
                        self.build_locals(state, &chunks[*chunk as usize], *ip, frame.frame_pointer)
                    }
                    CallFrameKind::Native { .. } => vec![],
                };

                CallFrameSnapshot {
                    index: i,
                    kind,
                    locals,
                }
            })
            .collect()
    }

    fn build_locals(
        &self,
        state: &LuaState,
        chunk: &Chunk,
        ip: usize,
        frame_pointer: usize,
    ) -> Vec<LocalVar> {
        chunk
            .local_vars
            .iter()
            .filter(|var| ip >= var.start_ip && ip <= var.end_ip)
            .map(|var| {
                let reg = var.register as usize;
                let value = if frame_pointer + reg < state.stack.len() {
                    value_to_display(&state.stack[frame_pointer + reg])
                } else {
                    DisplayValue::Nil
                };
                LocalVar {
                    name: String::from_utf8_lossy(&var.name).into_owned(),
                    register: var.register,
                    value,
                }
            })
            .collect()
    }

    fn build_frame_stack(&self, state: &LuaState) -> Vec<DisplayValue> {
        if state.call_stack_index == 0 {
            return vec![];
        }
        let frame = &state.call_stack[state.call_stack_index - 1];
        let start = frame.frame_pointer;
        let end = (start + frame.max_registers as usize).min(state.stack.len());
        state.stack[start..end]
            .iter()
            .map(value_to_display)
            .collect()
    }
}

fn opcode_display_name(opcode: OpCode) -> String {
    match opcode {
        OpCode::LoadConst => "LOAD_CONST",
        OpCode::LoadClosure => "LOAD_CLOSURE",
        OpCode::LoadNil => "LOAD_NIL",
        OpCode::LoadShort => "LOAD_SHORT",
        OpCode::LoadShortFloat => "LOAD_SHORT_FLOAT",
        OpCode::Add => "ADD",
        OpCode::Sub => "SUB",
        OpCode::Mul => "MUL",
        OpCode::Div => "DIV",
        OpCode::Mod => "MOD",
        OpCode::Pow => "POW",
        OpCode::IDiv => "IDIV",
        OpCode::Band => "BAND",
        OpCode::Bor => "BOR",
        OpCode::Bxor => "BXOR",
        OpCode::Shl => "SHL",
        OpCode::Shr => "SHR",
        OpCode::Eq => "EQ",
        OpCode::Ne => "NE",
        OpCode::Lt => "LT",
        OpCode::Le => "LE",
        OpCode::Concat => "CONCAT",
        OpCode::Neg => "NEG",
        OpCode::Not => "NOT",
        OpCode::Len => "LEN",
        OpCode::BNot => "BNOT",
        OpCode::Mov => "MOV",
        OpCode::SetUpval => "SET_UPVAL",
        OpCode::GetUpval => "GET_UPVAL",
        OpCode::LoadVararg => "LOAD_VARARG",
        OpCode::ToClose => "TO_CLOSE",
        OpCode::Close => "CLOSE",
        OpCode::NewTable => "NEW_TABLE",
        OpCode::SetTable => "SET_TABLE",
        OpCode::SetTableConstKey => "SET_TABLE_CK",
        OpCode::SetTableUpval => "SET_TABLE_UP",
        OpCode::SetTableUpvalConstKey => "SET_TABLE_UP_CK",
        OpCode::GetTable => "GET_TABLE",
        OpCode::GetTableConstKey => "GET_TABLE_CK",
        OpCode::GetTableUpval => "GET_TABLE_UP",
        OpCode::GetTableUpvalConstKey => "GET_TABLE_UP_CK",
        OpCode::AppendToTable => "APPEND_TO_TABLE",
        OpCode::AppendToTableM => "APPEND_TO_TABLE_M",
        OpCode::Call => "CALL",
        OpCode::CallM => "CALL_M",
        OpCode::CallT => "CALL_T",
        OpCode::CallTM => "CALL_T_M",
        OpCode::Return => "RETURN",
        OpCode::Return0 => "RETURN0",
        OpCode::Return1 => "RETURN1",
        OpCode::ReturnM => "RETURN_M",
        OpCode::Jmp => "JMP",
        OpCode::JmpTrue => "JMP_TRUE",
        OpCode::JmpFalse => "JMP_FALSE",
        OpCode::JmpClose => "JMP_CLOSE",
        OpCode::Error => "ERROR",
    }
    .to_string()
}

fn format_operands(chunk: &Chunk, instr: Instruction) -> String {
    let r = |n: u8| format!("R{n}");
    let num_results = |n: u8| {
        if n == 0 {
            "MULTRES".to_string()
        } else {
            format!("{}", n - 1)
        }
    };
    let fmt_const = |idx: ConstIndex| -> String {
        match chunk.get_const(idx) {
            LuaConst::Nil => "nil".to_string(),
            LuaConst::Boolean(b) => format!("{b}"),
            LuaConst::Number(n) => format!("{n}"),
            LuaConst::String(s) => format!("\"{}\"", String::from_utf8_lossy(s)),
            LuaConst::Table(t) => format!("table({} fields)", t.keys().count()),
        }
    };

    match instr.opcode() {
        OpCode::LoadConst => format!("{} {}", r(instr.a()), fmt_const(instr.b())),
        OpCode::LoadClosure => format!("{} chunk:{}", r(instr.a()), instr.bc()),
        OpCode::LoadNil => format!("{} {}", r(instr.a()), r(instr.b())),
        OpCode::LoadShort => format!("{} {}", r(instr.a()), instr.bc().cast_signed()),
        OpCode::LoadShortFloat => format!("{} {}", r(instr.a()), instr.bc().cast_signed() as f32),
        OpCode::Add | OpCode::Sub | OpCode::Mul | OpCode::Div | OpCode::Mod | OpCode::Pow
        | OpCode::IDiv | OpCode::Band | OpCode::Bor | OpCode::Bxor | OpCode::Shl
        | OpCode::Shr | OpCode::Eq | OpCode::Ne | OpCode::Lt | OpCode::Le | OpCode::Concat
        | OpCode::SetTable | OpCode::GetTable => {
            format!("{} {} {}", r(instr.a()), r(instr.b()), r(instr.c()))
        }
        OpCode::Neg | OpCode::Not | OpCode::Len | OpCode::BNot | OpCode::Mov => {
            format!("{} {}", r(instr.a()), r(instr.b()))
        }
        OpCode::SetUpval => format!("U{} {}", instr.a(), r(instr.b())),
        OpCode::GetUpval => format!("{} U{}", r(instr.a()), instr.b()),
        OpCode::LoadVararg => format!("{} {}", r(instr.a()), num_results(instr.b())),
        OpCode::ToClose | OpCode::Close | OpCode::NewTable | OpCode::Return1 => {
            r(instr.a())
        }
        OpCode::SetTableConstKey => {
            format!("{} {} {}", r(instr.a()), fmt_const(instr.b()), r(instr.c()))
        }
        OpCode::SetTableUpval => format!("{} U{} {}", r(instr.a()), instr.b(), r(instr.c())),
        OpCode::SetTableUpvalConstKey => {
            format!("U{} {} {}", instr.a(), fmt_const(instr.b()), r(instr.c()))
        }
        OpCode::GetTableConstKey => {
            format!("{} {} {}", r(instr.a()), r(instr.b()), fmt_const(instr.c()))
        }
        OpCode::GetTableUpval => format!("{} U{} {}", r(instr.a()), instr.b(), r(instr.c())),
        OpCode::GetTableUpvalConstKey => {
            format!("{} U{} {}", r(instr.a()), instr.b(), fmt_const(instr.c()))
        }
        OpCode::AppendToTable | OpCode::AppendToTableM => {
            format!("{} {}", r(instr.a()), instr.b())
        }
        OpCode::Call | OpCode::CallM | OpCode::CallT | OpCode::CallTM => {
            format!("{} {} {}", r(instr.a()), num_results(instr.b()), instr.c())
        }
        OpCode::Return | OpCode::ReturnM => format!("{} {}", r(instr.a()), instr.b()),
        OpCode::Return0 => String::new(),
        OpCode::Jmp => format!("{:04}", instr.bc()),
        OpCode::JmpTrue | OpCode::JmpFalse | OpCode::JmpClose => {
            format!("{} {:04}", r(instr.a()), instr.bc())
        }
        OpCode::Error => format!("{}", instr.a()),
    }
}

fn build_highlight_spans(chunk_name: &[u8], source: &[u8]) -> Vec<HighlightSpan> {
    let lexer = Lexer::new(chunk_name, source);
    let mut spans = Vec::new();
    let mut covered_end = 0;

    for result in lexer {
        let Ok(token) = result else { break };

        if token.span.start > covered_end {
            find_comments(source, covered_end, token.span.start, &mut spans);
        }

        let kind = match &token.kind {
            TokenKind::And
            | TokenKind::Break
            | TokenKind::Do
            | TokenKind::Else
            | TokenKind::ElseIf
            | TokenKind::End
            | TokenKind::For
            | TokenKind::Function
            | TokenKind::Goto
            | TokenKind::If
            | TokenKind::In
            | TokenKind::Local
            | TokenKind::Not
            | TokenKind::Or
            | TokenKind::Repeat
            | TokenKind::Return
            | TokenKind::Then
            | TokenKind::Until
            | TokenKind::While => HighlightKind::Keyword,

            TokenKind::True | TokenKind::False | TokenKind::Nil => HighlightKind::Literal,
            TokenKind::Integer(_) | TokenKind::Float(_) => HighlightKind::Literal,
            TokenKind::String(_) => HighlightKind::String,
            TokenKind::Identifier(_) => HighlightKind::Identifier,

            _ => HighlightKind::Operator,
        };

        spans.push(HighlightSpan {
            start: token.span.start,
            end: token.span.end,
            kind,
        });
        covered_end = token.span.end;
    }

    if covered_end < source.len() {
        find_comments(source, covered_end, source.len(), &mut spans);
    }

    spans.sort_by_key(|s| s.start);
    spans
}

fn find_comments(source: &[u8], from: usize, to: usize, spans: &mut Vec<HighlightSpan>) {
    let region = &source[from..to];
    let mut pos = 0;
    while pos < region.len() {
        if pos + 1 < region.len() && region[pos] == b'-' && region[pos + 1] == b'-' {
            let comment_start = from + pos;
            if pos + 3 < region.len() && region[pos + 2] == b'[' {
                let eq_count = region[pos + 3..]
                    .iter()
                    .take_while(|&&b| b == b'=')
                    .count();
                if pos + 3 + eq_count < region.len() && region[pos + 3 + eq_count] == b'[' {
                    let mut close_bracket = b"]".to_vec();
                    close_bracket.extend(std::iter::repeat_n(b'=', eq_count));
                    close_bracket.push(b']');
                    if let Some(end_pos) = find_subsequence(&source[comment_start..], &close_bracket) {
                        let comment_end = comment_start + end_pos + close_bracket.len();
                        spans.push(HighlightSpan {
                            start: comment_start,
                            end: comment_end.min(to),
                            kind: HighlightKind::Comment,
                        });
                        pos = comment_end - from;
                        continue;
                    }
                }
            }
            let line_end = region[pos..]
                .iter()
                .position(|&b| b == b'\n')
                .map_or(to, |p| from + pos + p);
            spans.push(HighlightSpan {
                start: comment_start,
                end: line_end,
                kind: HighlightKind::Comment,
            });
            pos = line_end - from;
            continue;
        }
        pos += 1;
    }
}

fn find_subsequence(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    haystack
        .windows(needle.len())
        .position(|window| window == needle)
}

fn value_to_display(value: &LuaValue) -> DisplayValue {
    if value.is_nil() {
        return DisplayValue::Nil;
    }
    if value.is_bool() {
        return DisplayValue::Boolean(unsafe { value.as_bool() });
    }
    if value.is_float() {
        return DisplayValue::Float(unsafe { value.as_float() });
    }
    if value.is_integer() {
        return DisplayValue::Integer(unsafe { value.as_integer() });
    }
    if value.is_string() {
        let s = unsafe { value.as_string() };
        return DisplayValue::String(String::from_utf8_lossy(s).into_owned());
    }
    if value.is_object() {
        let type_name = value.type_name();
        let summary = format!("{value}");
        return DisplayValue::Object { type_name, summary };
    }
    if value.is_upvalue() {
        let inner = unsafe { value.as_upvalue() }.read().unwrap();
        return value_to_display(&inner.0);
    }
    DisplayValue::Nil
}
