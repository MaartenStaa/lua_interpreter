use std::collections::HashMap;

use tree_sitter::Point;

use crate::{value::LuaValue, vm::ChunkIndex};

#[derive(Debug, Clone)]
pub struct HighlightSpan {
    pub start: Point,
    pub end: Point,
    pub kind: String,
}

#[derive(Debug, Clone)]
pub enum DebugCommand {
    Step,
    StepOver,
    StepOut,
    Continue,
    AddBreakpoint(Breakpoint),
    RemoveBreakpoint(Breakpoint),
    Quit,
}

#[derive(Debug)]
pub enum DebugEvent {
    Paused(Box<StateSnapshot>),
    Completed,
    Error(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Breakpoint {
    Instruction { chunk: ChunkIndex, ip: usize },
    SourceLine { filename: String, line: usize },
}

#[derive(Debug, Clone)]
pub struct LocalVar {
    pub name: String,
    pub register: u8,
    pub value: LuaValue,
}

#[derive(Debug, Clone)]
pub enum CallFrameKindSnapshot {
    Lua {
        chunk_index: ChunkIndex,
        chunk_name: String,
        ip: usize,
        filename: Option<String>,
    },
    Native {
        name: String,
    },
}

#[derive(Debug, Clone)]
pub struct CallFrameSnapshot {
    pub index: usize,
    pub kind: CallFrameKindSnapshot,
    pub locals: Vec<LocalVar>,
}

#[derive(Debug, Clone)]
pub struct BytecodeLine {
    pub ip: usize,
    pub address: String,
    pub raw: String,
    pub opcode_name: String,
    pub operands: String,
    pub source_line: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct SourceMap {
    pub line_to_ips: HashMap<usize, Vec<usize>>,
    pub ip_to_line: HashMap<usize, usize>,
}

#[derive(Debug, Clone)]
pub struct StateSnapshot {
    pub current_chunk: ChunkIndex,
    pub current_ip: usize,
    pub source: String,
    pub filename: Option<String>,
    pub bytecode_lines: Vec<BytecodeLine>,
    pub current_span: Option<(usize, usize)>,
    pub call_stack: Vec<CallFrameSnapshot>,
    pub frame_stack: Vec<LuaValue>,
    pub breakpoints: Vec<Breakpoint>,
    pub source_map: SourceMap,
    pub source_spans: Vec<HighlightSpan>,
}
