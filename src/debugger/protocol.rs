use std::collections::HashMap;

use crate::vm::ChunkIndex;

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
    Paused(StateSnapshot),
    Completed,
    Error(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Breakpoint {
    Instruction { chunk: ChunkIndex, ip: usize },
    SourceLine { filename: String, line: usize },
}

#[derive(Debug, Clone)]
pub enum DisplayValue {
    Nil,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Object {
        type_name: &'static str,
        summary: String,
    },
}

impl std::fmt::Display for DisplayValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::Integer(i) => write!(f, "{i}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::String(s) => write!(f, "\"{s}\""),
            Self::Object { type_name, summary } => write!(f, "{type_name}: {summary}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocalVar {
    pub name: String,
    pub register: u8,
    pub value: DisplayValue,
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
    pub text: String,
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
    pub frame_stack: Vec<DisplayValue>,
    pub breakpoints: Vec<Breakpoint>,
    pub source_map: SourceMap,
}
