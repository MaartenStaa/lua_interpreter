use std::collections::HashMap;

use crate::{
    instruction::Instruction,
    token::Span,
    value::{LuaConst, LuaValue},
    vm::ChunkIndex,
};

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
    Paused(Box<VmState>),
    Completed,
    Error(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Breakpoint {
    Instruction { chunk: ChunkIndex, ip: usize },
    SourceLine { filename: String, line: usize },
}

#[derive(Debug, Clone)]
pub struct ChunkSnapshot {
    pub name: String,
    pub filename: Option<String>,
    pub source: Vec<u8>,
    pub instructions: Vec<Instruction>,
    pub instruction_spans: HashMap<usize, Span>,
    pub consts: Vec<LuaConst>,
    pub local_vars: Vec<LocalVarInfo>,
}

#[derive(Debug, Clone)]
pub struct LocalVarInfo {
    pub name: String,
    pub register: u8,
    pub start_ip: usize,
    pub end_ip: usize,
}

#[derive(Debug, Clone)]
pub enum FrameKind {
    Lua { chunk: ChunkIndex, ip: usize },
    Native { name: String },
}

#[derive(Debug, Clone)]
pub struct FrameSnapshot {
    pub kind: FrameKind,
    pub frame_pointer: usize,
    pub max_registers: u8,
}

#[derive(Debug, Clone)]
pub struct VmState {
    pub current_chunk: ChunkIndex,
    pub current_ip: usize,
    pub chunks: Vec<ChunkSnapshot>,
    pub call_stack: Vec<FrameSnapshot>,
    pub stack: Vec<LuaValue>,
    pub breakpoints: Vec<Breakpoint>,
}
