use std::sync::{
    Mutex,
    mpsc::{Receiver, Sender},
};

use crate::{
    chunk::Chunk,
    error::lua_error,
    state::LuaState,
    value::LuaValue,
    vm::{ChunkIndex, call_frame::CallFrameKind},
};

use super::{
    breakpoint::BreakpointSet,
    protocol::{
        Breakpoint, BytecodeLine, CallFrameKindSnapshot, CallFrameSnapshot, DebugCommand,
        DebugEvent, DisplayValue, LocalVar, StateSnapshot,
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

        let breakpoints = self.breakpoints.specs().to_vec();

        StateSnapshot {
            current_chunk: chunk_index,
            current_ip: ip,
            source,
            filename,
            bytecode_lines,
            current_span,
            call_stack,
            frame_stack,
            breakpoints,
            source_map,
        }
    }

    fn build_snapshot_with_global(
        &self,
        state: &LuaState,
        global: &crate::state::GlobalState,
    ) -> StateSnapshot {
        let CallFrameKind::Lua { chunk: chunk_index, ip } = state.call_frame().kind else {
            unreachable!();
        };
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
                let text = format!(
                    "{chunk_index}:{ip:04}  0x{opcode:02x} {raw:#010x}  {name}",
                    opcode = instr.opcode().raw(),
                    raw = instr.raw(),
                    name = format!("{:?}", instr.opcode()),
                );
                BytecodeLine {
                    ip,
                    text,
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
