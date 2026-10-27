use std::sync::{
    Mutex,
    mpsc::{Receiver, Sender},
};

use crate::{
    error::lua_error,
    state::{GlobalState, LuaState},
    vm::{ChunkIndex, call_frame::CallFrameKind},
};

use super::{
    breakpoint::BreakpointSet,
    protocol::{
        Breakpoint, ChunkSnapshot, DebugCommand, DebugEvent, FrameKind, FrameSnapshot,
        LocalVarInfo, VmState,
    },
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
        chunks: &[crate::chunk::Chunk],
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
            RunMode::Paused | RunMode::Step => true,
            RunMode::StepOver { target_depth } => state.call_stack_index <= target_depth,
            RunMode::StepOut { target_depth } => state.call_stack_index < target_depth,
            RunMode::Continue => self.breakpoints.is_hit(chunk_index, ip),
        };

        if !should_pause {
            return Ok(());
        }

        let vm_state = self.build_vm_state(state, chunk_index, ip);
        let _ = self.evt_tx.send(DebugEvent::Paused(Box::new(vm_state)));

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
                    let CallFrameKind::Lua {
                        chunk: ci, ip: ip2, ..
                    } = state.call_frame().kind
                    else {
                        unreachable!();
                    };
                    let vm_state = self.build_vm_state_with_global(state, &global, ci, ip2);
                    let _ = self.evt_tx.send(DebugEvent::Paused(Box::new(vm_state)));
                }
                Ok(DebugCommand::RemoveBreakpoint(bp)) => {
                    self.breakpoints.remove(&bp);
                    let global = state.global.read().unwrap();
                    let CallFrameKind::Lua {
                        chunk: ci, ip: ip2, ..
                    } = state.call_frame().kind
                    else {
                        unreachable!();
                    };
                    let vm_state = self.build_vm_state_with_global(state, &global, ci, ip2);
                    let _ = self.evt_tx.send(DebugEvent::Paused(Box::new(vm_state)));
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

    fn build_vm_state(&self, state: &LuaState, chunk_index: ChunkIndex, ip: usize) -> VmState {
        let global = state.global.read().unwrap();
        self.build_vm_state_with_global(state, &global, chunk_index, ip)
    }

    fn build_vm_state_with_global(
        &self,
        state: &LuaState,
        global: &GlobalState,
        chunk_index: ChunkIndex,
        ip: usize,
    ) -> VmState {
        let chunks = global
            .chunks
            .iter()
            .map(|c| ChunkSnapshot {
                name: String::from_utf8_lossy(&c.name).into_owned(),
                filename: c
                    .filename
                    .as_ref()
                    .and_then(|p| p.file_name())
                    .and_then(|n| n.to_str())
                    .map(String::from),
                source: c.source.as_ref().clone(),
                instructions: c.instructions.clone(),
                instruction_spans: c.instruction_spans.clone(),
                consts: c.consts.clone(),
                local_vars: c
                    .local_vars
                    .iter()
                    .map(|v| LocalVarInfo {
                        name: String::from_utf8_lossy(&v.name).into_owned(),
                        register: v.register,
                        start_ip: v.start_ip,
                        end_ip: v.end_ip,
                    })
                    .collect(),
            })
            .collect();

        let call_stack = (0..state.call_stack_index)
            .map(|i| {
                let frame = &state.call_stack[i];
                let kind = match &frame.kind {
                    CallFrameKind::Lua { chunk, ip } => FrameKind::Lua {
                        chunk: *chunk,
                        ip: *ip,
                    },
                    CallFrameKind::Native { name } => FrameKind::Native {
                        name: name.to_string(),
                    },
                };
                FrameSnapshot {
                    kind,
                    frame_pointer: frame.frame_pointer,
                    max_registers: frame.max_registers,
                }
            })
            .collect();

        let stack = state.get_stack().to_vec();

        VmState {
            current_chunk: chunk_index,
            current_ip: ip,
            chunks,
            call_stack,
            stack,
            breakpoints: self.breakpoints.specs().to_vec(),
        }
    }
}
