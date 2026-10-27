use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

use crate::{
    value::{LuaValue, UpValue},
    vm::ChunkIndex,
};

#[derive(Debug, Copy, Clone)]
pub(crate) enum CallFrameKind {
    Lua { chunk: ChunkIndex, ip: usize },
    Native { name: &'static str },
}

impl CallFrameKind {
    pub(crate) fn for_chunk(chunk: ChunkIndex) -> Self {
        Self::Lua { chunk, ip: 0 }
    }
}

#[derive(Debug)]
pub(crate) struct CallFrame {
    pub(crate) kind: CallFrameKind,
    pub(crate) border_frame: bool,
    pub(crate) frame_pointer: usize,
    pub(crate) return_addr: usize,
    pub(crate) return_value_register: u8,
    pub(crate) upvalues: Option<HashMap<usize, Arc<RwLock<UpValue>>>>,
    pub(crate) num_args: usize,
    pub(crate) varargs: Vec<LuaValue>,
    pub(crate) takes_varargs: bool,
    /// 0 = multres, otherwise expect `num_return_values - 1` values.
    pub(crate) num_return_values: u8,
    pub(crate) to_close: Vec<u8>,
    pub(crate) max_registers: u8,
}

impl CallFrame {
    /// Sets the instruction pointer (IP) of the call frame. Panics if the call frame is not a Lua
    /// call frame.
    pub(crate) fn set_ip(&mut self, new_ip: usize) {
        if let CallFrameKind::Lua { ref mut ip, .. } = self.kind {
            *ip = new_ip;
        } else {
            panic!("attempted to set IP on non-Lua call frame");
        }
    }

    #[inline]
    pub(crate) fn inc_ip(&mut self) {
        if let CallFrameKind::Lua { ref mut ip, .. } = self.kind {
            *ip += 1;
        } else {
            panic!("attempted to increment IP on non-Lua call frame");
        }
    }
}

impl CallFrame {
    pub(crate) const fn default() -> Self {
        Self {
            kind: CallFrameKind::Lua { chunk: 0, ip: 0 },
            border_frame: false,
            frame_pointer: 0,
            return_addr: 0,
            return_value_register: 0,
            upvalues: None,
            num_args: 0,
            varargs: vec![],
            takes_varargs: false,
            num_return_values: 0,
            to_close: vec![],
            max_registers: 0,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct PoppedCallFrame {
    pub(crate) kind: CallFrameKind,
    pub(crate) frame_pointer: usize,
    pub(crate) border_frame: bool,
    pub(crate) return_addr: usize,
    pub(crate) return_value_register: u8,
    /// 0 = multres, otherwise expect `num_return_values - 1` values.
    pub(crate) num_return_values: u8,
}
