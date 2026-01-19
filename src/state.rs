use crate::{
    chunk::Chunk,
    env,
    value::{LuaConst, LuaValue, UpValue},
    vm::{CallFrameKind, ConstIndex, MAX_STACK_SIZE, call_frame::CallFrame, trace::VMTracer},
};
use std::{
    collections::HashMap,
    sync::{Arc, LazyLock, RwLock},
};

/// Global state shared across all threads (mirrors C's global_State)
#[derive(Debug)]
pub struct GlobalState {
    pub(crate) chunks: Vec<Chunk>,
    pub(crate) consts: Vec<LuaConst>,
    pub(crate) const_index: u16,
    pub(crate) global_env: Arc<RwLock<UpValue>>,
}

static GLOBAL_STATE: LazyLock<Arc<RwLock<GlobalState>>> =
    LazyLock::new(|| Arc::new(RwLock::new(GlobalState::new())));

impl GlobalState {
    pub fn get() -> Arc<RwLock<Self>> {
        GLOBAL_STATE.clone()
    }

    fn new() -> Self {
        Self {
            chunks: Vec::new(),
            consts: Vec::new(),
            const_index: 0,
            global_env: env::create_global_env(),
        }
    }

    pub fn add_chunk(&mut self, chunk: Chunk) -> crate::Result<u16> {
        let index = self.chunks.len();
        if index >= u16::MAX as usize {
            return Err(crate::error::lua_error!("too many chunks"));
        }
        self.chunks.push(chunk);
        Ok(index as u16)
    }

    pub fn get_chunk(&self, index: u16) -> Option<&Chunk> {
        self.chunks.get(index as usize)
    }

    pub fn register_const(&mut self, constant: LuaConst) -> u16 {
        let index = self.const_index;
        self.consts.push(constant);
        self.const_index += 1;
        index
    }

    pub fn lookup_const(&self, constant: &LuaConst) -> Option<u16> {
        self.consts[..self.const_index as usize]
            .iter()
            .position(|c| c == constant)
            .map(|i| i as u16)
    }

    pub fn get_consts(&self) -> &[LuaConst] {
        &self.consts
    }

    pub fn get_global_env(&self) -> Arc<RwLock<UpValue>> {
        self.global_env.clone()
    }
}

/// Execution context for a thread/coroutine (mirrors C's lua_State)
#[derive(Debug)]
pub struct LuaState {
    // Reference to shared global state
    pub(crate) global: Arc<RwLock<GlobalState>>,

    // Execution state
    pub(crate) stack: Vec<LuaValue>,
    pub(crate) multres: usize,
    pub(crate) call_stack: [CallFrame; MAX_STACK_SIZE],
    pub(crate) call_stack_index: usize,

    // Debug/trace support
    pub(crate) tracer: Option<VMTracer>,
}

impl LuaState {
    pub(crate) fn new(global: Arc<RwLock<GlobalState>>, chunk_index: ConstIndex) -> Self {
        let max_registers = {
            let global_state = global.read().unwrap();
            assert!(chunk_index < global_state.chunks.len() as u16);

            let chunk = &global_state.chunks[chunk_index as usize];
            assert!(
                chunk.upvalues.len() == 1,
                "should have exactly one upvalue for _ENV"
            );

            chunk.max_registers
        };

        let mut result = Self {
            global,
            stack: Vec::new(),
            multres: 0,
            call_stack: [const { CallFrame::default() }; MAX_STACK_SIZE],
            call_stack_index: 0,
            tracer: None,
        };

        // Initialize with one call frame to start execution
        result
            .push_call_frame(
                CallFrameKind::for_chunk(chunk_index),
                true,
                0,
                max_registers,
                0, // Not applicable for border frames
                0,
                Some({
                    let mut map = HashMap::new();
                    map.insert(0, result.get_global_env());
                    map
                }),
                0,
                true,
                true,
            )
            .expect("cannot stack overflow");

        result
    }

    pub fn with_tracer(mut self, trace_vm: bool) -> Self {
        self.tracer = trace_vm.then(VMTracer::new);
        self
    }
}
