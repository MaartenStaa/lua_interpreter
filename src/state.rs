use crate::{
    chunk::Chunk,
    env,
    value::{
        LuaConst, LuaValue, UpValue,
        callable::{Callable, Method},
    },
    vm::{CallFrameKind, ConstIndex, MAX_STACK_SIZE, call_frame::CallFrame, trace::VMTracer},
};
use std::{
    collections::HashMap,
    fmt::Debug,
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

/// Thread status (coroutine state)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThreadStatus {
    /// Newly created, hasn't run yet
    Created,
    /// Created or yielded, waiting for resume
    Suspended { resume_args_register: Option<u8> },
    /// Currently executing
    Running,
    /// Finished or error
    Dead,
}

/// Result of executing Lua code - either completes or yields
#[derive(Debug)]
pub enum ExecutionResult {
    /// Execution completed normally with return values
    Completed(Vec<LuaValue>),
    /// Execution yielded with values (coroutine suspended)
    Yielded(Vec<LuaValue>),
}

impl ExecutionResult {
    pub(crate) fn unwrap_completed(self) -> Vec<LuaValue> {
        match self {
            ExecutionResult::Completed(v) => v,
            ExecutionResult::Yielded(_) => {
                panic!("expected Completed execution result, got Yielded")
            }
        }
    }
}

/// Execution context for a thread/coroutine (mirrors C's lua_State)
pub struct LuaState {
    // Reference to shared global state
    pub(crate) global: Arc<RwLock<GlobalState>>,

    // Execution state
    pub(crate) stack: Vec<LuaValue>,
    pub(crate) multres: usize,
    pub(crate) call_stack: [CallFrame; MAX_STACK_SIZE],
    pub(crate) call_stack_index: usize,

    // Status tracking
    pub(crate) status: ThreadStatus,
    pub(crate) is_main_thread: bool,

    // Yield handling - set by coroutine.yield to signal a pending yield
    pub(crate) pending_yield: Option<Vec<LuaValue>>,

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

        let mut result = Self::new_uninit(global, true);

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

    pub(crate) fn for_callable(global: Arc<RwLock<GlobalState>>, callable: Callable) -> Self {
        let mut result = Self::new_uninit(global, false);
        match callable.method {
            Method::Closure(closure) => {
                let (max_registers, num_params, has_varargs) = {
                    let global_state = result.global.read().unwrap();
                    let chunk = &global_state.chunks[closure.chunk as usize];
                    (chunk.max_registers, chunk.num_params, chunk.has_varargs)
                };

                // TODO: Do we need to do something with `closure.is_metamethod` here?
                result
                    .push_call_frame(
                        CallFrameKind::for_chunk(closure.chunk),
                        true,
                        0,
                        max_registers,
                        0,
                        0,
                        Some(closure.upvalues),
                        num_params.into(),
                        has_varargs,
                        true,
                    )
                    .expect("cannot stack overflow");
            }
            // Note: This case should never be reached - coroutine.create() rejects native functions
            // If native function support is added in the future, we would need to store `f` in the call frame
            Method::NativeFunction(name, _f) => {
                result
                    .push_call_frame(
                        CallFrameKind::Native { name },
                        true,
                        0,
                        0,
                        0,
                        0,
                        None,
                        0,
                        false,
                        true,
                    )
                    .expect("cannot stack overflow");
            }
        }

        result
    }

    fn new_uninit(global: Arc<RwLock<GlobalState>>, is_main_thread: bool) -> Self {
        Self {
            global,
            stack: Vec::new(),
            multres: 0,
            call_stack: [const { CallFrame::default() }; MAX_STACK_SIZE],
            call_stack_index: 0,
            status: ThreadStatus::Created,
            is_main_thread,
            pending_yield: None,
            tracer: None,
        }
    }

    pub fn with_tracer(mut self, trace_vm: bool) -> Self {
        self.tracer = trace_vm.then(VMTracer::new);
        self
    }

    pub fn status(&self) -> ThreadStatus {
        self.status
    }

    /// Trigger a yield with the given values. This sets pending_yield which will
    /// be checked after the current native function returns.
    pub fn yield_values(&mut self, values: Vec<LuaValue>) {
        self.pending_yield = Some(values);
    }

    /// Check if execution can yield (not across C-call boundary)
    /// A yield is not allowed if there's a C function (native function) in the call stack,
    /// except for the immediate caller (which would be coroutine.yield itself)
    pub fn is_yieldable(&self) -> bool {
        // Check all frames except the top one (which is coroutine.yield)
        // If any is a native function, we cannot yield
        !(0..self.call_stack_index.saturating_sub(1)).any(|i| {
            matches!(
                self.call_stack[i].kind,
                crate::vm::CallFrameKind::Native { .. }
            )
        })
    }
}

impl Debug for LuaState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LuaState")
            .field("stack", &self.stack)
            .field("call_stack", &&self.call_stack[..self.call_stack_index])
            .field("call_stack_index", &self.call_stack_index)
            .field("status", &self.status)
            .finish()
    }
}
