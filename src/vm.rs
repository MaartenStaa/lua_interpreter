use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::Debug,
    sync::{Arc, RwLock},
};

use crate::{
    ExecutionResult, ThreadStatus,
    chunk::{Chunk, RegisterName},
    error::{IntoLuaError, RuntimeError, lua_error},
    instruction::OpCode,
    macros::{assert_closure, assert_table},
    state::LuaState,
    value::{
        LuaClosure, LuaConst, LuaObject, LuaString, LuaTable, LuaValue, NativeLuaFunction, UpValue,
        callable::{Callable, Method},
        metatables::{self, INDEX_KEY, NEWINDEX_KEY},
    },
    vm::call_frame::{CallFrame, PoppedCallFrame},
};

pub(crate) use call_frame::CallFrameKind;

mod arg;
pub(crate) mod call_frame;
mod close;
mod error;
pub(crate) mod trace;

pub(crate) const MAX_STACK_SIZE: usize = 256;

pub type JumpAddr = u16;
pub type ConstIndex = u8;
pub type ChunkIndex = u16;

impl LuaState {
    #[must_use]
    pub fn get_global_env(&self) -> Arc<RwLock<UpValue>> {
        self.global.read().unwrap().get_global_env()
    }

    // Safe way to access chunk data - takes a callback that processes the chunk while lock is held
    pub(crate) fn with_chunk<F, R>(&self, index: ChunkIndex, f: F) -> Option<R>
    where
        F: FnOnce(&Chunk) -> R,
    {
        let global = self.global.read().unwrap();
        global.chunks.get(index as usize).map(f)
    }

    #[must_use]
    pub fn get_current_chunk_name(&self) -> Option<LuaString> {
        match self.call_frame().kind {
            CallFrameKind::Lua { chunk, .. } => self.with_chunk(chunk, |c| c.name.clone()),
            CallFrameKind::Native { .. } => None,
        }
    }

    #[must_use]
    pub fn get_chunk_name(&self, chunk: ChunkIndex) -> Option<LuaString> {
        self.with_chunk(chunk, |c| c.name.clone())
    }

    fn describe_register(
        &self,
        chunk_index: ChunkIndex,
        ip: usize,
        register: u8,
    ) -> Option<String> {
        let global = self.global.read().unwrap();
        let chunk = global
            .chunks
            .get(chunk_index as usize)
            .expect("chunk index is always valid");
        match chunk.get_register_name(register, ip) {
            Some(RegisterName::Local(n)) => Some(format!("local '{}'", String::from_utf8_lossy(n))),
            Some(RegisterName::Upvalue(n)) => {
                Some(format!("upvalue '{}'", String::from_utf8_lossy(n)))
            }
            Some(RegisterName::Global { key_const_index }) => {
                if let LuaConst::String(s) = chunk.get_const(key_const_index) {
                    Some(format!("global '{}'", String::from_utf8_lossy(s)))
                } else {
                    None
                }
            }
            Some(RegisterName::Field { key_const_index }) => {
                if let LuaConst::String(s) = chunk.get_const(key_const_index) {
                    Some(format!("field '{}'", String::from_utf8_lossy(s)))
                } else {
                    None
                }
            }
            None => None,
        }
    }

    pub(crate) fn get_stack(&self) -> &[LuaValue] {
        &self.stack
    }

    #[inline]
    pub(crate) fn get(&self, register: impl Into<usize>) -> LuaValue {
        let frame_pointer = self.call_frame().frame_pointer;
        let value = &self.stack[frame_pointer + register.into()];
        if value.is_upvalue() {
            unsafe { value.as_upvalue() }.read().unwrap().0.clone()
        } else {
            value.clone()
        }
    }

    #[inline]
    fn get_upvalue(&self, index: impl Into<usize>) -> LuaValue {
        let upval_index = index.into();
        let frame = self.call_frame();
        let upvalues = frame.upvalues.as_ref().expect("frame has no upvalues");
        let upvalue = upvalues
            .get(&upval_index)
            .unwrap_or_else(|| panic!("upvalue {upval_index} not found"));
        upvalue.read().unwrap().clone()
    }

    #[inline]
    fn get_ref(&self, register: impl Into<usize>) -> Cow<'_, LuaValue> {
        let frame_pointer = self.call_frame().frame_pointer;
        let value = &self.stack[frame_pointer + register.into()];
        if value.is_upvalue() {
            Cow::Owned(unsafe { value.as_upvalue().read().unwrap().0.clone() })
        } else {
            Cow::Borrowed(value)
        }
    }

    #[inline]
    pub fn set(&mut self, register: impl Into<usize>, value: LuaValue) {
        let frame_pointer = self.call_frame().frame_pointer;
        self.stack[frame_pointer + register.into()] = value;
    }

    #[allow(clippy::inline_always)]
    #[inline(always)]
    pub(crate) fn call_frame(&self) -> &CallFrame {
        debug_assert!(self.call_stack_index > 0, "no call frame available");

        &self.call_stack[self.call_stack_index - 1]
    }

    #[allow(clippy::inline_always)]
    #[inline(always)]
    pub(crate) fn call_frame_mut(&mut self) -> &mut CallFrame {
        debug_assert!(self.call_stack_index > 0, "no call frame available");

        &mut self.call_stack[self.call_stack_index - 1]
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn push_call_frame(
        &mut self,
        kind: CallFrameKind,
        border_frame: bool,
        frame_pointer: usize,
        max_registers: u8,
        return_addr: usize,
        return_value_register: u8,
        upvalues: Option<HashMap<usize, Arc<RwLock<UpValue>>>>,
        num_args: usize,
        takes_varargs: bool,
        allow_multi_return_values: bool,
    ) -> crate::Result {
        if self.call_stack_index >= MAX_STACK_SIZE {
            return Err(lua_error!("stack overflow"));
        }

        // Reuse the existing object to keep the `upvalues` vec
        self.call_stack[self.call_stack_index].kind = kind;
        self.call_stack[self.call_stack_index].border_frame = border_frame;
        self.call_stack[self.call_stack_index].frame_pointer = frame_pointer;
        self.call_stack[self.call_stack_index].return_addr = return_addr;
        self.call_stack[self.call_stack_index].return_value_register = return_value_register;
        self.call_stack[self.call_stack_index].allow_multi_return_values =
            allow_multi_return_values;
        self.call_stack[self.call_stack_index].num_args = num_args;
        self.call_stack[self.call_stack_index].takes_varargs = takes_varargs;
        match self.call_stack[self.call_stack_index].upvalues.as_mut() {
            Some(u) => u.clear(),
            None => self.call_stack[self.call_stack_index].upvalues = Some(HashMap::new()),
        }
        self.call_stack[self.call_stack_index].max_registers = match kind {
            CallFrameKind::Lua { chunk, .. } => {
                self.with_chunk(chunk, |c| c.max_registers).unwrap_or(0)
            }
            CallFrameKind::Native { .. } => num_args as u8,
        };

        if let Some(upvalues) = upvalues {
            let frame_upvalues = self.call_stack[self.call_stack_index]
                .upvalues
                .as_mut()
                .unwrap();
            for (i, upvalue) in upvalues {
                frame_upvalues.insert(i, upvalue);
            }
        }

        self.call_stack_index += 1;

        // Reserve stack space for the registers
        self.ensure_stack_space(max_registers);

        Ok(())
    }

    pub(crate) fn ensure_stack_space(&mut self, max_registers: impl Into<usize>) {
        let frame_pointer = self.call_frame().frame_pointer;
        let max_registers = max_registers.into();
        let required_stack_size = frame_pointer + max_registers;
        if required_stack_size >= self.stack.len() {
            self.stack
                .resize(frame_pointer + max_registers, LuaValue::NIL);
        }
    }

    /// Create extra space on the stack for the given values if needed, and place the values there.
    /// Returns the register (relative to the current frame pointer) where the first value is
    /// located.
    pub(crate) fn extend_stack(
        &mut self,
        callable: impl Into<LuaValue>,
        values: impl IntoIterator<Item = LuaValue> + Debug + WithCount,
    ) -> u8 {
        let call_frame = self.call_frame();
        let frame_pointer = call_frame.frame_pointer;
        let max_registers = call_frame.max_registers;
        let register = max_registers;

        self.ensure_stack_space(register as usize + values.count() + 1);

        // Copy over the values into the right area of the stack.
        self.stack[frame_pointer + register as usize] = callable.into();
        for (i, value) in values.into_iter().enumerate() {
            self.stack[frame_pointer + register as usize + i + 1] = value;
        }

        register
    }

    fn pop_call_frame(&mut self) -> PoppedCallFrame {
        self.call_stack_index -= 1;

        let frame = &mut self.call_stack[self.call_stack_index];
        let popped_frame = PoppedCallFrame {
            kind: frame.kind,
            border_frame: frame.border_frame,
            return_addr: frame.return_addr,
            return_value_register: frame.return_value_register,
            allow_multi_return_values: frame.allow_multi_return_values,
        };

        debug_assert!(
            self.call_stack_index > 0 || popped_frame.border_frame,
            "popped the last call frame without a border frame"
        );

        frame.upvalues.as_mut().unwrap().clear();
        frame.varargs.clear();
        frame.to_close.clear();

        popped_frame
    }

    pub(crate) fn capture_upvalue(&mut self, register: u8) -> Arc<RwLock<UpValue>> {
        let frame_pointer = self.call_frame().frame_pointer;
        let value = &self.stack[frame_pointer + register as usize];
        if value.is_upvalue() {
            return unsafe { value.into_upvalue() };
        }

        let mut value = LuaValue::NIL;
        std::mem::swap(
            &mut value,
            &mut self.stack[frame_pointer + register as usize],
        );
        let upvalue = Arc::new(RwLock::new(UpValue(value)));
        self.set(register, LuaValue::from_upvalue(Arc::clone(&upvalue)));

        upvalue
    }

    // Helper method to get const values
    #[inline]
    fn get_const(&self, chunk_index: ChunkIndex, index: ConstIndex) -> LuaConst {
        let global = self.global.read().unwrap();
        global.chunks[chunk_index as usize].consts[index as usize].clone()
    }

    pub fn run(&mut self) -> crate::Result<Vec<LuaValue>> {
        assert!(
            self.call_stack_index > 0,
            "no call frame to run the chunk in"
        );

        match self.run_inner() {
            Ok(ExecutionResult::Completed(values)) => Ok(values),
            Ok(ExecutionResult::Yielded(_)) => {
                unreachable!("unexpected yield from main thread")
            }
            Err(mut err) => {
                self.print_backtrace(&mut err);
                Err(err)
            }
        }
    }

    pub(crate) fn resume(&mut self, args: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
        match self.status {
            ThreadStatus::Created => {
                // Put the passed args on the stack and load varargs
                let num_args = args.len();
                self.stack
                    .reserve(num_args.saturating_sub(self.stack.len()));
                self.stack.splice(1..=num_args, args);

                // Note: Native functions are not supported as coroutines - coroutine.create() rejects them
                // If this is ever extended to support native functions, we would need to store the function
                // pointer in the call frame and handle it specially here
                let (chunk_num_args, has_varargs) = match self.call_frame().kind {
                    CallFrameKind::Lua { chunk, .. } => self
                        .with_chunk(chunk, |c| (c.num_params, c.has_varargs))
                        .unwrap(),
                    CallFrameKind::Native { .. } => {
                        unreachable!("native functions cannot be used as coroutines")
                    }
                };
                self.align_closure_args(chunk_num_args, num_args, has_varargs);
            }
            ThreadStatus::Suspended {
                resume_args_register,
            } => {
                if let Some(resume_args_register) = resume_args_register {
                    let num_args = args.len();
                    self.stack
                        .reserve(num_args.saturating_sub(self.stack.len()));

                    for (i, arg) in args.into_iter().enumerate() {
                        self.set(resume_args_register as usize + i, arg);
                    }
                }
            }
            ThreadStatus::Running => {
                return Err(lua_error!("cannot resume a running coroutine"));
            }
            ThreadStatus::Dead => {
                return Err(lua_error!("cannot resume a dead coroutine"));
            }
        }

        assert!(self.call_stack_index > 0, "no call frame to resume");

        self.status = ThreadStatus::Running;
        match self.run_inner() {
            Ok(ExecutionResult::Completed(values)) => {
                self.status = ThreadStatus::Dead;
                Ok(values)
            }
            Ok(ExecutionResult::Yielded(values)) => {
                debug_assert!(
                    matches!(self.status, ThreadStatus::Suspended { .. }),
                    "run_inner returned yielded, but thread status doesn't match"
                );
                Ok(values)
            }
            Err(err) => {
                self.status = ThreadStatus::Dead;
                Err(err)
            }
        }
    }

    pub(crate) fn run_closure(
        &mut self,
        value: &LuaClosure,
        args_r: u8,
        num_args: usize,
    ) -> crate::Result<ExecutionResult> {
        let original_call_stack_index = self.call_stack_index;
        let frame_pointer = self.call_frame().frame_pointer + args_r as usize;

        let (max_registers, num_params, has_varargs) = self
            .with_chunk(value.chunk, |chunk| {
                (
                    chunk.max_registers,
                    u16::from(chunk.num_params),
                    chunk.has_varargs,
                )
            })
            .unwrap();

        self.push_call_frame(
            CallFrameKind::for_chunk(value.chunk),
            true,
            frame_pointer,
            max_registers,
            // NOTE: `return_addr` and `return_value_register` don't matter for a border frame
            0,
            0,
            Some(value.upvalues.clone()),
            num_params.into(),
            has_varargs,
            false,
        )?;

        // TODO: Move this responsibility out of here so we can get it set up right before even
        // putting the values on the stack?
        self.align_closure_args(num_params, num_args, has_varargs);

        let result = match self.run_inner() {
            Ok(result) => {
                // NOTE: in this case we don't need to pop the call frame, as it was
                // already popped by the `return` instruction (which is always present).
                Ok(result)
            }
            Err(err) => {
                println!("err {err}");
                Err(self.pop_call_frames_into_error(original_call_stack_index, *err))
            }
        };

        let chunk_name = self
            .with_chunk(value.chunk, |c| c.name.to_string())
            .unwrap();
        assert_eq!(
            original_call_stack_index, self.call_stack_index,
            "unbalanced call stack after closure call '{chunk_name}'",
        );

        result
    }

    fn set_up_closure_call_frame(
        &mut self,
        frame_pointer: usize,
        return_addr: usize,
        return_value_register: u8,
        closure: &LuaClosure,
        num_passed_params: impl Into<usize>,
    ) -> crate::Result {
        let (max_registers, num_params, has_varargs) = self
            .with_chunk(closure.chunk, |chunk| {
                (
                    chunk.max_registers,
                    u16::from(chunk.num_params),
                    chunk.has_varargs,
                )
            })
            .unwrap();

        self.push_call_frame(
            CallFrameKind::for_chunk(closure.chunk),
            false,
            frame_pointer,
            max_registers,
            return_addr,
            return_value_register,
            Some(closure.upvalues.clone()),
            num_params.into(),
            has_varargs,
            false,
        )?;

        self.align_closure_args(num_params, num_passed_params, has_varargs);

        Ok(())
    }

    /// Handle arguments when entering a closure, i.e. pad extra `nil` values if
    /// the actual number of passed arguments is less than `num_params`, store
    /// any varargs in the frame's varargs field, and trim any extraneous values
    /// from the stack if `has_varargs = false`.
    /// Call this after pushing the call frame.
    fn align_closure_args(
        &mut self,
        num_params: impl Into<usize>,
        num_passed_params: impl Into<usize>,
        has_varargs: bool,
    ) {
        let num_params = num_params.into();
        let num_passed_params = num_passed_params.into();
        if num_passed_params > num_params && has_varargs {
            // TODO: Can just copy these once we have static stack access.
            let num_varargs = num_passed_params - num_params;
            for i in 0..num_varargs {
                let arg = self.get(num_params + i + 1);
                self.call_frame_mut().varargs.push(arg);
            }
        }

        // Clear out the stack beyond the number of fixed params by filling it
        // with `nil` values. This gets rid of any stale values.
        self.clear_registers_from(num_params.min(num_passed_params) + 1);
    }

    fn clear_registers_from(&mut self, from_register: impl Into<usize>) {
        let frame_pointer = self.call_frame().frame_pointer;
        self.stack[frame_pointer + from_register.into()..].fill(LuaValue::NIL);
    }

    fn clear_registers_between(&mut self, from_register: u8, to_register: u8) {
        let frame_pointer = self.call_frame().frame_pointer;
        self.stack[frame_pointer + from_register as usize..=frame_pointer + to_register as usize]
            .fill(LuaValue::NIL);
    }

    /// Run a native function in this thread ([`LuaState`]). This will handle automatically pushing
    /// and popping the call frame for the native function, and will ensure that the stack is in a
    /// valid state before and after the call.
    ///
    /// # Expectations
    ///
    /// The stack is assumed to be in a correct state before the call. This means that:
    /// - `args_r` is valid, and is an offset from the parent frame pointer.
    /// - The actual `num_args` arguments sit at `stack[args_r + 1], stack[args_r + 2], ...,
    ///   stack[args_r + num_args]`.
    /// - The invoked function _may_ be at `stack[args_r]`, but this is not a requirement.
    /// - The _return values_ of the native function will be placed starting at `stack[args_r]`.
    ///
    /// Like the native function itself, this returns the number of return values.
    pub(crate) fn run_native_function(
        &mut self,
        name: &'static str,
        function: NativeLuaFunction,
        args_r: u8,
        num_args: usize,
    ) -> crate::Result<usize> {
        let original_call_stack_index = self.call_stack_index;
        let parent_frame_pointer = if self.call_stack_index > 0 {
            self.call_frame().frame_pointer
        } else {
            0
        };

        debug_assert!(
            (parent_frame_pointer + args_r as usize) <= self.stack.len(),
            "not enough stack space for native function arguments!\nfn = {name}\nargs_r = {args_r}\nstack = {:?}",
            &self.stack[parent_frame_pointer..]
        );
        debug_assert!(
            (parent_frame_pointer + args_r as usize + num_args) <= self.stack.len(),
            "native function args run beyond the stack!\nfn = {name}\nargs_r = {args_r}, num_args = {num_args}\nstack = {:?}",
            &self.stack[parent_frame_pointer + args_r as usize..]
        );

        // NOTE: Some call frame values are irrelevant for native functions.
        self.push_call_frame(
            CallFrameKind::Native { name },
            true, // Not applicable
            parent_frame_pointer + args_r as usize,
            0, // Not applicable
            0, // Not applicable
            0, // Not applicable
            None,
            num_args,
            false, // Not applicable
            false, // Not applicable
        )?;

        let result = function(self)
            .inspect(|_| {
                self.pop_call_frame();
            })
            .map_err(|err| self.pop_call_frames_into_error(original_call_stack_index, *err));

        assert_eq!(
            original_call_stack_index, self.call_stack_index,
            "unbalanced call stack after native function call"
        );

        result
    }

    fn run_inner(&mut self) -> crate::Result<ExecutionResult> {
        'main: loop {
            let CallFrameKind::Lua {
                chunk: chunk_index,
                ip,
            } = self.call_frame().kind
            else {
                unreachable!("native function call frame in run_inner");
            };

            // Get instruction and handle tracing
            let instruction = {
                let global = self.global.read().unwrap();
                let chunk_ref = &global.chunks[chunk_index as usize];
                let instr = chunk_ref.instructions[ip];

                if let Some(tracer) = &mut self.tracer {
                    tracer.trace_instruction(
                        &self.stack[self.call_stack[self.call_stack_index - 1].frame_pointer..],
                        &self.call_stack[self.call_stack_index - 1].varargs,
                        self.call_stack[self.call_stack_index - 1].upvalues.as_ref(),
                        chunk_ref,
                        chunk_index,
                        ip,
                        self.multres,
                    );
                }

                instr
            };

            macro_rules! binary_op_num {
                (method = $name:ident, $metamethod:ident, $op_name:expr) => {
                    binary_op_num!(_handle $metamethod, $op_name, method, $name)
                };
                (op = $tt:tt, $metamethod:ident, $op_name:expr) => {
                    binary_op_num!(_handle $metamethod, $op_name, op, $tt)
                };
                (op_bool_result = $tt:tt, $metamethod:ident, $op_name:expr) => {
                    binary_op_num!(_handle $metamethod, $op_name, op_bool_result, $tt)
                };
                (fallible_op = $tt:tt, $metamethod:ident, $op_name:expr) => {
                    binary_op_num!(_handle $metamethod, $op_name, fallible_op, $tt)
                };
                (_handle $metamethod:ident, $op_name:expr, $($param:tt),*) => {{
                    let dest_r = instruction.a();
                    let lhs_r = instruction.b();
                    let rhs_r = instruction.c();
                    let a = self.get_ref(lhs_r);
                    let b = self.get_ref(rhs_r);
                    let result = if a.is_number() && b.is_number() {
                        // Safety: We've verified that both a and b are numbers.
                        let a = unsafe { a.as_number() };
                        let b = unsafe { b.as_number() };

                        binary_op_num!(_internal a, b, $($param),*).into()
                    } else {
                        let a = a.into_owned();
                        let b = b.into_owned();
                        match metatables::handle(
                            self,
                            &metatables::$metamethod,
                            vec![a, b],
                        ) {
                            Some(result) => binary_op_num!(_internal_metamethod_result result $($param),*),
                            None => {
                                return Err(self.op_err($op_name, self.get_ref(lhs_r).as_ref(), Some(self.get_ref(rhs_r).as_ref())));

                            }
                        }
                    };
                    self.set(dest_r, result);
                }};
                (_internal $a:ident, $b:ident, method, $name:ident) => {{
                    $a.$name(&$b)
                }};
                (_internal $a:ident, $b:ident, op_bool_result, $tt:tt) => {{
                    ($a $tt $b)
                }};
                (_internal $a:ident, $b:ident, op, $tt:tt) => {{
                    ($a $tt $b)
                }};
                (_internal $a:ident, $b:ident, fallible_op, $tt:tt) => {{
                    ($a $tt $b)?
                }};
                (_internal_metamethod_result $value:ident op_bool_result, $($_:tt),*) => {{
                    match $value? {
                        ExecutionResult::Completed(mut values) => {
                            if values.is_empty() {
                                false
                            } else {
                                values.swap_remove(0).cast_to_bool()
                            }
                        },
                        result @ ExecutionResult::Yielded(_) => {
                            return Ok(result);
                        }
                    }.into()
                }};
                (_internal_metamethod_result $value:ident $($_:tt),*) => {{
                    match $value? {
                        ExecutionResult::Completed(mut values) => {
                            if values.is_empty() {
                                LuaValue::NIL
                            } else {
                                values.swap_remove(0)
                            }
                        },
                        result @ ExecutionResult::Yielded(_) => {
                            return Ok(result);
                        }
                    }.cast_to_bool().into()
                }};
                (_internal_metamethod_result $value:ident $($_:tt),*) => {
                    match $value? {
                        ExecutionResult::Completed(mut values) => {
                            if values.is_empty() {
                                LuaValue::NIL
                            } else {
                                values.swap_remove(0)
                            }
                        },
                        result @ ExecutionResult::Yielded(_) => {
                            return Ok(result);
                        }
                    }
                };
            }
            macro_rules! unary_op_num {
                (method = $name:ident, $metamethod:ident, $op_name:expr) => {
                    unary_op_num!(_handle $metamethod, $op_name, method, $name)
                };
                (op = $tt:tt, $metamethod:ident, $op_name:expr) => {
                    unary_op_num!(_handle $metamethod, $op_name, op, $tt)
                };
                (fallible_op = $tt:tt, $metamethod:ident, $op_name:expr) => {
                    unary_op_num!(_handle $metamethod, $op_name, fallible_op, $tt)
                };
                (_handle $metamethod:ident, $op_name:expr, $($param:tt),*) => {{
                    let dest_r = instruction.a();
                    let value_r = instruction.b();
                    let value = self.get_ref(value_r);
                    let result: LuaValue = if value.is_number() {
                        // Safety: We've verified that num is a number.
                        let num = unsafe { value.as_number() };
                        unary_op_num!(_internal dest_r num, $($param),*).into()
                    } else {
                        let value = value.into_owned();
                        match metatables::handle(
                            self,
                            &metatables::$metamethod,
                            vec![value],
                        ) {
                            Some(result) => {
                                handle_execution_result!({ result? }, dest_r)
                            },
                            None => {
                                return Err(self.op_err($op_name, self.get_ref(value_r).as_ref(), None));
                            }
                        }
                    };
                    self.set(dest_r, result);
                }};
                (_internal $dest_r:ident $value:ident, method, $name:ident) => {{
                    $value.$name()
                }};
                (_internal $dest_r:ident $value:ident, op, $tt:tt) => {{
                    $tt $value
                }};
                (_internal $dest_r:ident $value:ident, fallible_op, $tt:tt) => {{
                    ($tt $value)?
                }};
                (_internal handle_result $dest_r:ident $value:tt) => {
                    match $value {
                        ExecutionResult::Completed(mut values) => {
                            if values.is_empty() {
                                LuaValue::NIL
                            } else {
                                values.swap_remove(0)
                            }
                        },
                        result @ ExecutionResult::Yielded(_) => {
                            self.status = ThreadStatus::Suspended {
                                resume_args_register: Some($dest_r),
                            };
                            self.call_frame_mut().inc_ip();
                            return Ok(result);
                        }
                    }
                };
            }
            macro_rules! handle_execution_result {
                ($result:expr, $dest_r:expr) => {
                    match $result {
                        ExecutionResult::Completed(mut values) => {
                            if values.is_empty() {
                                LuaValue::NIL
                            } else {
                                values.swap_remove(0)
                            }
                        }
                        result @ ExecutionResult::Yielded(_) => {
                            self.status = ThreadStatus::Suspended {
                                resume_args_register: Some($dest_r),
                            };
                            self.call_frame_mut().inc_ip();
                            return Ok(result);
                        }
                    }
                };
            }

            match instruction.opcode() {
                // Stack manipulation
                OpCode::LoadConst => {
                    let register = instruction.a();
                    let const_index = instruction.b();
                    let constant = self.get_const(chunk_index, const_index);
                    self.set(register, constant.into());
                }
                OpCode::LoadClosure => {
                    let register = instruction.a();
                    let chunk_index: ChunkIndex = instruction.bc();
                    let closure = LuaClosure::new(chunk_index);

                    // NOTE: We MUST set the closure in the register before capturing upvalues,
                    // as capturing an upvalue may refer to the closure itself (in case of
                    // recursive functions).
                    self.set(register, closure.into());

                    assert_closure!(write, self.get(register), closure, {
                        let upvalues_to_capture = self
                            .with_chunk(chunk_index, |c| c.upvalues.clone())
                            .unwrap();
                        for (i, upvalue) in upvalues_to_capture.iter().enumerate() {
                            let index = upvalue.index;
                            if upvalue.is_local {
                                closure.upvalues.insert(i, self.capture_upvalue(index));
                            } else {
                                closure.upvalues.insert(
                                    i,
                                    self.call_frame()
                                        .upvalues
                                        .as_ref()
                                        .unwrap()
                                        .get(&(index as usize))
                                        .cloned()
                                        .unwrap_or_else(|| panic!("upvalue {index} not found")),
                                );
                            }
                        }
                    });
                }
                OpCode::LoadNil => {
                    let from_register = instruction.a();
                    let to_register = instruction.b();
                    self.clear_registers_between(from_register, to_register);
                }
                OpCode::LoadShort => {
                    let register = instruction.a();
                    let int_value = instruction.bc();
                    let int_value = u16::cast_signed(int_value);
                    self.set(register, LuaValue::from_integer(int_value.into()));
                }
                OpCode::LoadShortFloat => {
                    let register = instruction.a();
                    let int_value = instruction.bc();
                    let int_value = u16::cast_signed(int_value);
                    self.set(register, LuaValue::from_float(int_value.into()));
                }

                // Binary operations
                // Arithmetic
                OpCode::Add => {
                    binary_op_num!(op = +, ADD_KEY, "add");
                }
                OpCode::Sub => {
                    binary_op_num!(op = -, SUB_KEY, "subtract");
                }
                OpCode::Mul => {
                    binary_op_num!(op = *, MUL_KEY, "multiply");
                }
                OpCode::Div => {
                    binary_op_num!(op = /, DIV_KEY, "divide");
                }
                OpCode::Mod => {
                    binary_op_num!(op = %, MOD_KEY, "modulo");
                }
                OpCode::Pow => {
                    binary_op_num!(method = pow, POW_KEY, "power");
                }
                OpCode::IDiv => {
                    binary_op_num!(method = idiv, IDIV_KEY, "integer divide");
                }
                OpCode::Band => {
                    binary_op_num!(fallible_op = &, BAND_KEY, "bitwise and");
                }
                OpCode::Bor => {
                    binary_op_num!(fallible_op = |, BOR_KEY, "bitwise or");
                }
                OpCode::Bxor => {
                    binary_op_num!(fallible_op = ^, BXOR_KEY, "bitwise xor");
                }
                OpCode::Shl => {
                    binary_op_num!(fallible_op = <<, SHL_KEY, "shift left");
                }
                OpCode::Shr => {
                    binary_op_num!(fallible_op = >>, SHR_KEY, "shift left");
                }

                // Comparison
                OpCode::Eq => {
                    let dest_r = instruction.a();
                    let lhs_r = instruction.b();
                    let rhs_r = instruction.c();
                    let a = self.get_ref(lhs_r);
                    let b = self.get_ref(rhs_r);

                    // __eq: the equal (==) operation. Behavior similar to the addition operation, except that Lua will
                    // try a metamethod only when the values being compared are either both tables or both full
                    // userdata and they are not primitively equal. The result of the call is always converted to a
                    // boolean.
                    let is_equal = a == b;
                    let value = if !is_equal && a.is_table() && b.is_table() {
                        if let Some(result) = metatables::handle(
                            self,
                            &metatables::EQ_KEY,
                            vec![a.into_owned(), b.into_owned()],
                        ) {
                            handle_execution_result!(result?, dest_r).cast_to_bool()
                        } else {
                            is_equal
                        }
                    } else {
                        is_equal
                    };
                    self.set(dest_r, value.into());
                }
                OpCode::Ne => {
                    let dest_r = instruction.a();
                    let lhs_r = instruction.b();
                    let rhs_r = instruction.c();
                    let a = self.get_ref(lhs_r);
                    let b = self.get_ref(rhs_r);

                    // The operator ~= is exactly the negation of equality (==).
                    let is_equal = a == b;
                    let value = if !is_equal && a.is_table() && b.is_table() {
                        if let Some(result) = metatables::handle(
                            self,
                            &metatables::EQ_KEY,
                            vec![a.into_owned(), b.into_owned()],
                        ) {
                            handle_execution_result!(result?, dest_r).cast_to_bool()
                        } else {
                            is_equal
                        }
                    } else {
                        is_equal
                    };
                    self.set(dest_r, (!value).into());
                }
                OpCode::Lt => {
                    binary_op_num!(op_bool_result = <, LT_KEY, "compare");
                }
                OpCode::Le => {
                    binary_op_num!(op_bool_result = <=, LE_KEY, "compare");
                }

                // Concatenation
                OpCode::Concat => {
                    let dest_r = instruction.a();
                    let lhs_r = instruction.b();
                    let rhs_r = instruction.c();
                    let a = self.get(lhs_r);
                    let b = self.get(rhs_r);
                    match a.concat(b) {
                        Ok(value) => self.set(dest_r, value),
                        Err(err) => {
                            let (e, a, b) = *err;
                            match metatables::handle(
                                self,
                                &metatables::CONCAT_KEY,
                                vec![a.clone(), b.clone()],
                            ) {
                                Some(value) => {
                                    let value = handle_execution_result!(value?, dest_r);
                                    self.set(dest_r, value);
                                }
                                None => {
                                    return Err(Box::new(e));
                                }
                            }
                        }
                    }
                }

                // Unary operations
                OpCode::Neg => {
                    unary_op_num!(op = -, UNM_KEY, "negate");
                }
                OpCode::Not => {
                    // NOTE: There is no metamethod for `not` in Lua
                    let dest_r = instruction.a();
                    let val_r = instruction.b();
                    let value = self.get_ref(val_r);
                    self.set(dest_r, (!&*value).into());
                }
                OpCode::Len => {
                    // NOTE: The `__len` metamethod is only called when the value is not a string.
                    let dest_r = instruction.a();
                    let val_r = instruction.b();
                    let value = self.get_ref(val_r);

                    let result: LuaValue = if value.is_string() {
                        (unsafe { value.as_string() }.len() as i64).into()
                    } else {
                        let value = value.into_owned();
                        if let Some(result) =
                            metatables::handle(self, &metatables::LEN_KEY, vec![value])
                        {
                            handle_execution_result!(result?, dest_r)
                        } else {
                            // If the value is a table (but it didn't have a __len
                            // metamethod), return the length of the array part
                            let value = self.get_ref(val_r);
                            if value.is_object()
                                && let obj = unsafe { value.as_object() }
                                && let obj = &*obj.read().unwrap()
                                && let LuaObject::Table(table) = obj
                            {
                                (table.len() as i64).into()
                            } else {
                                return Err(self.op_err("length", &value, None));
                            }
                        }
                    };
                    self.set(dest_r, result);
                }
                OpCode::BNot => {
                    unary_op_num!(fallible_op = !, BNOT_KEY, "bitwise not");
                }

                // Variables
                OpCode::Mov => {
                    let dest = instruction.a();
                    let src = instruction.b();

                    let frame_pointer = self.call_frame().frame_pointer;
                    let value = self.get(src);
                    if self.stack[frame_pointer + dest as usize].is_upvalue() {
                        let mut upval =
                            (unsafe { self.stack[frame_pointer + dest as usize].as_upvalue() })
                                .write()
                                .unwrap();
                        *upval = value.into();
                    } else {
                        self.set(dest, value);
                    }
                }
                OpCode::SetUpval => {
                    let upval_index = instruction.a();
                    let register = instruction.b();
                    let value = self.get(register);
                    let mut upval = self
                        .call_frame()
                        .upvalues
                        .as_ref()
                        .unwrap()
                        .get(&(upval_index as usize))
                        .as_mut()
                        .unwrap()
                        .write()
                        .unwrap();
                    *upval = value.into();
                }
                OpCode::GetUpval => {
                    let register = instruction.a();
                    let upval_index = instruction.b();
                    let value = self.get_upvalue(upval_index);
                    self.set(register, value);
                }
                OpCode::LoadVararg => {
                    let register = instruction.a();
                    let is_single_vararg = instruction.b() == 1;
                    if is_single_vararg {
                        let value = self
                            .call_frame()
                            .varargs
                            .first()
                            .cloned()
                            .unwrap_or(LuaValue::NIL);
                        self.set(register, value);
                    } else {
                        // Ensure we'll have enough space on the stack.
                        let varargs = self.call_frame().varargs.clone();
                        self.multres = varargs.len();
                        self.ensure_stack_space(register as usize + varargs.len());

                        for (i, value) in varargs.into_iter().enumerate() {
                            self.set(register as usize + i, value);
                        }

                        // Clear out extraneous registers
                        self.clear_registers_from(register as usize + self.multres);
                    }
                }
                OpCode::ToClose => {
                    let register = instruction.a();
                    self.call_frame_mut().to_close.push(register);
                }
                OpCode::Close => {
                    let from_register = instruction.a();
                    self.close(from_register, None)?;
                }

                // Table
                OpCode::NewTable => {
                    let register = instruction.a();
                    let table = LuaObject::Table(LuaTable::default());
                    self.set(register, table.into());
                }
                instr @ (OpCode::SetTable
                | OpCode::SetTableConstKey
                | OpCode::SetTableUpval
                | OpCode::SetTableUpvalConstKey) => {
                    let (table, key, value_r) = match instr {
                        OpCode::SetTable => {
                            let table_r = instruction.a();
                            let key_r = instruction.b();
                            let value_r = instruction.c();

                            let table = self.get(table_r);
                            let key = self.get(key_r);

                            (table, key, value_r)
                        }
                        OpCode::SetTableConstKey => {
                            let table_r = instruction.a();
                            let const_index = instruction.b();
                            let key = self.get_const(chunk_index, const_index).into();
                            let value_r = instruction.c();

                            let table = self.get(table_r);

                            (table, key, value_r)
                        }
                        OpCode::SetTableUpval => {
                            let upval_index = instruction.a();
                            let key_r = instruction.b();
                            let value_r = instruction.c();

                            let table = self.get_upvalue(upval_index);
                            let key = self.get(key_r);

                            (table, key, value_r)
                        }
                        OpCode::SetTableUpvalConstKey => {
                            let upval_index = instruction.a();
                            let const_index = instruction.b();
                            let key = self.get_const(chunk_index, const_index).into();
                            let value_r = instruction.c();

                            let table = self.get_upvalue(upval_index);

                            (table, key, value_r)
                        }
                        _ => unreachable!(),
                    };

                    let value = self.get(value_r);

                    let mut target = table.clone();
                    let mut handled = false;
                    loop {
                        let mut is_table_with_missing_key = false;
                        if target.is_object()
                            && let o = unsafe { target.as_object() }
                            && let LuaObject::Table(t) = &mut *o.write().unwrap()
                        {
                            if t.get(&key).is_some() {
                                t.insert(key, value);
                                handled = true;
                                break;
                            }

                            is_table_with_missing_key = true;
                        }

                        if let Some(__newindex) = target.get_metavalue(&NEWINDEX_KEY) {
                            let __newindex_callable: Result<Callable, _> = (&__newindex).try_into();
                            if let Ok(__newindex) = __newindex_callable {
                                match __newindex.method {
                                    Method::Closure(closure) => {
                                        // Ensure that we have an `args_r` value, such that:
                                        // - args_r points to the target table
                                        // - args_r + 1 points to the key
                                        // - args_r + 2 points to the value
                                        let args_r = match instr {
                                            OpCode::SetTable
                                            | OpCode::SetTableConstKey
                                            | OpCode::SetTableUpval
                                            | OpCode::SetTableUpvalConstKey => self.extend_stack(
                                                closure.clone(),
                                                [target, key, value],
                                            ),
                                            _ => unreachable!(),
                                        };
                                        self.set_up_closure_call_frame(
                                            self.call_frame().frame_pointer + args_r as usize,
                                            ip + 1,
                                            args_r,
                                            &closure,
                                            3usize,
                                        )?;
                                        continue 'main;
                                    }
                                    nf @ Method::NativeFunction(name, f) => {
                                        let args_r =
                                            self.extend_stack(nf, [table.clone(), key, value]);
                                        self.run_native_function(name, f, args_r, 3)?;

                                        // Check if the native function yielded
                                        if let Some(yield_values) = self.pending_yield.take() {
                                            // In this case, we don't care about the return values,
                                            // so set status to suspended with an empty
                                            // `resume_args_register`.
                                            self.status = ThreadStatus::Suspended {
                                                resume_args_register: None,
                                            };
                                            return Ok(ExecutionResult::Yielded(yield_values));
                                        }
                                    }
                                }
                                handled = true;
                                break;
                            }

                            target = __newindex;
                        } else if is_table_with_missing_key {
                            assert_table!(write, target, table, {
                                table.insert(key, value);
                            });
                            handled = true;
                            break;
                        } else {
                            break;
                        }
                    }

                    if !handled {
                        match instr {
                            OpCode::SetTable | OpCode::SetTableConstKey => {
                                let table_r = instruction.a();
                                if let Some(name_info) =
                                    self.describe_register(chunk_index, ip, table_r)
                                {
                                    return Err(lua_error!(
                                        "attempt to index {name_info} (a {} value)",
                                        table.type_name()
                                    ));
                                }
                            }
                            OpCode::SetTableUpval | OpCode::SetTableUpvalConstKey => {
                                let upval_index = instruction.a();
                                if let Some(upval_name) = self
                                    .with_chunk(chunk_index, |chunk| {
                                        chunk
                                            .upvalues
                                            .get(upval_index as usize)
                                            .and_then(|u| u.name.as_ref())
                                            .cloned()
                                    })
                                    .flatten()
                                {
                                    return Err(lua_error!(
                                        "attempt to index upvalue '{upval_name}' (a {} value)",
                                        table.type_name()
                                    ));
                                }
                            }
                            _ => unreachable!(),
                        }

                        return Err(lua_error!("attempt to index a non-table"));
                    }
                }
                instr @ (OpCode::GetTable
                | OpCode::GetTableConstKey
                | OpCode::GetTableUpval
                | OpCode::GetTableUpvalConstKey) => {
                    let dest_r = instruction.a();

                    let (table, key) = match instr {
                        OpCode::GetTable => {
                            let table_r = instruction.b();
                            let key_r = instruction.c();
                            (self.get(table_r), self.get(key_r))
                        }
                        OpCode::GetTableConstKey => {
                            let table_r = instruction.b();
                            let const_index = instruction.c();
                            let key = self.get_const(chunk_index, const_index).into();
                            (self.get(table_r), key)
                        }
                        OpCode::GetTableUpval => {
                            let upval_index = instruction.b();
                            let key_r = instruction.c();
                            let table = self.get_upvalue(upval_index);
                            let key = self.get(key_r);
                            (table, key)
                        }
                        OpCode::GetTableUpvalConstKey => {
                            let upval_index = instruction.b();
                            let const_index = instruction.c();
                            let table = self.get_upvalue(upval_index);
                            let key = self.get_const(chunk_index, const_index).into();

                            (table, key)
                        }
                        _ => unreachable!(),
                    };

                    let mut handled = false;
                    let mut valid_object = false;

                    let mut target = table.clone();
                    loop {
                        if target.is_object()
                            && let o = unsafe { target.as_object() }
                            && let LuaObject::Table(t) = &*o.read().unwrap()
                        {
                            valid_object = true;

                            if let Some(value) = t.get(&key).cloned() {
                                self.set(dest_r, value);
                                handled = true;
                                break;
                            }
                        }

                        if let Some(__index) = target.get_metavalue(&INDEX_KEY) {
                            let __index_callable: Result<Callable, _> = (&__index).try_into();
                            if let Ok(__index) = __index_callable {
                                let args_r = match instr {
                                    OpCode::GetTable => instruction.b(),
                                    OpCode::GetTableConstKey => {
                                        let table_r = instruction.b();
                                        self.set(table_r + 1, key);
                                        table_r
                                    }
                                    OpCode::GetTableUpval | OpCode::GetTableUpvalConstKey => {
                                        // In these cases, the table and/or key are not yet
                                        // loaded into registers, which it needs to be for
                                        // the __index call.
                                        self.extend_stack(
                                            __index.method.clone(),
                                            [table.clone(), key],
                                        )
                                    }
                                    _ => unreachable!(),
                                };
                                match __index.method {
                                    Method::Closure(closure) => {
                                        let parent_frame_pointer = self.call_frame().frame_pointer;
                                        self.set_up_closure_call_frame(
                                            parent_frame_pointer + args_r as usize,
                                            ip + 1,
                                            dest_r,
                                            &closure,
                                            2usize,
                                        )?;
                                        continue 'main;
                                    }
                                    Method::NativeFunction(name, f) => {
                                        self.run_native_function(name, f, args_r, 2)?;

                                        // Check if the native function yielded
                                        if let Some(yield_values) = self.pending_yield.take() {
                                            // Set status to Suspended, storing where to put resume args
                                            self.status = ThreadStatus::Suspended {
                                                resume_args_register: Some(dest_r),
                                            };
                                            return Ok(ExecutionResult::Yielded(yield_values));
                                        }

                                        self.set(dest_r, self.get(args_r));
                                    }
                                }
                                handled = true;
                                break;
                            }

                            target = __index;
                        } else {
                            break;
                        }
                    }

                    if !handled {
                        if valid_object {
                            self.set(dest_r, LuaValue::NIL);
                        } else {
                            match instr {
                                OpCode::GetTable | OpCode::GetTableConstKey => {
                                    let table_r = instruction.a();
                                    if let Some(name_info) =
                                        self.describe_register(chunk_index, ip, table_r)
                                    {
                                        return Err(lua_error!(
                                            "attempt to index {name_info} (a {} value)",
                                            table.type_name()
                                        ));
                                    }
                                }
                                OpCode::GetTableUpval | OpCode::GetTableUpvalConstKey => {
                                    let upval_index = instruction.a();
                                    if let Some(upval_name) = self
                                        .with_chunk(chunk_index, |chunk| {
                                            chunk
                                                .upvalues
                                                .get(upval_index as usize)
                                                .and_then(|u| u.name.as_ref())
                                                .cloned()
                                        })
                                        .flatten()
                                    {
                                        return Err(lua_error!(
                                            "attempt to index upvalue '{upval_name}' (a {} value)",
                                            table.type_name()
                                        ));
                                    }
                                }
                                _ => unreachable!(),
                            }

                            return Err(lua_error!("attempt to index a non-table"));
                        }
                    }
                }
                instr @ (OpCode::AppendToTable | OpCode::AppendToTableM) => {
                    // Append each value to the table at the stack right before the values
                    let table_r = instruction.a();
                    let mut num_values = instruction.b() as usize;
                    if matches!(instr, OpCode::AppendToTableM) {
                        num_values += self.multres;
                    }
                    let table = self.get(table_r);

                    if table.is_object()
                        && let o = unsafe { table.as_object() }
                        && let o = &mut *o.write().unwrap()
                        && let LuaObject::Table(t) = o
                    {
                        for i in 0..num_values {
                            let value = self.get(table_r as usize + 1 + i);
                            let index = (i + 1) as i64;
                            t.insert(index.into(), value);
                        }
                    } else {
                        return Err(lua_error!("attempt to index a non-table"));
                    }
                }

                // Function
                instr @ (OpCode::Call | OpCode::CallM) => {
                    let func_r = instruction.a();
                    let is_single_return = instruction.b() == 1;
                    let mut num_args = instruction.c() as usize;

                    let is_callm = matches!(instr, OpCode::CallM);
                    if is_callm {
                        num_args += self.multres;
                    }

                    // Get the function value
                    let function = self.get(func_r);

                    // Find the underlying closure or native function, handling metamethods
                    let Some(Callable {
                        is_metamethod,
                        method,
                    }) = (&function).try_into().ok()
                    else {
                        return Err(match self.describe_register(chunk_index, ip, func_r) {
                            Some(name_info) => lua_error!(
                                "attempt to call {name_info} (a {} value)",
                                function.type_name(),
                            ),
                            None => {
                                lua_error!("attempt to call a {} value", function.type_name(),)
                            }
                        });
                    };

                    if is_metamethod {
                        // This means that the function being called is actually a value with
                        // a `__call` metamethod. Unfortunately, because `func_r` refers to the source
                        // of the metamethod, we need to copy args over, as function calls expect
                        // the arguments to start at `func_r + 1`.
                        self.ensure_stack_space(func_r as usize + num_args + 1);
                        for i in (0..num_args).rev() {
                            let value = self.get(func_r as usize + i + 1);
                            self.set(func_r as usize + i + 2, value);
                        }
                        self.set(func_r + 1, function);
                        num_args += 1;
                    }

                    match method {
                        Method::Closure(closure) => {
                            let parent_frame_pointer = self.call_frame().frame_pointer;
                            let (max_regs, num_params, has_varargs) = self
                                .with_chunk(closure.chunk, |c| {
                                    (c.max_registers, c.num_params, c.has_varargs)
                                })
                                .unwrap();
                            self.push_call_frame(
                                CallFrameKind::for_chunk(closure.chunk),
                                false,
                                parent_frame_pointer + func_r as usize,
                                max_regs,
                                ip + 1,
                                func_r,
                                Some(closure.upvalues.clone()),
                                num_params.into(),
                                has_varargs,
                                !is_single_return,
                            )?;
                            self.align_closure_args(num_params, num_args, has_varargs);

                            continue;
                        }
                        Method::NativeFunction(name, f) => {
                            let result = self.run_native_function(
                                name,
                                f,
                                func_r,
                                num_args,
                            )?;

                            // Check if the native function yielded
                            if let Some(yield_values) = self.pending_yield.take() {
                                // Set status to Suspended, storing where to put resume args
                                self.status = ThreadStatus::Suspended {
                                    resume_args_register: Some(func_r),
                                };
                                // Increment IP past the Call instruction so we don't re-execute it on resume
                                self.call_frame_mut().inc_ip();
                                return Ok(ExecutionResult::Yielded(yield_values));
                            }

                            let value_slots_needed = match (is_single_return, result) {
                                (true, 0) => 1,
                                (true, n) => n.min(1),
                                (false, n) => n,
                            };
                            self.ensure_stack_space(func_r as usize + value_slots_needed as usize);

                            if result == 0 && is_single_return {
                                self.set(func_r, LuaValue::NIL);
                                self.multres = 0;
                            } else if is_single_return {
                                // TODO: Do we need to cap the values set by the native function?
                                self.multres = 1;
                            } else {
                                self.multres = result as usize;
                            }

                            self.clear_registers_from(func_r as usize + self.multres);
                        }
                    }
                }
                instr @ (OpCode::CallT | OpCode::CallTM) => {
                    let args_r = instruction.a();
                    // let is_single_return = instruction.b() == 1;
                    let mut num_passed_args = instruction.c() as usize;

                    let is_callm = matches!(instr, OpCode::CallTM);
                    if is_callm {
                        num_passed_args += self.multres;
                    }

                    let CallFrame {
                        num_args,
                        takes_varargs,
                        ..
                    } = self.call_frame();
                    let num_args = *num_args;
                    let takes_varargs = *takes_varargs;

                    // Copy over the args to the start of the frame pointer
                    for i in 0..num_passed_args {
                        let value = self.get(args_r as usize + i);
                        self.set(i, value);
                    }

                    self.align_closure_args(num_args, num_passed_args, takes_varargs);

                    // Then just jump back to the start of this closure.
                    self.call_frame_mut().set_ip(0);
                    continue;
                }
                instr @ (OpCode::Return | OpCode::Return0 | OpCode::Return1 | OpCode::ReturnM) => {
                    // Find the marker indicating the start of the return values
                    let (values_starting_register, num_return_values) = match instr {
                        OpCode::Return => (instruction.a(), instruction.b() as usize),
                        OpCode::ReturnM => {
                            (instruction.a(), instruction.b() as usize + self.multres)
                        }
                        OpCode::Return1 => (instruction.a(), 1),
                        // NOTE: Register doesn't matter for Return0, as the iterator below to
                        // collect return values won't run.
                        OpCode::Return0 => (0, 0),
                        _ => unreachable!(),
                    };

                    // Collect the return values
                    let mut return_values: Vec<_> = (0..num_return_values)
                        .map(|i| self.get(values_starting_register as usize + i))
                        .collect();

                    let frame = self.pop_call_frame();
                    if return_values.is_empty() && !frame.allow_multi_return_values {
                        return_values.push(LuaValue::NIL);
                    }

                    self.multres = if frame.allow_multi_return_values {
                        num_return_values
                    } else {
                        1
                    };

                    // Check if this is the root frame, or if we're leaving this chunk
                    if frame.border_frame {
                        return Ok(ExecutionResult::Completed(return_values));
                    }

                    self.call_frame_mut().set_ip(frame.return_addr);

                    self.ensure_stack_space(
                        frame.return_value_register as usize + return_values.len(),
                    );

                    // Put the return values back
                    for (i, value) in return_values.into_iter().enumerate() {
                        self.set(frame.return_value_register as usize + i, value);
                        if !frame.allow_multi_return_values {
                            break;
                        }
                    }

                    // Clear out any stale values after the last return value
                    let clear_registers_from = if frame.allow_multi_return_values {
                        frame.return_value_register as usize + num_return_values
                    } else {
                        frame.return_value_register as usize + 1
                    };
                    self.clear_registers_from(clear_registers_from);

                    continue;
                }

                // Control
                OpCode::Jmp => {
                    let addr: JumpAddr = instruction.bc();
                    if addr == JumpAddr::MAX {
                        // TODO: We probably want a nicer error here. Was this a break
                        // outside of a loop? Was it a goto statement with no matching
                        // label?
                        return Err(lua_error!("attempt to jump to an invalid address"));
                    }
                    self.call_frame_mut().set_ip(addr as usize);
                    continue;
                }
                OpCode::JmpTrue => {
                    let register = instruction.a();
                    let value = self.get_ref(register);
                    if value.cast_to_bool() {
                        let addr: JumpAddr = instruction.bc();
                        self.call_frame_mut().set_ip(addr as usize);
                        continue;
                    }
                }
                OpCode::JmpFalse => {
                    let register = instruction.a();
                    let value = self.get_ref(register);
                    if !value.cast_to_bool() {
                        let addr: JumpAddr = instruction.bc();
                        self.call_frame_mut().set_ip(addr as usize);
                        continue;
                    }
                }
                OpCode::JmpClose => {
                    let from_register = instruction.a();
                    let addr: JumpAddr = instruction.bc();
                    if addr == JumpAddr::MAX {
                        // TODO: We probably want a nicer error here. Was this a break
                        // outside of a loop? Was it a goto statement with no matching
                        // label?
                        return Err(lua_error!("attempt to jump to an invalid address"));
                    }

                    self.close(from_register, None)?;
                    self.call_frame_mut().set_ip(addr as usize);

                    continue;
                }

                // Other
                OpCode::Error => {
                    let error_type = instruction.a();
                    return Err(RuntimeError::try_from(error_type)?).into_lua_error();
                }
            }

            self.call_frame_mut().inc_ip();
        }
    }
}

pub(crate) trait WithCount {
    fn count(&self) -> usize;
}

impl<T> WithCount for Vec<T> {
    fn count(&self) -> usize {
        self.len()
    }
}

impl<T, const N: usize> WithCount for [T; N] {
    fn count(&self) -> usize {
        N
    }
}
