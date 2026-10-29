use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::Debug,
    sync::{Arc, RwLock},
};

use crate::{
    ExecutionResult, ThreadStatus,
    chunk::{Chunk, RegisterName},
    error::lua_error,
    instruction::OpCode,
    state::LuaState,
    value::{
        JitLuaFunction, LuaClosure, LuaConst, LuaNumber, LuaObject, LuaString, LuaTable, LuaValue,
        NativeLuaFunction, UpValue, metatables, string::coerce_number,
    },
    vm::{
        call_frame::{CallFrame, PoppedCallFrame},
        instructions::InstructionResult,
        macros::handle_execution_result,
    },
};

pub(crate) use call_frame::CallFrameKind;

mod arg;
pub(crate) mod call_frame;
mod close;
mod error;
mod instructions;
mod macros;
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

    /// Take the value from a register, and put a [`LuaValue::NIL`] back in its place.
    #[inline]
    pub(crate) fn take(&mut self, register: impl Into<usize>) -> LuaValue {
        let frame_pointer = self.call_frame().frame_pointer;
        let mut value = LuaValue::NIL;
        std::mem::swap(&mut value, &mut self.stack[frame_pointer + register.into()]);
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

    #[inline]
    pub fn swap(&mut self, register: impl Into<usize>, with: impl Into<usize>) {
        let frame_pointer = self.call_frame().frame_pointer;
        self.stack
            .swap(frame_pointer + register.into(), frame_pointer + with.into());
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
        num_return_values: u8,
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
        self.call_stack[self.call_stack_index].num_return_values = num_return_values;
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
            frame_pointer: frame.frame_pointer,
            border_frame: frame.border_frame,
            return_addr: frame.return_addr,
            return_value_register: frame.return_value_register,
            num_return_values: frame.num_return_values,
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
        num_return_values: u8,
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
            num_return_values,
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
        num_return_values: u8,
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
            num_return_values,
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
                let arg = self.take(num_params + i + 1);
                self.call_frame_mut().varargs.push(arg);
            }
        }
    }

    fn clear_registers_from(&mut self, from_register: impl Into<usize>, to: Option<usize>) {
        let frame_pointer = self.call_frame().frame_pointer;
        // Manual .fill(LuaValue::NIL), so we don't end up going through `Clone`
        // which does unnecessary work here.
        for i in (frame_pointer + from_register.into())
            ..to.unwrap_or(self.stack.len()).min(self.stack.len())
        {
            self.stack[i] = LuaValue::NIL;
        }
    }

    fn clear_registers_between(&mut self, from_register: u8, to_register: u8) {
        let frame_pointer = self.call_frame().frame_pointer;
        self.stack[frame_pointer + from_register as usize..=frame_pointer + to_register as usize]
            .fill(LuaValue::NIL);
    }

    /// Run a JIT'ed function in this thread ([`LuaState`]). This will handle automatically pushing
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
    pub(crate) fn run_jit_function(
        &mut self,
        name: &'static str,
        function: JitLuaFunction,
        args_r: u8,
        num_args: usize,
        num_return_values: u8,
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
            num_return_values,
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
        num_return_values: u8,
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
            num_return_values,
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

            #[cfg(feature = "debugger")]
            {
                if let Some(mut dbg) = self.debugger.take() {
                    let result = dbg.on_instruction(self, chunk_index, ip);
                    self.debugger = Some(dbg);
                    result?;
                }
            }

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
                                handle_execution_result!(self, { result? }, dest_r)
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

            match instruction.opcode() {
                // Stack manipulation
                OpCode::LoadConst => {
                    self.instr_load_const(chunk_index, instruction);
                }
                OpCode::LoadClosure => {
                    self.instr_load_closure(instruction);
                }
                OpCode::LoadNil => {
                    self.instr_load_nil(instruction);
                }
                OpCode::LoadShort => {
                    self.instr_load_short(instruction);
                }
                OpCode::LoadShortFloat => {
                    self.instr_load_short_float(instruction);
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
                    if let Some(execution_result) = self.instr_eq(instruction)? {
                        return Ok(execution_result);
                    }
                }
                OpCode::Ne => {
                    if let Some(execution_result) = self.instr_ne(instruction)? {
                        return Ok(execution_result);
                    }
                }
                OpCode::Lt => {
                    binary_op_num!(op_bool_result = <, LT_KEY, "compare");
                }
                OpCode::Le => {
                    binary_op_num!(op_bool_result = <=, LE_KEY, "compare");
                }

                // Concatenation
                OpCode::Concat => {
                    if let Some(value) = self.instr_concat(instruction)? {
                        return Ok(value);
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
                    if let Some(value) = self.instr_len(instruction)? {
                        return Ok(value);
                    }
                }
                OpCode::BNot => {
                    unary_op_num!(fallible_op = !, BNOT_KEY, "bitwise not");
                }

                // Variables
                OpCode::MovDirect => {
                    let frame_pointer = self.call_frame().frame_pointer;
                    let value = self.stack[frame_pointer + instruction.b() as usize].clone();
                    self.set(instruction.a(), value);
                }
                OpCode::Mov => {
                    self.instr_mov(instruction);
                }
                OpCode::SetUpval => {
                    self.instr_set_upval(instruction);
                }
                OpCode::GetUpval => {
                    self.instr_get_upval(instruction);
                }
                OpCode::LoadVararg => {
                    self.instr_load_vararg(instruction);
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
                    match self.instr_set_table(chunk_index, ip, instruction, instr)? {
                        InstructionResult::Normal => {}
                        InstructionResult::Continue => {
                            continue 'main;
                        }
                        InstructionResult::ExecutionResult(execution_result) => {
                            return Ok(execution_result);
                        }
                    }
                }
                instr @ (OpCode::GetTable
                | OpCode::GetTableConstKey
                | OpCode::GetTableUpval
                | OpCode::GetTableUpvalConstKey) => {
                    self.instr_get_table(chunk_index, ip, instruction, instr)?;
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
                    match self.instr_call(instruction, instr, chunk_index, ip)? {
                        InstructionResult::Normal => {}
                        InstructionResult::Continue => {
                            continue 'main;
                        }
                        InstructionResult::ExecutionResult(execution_result) => {
                            return Ok(execution_result);
                        }
                    }
                }
                instr @ (OpCode::CallT | OpCode::CallTM) => {
                    self.instr_call_t(instruction, instr);

                    // Then just jump back to the start of this closure.
                    continue;
                }
                instr @ (OpCode::Return | OpCode::Return0 | OpCode::Return1 | OpCode::ReturnM) => {
                    if let Some(execution_result) = self.instr_return(instruction, instr) {
                        return Ok(execution_result);
                    }

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

                // Loops
                OpCode::ForPrep => {
                    let base = instruction.a();
                    let exit_addr = instruction.bc();

                    let mut initial_num = coerce_number(self.get(base))
                        .map_err(|_| lua_error!("'for' initial value must be a number"))?;
                    let mut limit_num = coerce_number(self.get(base + 1))
                        .map_err(|_| lua_error!("'for' limit must be a number"))?;
                    let mut step_num = coerce_number(self.get(base + 2))
                        .map_err(|_| lua_error!("'for' step must be a number"))?;

                    // Coerce all to float if any is a float
                    if matches!(
                        (&initial_num, &limit_num, &step_num),
                        (LuaNumber::Float(_), _, _)
                            | (_, LuaNumber::Float(_), _)
                            | (_, _, LuaNumber::Float(_))
                    ) {
                        initial_num = LuaNumber::Float(f64::from(initial_num));
                        limit_num = LuaNumber::Float(f64::from(limit_num));
                        step_num = LuaNumber::Float(f64::from(step_num));
                    }

                    // Write back coerced values
                    self.set(base, LuaValue::from_number(&initial_num));
                    self.set(base + 1, LuaValue::from_number(&limit_num));
                    self.set(base + 2, LuaValue::from_number(&step_num));

                    let step_is_zero = match &step_num {
                        LuaNumber::Integer(n) => *n == 0,
                        LuaNumber::Float(n) => *n == 0.0,
                    };
                    if step_is_zero {
                        return Err(lua_error!("'for' step is zero"));
                    }

                    let step_positive = match &step_num {
                        LuaNumber::Integer(n) => *n > 0,
                        LuaNumber::Float(n) => *n > 0.0,
                    };

                    let skip = if step_positive {
                        initial_num > limit_num
                    } else {
                        initial_num < limit_num
                    };

                    if skip {
                        self.call_frame_mut().set_ip(exit_addr as usize);
                        continue;
                    }
                }
                OpCode::ForLoop => {
                    let base = instruction.a();
                    let body_addr = instruction.bc();

                    let i = self.get(base);
                    let limit = self.get(base + 1);
                    let step = self.get(base + 2);

                    let i_num = unsafe { i.as_number() };
                    let step_num = unsafe { step.as_number() };
                    let limit_num = unsafe { limit.as_number() };

                    let new_i = &i_num + &step_num;
                    self.set(base, LuaValue::from_number(&new_i));

                    let step_positive = match &step_num {
                        LuaNumber::Integer(n) => *n > 0,
                        LuaNumber::Float(n) => *n > 0.0,
                    };

                    let in_range = if step_positive {
                        new_i <= limit_num
                    } else {
                        new_i >= limit_num
                    };

                    if in_range {
                        self.call_frame_mut().set_ip(body_addr as usize);
                        continue;
                    }
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
