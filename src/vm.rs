use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::Debug,
    mem::size_of,
    sync::{Arc, RwLock},
};

use crate::{
    ExecutionResult, ThreadStatus,
    chunk::Chunk,
    error::{IntoLuaError, RuntimeError, lua_error},
    instruction::Instruction,
    macros::{assert_closure, assert_table},
    state::LuaState,
    value::{
        LuaClosure, LuaConst, LuaObject, LuaString, LuaValue, NativeLuaFunction, UpValue,
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
pub type ConstIndex = u16;

impl LuaState {
    pub fn get_global_env(&self) -> Arc<RwLock<UpValue>> {
        self.global.read().unwrap().get_global_env()
    }

    // Helper to read instruction bytes at an offset while holding the lock
    fn read_instr_byte(&self, chunk_index: ConstIndex, offset: usize) -> u8 {
        let global = self.global.read().unwrap();
        global.chunks[chunk_index as usize].instructions[offset]
    }

    // Helper to read a slice of instruction bytes
    fn read_instr_bytes(&self, chunk_index: ConstIndex, start: usize, len: usize) -> Vec<u8> {
        let global = self.global.read().unwrap();
        global.chunks[chunk_index as usize].instructions[start..start + len].to_vec()
    }

    // Safe way to access chunk data - takes a callback that processes the chunk while lock is held
    pub(crate) fn with_chunk<F, R>(&self, index: ConstIndex, f: F) -> Option<R>
    where
        F: FnOnce(&Chunk) -> R,
    {
        let global = self.global.read().unwrap();
        global.chunks.get(index as usize).map(f)
    }

    pub fn get_current_chunk_name(&self) -> Option<LuaString> {
        match self.call_stack[self.call_stack_index - 1].kind {
            CallFrameKind::Lua { chunk, .. } => self.with_chunk(chunk, |c| c.name.clone()),
            CallFrameKind::Native { .. } => None,
        }
    }

    pub fn get_chunk_name(&self, chunk: ConstIndex) -> Option<LuaString> {
        self.with_chunk(chunk, |c| c.name.clone())
    }

    pub(crate) fn get_stack(&self) -> &[LuaValue] {
        &self.stack
    }

    #[inline]
    fn get(&self, register: impl Into<usize>) -> LuaValue {
        let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
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
        let frame = &self.call_stack[self.call_stack_index - 1];
        let upvalues = frame.upvalues.as_ref().expect("frame has no upvalues");
        let upvalue = upvalues
            .get(&upval_index)
            .unwrap_or_else(|| panic!("upvalue {upval_index} not found"));
        upvalue.read().unwrap().clone()
    }

    #[inline]
    fn get_ref(&self, register: impl Into<usize>) -> Cow<'_, LuaValue> {
        let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
        let value = &self.stack[frame_pointer + register.into()];
        if value.is_upvalue() {
            Cow::Owned(unsafe { value.as_upvalue().read().unwrap().0.clone() })
        } else {
            Cow::Borrowed(value)
        }
    }

    #[inline]
    fn set(&mut self, register: impl Into<usize>, value: LuaValue) {
        let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
        self.stack[frame_pointer + register.into()] = value;
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
        num_args: u16,
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
            for (i, upvalue) in upvalues.into_iter() {
                frame_upvalues.insert(i, upvalue);
            }
        }

        self.call_stack_index += 1;

        // Reserve stack space for the registers
        self.ensure_stack_space(max_registers);

        Ok(())
    }

    fn ensure_stack_space(&mut self, max_registers: impl Into<usize>) {
        let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
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
        values: impl IntoIterator<Item = LuaValue> + Debug + WithCount,
    ) -> u8 {
        let call_frame = &self.call_stack[self.call_stack_index - 1];
        let frame_pointer = call_frame.frame_pointer;
        let max_registers = call_frame.max_registers;
        let register = max_registers;

        self.ensure_stack_space(register + values.count() as u8);

        // Copy over the values into the right area of the stack.
        for (i, value) in values.into_iter().enumerate() {
            self.stack[frame_pointer + register as usize + i] = value;
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

        frame.upvalues.as_mut().unwrap().clear();
        frame.varargs.clear();
        frame.to_close.clear();

        popped_frame
    }

    pub(crate) fn capture_upvalue(&mut self, register: u8) -> Arc<RwLock<UpValue>> {
        let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
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
    fn get_const(&self, index: ConstIndex) -> LuaConst {
        let global = self.global.read().unwrap();
        global.consts[index as usize].clone()
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
                self.stack.splice(..num_args, args);

                // Note: Native functions are not supported as coroutines - coroutine.create() rejects them
                // If this is ever extended to support native functions, we would need to store the function
                // pointer in the call frame and handle it specially here
                let (chunk_num_args, has_varargs) =
                    match self.call_stack[self.call_stack_index - 1].kind {
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
                // Status will be set to Suspended with resume args register
                // This is handled when we actually yield
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
        value: LuaClosure,
        args: Vec<LuaValue>,
    ) -> crate::Result<ExecutionResult> {
        let original_call_stack_index = self.call_stack_index;
        let frame_pointer = self.stack.len();

        let (max_registers, num_params, has_varargs) = self
            .with_chunk(value.chunk, |chunk| {
                (
                    chunk.max_registers,
                    chunk.num_params as u16,
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
            num_params,
            has_varargs,
            false,
        )?;

        let num_args = args.len();
        self.ensure_stack_space(max_registers.max(num_args as u8));
        for (index, arg) in args.into_iter().enumerate() {
            self.set(index, arg);
        }

        self.align_closure_args(num_params, num_args, has_varargs);

        let result = match self.run_inner() {
            Ok(ExecutionResult::Completed(values)) => {
                // NOTE: in this case we don't need to pop the call frame, as it was
                // already popped by the `return` instruction (which is always present).
                Ok(ExecutionResult::Completed(values))
            }
            Ok(ExecutionResult::Yielded(values)) => Ok(ExecutionResult::Yielded(values)),
            Err(err) => {
                println!("err {err}");
                Err(self.pop_call_frames_into_error(original_call_stack_index, *err))
            }
        };

        let chunk_name = self
            .with_chunk(value.chunk, |c| c.name.to_string())
            .unwrap();
        assert_eq!(
            original_call_stack_index,
            self.call_stack_index,
            "unbalanced call stack after closure call '{name}'",
            name = chunk_name
        );

        result
    }

    pub(crate) fn set_up_closure_call_frame(
        &mut self,
        frame_pointer: usize,
        return_addr: usize,
        return_value_register: u8,
        closure: LuaClosure,
        num_passed_params: impl Into<usize>,
    ) -> crate::Result {
        let (max_registers, num_params, has_varargs) = self
            .with_chunk(closure.chunk, |chunk| {
                (
                    chunk.max_registers,
                    chunk.num_params as u16,
                    chunk.has_varargs,
                )
            })
            .unwrap();

        self.push_call_frame(
            CallFrameKind::for_chunk(closure.chunk),
            true,
            frame_pointer,
            max_registers,
            return_addr,
            return_value_register,
            Some(closure.upvalues.clone()),
            num_params,
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
                let arg = self.get(num_params + i);
                self.call_stack[self.call_stack_index - 1].varargs.push(arg);
            }
        }

        // Clear out the stack beyond the number of fixed params by filling it
        // with `nil` values. This gets rid of any stale values.
        self.clear_registers_from(num_params.min(num_passed_params));
    }

    fn clear_registers_from(&mut self, from_register: impl Into<usize>) {
        let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
        self.stack[frame_pointer + from_register.into()..].fill(LuaValue::NIL);
    }

    fn clear_registers_between(&mut self, from_register: u8, to_register: u8) {
        let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
        self.stack[frame_pointer + from_register as usize..=frame_pointer + to_register as usize]
            .fill(LuaValue::NIL);
    }

    pub(crate) fn run_native_function(
        &mut self,
        name: &'static str,
        function: NativeLuaFunction,
        args_r: u8,
        num_args: u16,
    ) -> crate::Result<Vec<LuaValue>> {
        let original_call_stack_index = self.call_stack_index;
        let parent_frame_pointer = if self.call_stack_index > 0 {
            self.call_stack[self.call_stack_index - 1].frame_pointer
        } else {
            0
        };

        debug_assert!(
            (parent_frame_pointer + args_r as usize) <= self.stack.len(),
            "not enough stack space for native function arguments!\nfn = {name}\nargs_r = {args_r}\nstack = {:?}",
            &self.stack[parent_frame_pointer..]
        );
        debug_assert!(
            (parent_frame_pointer + args_r as usize + num_args as usize) <= self.stack.len(),
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
            } = self.call_stack[self.call_stack_index - 1].kind
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

            // Macros to safely read instruction parameters while holding the lock
            macro_rules! instr_param {
                ($offset:expr) => {
                    self.read_instr_byte(chunk_index, ip + $offset)
                };
                () => {
                    instr_param!(1)
                };
            }
            macro_rules! register {
                ($offset:expr) => {
                    self.read_instr_byte(chunk_index, ip + $offset)
                };
                () => {
                    register!(1)
                };
            }
            macro_rules! const_index {
                ($offset:expr) => {{
                    let bytes =
                        self.read_instr_bytes(chunk_index, ip + $offset, size_of::<ConstIndex>());
                    ConstIndex::from_be_bytes(bytes.try_into().unwrap())
                }};
                () => {
                    const_index!(2)
                };
            }
            macro_rules! jump_addr {
                ($offset:expr) => {{
                    let bytes =
                        self.read_instr_bytes(chunk_index, ip + $offset, size_of::<JumpAddr>());
                    JumpAddr::from_be_bytes(bytes.try_into().unwrap())
                }};
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
                    let dest_r = register!(1);
                    let lhs_r = register!(2);
                    let rhs_r = register!(3);
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
                    4
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
                    let dest_r = register!(1);
                    let value_r = register!(2);
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
                                handle_execution_result!({ result? }, dest_r, 3)
                            },
                            None => {
                                return Err(self.op_err($op_name, self.get_ref(value_r).as_ref(), None));
                            }
                        }
                    };
                    self.set(dest_r, result);
                    3
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
                            self.call_stack[self.call_stack_index - 1].inc_ip(3);
                            return Ok(result);
                        }
                    }
                };
            }
            macro_rules! handle_execution_result {
                ($result:expr, $dest_r:expr, $ip_inc:expr) => {
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
                            self.call_stack[self.call_stack_index - 1].inc_ip($ip_inc);
                            return Ok(result);
                        }
                    }
                };
            }

            let instruction_increment = match Instruction::from(instruction) {
                // Stack manipulation
                Instruction::LoadConst => {
                    let register = register!();
                    let const_index = const_index!();
                    let constant = self.get_const(const_index);
                    self.set(register, constant.into());
                    2 + size_of::<ConstIndex>()
                }
                Instruction::LoadClosure => {
                    let register = register!();
                    let chunk_index = const_index!();
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
                                    self.call_stack[self.call_stack_index - 1]
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

                    2 + size_of::<ConstIndex>()
                }
                Instruction::LoadNil => {
                    let from_register = register!(1);
                    let to_register = register!(2);
                    self.clear_registers_between(from_register, to_register);
                    3
                }

                // Binary operations
                // Arithmetic
                Instruction::Add => {
                    binary_op_num!(op = +, ADD_KEY, "add")
                }
                Instruction::Sub => {
                    binary_op_num!(op = -, SUB_KEY, "subtract")
                }
                Instruction::Mul => {
                    binary_op_num!(op = *, MUL_KEY, "multiply")
                }
                Instruction::Div => {
                    binary_op_num!(op = /, DIV_KEY, "divide")
                }
                Instruction::Mod => {
                    binary_op_num!(op = %, MOD_KEY, "modulo")
                }
                Instruction::Pow => {
                    binary_op_num!(method = pow, POW_KEY, "power")
                }
                Instruction::IDiv => {
                    binary_op_num!(method = idiv, IDIV_KEY, "integer divide")
                }
                Instruction::Band => {
                    binary_op_num!(fallible_op = &, BAND_KEY, "bitwise and")
                }
                Instruction::Bor => {
                    binary_op_num!(fallible_op = |, BOR_KEY, "bitwise or")
                }
                Instruction::Bxor => {
                    binary_op_num!(fallible_op = ^, BXOR_KEY, "bitwise xor")
                }
                Instruction::Shl => {
                    binary_op_num!(fallible_op = <<, SHL_KEY, "shift left")
                }
                Instruction::Shr => {
                    binary_op_num!(fallible_op = >>, SHR_KEY, "shift left")
                }

                // Comparison
                Instruction::Eq => {
                    let dest_r = register!(1);
                    let lhs_r = register!(2);
                    let rhs_r = register!(3);
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
                            handle_execution_result!(result?, dest_r, 4).cast_to_bool()
                        } else {
                            is_equal
                        }
                    } else {
                        is_equal
                    };
                    self.set(dest_r, value.into());

                    4
                }
                Instruction::Ne => {
                    let dest_r = register!(1);
                    let lhs_r = register!(2);
                    let rhs_r = register!(3);
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
                            handle_execution_result!(result?, dest_r, 4).cast_to_bool()
                        } else {
                            is_equal
                        }
                    } else {
                        is_equal
                    };
                    self.set(dest_r, (!value).into());

                    4
                }
                Instruction::Lt => {
                    binary_op_num!(op_bool_result = <, LT_KEY, "compare")
                }
                Instruction::Le => {
                    binary_op_num!(op_bool_result = <=, LE_KEY, "compare")
                }

                // Concatenation
                Instruction::Concat => {
                    let dest_r = register!(1);
                    let lhs_r = register!(2);
                    let rhs_r = register!(3);
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
                                    let value = handle_execution_result!(value?, dest_r, 4);
                                    self.set(dest_r, value);
                                }
                                None => {
                                    return Err(Box::new(e));
                                }
                            }
                        }
                    }
                    4
                }

                // Unary operations
                Instruction::Neg => {
                    unary_op_num!(op = -, UNM_KEY, "negate")
                }
                Instruction::Not => {
                    // NOTE: There is no metamethod for `not` in Lua
                    let dest_r = register!(1);
                    let val_r = register!(2);
                    let value = self.get_ref(val_r);
                    self.set(dest_r, (!&*value).into());
                    3
                }
                Instruction::Len => {
                    // NOTE: The `__len` metamethod is only called when the value is not a string.
                    let dest_r = register!(1);
                    let val_r = register!(2);
                    let value = self.get_ref(val_r);

                    let result: LuaValue = if value.is_string() {
                        (unsafe { value.as_string() }.len() as i64).into()
                    } else {
                        let value = value.into_owned();
                        match metatables::handle(self, &metatables::LEN_KEY, vec![value]) {
                            Some(result) => handle_execution_result!(result?, dest_r, 3),
                            None => {
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
                        }
                    };
                    self.set(dest_r, result);
                    3
                }
                Instruction::BNot => {
                    unary_op_num!(fallible_op = !, BNOT_KEY, "bitwise not")
                }

                // Variables
                Instruction::Mov => {
                    let dest = register!(1);
                    let src = register!(2);

                    let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
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

                    3
                }
                Instruction::SetUpval => {
                    let upval_index = instr_param!();
                    let register = register!(2);
                    let value = self.get(register);
                    let mut upval = self.call_stack[self.call_stack_index - 1]
                        .upvalues
                        .as_ref()
                        .unwrap()
                        .get(&(upval_index as usize))
                        .as_mut()
                        .unwrap()
                        .write()
                        .unwrap();
                    *upval = value.into();

                    3
                }
                Instruction::GetUpval => {
                    let register = register!();
                    let upval_index = instr_param!(2);
                    let value = self.get_upvalue(upval_index);
                    self.set(register, value);

                    3
                }
                Instruction::LoadVararg => {
                    let register = register!();
                    let is_single_vararg = instr_param!(2) == 1;
                    if is_single_vararg {
                        let value = self.call_stack[self.call_stack_index - 1]
                            .varargs
                            .first()
                            .cloned()
                            .unwrap_or(LuaValue::NIL);
                        self.set(register, value);
                    } else {
                        // Ensure we'll have enough space on the stack.
                        let varargs = self.call_stack[self.call_stack_index - 1].varargs.clone();
                        self.multres = varargs.len();
                        self.ensure_stack_space(register as usize + varargs.len());

                        for (i, value) in varargs.into_iter().enumerate() {
                            self.set(register as usize + i, value);
                        }

                        // Clear out extraneous registers
                        self.clear_registers_from(register as usize + self.multres);
                    }
                    3
                }
                Instruction::ToClose => {
                    let register = register!();
                    self.call_stack[self.call_stack_index - 1]
                        .to_close
                        .push(register);
                    2
                }
                Instruction::Close => {
                    let from_register = register!();
                    self.close(from_register, None)?;
                    2
                }

                // Table
                Instruction::NewTable => {
                    let register = register!();
                    let table = LuaObject::Table(Default::default());
                    self.set(register, table.into());
                    2
                }
                instr @ Instruction::SetTable
                | instr @ Instruction::SetTableConstKey
                | instr @ Instruction::SetTableUpval
                | instr @ Instruction::SetTableUpvalConstKey => {
                    let (table, key, value_r, increment) = match instr {
                        Instruction::SetTable => {
                            let table_r = register!(1);
                            let key_r = register!(2);
                            let value_r = register!(3);

                            let table = self.get(table_r);
                            let key = self.get(key_r);

                            (table, key, value_r, 4)
                        }
                        Instruction::SetTableConstKey => {
                            let table_r = register!(1);
                            let const_index = const_index!(2);
                            let key = self.get_const(const_index).into();
                            let value_r = register!(2 + size_of::<ConstIndex>());

                            let table = self.get(table_r);

                            (table, key, value_r, 3 + size_of::<ConstIndex>())
                        }
                        Instruction::SetTableUpval => {
                            let upval_index = instr_param!(1);
                            let key_r = register!(2);
                            let value_r = register!(3);

                            let table = self.get_upvalue(upval_index);
                            let key = self.get(key_r);

                            (table, key, value_r, 4)
                        }
                        Instruction::SetTableUpvalConstKey => {
                            let upval_index = instr_param!(1);
                            let const_index = const_index!(2);
                            let key = self.get_const(const_index).into();
                            let value_r = register!(2 + size_of::<ConstIndex>());

                            let table = self.get_upvalue(upval_index);

                            (table, key, value_r, 3 + size_of::<ConstIndex>())
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
                            if t.contains_key(&key) {
                                t.insert(key, value);
                                handled = true;
                                break;
                            } else {
                                is_table_with_missing_key = true;
                            }
                        }

                        if let Some(__newindex) = target.get_metavalue(&NEWINDEX_KEY) {
                            let __newindex_callable: Result<Callable, _> = (&__newindex).try_into();
                            if let Ok(__newindex) = __newindex_callable {
                                match __newindex.method {
                                    Method::Closure(closure) => {
                                        let frame_pointer = match instr {
                                            Instruction::SetTable => {
                                                // Arguments are already correctly placed
                                                register!(1)
                                            }
                                            Instruction::SetTableConstKey => {
                                                let table_r = register!(1);
                                                self.set(table_r + 2, value);
                                                table_r
                                            }
                                            Instruction::SetTableUpval => {
                                                let frame_pointer = register!(1);
                                                self.set(frame_pointer, target);
                                                self.set(frame_pointer + 1, key);
                                                self.set(frame_pointer + 2, value);
                                                frame_pointer
                                            }
                                            Instruction::SetTableUpvalConstKey => {
                                                let frame_pointer = register!(1);
                                                self.set(frame_pointer, target);
                                                self.set(frame_pointer + 1, key);
                                                self.set(frame_pointer + 2, value);
                                                frame_pointer
                                            }
                                            _ => unreachable!(),
                                        };
                                        self.set_up_closure_call_frame(
                                            frame_pointer as usize,
                                            ip + increment,
                                            frame_pointer,
                                            closure,
                                            3usize,
                                        )?;
                                        continue 'main;
                                    }
                                    Method::NativeFunction(name, f) => {
                                        let args_r = self.extend_stack([table, key, value]);
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
                        return Err(lua_error!("attempt to index a non-table"));
                    }

                    increment
                }
                instr @ Instruction::GetTable
                | instr @ Instruction::GetTableConstKey
                | instr @ Instruction::GetTableUpval
                | instr @ Instruction::GetTableUpvalConstKey => {
                    let dest_r = register!(1);

                    let (table, key, increment) = match instr {
                        Instruction::GetTable => {
                            let table_r = register!(2);
                            let key_r = register!(3);
                            (self.get(table_r), self.get(key_r), 4)
                        }
                        Instruction::GetTableConstKey => {
                            let table_r = register!(2);
                            let const_index = const_index!(3);
                            let key = self.get_const(const_index).into();
                            (self.get(table_r), key, 3 + size_of::<ConstIndex>())
                        }
                        Instruction::GetTableUpval => {
                            let upval_index = instr_param!(2);
                            let key_r = register!(3);
                            let table = self.get_upvalue(upval_index);
                            let key = self.get(key_r);
                            (table, key, 4)
                        }
                        Instruction::GetTableUpvalConstKey => {
                            let upval_index = instr_param!(2);
                            let const_index = const_index!(3);
                            let table = self.get_upvalue(upval_index);
                            let key = self.get_const(const_index).into();

                            (table, key, 3 + size_of::<ConstIndex>())
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
                                match __index.method {
                                    Method::Closure(closure) => {
                                        let table_r = match instr {
                                            Instruction::GetTable => register!(2),
                                            Instruction::GetTableConstKey
                                            | Instruction::GetTableUpval
                                            | Instruction::GetTableUpvalConstKey => {
                                                // In these cases, the table and/or key are not yet
                                                // loaded into registers, which it needs to be for
                                                // the __index call.
                                                self.extend_stack([table, key])
                                            }
                                            _ => unreachable!(),
                                        };
                                        let parent_frame_pointer = self.call_stack
                                            [self.call_stack_index - 1]
                                            .frame_pointer;
                                        self.set_up_closure_call_frame(
                                            parent_frame_pointer + table_r as usize,
                                            ip + increment,
                                            dest_r,
                                            closure,
                                            1usize,
                                        )?;
                                        continue 'main;
                                    }
                                    Method::NativeFunction(name, f) => {
                                        let args_r = self.extend_stack([table, key]);
                                        let result =
                                            self.run_native_function(name, f, args_r, 2)?;

                                        // Check if the native function yielded
                                        if let Some(yield_values) = self.pending_yield.take() {
                                            // Set status to Suspended, storing where to put resume args
                                            self.status = ThreadStatus::Suspended {
                                                resume_args_register: Some(dest_r),
                                            };
                                            return Ok(ExecutionResult::Yielded(yield_values));
                                        }

                                        if let Some(return_value) = result.into_iter().next() {
                                            self.set(dest_r, return_value);
                                        } else {
                                            self.set(dest_r, LuaValue::NIL);
                                        }
                                        self.pop_call_frame();
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
                            return Err(lua_error!("attempt to index a non-table"));
                        }
                    }

                    increment
                }
                instr @ Instruction::AppendToTable | instr @ Instruction::AppendToTableM => {
                    // Append each value to the table at the stack right before the values
                    let table_r = register!();
                    let mut num_values = instr_param!(2) as usize;
                    if matches!(instr, Instruction::AppendToTableM) {
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

                    3
                }

                // Function
                instr @ Instruction::Call | instr @ Instruction::CallM => {
                    let func_r = register!();
                    let is_single_return = instr_param!(2) == 1;
                    let mut num_args = instr_param!(3) as usize;

                    let is_callm = matches!(instr, Instruction::CallM);
                    if is_callm {
                        num_args += self.multres;
                    }

                    // Get the function value
                    let function = self.get_ref(func_r);

                    // Find the underlying closure or native function, handling metamethods
                    let callable: Option<Callable> = function.as_ref().try_into().ok();

                    let params_r = if callable.as_ref().is_some_and(|c| c.is_metamethod) {
                        // This means that the function being called is actually a value with
                        // a `__call` metamethod. Account for an extra argument, and use the
                        // register for the "function" as the frame pointer, so it will be passed
                        // as the implicit self paremeter.
                        num_args += 1;
                        func_r
                    } else {
                        // Normally, the parameters start just after the function register.
                        func_r + 1
                    };

                    let callable = callable.map(|c| c.method);
                    match callable {
                        Some(Method::Closure(closure)) => {
                            let parent_frame_pointer =
                                self.call_stack[self.call_stack_index - 1].frame_pointer;
                            let (max_regs, num_params, has_varargs) = self
                                .with_chunk(closure.chunk, |c| {
                                    (c.max_registers, c.num_params, c.has_varargs)
                                })
                                .unwrap();
                            self.push_call_frame(
                                CallFrameKind::for_chunk(closure.chunk),
                                false,
                                parent_frame_pointer + params_r as usize,
                                max_regs,
                                ip + 4,
                                func_r,
                                Some(closure.upvalues.clone()),
                                num_params as u16,
                                has_varargs,
                                !is_single_return,
                            )?;
                            self.align_closure_args(num_params, num_args, has_varargs);

                            continue;
                        }
                        Some(Method::NativeFunction(name, f)) => {
                            let result =
                                self.run_native_function(name, f, params_r, num_args as u16)?;

                            // Check if the native function yielded
                            if let Some(yield_values) = self.pending_yield.take() {
                                // Set status to Suspended, storing where to put resume args
                                self.status = ThreadStatus::Suspended {
                                    resume_args_register: Some(func_r),
                                };
                                // Increment IP past the Call instruction so we don't re-execute it on resume
                                self.call_stack[self.call_stack_index - 1].inc_ip(4);
                                return Ok(ExecutionResult::Yielded(yield_values));
                            }

                            let value_slots_needed = match (is_single_return, result.len()) {
                                (true, 0) => 1,
                                (true, n) => n.min(1),
                                (false, n) => n,
                            };
                            self.ensure_stack_space(func_r as usize + value_slots_needed);

                            if result.is_empty() && is_single_return {
                                self.set(func_r, LuaValue::NIL);
                                self.multres = 0;
                            } else {
                                self.multres = result.len();
                                for (index, value) in result.into_iter().enumerate() {
                                    self.set(func_r as usize + index, value);
                                    if is_single_return {
                                        self.multres = 1;
                                        break;
                                    }
                                }
                            }

                            self.clear_registers_from(func_r as usize + self.multres);

                            4
                        }
                        _ => {
                            // TODO: Might be nice here to print the name of the variable.
                            return Err(lua_error!(
                                "attempt to call a {} value",
                                function.type_name()
                            ));
                        }
                    }
                }
                instr @ Instruction::CallT | instr @ Instruction::CallTM => {
                    let args_r = register!();
                    // let is_single_return = instr_param!(2) == 1;
                    let mut num_passed_args = instr_param!(3) as usize;

                    let is_callm = matches!(instr, Instruction::CallTM);
                    if is_callm {
                        num_passed_args += self.multres;
                    }

                    let CallFrame {
                        num_args,
                        takes_varargs,
                        ..
                    } = self.call_stack[self.call_stack_index - 1];

                    // Copy over the args to the start of the frame pointer
                    for i in 0..num_passed_args {
                        let value = self.get(args_r as usize + i);
                        self.set(i, value);
                    }

                    self.align_closure_args(num_args, num_passed_args, takes_varargs);

                    // Then just jump back to the start of this closure.
                    self.call_stack[self.call_stack_index - 1].set_ip(0);
                    continue;
                }
                instr @ Instruction::Return
                | instr @ Instruction::Return0
                | instr @ Instruction::Return1
                | instr @ Instruction::ReturnM => {
                    // Find the marker indicating the start of the return values
                    let (values_starting_register, num_return_values) = match instr {
                        Instruction::Return => (register!(), instr_param!(2) as usize),
                        Instruction::ReturnM => {
                            (register!(), instr_param!(2) as usize + self.multres)
                        }
                        Instruction::Return1 => (register!(), 1),
                        // NOTE: Register doesn't matter for Return0, as the iterator below to
                        // collect return values won't run.
                        Instruction::Return0 => (0, 0),
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

                    self.call_stack[self.call_stack_index - 1].set_ip(frame.return_addr);

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
                Instruction::Jmp => {
                    let addr = jump_addr!(1);
                    if addr == JumpAddr::MAX {
                        // TODO: We probably want a nicer error here. Was this a break
                        // outside of a loop? Was it a goto statement with no matching
                        // label?
                        return Err(lua_error!("attempt to jump to an invalid address"));
                    }
                    self.call_stack[self.call_stack_index - 1].set_ip(addr as usize);
                    continue;
                }
                Instruction::JmpTrue => {
                    let register = register!();
                    let value = self.get_ref(register);
                    if value.cast_to_bool() {
                        let addr = jump_addr!(2);
                        self.call_stack[self.call_stack_index - 1].set_ip(addr as usize);
                        continue;
                    }
                    2 + size_of::<JumpAddr>()
                }
                Instruction::JmpFalse => {
                    let register = register!();
                    let value = self.get_ref(register);
                    if !value.cast_to_bool() {
                        let addr = jump_addr!(2);
                        self.call_stack[self.call_stack_index - 1].set_ip(addr as usize);
                        continue;
                    }
                    2 + size_of::<JumpAddr>()
                }
                Instruction::JmpClose => {
                    let from_register = register!();
                    let addr = jump_addr!(2);
                    if addr == JumpAddr::MAX {
                        // TODO: We probably want a nicer error here. Was this a break
                        // outside of a loop? Was it a goto statement with no matching
                        // label?
                        return Err(lua_error!("attempt to jump to an invalid address"));
                    }

                    self.close(from_register, None)?;
                    self.call_stack[self.call_stack_index - 1].set_ip(addr as usize);

                    continue;
                }

                // Other
                Instruction::Error => {
                    let error_type = instr_param!();
                    return Err(RuntimeError::try_from(error_type)?).into_lua_error();
                }
            };
            self.call_stack[self.call_stack_index - 1].inc_ip(instruction_increment);
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
