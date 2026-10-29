use crate::{
    ExecutionResult, LuaState, ThreadStatus,
    error::lua_error,
    instruction::{Instruction, OpCode},
    macros::{assert_closure, assert_table},
    value::{
        LuaClosure, LuaObject, LuaValue,
        callable::{Callable, Method},
        metatables::{self, INDEX_KEY, NEWINDEX_KEY},
    },
    vm::{CallFrameKind, ChunkIndex, call_frame::CallFrame, handle_execution_result},
};

#[derive(Debug)]
pub(super) enum InstructionResult {
    /// Continue as normal, incrementing the instruction pointer, and moving on to the next
    /// instruction.
    Normal,
    /// Continue the main VM loop, skipping the usual instruction pointer increment. This is used
    /// when a function call is made, and the VM needs to jump into the called function's
    /// instructions.
    Continue,
    /// The instruction resulted in a yield, and the VM should suspend execution. The yielded
    /// values are stored in the `ExecutionResult`.
    ExecutionResult(ExecutionResult),
}

impl From<ExecutionResult> for InstructionResult {
    fn from(result: ExecutionResult) -> Self {
        Self::ExecutionResult(result)
    }
}

impl LuaState {
    pub(super) fn instr_load_const(&mut self, chunk_index: u16, instruction: Instruction) {
        let register = instruction.a();
        let const_index = instruction.b();
        let constant = self.get_const(chunk_index, const_index);
        self.set(register, constant.into());
    }

    pub(super) fn instr_load_closure(&mut self, instruction: Instruction) {
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

    pub(super) fn instr_load_nil(&mut self, instruction: Instruction) {
        let from_register = instruction.a();
        let to_register = instruction.b();
        self.clear_registers_between(from_register, to_register);
    }

    pub(super) fn instr_load_short(&mut self, instruction: Instruction) {
        let register = instruction.a();
        let int_value = instruction.bc();
        let int_value = u16::cast_signed(int_value);
        self.set(register, LuaValue::from_integer(int_value.into()));
    }

    pub(super) fn instr_load_short_float(&mut self, instruction: Instruction) {
        let register = instruction.a();
        let int_value = instruction.bc();
        let int_value = u16::cast_signed(int_value);
        self.set(register, LuaValue::from_float(int_value.into()));
    }

    pub(super) fn instr_eq(
        &mut self,
        instruction: Instruction,
    ) -> crate::Result<Option<ExecutionResult>> {
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
                handle_execution_result!(self, result?, dest_r).cast_to_bool()
            } else {
                is_equal
            }
        } else {
            is_equal
        };
        self.set(dest_r, value.into());

        Ok(None)
    }

    pub(super) fn instr_ne(
        &mut self,
        instruction: Instruction,
    ) -> crate::Result<Option<ExecutionResult>> {
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
                handle_execution_result!(self, result?, dest_r).cast_to_bool()
            } else {
                is_equal
            }
        } else {
            is_equal
        };
        self.set(dest_r, (!value).into());
        Ok(None)
    }

    pub(super) fn instr_concat(
        &mut self,
        instruction: Instruction,
    ) -> crate::Result<Option<ExecutionResult>> {
        let dest_r = instruction.a();
        let lhs_r = instruction.b();
        let rhs_r = instruction.c();
        let a = self.get(lhs_r);
        let b = self.get(rhs_r);
        match a.concat(b) {
            Ok(value) => self.set(dest_r, value),
            Err(err) => {
                let (e, a, b) = *err;
                match metatables::handle(self, &metatables::CONCAT_KEY, vec![a.clone(), b.clone()])
                {
                    Some(value) => {
                        let value = handle_execution_result!(self, value?, dest_r);
                        self.set(dest_r, value);
                    }
                    None => {
                        return Err(Box::new(e));
                    }
                }
            }
        }
        Ok(None)
    }

    pub(super) fn instr_len(
        &mut self,
        instruction: crate::instruction::Instruction,
    ) -> crate::Result<Option<ExecutionResult>> {
        // NOTE: The `__len` metamethod is only called when the value is not a string.
        let dest_r = instruction.a();
        let val_r = instruction.b();
        let value = self.get_ref(val_r);
        let result: LuaValue = if value.is_string() {
            (unsafe { value.as_string() }.len() as i64).into()
        } else {
            let value = value.into_owned();
            if let Some(result) = metatables::handle(self, &metatables::LEN_KEY, vec![value]) {
                handle_execution_result!(self, result?, dest_r)
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
        Ok(None)
    }

    pub(super) fn instr_mov(&mut self, instruction: crate::instruction::Instruction) {
        let dest = instruction.a();
        let src = instruction.b();

        let frame_pointer = self.call_frame().frame_pointer;
        let value = self.get(src);
        if self.stack[frame_pointer + dest as usize].is_upvalue() {
            let mut upval = (unsafe { self.stack[frame_pointer + dest as usize].as_upvalue() })
                .write()
                .unwrap();
            *upval = value.into();
        } else {
            self.set(dest, value);
        }
    }

    pub(super) fn instr_set_upval(&mut self, instruction: crate::instruction::Instruction) {
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

    pub(super) fn instr_get_upval(&mut self, instruction: crate::instruction::Instruction) {
        let register = instruction.a();
        let upval_index = instruction.b();
        let value = self.get_upvalue(upval_index);
        self.set(register, value);
    }

    pub(super) fn instr_load_vararg(&mut self, instruction: crate::instruction::Instruction) {
        let register = instruction.a();
        // 0 = multres, otherwise expect n - 1 results.
        let num_results = instruction.b();

        if num_results > 0 {
            // We shouldn't need to `ensure_stack_space` here, because we're loading a statically
            // known amount of values. This means the compiler must have expected the registers
            // needed, and reserved them.
            let num_expected_values = num_results - 1;
            for i in 0..num_expected_values {
                let value = self
                    .call_frame()
                    .varargs
                    .get(i as usize)
                    .cloned()
                    .unwrap_or(LuaValue::NIL);
                self.set(register as usize + i as usize, value);
            }
        } else {
            // Ensure we'll have enough space on the stack.
            let varargs = self.call_frame().varargs.clone();
            self.multres = varargs.len();
            self.ensure_stack_space(register as usize + varargs.len());

            for (i, value) in varargs.into_iter().enumerate() {
                self.set(register as usize + i, value);
            }

            // Clear out extraneous registers
            // TODO: Store the number of expected values in the instruction (not just
            // single or multiple) so we know if we need to do this or not.
            self.clear_registers_from(register as usize + self.multres, None);
        }
    }

    pub(super) fn instr_set_table(
        &mut self,
        chunk_index: u16,
        ip: usize,
        instruction: crate::instruction::Instruction,
        instr: OpCode,
    ) -> crate::Result<InstructionResult> {
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
                                | OpCode::SetTableUpvalConstKey => {
                                    self.extend_stack(closure.clone(), [target, key, value])
                                }
                                _ => unreachable!(),
                            };
                            self.set_up_closure_call_frame(
                                self.call_frame().frame_pointer + args_r as usize,
                                ip + 1,
                                args_r,
                                &closure,
                                3usize,
                                1,
                            )?;
                            return Ok(InstructionResult::Continue);
                        }
                        jf @ Method::JitFunction(name, f) => {
                            let args_r = self.extend_stack(jf, [table.clone(), key, value]);
                            self.run_jit_function(name, f, args_r, 3, 1)?;

                            // Check if the native function yielded
                            if let Some(yield_values) = self.pending_yield.take() {
                                // In this case, we don't care about the return values,
                                // so set status to suspended with an empty
                                // `resume_args_register`.
                                self.status = ThreadStatus::Suspended {
                                    resume_args_register: None,
                                };
                                return Ok(ExecutionResult::Yielded(yield_values).into());
                            }
                        }
                        nf @ Method::NativeFunction(name, f) => {
                            let args_r = self.extend_stack(nf, [table.clone(), key, value]);
                            self.run_native_function(name, f, args_r, 3, 1)?;

                            // Check if the native function yielded
                            if let Some(yield_values) = self.pending_yield.take() {
                                // In this case, we don't care about the return values,
                                // so set status to suspended with an empty
                                // `resume_args_register`.
                                self.status = ThreadStatus::Suspended {
                                    resume_args_register: None,
                                };
                                return Ok(ExecutionResult::Yielded(yield_values).into());
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
                    if let Some(name_info) = self.describe_register(chunk_index, ip, table_r) {
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

        Ok(InstructionResult::Normal)
    }

    pub(super) fn instr_get_table(
        &mut self,
        chunk_index: u16,
        ip: usize,
        instruction: crate::instruction::Instruction,
        instr: OpCode,
    ) -> crate::Result<InstructionResult> {
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
                            self.extend_stack(__index.method.clone(), [table.clone(), key])
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
                                2,
                            )?;
                            return Ok(InstructionResult::Continue);
                        }
                        Method::JitFunction(name, f) => {
                            self.run_jit_function(name, f, args_r, 2, 2)?;

                            // Check if the native function yielded
                            if let Some(yield_values) = self.pending_yield.take() {
                                // Set status to Suspended, storing where to put resume args
                                self.status = ThreadStatus::Suspended {
                                    resume_args_register: Some(dest_r),
                                };
                                return Ok(ExecutionResult::Yielded(yield_values).into());
                            }

                            self.set(dest_r, self.get(args_r));
                        }
                        Method::NativeFunction(name, f) => {
                            self.run_native_function(name, f, args_r, 2, 2)?;

                            // Check if the native function yielded
                            if let Some(yield_values) = self.pending_yield.take() {
                                // Set status to Suspended, storing where to put resume args
                                self.status = ThreadStatus::Suspended {
                                    resume_args_register: Some(dest_r),
                                };
                                return Ok(ExecutionResult::Yielded(yield_values).into());
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
                        if let Some(name_info) = self.describe_register(chunk_index, ip, table_r) {
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

        Ok(InstructionResult::Normal)
    }

    pub(super) fn instr_call(
        &mut self,
        instruction: Instruction,
        instr: OpCode,
        chunk_index: ChunkIndex,
        ip: usize,
    ) -> crate::Result<InstructionResult> {
        let func_r = instruction.a();
        // let is_single_return = instruction.b() == 1;
        // 0 = multres, otherwise expect n - 1 results.
        let expected_num_results = instruction.b();
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
                    expected_num_results,
                )?;
                self.align_closure_args(num_params, num_args, has_varargs);

                Ok(InstructionResult::Continue)
            }
            Method::JitFunction(name, f) => {
                let result =
                    self.run_jit_function(name, f, func_r, num_args, expected_num_results)?;

                // Check if the native function yielded
                if let Some(yield_values) = self.pending_yield.take() {
                    // Set status to Suspended, storing where to put resume args
                    self.status = ThreadStatus::Suspended {
                        resume_args_register: Some(func_r),
                    };
                    // Increment IP past the Call instruction so we don't re-execute it on resume
                    self.call_frame_mut().inc_ip();
                    return Ok(ExecutionResult::Yielded(yield_values).into());
                }

                let value_slots_needed = if expected_num_results == 0 {
                    result
                } else {
                    (expected_num_results as usize - 1).min(result)
                };
                self.ensure_stack_space(func_r as usize + value_slots_needed);

                self.multres = result;
                if result == 0 && expected_num_results > 0 {
                    self.set(func_r, LuaValue::NIL);
                }

                // If we expected a specific number of results, and we got fewer than that, clear
                // the extra registers beyond the returned values to get rid of stale values, such
                // as arguments to the called function.
                if expected_num_results > 0 && (expected_num_results as usize - 1) > result {
                    self.clear_registers_from(
                        func_r as usize + self.multres,
                        Some(func_r as usize + expected_num_results as usize - 1),
                    );
                }

                Ok(InstructionResult::Normal)
            }
            Method::NativeFunction(name, f) => {
                let result =
                    self.run_native_function(name, f, func_r, num_args, expected_num_results)?;

                // Check if the native function yielded
                if let Some(yield_values) = self.pending_yield.take() {
                    // Set status to Suspended, storing where to put resume args
                    self.status = ThreadStatus::Suspended {
                        resume_args_register: Some(func_r),
                    };
                    // Increment IP past the Call instruction so we don't re-execute it on resume
                    self.call_frame_mut().inc_ip();
                    return Ok(ExecutionResult::Yielded(yield_values).into());
                }

                let value_slots_needed = if expected_num_results == 0 {
                    result
                } else {
                    (expected_num_results as usize - 1).min(result)
                };
                self.ensure_stack_space(func_r as usize + value_slots_needed);

                self.multres = result;
                if result == 0 && expected_num_results > 0 {
                    self.set(func_r, LuaValue::NIL);
                }

                // If we expected a specific number of results, and we got fewer than that, clear
                // the extra registers beyond the returned values to get rid of stale values, such
                // as arguments to the called function.
                if expected_num_results > 0 && (expected_num_results as usize - 1) > result {
                    self.clear_registers_from(
                        func_r as usize + self.multres,
                        Some(func_r as usize + expected_num_results as usize - 1),
                    );
                }

                Ok(InstructionResult::Normal)
            }
        }
    }

    pub(super) fn instr_call_t(
        &mut self,
        instruction: crate::instruction::Instruction,
        instr: OpCode,
    ) {
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

        // Start executing the function from the start again.
        self.call_frame_mut().set_ip(0);
    }

    pub(super) fn instr_return(
        &mut self,
        instruction: Instruction,
        instr: OpCode,
    ) -> Option<ExecutionResult> {
        // Find the marker indicating the start of the return values
        let (values_starting_register, num_return_values) = match instr {
            OpCode::Return => (instruction.a(), instruction.b() as usize),
            OpCode::ReturnM => (instruction.a(), instruction.b() as usize + self.multres),
            OpCode::Return1 => (instruction.a(), 1),
            // NOTE: Register doesn't matter for Return0, as the iterator below to
            // collect return values won't run.
            OpCode::Return0 => (0, 0),
            _ => unreachable!(),
        };

        // Strip upvalue wrappers from the returning function's registers so they
        // don't leak into the caller's frame. Without this, MOV instructions in
        // the caller that happen to target a leaked upvalue-wrapped register would
        // write through to the shared UpValue, corrupting closures that captured it.
        {
            let fp = self.call_frame().frame_pointer;
            for slot in &mut self.stack[fp..] {
                if slot.is_upvalue() {
                    let inner = unsafe { slot.as_upvalue() }.read().unwrap().0.clone();
                    *slot = inner;
                }
            }
        }

        let frame = self.pop_call_frame();
        let mut corrected_starting_register =
            frame.frame_pointer + values_starting_register as usize;
        if self.call_stack_index > 0 {
            corrected_starting_register -= self.call_frame().frame_pointer;
        }

        self.multres = if frame.num_return_values == 0 {
            num_return_values
        } else {
            1
        };

        // Check if this is the root frame, or if we're leaving this chunk
        if frame.border_frame {
            // Collect the return values. When call_stack_index == 0 (e.g. a
            // coroutine's root frame), there is no parent frame so we must
            // access the stack directly — corrected_starting_register is
            // already an absolute position in that case.
            let mut return_values: Vec<_> = (0..num_return_values)
                .map(|i| {
                    if self.call_stack_index > 0 {
                        self.get(corrected_starting_register + i)
                    } else {
                        self.stack[corrected_starting_register + i].clone()
                    }
                })
                .collect();

            // Ensure we always return a value.
            if return_values.is_empty() && frame.num_return_values > 0 {
                return_values.push(LuaValue::NIL);
            }

            return ExecutionResult::Completed(return_values).into();
        }

        self.call_frame_mut().set_ip(frame.return_addr);

        // Put the return values back
        for i in 0..num_return_values {
            self.swap(
                frame.return_value_register as usize + i,
                corrected_starting_register + i,
            );

            // Did we reach the number of expected values? Then we can stop moving values around.
            if frame.num_return_values > 0 && frame.num_return_values as usize - 1 == i {
                break;
            }
        }

        // If we expected a specific number of results, and we got fewer than that, clear
        // the extra registers beyond the returned values to get rid of stale values, such
        // as arguments to the called function.
        if frame.num_return_values > 0 && (frame.num_return_values as usize - 1) > num_return_values
        {
            self.clear_registers_from(
                frame.return_value_register as usize + num_return_values,
                Some(frame.return_value_register as usize + frame.num_return_values as usize - 1),
            );
        }

        None
    }
}
