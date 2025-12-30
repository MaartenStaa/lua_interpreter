use std::{
    borrow::Cow,
    collections::HashMap,
    mem::size_of,
    path::PathBuf,
    sync::{Arc, RwLock},
};

use crate::{
    chunk::Chunk,
    debug,
    error::{IntoLuaError, LuaError, RuntimeError},
    instruction::Instruction,
    macros::{assert_closure, assert_table, assert_table_object},
    value::{
        LuaClosure, LuaConst, LuaObject, LuaValue, LuaVariableAttribute, UpValue,
        callable::{Callable, Method},
        metatables::{self, CLOSE_KEY, INDEX_KEY, NEWINDEX_KEY},
    },
};

const MAX_STACK_SIZE: usize = 256;

pub type JumpAddr = u16;
pub type ConstIndex = u16;

#[derive(Debug)]
pub struct VM<'source> {
    global_env: Arc<RwLock<LuaObject>>,
    chunks: Vec<Chunk<'source>>,
    chunk_map: HashMap<PathBuf, usize>,
    consts: Vec<LuaConst>,
    const_index: ConstIndex,
    stack: Vec<LuaValue>,
    stack_attrs: Vec<u8>,
    // Ephemeral result count for multi-return value calls
    multres: usize,
    call_stack: [CallFrame; MAX_STACK_SIZE],
    call_stack_index: usize,
    print_bytecode: bool,
}

#[derive(Debug)]
struct CallFrame {
    name: Option<Vec<u8>>,
    chunk: usize,
    start_ip: JumpAddr,
    border_frame: bool,
    frame_pointer: usize,
    caller_addr: usize,
    return_addr: usize,
    return_value_register: u8,
    upvalues: Option<HashMap<usize, Arc<RwLock<UpValue>>>>,
    num_args: u8,
    varargs: Vec<LuaValue>,
    takes_varargs: bool,
    allow_multi_return_values: bool,
}

impl CallFrame {
    const fn default() -> Self {
        Self {
            name: None,
            chunk: 0,
            start_ip: 0,
            border_frame: false,
            frame_pointer: 0,
            caller_addr: 0,
            return_addr: 0,
            return_value_register: 0,
            upvalues: None,
            num_args: 0,
            varargs: vec![],
            takes_varargs: false,
            allow_multi_return_values: true,
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct PoppedCallFrame {
    border_frame: bool,
    return_addr: usize,
    return_value_register: u8,
    allow_multi_return_values: bool,
}

impl<'source> VM<'source> {
    pub fn new(global_env: Arc<RwLock<LuaObject>>, print_bytecode: bool) -> Self {
        Self {
            global_env,
            chunks: vec![],
            chunk_map: HashMap::new(),
            consts: vec![],
            const_index: 0,
            stack: vec![],
            stack_attrs: vec![],
            multres: 0,
            call_stack: [const { CallFrame::default() }; MAX_STACK_SIZE],
            call_stack_index: 0,
            print_bytecode,
        }
    }

    pub fn get_global_env(&self) -> Arc<RwLock<LuaObject>> {
        Arc::clone(&self.global_env)
    }

    pub fn get_next_chunk_index(&self) -> usize {
        self.chunks.len()
    }

    pub fn add_chunk(&mut self, mut chunk: Chunk<'source>) -> usize {
        let index = self.chunks.len();
        chunk.index = index;

        if let Some(filename) = &chunk.filename {
            self.chunk_map.insert(filename.clone(), index);
        }
        self.chunks.push(chunk);

        index
    }

    pub fn get_chunk(&self, index: usize) -> Option<&Chunk<'source>> {
        self.chunks.get(index)
    }

    pub fn get_current_chunk(&self) -> &Chunk<'source> {
        &self.chunks[self.call_stack[self.call_stack_index - 1].chunk]
    }

    pub fn register_const(&mut self, constant: LuaConst) -> ConstIndex {
        let index = self.const_index;
        self.consts.push(constant);
        self.const_index += 1;
        index
    }

    pub fn lookup_const(&self, constant: &LuaConst) -> Option<ConstIndex> {
        // TODO: Maybe a hashmap would be better
        self.consts[..self.const_index as usize]
            .iter()
            .position(|c| c == constant)
            .map(|i| i as ConstIndex)
    }

    pub fn get_stack(&self) -> &[LuaValue] {
        &self.stack
    }

    #[inline]
    fn get(&self, register: impl Into<usize>) -> LuaValue {
        let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
        match &self.stack[frame_pointer + register.into()] {
            LuaValue::UpValue(upvalue) => upvalue.read().unwrap().0.clone(),
            value => value.clone(),
        }
    }

    #[inline]
    fn get_ref(&self, register: impl Into<usize>) -> Cow<'_, LuaValue> {
        let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
        match &self.stack[frame_pointer + register.into()] {
            LuaValue::UpValue(upvalue) => Cow::Owned(upvalue.read().unwrap().0.clone()),
            value => Cow::Borrowed(value),
        }
    }

    #[inline]
    fn set(&mut self, register: impl Into<usize>, value: LuaValue) {
        let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
        self.stack[frame_pointer + register.into()] = value;
    }

    pub fn get_consts(&self) -> &[LuaConst] {
        &self.consts
    }

    #[allow(clippy::too_many_arguments)]
    fn push_call_frame(
        &mut self,
        name: Option<Vec<u8>>,
        chunk: usize,
        start_ip: JumpAddr,
        border_frame: bool,
        frame_pointer: usize,
        max_registers: u8,
        caller_ip: usize,
        return_addr: usize,
        return_value_register: u8,
        upvalues: Option<Vec<Option<Arc<RwLock<UpValue>>>>>,
        num_args: u8,
        takes_varargs: bool,
        allow_multi_return_values: bool,
    ) -> crate::Result {
        if self.call_stack_index >= MAX_STACK_SIZE {
            return Err(self.err("stack overflow"));
        }

        // Reuse the existing object to keep the `upvalues` vec
        self.call_stack[self.call_stack_index].name = name;
        self.call_stack[self.call_stack_index].chunk = chunk;
        self.call_stack[self.call_stack_index].start_ip = start_ip;
        self.call_stack[self.call_stack_index].border_frame = border_frame;
        self.call_stack[self.call_stack_index].frame_pointer = frame_pointer;
        self.call_stack[self.call_stack_index].caller_addr = caller_ip;
        self.call_stack[self.call_stack_index].return_addr = return_addr;
        self.call_stack[self.call_stack_index].return_value_register = return_value_register;
        self.call_stack[self.call_stack_index].allow_multi_return_values =
            allow_multi_return_values;
        self.call_stack[self.call_stack_index].num_args = num_args;
        self.call_stack[self.call_stack_index].takes_varargs = takes_varargs;
        if self.call_stack[self.call_stack_index].upvalues.is_none() {
            self.call_stack[self.call_stack_index].upvalues = Some(HashMap::new());
        }

        if let Some(upvalues) = upvalues {
            // TODO: See if we can be more efficient here
            for (i, upvalue) in upvalues.into_iter().enumerate() {
                if let Some(upvalue) = upvalue {
                    self.call_stack[self.call_stack_index]
                        .upvalues
                        .as_mut()
                        .unwrap()
                        .insert(i, upvalue);
                }
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
                .resize(frame_pointer + max_registers, LuaValue::Nil);
            // NOTE: `stack` and `stack_attrs` are always the same size
            self.stack_attrs.resize(frame_pointer + max_registers, 0);
        }

        // FIXME: If the stack is already big enough, we might need to
        // clear out old values. For example, if we have a function that
        // takes 1 argument, but there are 5 values on the stack from
        // the parent call frame, we need to fill anything beyond the
        // first register with `Nil`.
    }

    fn pop_call_frame(&mut self) -> PoppedCallFrame {
        self.call_stack_index -= 1;

        let frame = &mut self.call_stack[self.call_stack_index];
        let popped_frame = PoppedCallFrame {
            border_frame: frame.border_frame,
            return_addr: frame.return_addr,
            return_value_register: frame.return_value_register,
            allow_multi_return_values: frame.allow_multi_return_values,
        };

        frame.upvalues.as_mut().unwrap().clear();
        frame.varargs.clear();

        popped_frame
    }

    fn capture_upvalue(&mut self, register: u8) -> Arc<RwLock<UpValue>> {
        let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
        if let LuaValue::UpValue(upvalue) = &self.stack[frame_pointer + register as usize] {
            return Arc::clone(upvalue);
        }

        let mut value = LuaValue::Nil;
        std::mem::swap(
            &mut value,
            &mut self.stack[frame_pointer + register as usize],
        );
        let upvalue = Arc::new(RwLock::new(UpValue(value)));
        self.set(register, LuaValue::UpValue(Arc::clone(&upvalue)));

        upvalue
    }

    pub fn run(&mut self) {
        assert!(!self.chunks.is_empty(), "no chunks to run");

        if let Err(mut err) = self.run_chunk(0) {
            // Attach stack trace
            for (i, frame) in self.call_stack[1..self.call_stack_index]
                .iter()
                .rev()
                .enumerate()
            {
                let mut frame_err = LuaError::new(format!(
                    "#{i} {}",
                    String::from_utf8_lossy(frame.name.as_deref().unwrap_or(b"<anonymous>")),
                ));

                // The initial error should already have a span attached, beyond
                // that we want to ensure that we attach labels for every frame
                // of the call stack.
                // Note that on the frames:
                // - We have the name, as used above.
                // - We have the `caller_addr`, which refers to from where that frame was invoked.
                //   Combine that with the `chunk` on the parent frame, to find the actual
                //   location.
                // - We skip the top item in the call stack (see above), as there's no source for
                //   that "call".
                if i < self.call_stack_index - 1 {
                    let parent_frame = &self.call_stack[i];
                    let chunk = &self.chunks[parent_frame.chunk];
                    let source = chunk.named_source();
                    let span = chunk.instruction_spans.get(&frame.caller_addr);
                    if let Some(span) = span {
                        frame_err = frame_err.with_source_code(source).with_labels(*span);
                    }
                }

                eprintln!("{frame_err:?}");
            }

            // Attach source code to the original error if possible
            if err.source_code.is_none()
                && err.labels.as_ref().is_some_and(|v| !v.is_empty())
                && let Some(chunk) = err.chunk
            {
                err = err.with_source_code(self.chunks[chunk].named_source());
            }

            eprintln!("{err:?}");
            std::process::exit(1);
        }
    }

    pub fn run_chunk(&mut self, chunk_index: usize) -> crate::Result<Vec<LuaValue>> {
        if self.print_bytecode {
            debug::print_instructions(self, &self.chunks[chunk_index]);
        }

        self.push_call_frame(
            Some(self.chunks[chunk_index].chunk_name.as_bytes().to_vec()),
            chunk_index,
            0,
            true,
            self.stack.len(),
            self.chunks[chunk_index].max_registers,
            0,
            0,
            0,
            None,
            0,
            false,
            false,
        )?;

        // NOTE: Don't pop the call frame here. Chunks always end in a `return`
        // statement (implicit or explicit), which will pop the call frame.
        self.run_inner()
    }

    pub(crate) fn run_closure(
        &mut self,
        value: LuaClosure,
        args: Vec<LuaValue>,
    ) -> crate::Result<Vec<LuaValue>> {
        let call_frame_offset = self.call_stack_index;
        let frame_pointer = self.stack.len();

        let old_ip = self.chunks[value.chunk].ip;
        self.chunks[value.chunk].ip = value.ip as usize;
        self.push_call_frame(
            value.name.clone(),
            value.chunk,
            value.ip,
            true,
            frame_pointer,
            value.max_registers,
            old_ip,
            // NOTE: `return_addr` and `return_value_register` doesn't matter for a border frame
            0,
            0,
            Some(value.upvalues),
            value.num_params,
            value.has_varargs,
            false,
        )?;

        let num_args = args.len();
        for (index, arg) in args.into_iter().enumerate() {
            self.set(index, arg);
        }

        self.align_closure_args(value.num_params, num_args, value.has_varargs);

        let result = self.run_inner();
        if result.is_err() {
            // In this case we wouldn't hit a `return` instruction, so we need to clean up the
            // call stack manually
            for _ in 0..self.call_stack_index - call_frame_offset {
                self.pop_call_frame();
            }
        }

        self.chunks[value.chunk].ip = old_ip;
        result
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
        self.stack[frame_pointer + from_register.into()..].fill(LuaValue::Nil);
    }

    fn clear_registers_between(&mut self, from_register: u8, to_register: u8) {
        let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
        self.stack[frame_pointer + from_register as usize..=frame_pointer + to_register as usize]
            .fill(LuaValue::Nil);
    }

    fn run_native_function(
        &mut self,
        name: &str,
        function: fn(&mut VM, Vec<LuaValue>) -> crate::Result<Vec<LuaValue>>,
        args: Vec<LuaValue>,
    ) -> crate::Result<Vec<LuaValue>> {
        // NOTE: Most values here are not relevant for native functions.
        let current_chunk = self.get_current_chunk();
        self.push_call_frame(
            Some(format!("native function '{}'", name).as_bytes().to_vec()),
            current_chunk.index,
            0, // Not relevant
            false,
            self.stack.len(), // Not relevant
            0,                // Not relevant
            current_chunk.ip,
            current_chunk.ip,
            0, // Not relevant
            None,
            0,     // Not relevant
            false, // Not relevant
            false, // Not relevant
        )?;

        let result = function(self, args).map_err(|mut err| {
            let current_chunk = self.get_current_chunk();
            if let Some(span) = current_chunk.instruction_spans.get(&current_chunk.ip) {
                err = err.with_labels(*span);
            }
            err.with_source_code(current_chunk.named_source())
        });

        self.pop_call_frame();

        result
    }

    fn run_inner(&mut self) -> crate::Result<Vec<LuaValue>> {
        'main: loop {
            let chunk_index = self.call_stack[self.call_stack_index - 1].chunk;
            let instruction = self.chunks[chunk_index].instructions[self.chunks[chunk_index].ip];

            macro_rules! instr_param {
                ($offset:expr) => {
                    self.chunks[chunk_index].instructions[self.chunks[chunk_index].ip + $offset]
                };
                () => {
                    instr_param!(1)
                };
            }
            macro_rules! register {
                ($offset:expr) => {
                    self.chunks[chunk_index].instructions[self.chunks[chunk_index].ip + $offset]
                };
                () => {
                    register!(1)
                };
            }
            macro_rules! const_index {
                () => {{
                    let bytes = &self.chunks[chunk_index].instructions[self.chunks[chunk_index].ip
                        + 2
                        ..self.chunks[chunk_index].ip + 2 + size_of::<ConstIndex>()];
                    ConstIndex::from_be_bytes(bytes.try_into().unwrap())
                }};
            }
            macro_rules! jump_addr {
                ($offset:expr) => {{
                    let bytes = &self.chunks[chunk_index].instructions[self.chunks[chunk_index].ip
                        + $offset
                        ..self.chunks[chunk_index].ip + $offset + size_of::<JumpAddr>()];
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
                    let result = match (&*a, &*b) {
                        (LuaValue::Number(a), LuaValue::Number(b)) => {
                            binary_op_num!(_internal a, b, $($param),*)
                        }.into(),
                        _ => {
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
                        },
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
                    $value?.as_boolean().into()
                }};
                (_internal_metamethod_result $value:ident $($_:tt),*) => {{
                    $value?
                }};
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
                    let result = match &*value {
                        LuaValue::Number(num) => {
                            unary_op_num!(_internal num, $($param),*)
                        }.into(),
                        _ => {
                            let value = value.into_owned();
                            match metatables::handle(
                                self,
                                &metatables::$metamethod,
                                vec![value],
                            ) {
                                Some(result) => result?,
                                None => {
                                    return Err(self.op_err($op_name, self.get_ref(value_r).as_ref(), None));

                                }
                            }
                        }
                    };
                    self.set(dest_r, result);
                    3
                }};
                (_internal $value:ident, method, $name:ident) => {{
                    $value.$name()
                }};
                (_internal $value:ident, op, $tt:tt) => {{
                    ($tt $value)
                }};
                (_internal $value:ident, fallible_op, $tt:tt) => {{
                    ($tt $value)?
                }};
            }

            let instruction_increment = match Instruction::from(instruction) {
                // Stack manipulation
                Instruction::LoadConst => {
                    let register = register!();
                    let const_index = const_index!();
                    let constant = self.consts[const_index as usize].clone();
                    self.set(register, constant.into());
                    2 + size_of::<ConstIndex>()
                }
                Instruction::LoadClosure => {
                    let register = register!();
                    let const_index = const_index!();
                    let function_definition = self.consts[const_index as usize].clone();
                    let mut closure: LuaValue = function_definition.into();
                    self.set(register, closure.clone());
                    let upval_bytes = assert_closure!(write, &mut closure, closure, {
                        let upvalues = closure.upvalues.capacity();
                        for i in 0..upvalues {
                            let upval_ip =
                                self.chunks[chunk_index].ip + size_of::<ConstIndex>() + 2 + i * 2;
                            let is_local = self.chunks[chunk_index].instructions[upval_ip] == 1;
                            let register = self.chunks[chunk_index].instructions[upval_ip + 1];
                            if is_local {
                                closure.upvalues[i] = Some(self.capture_upvalue(register));
                            } else {
                                closure.upvalues[i] = Some(
                                    self.call_stack[self.call_stack_index - 1]
                                        .upvalues
                                        .as_ref()
                                        .unwrap()
                                        .get(&(register as usize))
                                        .cloned()
                                        .unwrap_or_else(|| panic!("upvalue {register} not found")),
                                );
                            }
                        }

                        upvalues * 2
                    });

                    2 + size_of::<ConstIndex>() + upval_bytes
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
                            result?.as_boolean()
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
                            result?.as_boolean()
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
                                    self.set(dest_r, value?);
                                }
                                None => {
                                    return Err(e);
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

                    let result: LuaValue = match &*value {
                        LuaValue::String(s) => (s.len() as i64).into(),
                        _ => {
                            let value = value.into_owned();
                            match metatables::handle(self, &metatables::LEN_KEY, vec![value]) {
                                Some(result) => result?,
                                None => {
                                    // If the value is a table (but it didn't have a __len
                                    // metamethod), return the length of the array part
                                    let value = self.get_ref(val_r);
                                    match &*value {
                                        LuaValue::Object(obj) => match &*obj.read().unwrap() {
                                            LuaObject::Table(table) => (table.len() as i64).into(),
                                            _ => {
                                                return Err(self.op_err("length", &value, None));
                                            }
                                        },
                                        _ => {
                                            return Err(self.op_err("length", &value, None));
                                        }
                                    }
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
                Instruction::SetGlobal => {
                    let register = register!();
                    let global_index = const_index!();
                    let value = self.get(register);
                    let key = self.consts[global_index as usize].clone().into();
                    assert_table_object!(write, self.chunks[chunk_index].env, env, {
                        env.insert(key, value);
                    });
                    2 + size_of::<ConstIndex>()
                }
                Instruction::GetGlobal => {
                    let register = register!();
                    let global_index = const_index!();
                    let key = self.consts[global_index as usize].clone();
                    let value = match &key {
                        LuaConst::String(s) if s == b"_G" => {
                            LuaValue::Object(Arc::clone(&self.global_env))
                        }
                        LuaConst::String(s) if s == b"_ENV" => {
                            LuaValue::Object(Arc::clone(&self.chunks[chunk_index].env))
                        }
                        _ => {
                            assert_table_object!(read, self.chunks[chunk_index].env, env, {
                                env.get(&key.into()).cloned().unwrap_or(LuaValue::Nil)
                            })
                        }
                    };
                    self.set(register, value);
                    2 + size_of::<ConstIndex>()
                }
                Instruction::Mov => {
                    let dest = register!(1);
                    let src = register!(2);

                    let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
                    let value = self.get(src);
                    if let LuaValue::UpValue(upval) = &self.stack[frame_pointer + dest as usize] {
                        let mut upval = upval.write().unwrap();
                        *upval = value.into();
                    } else {
                        self.set(dest, value);
                    }

                    3
                }
                Instruction::SetLocalAttr => {
                    let local_index = instr_param!(1);
                    let attr_value = instr_param!(2);

                    self.stack_attrs[self.call_stack[self.call_stack_index - 1].frame_pointer
                        + local_index as usize] |= attr_value;

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
                    let value = self.call_stack[self.call_stack_index - 1]
                        .upvalues
                        .as_ref()
                        .unwrap()
                        .get(&(upval_index as usize))
                        .unwrap_or_else(|| panic!("upvalue {upval_index} not found"))
                        .read()
                        .unwrap()
                        .clone();
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
                            .unwrap_or(LuaValue::Nil);
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

                // Table
                Instruction::NewTable => {
                    let register = register!();
                    let table = LuaObject::Table(Default::default());
                    self.set(register, table.into());
                    2
                }
                Instruction::SetTable => {
                    let table_r = register!(1);
                    let key_r = register!(2);
                    let value_r = register!(3);

                    let table = self.get(table_r);
                    let value = self.get(value_r);
                    let key = self.get(key_r);

                    let mut target = table.clone();
                    let mut handled = false;
                    loop {
                        let mut is_table_with_missing_key = false;
                        if let LuaValue::Object(o) = &target
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
                                        // Run through `run_closure` rather than jumping to it
                                        // directly, so return values are cleaned up properly
                                        self.run_closure(closure, vec![table, key, value])?;
                                        handled = true;
                                        break;
                                    }
                                    Method::NativeFunction(name, f) => {
                                        let args = vec![table, key, value];
                                        self.run_native_function(&name, f, args)?;
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
                        return Err(self.err("attempt to index a non-table"));
                    }

                    4
                }
                Instruction::GetTable => {
                    let dest_r = register!(1);
                    let table_r = register!(2);
                    let key_r = register!(3);

                    let table = self.get(table_r);
                    let key = self.get(key_r);

                    let mut handled = false;
                    let mut valid_object = false;

                    let mut target = table.clone();
                    loop {
                        if let LuaValue::Object(o) = &target
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
                                        let parent_frame_pointer = self.call_stack
                                            [self.call_stack_index - 1]
                                            .frame_pointer;
                                        self.push_call_frame(
                                            closure.name,
                                            closure.chunk,
                                            closure.ip,
                                            false,
                                            parent_frame_pointer + table_r as usize,
                                            closure.max_registers,
                                            self.chunks[chunk_index].ip,
                                            self.chunks[chunk_index].ip + 4,
                                            dest_r,
                                            Some(closure.upvalues),
                                            closure.num_params,
                                            closure.has_varargs,
                                            false,
                                        )?;
                                        self.chunks[closure.chunk].ip = closure.ip as usize;
                                        self.align_closure_args(
                                            closure.num_params,
                                            1usize,
                                            closure.has_varargs,
                                        );
                                        continue 'main;
                                    }
                                    Method::NativeFunction(name, f) => {
                                        let args = vec![table, key];
                                        let result = self.run_native_function(&name, f, args)?;
                                        if let Some(return_value) = result.into_iter().next() {
                                            self.set(dest_r, return_value);
                                        } else {
                                            self.set(dest_r, LuaValue::Nil);
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
                            self.set(dest_r, LuaValue::Nil);
                        } else {
                            return Err(self.err("attempt to index a non-table"));
                        }
                    }

                    4
                }
                instr @ Instruction::AppendToTable | instr @ Instruction::AppendToTableM => {
                    // Append each value to the table at the stack right before the values
                    let table_r = register!();
                    let mut num_values = instr_param!(2) as usize;
                    if matches!(instr, Instruction::AppendToTableM) {
                        num_values += self.multres;
                    }
                    let table = self.get(table_r);

                    match table {
                        LuaValue::Object(o) => match &mut *o.write().unwrap() {
                            LuaObject::Table(t) => {
                                for i in 0..num_values {
                                    let value = self.get(table_r as usize + 1 + i);
                                    let index = (i + 1) as i64;
                                    t.insert(index.into(), value);
                                }
                            }
                            _ => {
                                return Err(self.err("attempt to index a non-table"));
                            }
                        },
                        _ => {
                            return Err(self.err("attempt to index a non-table"));
                        }
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
                            self.push_call_frame(
                                closure.name.clone(),
                                closure.chunk,
                                closure.ip,
                                false,
                                parent_frame_pointer + params_r as usize,
                                closure.max_registers,
                                self.chunks[chunk_index].ip,
                                self.chunks[chunk_index].ip + 4,
                                func_r,
                                Some(closure.upvalues),
                                closure.num_params,
                                closure.has_varargs,
                                !is_single_return,
                            )?;
                            self.chunks[closure.chunk].ip = closure.ip as usize;
                            self.align_closure_args(
                                closure.num_params,
                                num_args,
                                closure.has_varargs,
                            );

                            continue;
                        }
                        Some(Method::NativeFunction(name, f)) => {
                            let mut args = Vec::with_capacity(num_args);
                            for i in 0..num_args {
                                args.push(self.get(params_r as usize + i));
                            }

                            let result = self.run_native_function(&name, f, args)?;
                            let value_slots_needed = match (is_single_return, result.len()) {
                                (true, 0) => 1,
                                (true, n) => n.min(1),
                                (false, n) => n,
                            };
                            self.ensure_stack_space(func_r as usize + value_slots_needed);

                            if result.is_empty() && is_single_return {
                                self.set(func_r, LuaValue::Nil);
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
                            return Err(self.err("attempt to call a non-function"));
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
                        start_ip,
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
                    self.chunks[chunk_index].ip = start_ip as usize;
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
                        return_values.push(LuaValue::Nil);
                    }

                    self.multres = if frame.allow_multi_return_values {
                        num_return_values
                    } else {
                        1
                    };

                    // Check if this is the root frame, or if we're leaving this chunk
                    if frame.border_frame {
                        return Ok(return_values);
                    }

                    let parent_call_frame = &self.call_stack[self.call_stack_index - 1];
                    self.chunks[parent_call_frame.chunk].ip = frame.return_addr;

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
                        return Err(self.err("attempt to jump to an invalid address"));
                    }
                    self.chunks[chunk_index].ip = addr as usize;
                    continue;
                }
                Instruction::JmpTrue => {
                    let register = register!();
                    let value = self.get_ref(register);
                    if value.as_boolean() {
                        let addr = jump_addr!(2);
                        self.chunks[chunk_index].ip = addr as usize;
                        continue;
                    }
                    2 + size_of::<JumpAddr>()
                }
                Instruction::JmpFalse => {
                    let register = register!();
                    let value = self.get_ref(register);
                    if !value.as_boolean() {
                        let addr = jump_addr!(2);
                        self.chunks[chunk_index].ip = addr as usize;
                        continue;
                    }
                    2 + size_of::<JumpAddr>()
                }
                Instruction::JmpClose => {
                    let close_register = register!();
                    let addr = jump_addr!(2);
                    if addr == JumpAddr::MAX {
                        // TODO: We probably want a nicer error here. Was this a break
                        // outside of a loop? Was it a goto statement with no matching
                        // label?
                        return Err(self.err("attempt to jump to an invalid address"));
                    }

                    let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
                    let to_be_closed = LuaVariableAttribute::ToBeClosed as u8;
                    let attrs =
                        Vec::from(&self.stack_attrs[frame_pointer + close_register as usize..]);
                    for attr in attrs {
                        if attr & to_be_closed == 1 {
                            let value = self.get(close_register);
                            if let Some(__close) = value.get_metavalue(&CLOSE_KEY) {
                                let close: Callable = (&__close).try_into().map_err(|_| {
                                    self.err("__close metamethod must be a callable value")
                                })?;

                                // TODO: Second value refers to "the error object that caused the exit
                                // (if any)", but we don't have that yet. Sound like we'll need to
                                // unwind the stack for that.
                                let args = if close.is_metamethod {
                                    vec![__close, value, LuaValue::Nil]
                                } else {
                                    vec![value, LuaValue::Nil]
                                };
                                match close.method {
                                    Method::Closure(closure) => {
                                        self.run_closure(closure, args)?;
                                    }
                                    Method::NativeFunction(name, func) => {
                                        self.run_native_function(&name, func, args)?;
                                    }
                                }
                            } else {
                                return Err(self
                                    .err("variable marked for closing has a non-closeable value"));
                            }
                        } else {
                            break;
                        }
                    }

                    self.clear_registers_from(close_register);
                    self.chunks[chunk_index].ip = addr as usize;
                    continue;
                }

                // Other
                Instruction::Error => {
                    let error_type = instr_param!();
                    return Err(RuntimeError::try_from(error_type)?).into_lua_error();
                }
            };
            self.chunks[chunk_index].ip += instruction_increment;
        }
    }

    fn op_err(&self, op_name: &str, a: &LuaValue, b: Option<&LuaValue>) -> LuaError {
        let mut message = format!("cannot {} a '{}'", op_name, a.type_name());
        if let Some(b) = b {
            message.push_str(&format!(" and a '{}'", b.type_name()));
        }

        self.err(message)
    }

    pub(crate) fn err<T: std::fmt::Display>(&self, message: T) -> LuaError {
        let frame = &self.call_stack[self.call_stack_index - 1];
        let chunk = &self.chunks[frame.chunk];

        let mut error = LuaError::new(message.to_string());
        error.chunk = Some(frame.chunk);
        if let Some(span) = chunk.instruction_spans.get(&chunk.ip) {
            error.with_labels(*span)
        } else {
            error
        }
    }
}
