use std::{
    borrow::Cow,
    collections::HashMap,
    mem::size_of,
    path::{Path, PathBuf},
    sync::{Arc, RwLock},
};

use miette::{miette, IntoDiagnostic, NamedSource};

use crate::{
    error::RuntimeError,
    instruction::Instruction,
    macros::{assert_closure, assert_table},
    stdlib,
    token::Span,
    value::{
        metatables::{CALL_KEY, CLOSE_KEY},
        LuaClosure, LuaConst, LuaNumber, LuaObject, LuaTable, LuaValue, LuaVariableAttribute,
    },
};

const MAX_STACK_SIZE: usize = 256;

pub type JumpAddr = u16;
pub type ConstIndex = u16;

#[derive(Debug, Clone)]
pub struct Chunk<'source> {
    index: usize,
    filename: Option<PathBuf>,
    chunk_name: String,
    source: Cow<'source, str>,
    instructions: Vec<u8>,
    instruction_spans: HashMap<usize, Span>,
    ip: usize,
}

impl<'source> Chunk<'source> {
    pub fn new(filename: Option<PathBuf>, chunk_name: String, source: Cow<'source, str>) -> Self {
        Self {
            // NOTE: This is set by the VM when the chunk is added
            index: 0,
            filename,
            chunk_name,
            source,
            instructions: vec![],
            instruction_spans: HashMap::new(),
            ip: 0,
        }
    }

    pub fn get_filename(&self) -> Option<&Path> {
        self.filename.as_deref()
    }

    pub fn get_source(&self) -> &str {
        &self.source
    }
}

#[derive(Debug)]
pub struct VM<'source> {
    chunks: Vec<Chunk<'source>>,
    chunk_map: HashMap<PathBuf, usize>,
    consts: Vec<LuaConst>,
    const_index: ConstIndex,
    stack: [LuaValue; MAX_STACK_SIZE],
    stack_attrs: [u8; MAX_STACK_SIZE],
    stack_index: usize,
    globals: HashMap<ConstIndex, LuaValue>,
    call_stack: [CallFrame; MAX_STACK_SIZE],
    call_stack_index: usize,
}

#[derive(Debug)]
struct CallFrame {
    name: Option<String>,
    chunk: usize,
    border_frame: bool,
    frame_pointer: usize,
    return_addr: usize,
    upvalues: [Option<Arc<RwLock<LuaValue>>>; u8::MAX as usize],
    allow_multi_return_values: bool,
}

impl CallFrame {
    const fn default() -> Self {
        Self {
            name: None,
            chunk: 0,
            border_frame: false,
            frame_pointer: 0,
            return_addr: 0,
            upvalues: [const { None }; u8::MAX as usize],
            allow_multi_return_values: true,
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct PoppedCallFrame {
    border_frame: bool,
    frame_pointer: usize,
    return_addr: usize,
}

impl<'source> VM<'source> {
    pub fn new() -> Self {
        Self {
            chunks: vec![],
            chunk_map: HashMap::new(),
            consts: vec![],
            const_index: 0,
            stack: [const { LuaValue::Nil }; MAX_STACK_SIZE],
            stack_attrs: [0; MAX_STACK_SIZE],
            stack_index: 0,
            globals: HashMap::new(),
            call_stack: [const { CallFrame::default() }; MAX_STACK_SIZE],
            call_stack_index: 0,
        }
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

    pub fn get_chunk(&self, index: usize) -> Option<&Chunk> {
        self.chunks.get(index)
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

    fn push(&mut self, value: LuaValue) {
        let index = self.stack_index;
        self.stack[index] = value;
        self.stack_index += 1;
    }

    fn pop(&mut self) -> LuaValue {
        self.stack_index -= 1;
        self.stack_attrs[self.stack_index] = 0;
        std::mem::replace(&mut self.stack[self.stack_index], LuaValue::Nil)
    }

    fn peek(&self) -> &LuaValue {
        &self.stack[self.stack_index - 1]
    }

    fn shift_left(&mut self, amount: usize, offset: usize) {
        if amount == 0 || offset == 0 {
            return;
        }

        // TODO: Find a more efficient way to do this
        let start = self.stack_index - amount;
        for index in start..self.stack_index {
            self.stack.swap(index, index - offset);
        }
    }

    pub fn get_consts(&self) -> &[LuaConst] {
        &self.consts
    }

    fn push_call_frame(
        &mut self,
        name: Option<String>,
        chunk: usize,
        border_frame: bool,
        frame_pointer: usize,
        return_addr: usize,
        upvalues: Option<Vec<Option<Arc<RwLock<LuaValue>>>>>,
        allow_multi_return_values: bool,
    ) {
        // Reuse the existing object to keep the `upvalues` vec
        self.call_stack[self.call_stack_index].name = name;
        self.call_stack[self.call_stack_index].chunk = chunk;
        self.call_stack[self.call_stack_index].border_frame = border_frame;
        self.call_stack[self.call_stack_index].frame_pointer = frame_pointer;
        self.call_stack[self.call_stack_index].return_addr = return_addr;
        self.call_stack[self.call_stack_index].allow_multi_return_values =
            allow_multi_return_values;

        if let Some(upvalues) = upvalues {
            // TODO: See if we can be more efficient here
            for (i, upvalue) in upvalues.into_iter().enumerate() {
                self.call_stack[self.call_stack_index].upvalues[i] = upvalue;
            }
        }

        self.call_stack_index += 1;
    }

    fn pop_call_frame(&mut self) -> PoppedCallFrame {
        self.call_stack_index -= 1;

        let frame = &mut self.call_stack[self.call_stack_index];
        let popped_frame = PoppedCallFrame {
            border_frame: frame.border_frame,
            frame_pointer: frame.frame_pointer,
            return_addr: frame.return_addr,
        };

        frame.upvalues.fill(None);

        popped_frame
    }

    fn capture_upvalue(&mut self, index: usize) -> Arc<RwLock<LuaValue>> {
        let frame = &self.call_stack[self.call_stack_index - 1];
        let value = self.stack[frame.frame_pointer + index].clone();
        let upvalue = Arc::new(RwLock::new(value));
        self.stack[frame.frame_pointer + index] = LuaValue::UpValue(Arc::clone(&upvalue));

        upvalue
    }

    pub fn run(&mut self) {
        assert!(!self.chunks.is_empty(), "no chunks to run");

        if let Err(err) = self.run_chunk(0) {
            let labels =
                if let Some(span) = self.chunks[0].instruction_spans.get(&self.chunks[0].ip) {
                    vec![span.labeled("here")]
                } else {
                    vec![]
                };

            // FIXME: This is a workaround for the fact that miette doesn't support
            // adding labels ad-hoc, but it ends up printing the error message
            // kind of weirdly.
            let mut err = miette!(labels = labels, "{err:?}");
            // Attach stack trace
            for (i, frame) in self.call_stack[..self.call_stack_index]
                .iter()
                .enumerate()
                .rev()
            {
                err = err.wrap_err(format!(
                    "#{i} {}",
                    frame.name.as_deref().unwrap_or("<anonymous>"),
                ));
            }

            let Chunk {
                filename,
                chunk_name,
                source,
                ..
            } = &self.chunks[0];
            if let Some(filename) = filename {
                err = err.with_source_code(
                    NamedSource::new(filename.to_string_lossy(), source.to_string())
                        .with_language("lua"),
                );
            } else {
                err = err.with_source_code(
                    NamedSource::new(chunk_name, source.to_string()).with_language("lua"),
                );
            }

            eprintln!("{:?}", err);
            std::process::exit(1);
        } else {
            assert_eq!(self.stack_index, 0, "stack is not empty");
        }
    }

    pub(crate) fn run_chunk(
        &mut self,
        initial_chunk_index: usize,
    ) -> miette::Result<Vec<LuaValue>> {
        self.push_call_frame(
            Some(self.chunks[initial_chunk_index].chunk_name.clone()),
            initial_chunk_index,
            true,
            0,
            0,
            None,
            false,
        );

        self.run_inner()
    }

    pub(crate) fn run_closure(
        &mut self,
        value: LuaClosure,
        args: Vec<LuaValue>,
    ) -> miette::Result<Vec<LuaValue>> {
        let frame_pointer = self.stack_index;
        for arg in args {
            self.push(arg);
        }

        self.push_call_frame(
            value.name.clone(),
            value.chunk,
            true,
            frame_pointer,
            // NOTE: `return_addr` doesn't matter for a border frame
            0,
            Some(value.upvalues),
            false,
        );

        self.run_inner()
    }

    fn run_inner(&mut self) -> miette::Result<Vec<LuaValue>> {
        loop {
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
            macro_rules! const_index {
                () => {{
                    let bytes = &self.chunks[chunk_index].instructions[self.chunks[chunk_index].ip
                        + 1
                        ..self.chunks[chunk_index].ip + 1 + size_of::<ConstIndex>()];
                    ConstIndex::from_be_bytes(bytes.try_into().unwrap())
                }};
            }
            macro_rules! jump_addr {
                () => {{
                    let bytes = &self.chunks[chunk_index].instructions[self.chunks[chunk_index].ip
                        + 1
                        ..self.chunks[chunk_index].ip + 1 + size_of::<JumpAddr>()];
                    JumpAddr::from_be_bytes(bytes.try_into().unwrap())
                }};
            }

            let instruction_increment = match Instruction::from(instruction) {
                // Stack manipulation
                Instruction::LoadConst => {
                    let const_index = const_index!();
                    let constant = self.consts[const_index as usize].clone();
                    self.push(constant.into());
                    1 + size_of::<ConstIndex>()
                }
                Instruction::LoadClosure => {
                    let const_index = const_index!();
                    let function_definition = self.consts[const_index as usize].clone();
                    let mut closure = function_definition.into();
                    let upval_bytes = assert_closure!(write, &mut closure, closure, {
                        let upvalues = closure.upvalues.capacity();
                        for i in 0..upvalues {
                            let upval_ip =
                                self.chunks[chunk_index].ip + size_of::<ConstIndex>() + 1 + i * 2;
                            let is_local = self.chunks[chunk_index].instructions[upval_ip] == 1;
                            let index = self.chunks[chunk_index].instructions[upval_ip + 1];
                            if is_local {
                                closure.upvalues[index as usize] =
                                    Some(self.capture_upvalue(index as usize));
                            } else {
                                closure.upvalues[index as usize] = Some(
                                    self.call_stack[self.call_stack_index - 1].upvalues
                                        [index as usize]
                                        .as_ref()
                                        .expect("upvalue not found")
                                        .clone(),
                                );
                            }
                        }

                        upvalues * size_of::<ConstIndex>()
                    });

                    self.push(closure);
                    1 + size_of::<ConstIndex>() + upval_bytes
                }
                Instruction::Pop => {
                    let attr = self.stack_attrs[self.stack_index - 1];
                    let value = self.pop();
                    // If this is a closing value, and it's not nil or false, close it
                    let to_be_closed = LuaVariableAttribute::ToBeClosed as u8;
                    if attr & to_be_closed == to_be_closed && value.as_boolean() {
                        if let Some(__close) = value.get_metavalue(&CLOSE_KEY) {
                            let close = assert_closure!(read, __close, close, close.clone());
                            // TODO: Second value refers to "the error object that caused the exit
                            // (if any)", but we don't have that yet. Sound like we'll need to
                            // unwind the stack for that.
                            let args = vec![value, LuaValue::Nil];
                            self.run_closure(close, args)?;
                        } else {
                            return Err(miette!(
                                "variable marked for closing has a non-closeable value"
                            ));
                        }
                    }
                    1
                }
                Instruction::Discard => {
                    loop {
                        if self.pop() == LuaValue::Marker {
                            break;
                        }
                    }
                    1
                }
                Instruction::Swap => {
                    let swap_offset = instr_param!();
                    let a = self.stack_index - 1;
                    let b = self.stack_index - 1 - swap_offset as usize;
                    self.stack.swap(a, b);
                    2
                }
                instr @ Instruction::Align | instr @ Instruction::AlignVararg => {
                    let collect_varargs = matches!(instr, Instruction::AlignVararg);
                    let align_amount = instr_param!();

                    assert!(self.stack_index > 0, "cannot align an empty stack");

                    // Find either the latest marker or the start of the stack
                    // from the current call frame
                    let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
                    let marker_index = (frame_pointer..self.stack_index)
                        .rev()
                        .find(|&i| self.stack[i] == LuaValue::Marker);

                    // Align the stack, so that there are <align_amount> values
                    // between the marker and the top of the stack
                    let num_values = marker_index
                        .map(|i| self.stack_index - i - 1)
                        .unwrap_or(self.stack_index - frame_pointer);

                    // Get rid of the marker
                    if marker_index.is_some() {
                        self.shift_left(num_values, 1);
                        self.pop();
                    }

                    if num_values < align_amount as usize {
                        for _ in 0..align_amount as usize - num_values {
                            self.push(LuaValue::Nil);
                        }
                    } else if collect_varargs {
                        let num_varargs = num_values - align_amount as usize;
                        if num_varargs > 0 {
                            // Collect the varargs into a table
                            let mut table = LuaTable::new();
                            // Descending order!
                            let mut index = num_varargs;
                            for _ in 0..num_varargs {
                                let value = self.pop();
                                table.insert(
                                    LuaValue::Number(LuaNumber::Integer(index as i64)),
                                    value,
                                );
                                index -= 1;
                            }
                            table.mark_sequence_dangerous(num_varargs as i64);
                            self.push(LuaObject::Table(table).into());
                        } else {
                            self.push(LuaValue::Nil);
                        }
                    } else {
                        for _ in 0..num_values - align_amount as usize {
                            self.pop();
                        }
                    }

                    2
                }
                Instruction::DupFromMarker => {
                    let offset = instr_param!();
                    assert!(offset > 0, "dup_from_marker offset must be greater than 0");

                    let marker_index = self.stack[..self.stack_index]
                        .iter()
                        .rposition(|v| v == &LuaValue::Marker)
                        .expect("no marker found");
                    let value = self.stack[marker_index + offset as usize].clone();
                    self.push(value);
                    2
                }

                // Binary operations
                // Arithmetic
                Instruction::Add => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a + b)?);
                    1
                }
                Instruction::Sub => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a - b)?);
                    1
                }
                Instruction::Mul => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a * b)?);
                    1
                }
                Instruction::Div => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a / b)?);
                    1
                }
                Instruction::Mod => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a % b)?);
                    1
                }
                Instruction::Pow => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a.pow(b)?);
                    1
                }
                Instruction::IDiv => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a.idiv(b)?);
                    1
                }
                Instruction::Band => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a & b)?);
                    1
                }
                Instruction::Bor => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a | b)?);
                    1
                }
                Instruction::Bxor => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a ^ b)?);
                    1
                }
                Instruction::Shl => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a << b)?);
                    1
                }
                Instruction::Shr => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a >> b)?);
                    1
                }

                // Logical
                Instruction::And => {
                    let b = self.pop();
                    let a = self.pop();
                    if !a.as_boolean() {
                        self.push(a);
                    } else {
                        self.push(b);
                    }
                    1
                }
                Instruction::Or => {
                    let b = self.pop();
                    let a = self.pop();
                    if a.as_boolean() {
                        self.push(a);
                    } else {
                        self.push(b);
                    }
                    1
                }

                // Comparison
                Instruction::Eq => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(LuaValue::Boolean(a == b));
                    1
                }
                Instruction::Ne => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(LuaValue::Boolean(a != b));
                    1
                }
                Instruction::Lt => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(LuaValue::Boolean(a < b));
                    1
                }
                Instruction::Le => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(LuaValue::Boolean(a <= b));
                    1
                }
                Instruction::Gt => {
                    let b = self.pop();
                    let a = self.pop();
                    // https://www.lua.org/manual/5.4/manual.html#3.4.4
                    // A comparison a > b is translated to b < a and a >= b is translated to b <= a.
                    self.push(LuaValue::Boolean(b < a));
                    1
                }
                Instruction::Ge => {
                    let b = self.pop();
                    let a = self.pop();
                    // https://www.lua.org/manual/5.4/manual.html#3.4.4
                    // A comparison a > b is translated to b < a and a >= b is translated to b <= a.
                    self.push(LuaValue::Boolean(b <= a));
                    1
                }

                // Concatenation
                Instruction::Concat => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a.concat(b)?);
                    1
                }

                // Unary operations
                Instruction::Neg => {
                    let a = self.pop();
                    self.push((-a)?);
                    1
                }
                Instruction::Not => {
                    let a = self.pop();
                    self.push((!a).into());
                    1
                }
                Instruction::Len => {
                    let a = self.pop();
                    self.push(a.len()?);
                    1
                }
                Instruction::BNot => {
                    let a = self.pop();
                    self.push(a.bitwise_not()?);
                    1
                }

                // Variables
                Instruction::SetGlobal => {
                    let global_index = const_index!();
                    let value = self.pop();
                    self.globals.insert(global_index, value);
                    1 + size_of::<ConstIndex>()
                }
                Instruction::GetGlobal => {
                    let global_index = const_index!();
                    let value = self
                        .globals
                        .get(&global_index)
                        .cloned()
                        .or_else(|| {
                            // Maybe it's a global from the stdlib?
                            let global_name = match &self.consts[global_index as usize] {
                                LuaConst::String(s) => String::from_utf8_lossy(s),
                                _ => return None,
                            };
                            stdlib::lookup_global(&global_name)
                        })
                        .unwrap_or_else(|| LuaValue::Nil);
                    self.push(value);
                    1 + size_of::<ConstIndex>()
                }
                Instruction::SetLocal => {
                    let local_index = instr_param!();
                    let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
                    let value = self.pop();

                    if self.stack_attrs[frame_pointer + local_index as usize]
                        & (LuaVariableAttribute::Constant as u8)
                        == 1
                    {
                        return Err(miette!("attempt to modify a constant"));
                    }

                    let old_value = &self.stack[frame_pointer + local_index as usize];
                    if let LuaValue::UpValue(upval) = old_value {
                        let mut upval = upval.write().unwrap();
                        *upval = value;
                    } else {
                        self.stack[frame_pointer + local_index as usize] = value;
                    }
                    2
                }
                Instruction::GetLocal => {
                    let local_index = instr_param!();
                    let current_frame = &self.call_stack[self.call_stack_index - 1];

                    let value = &self.stack[current_frame.frame_pointer + local_index as usize];
                    if let LuaValue::UpValue(upval) = value {
                        let value = upval.read().unwrap().clone();
                        self.push(value);
                    } else {
                        self.push(value.clone());
                    }
                    2
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
                    let value = self.pop();
                    let mut upval = self.call_stack[self.call_stack_index - 1].upvalues
                        [upval_index as usize]
                        .as_mut()
                        .unwrap()
                        .write()
                        .unwrap();
                    *upval = value;

                    2
                }
                Instruction::GetUpval => {
                    let upval_index = instr_param!();
                    let value = self.call_stack[self.call_stack_index - 1].upvalues
                        [upval_index as usize]
                        .as_ref()
                        .expect("upvalue not found")
                        .read()
                        .unwrap()
                        .clone();
                    self.push(value);

                    2
                }
                Instruction::LoadVararg => {
                    let local_index = instr_param!(1);
                    let is_single_vararg = instr_param!(2) == 1;
                    let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
                    let vararg = self.stack[frame_pointer + local_index as usize].clone();
                    match vararg {
                        LuaValue::Object(o) => match &*o.read().unwrap() {
                            LuaObject::Table(table) => {
                                let max = if is_single_vararg { 1 } else { table.len() };
                                for i in 1..=max {
                                    let value = table
                                        .get(&LuaValue::Number(LuaNumber::Integer(i as i64)))
                                        .cloned()
                                        .unwrap_or(LuaValue::Nil);
                                    self.push(value);
                                }
                            }
                            _ => {
                                unreachable!("vararg is not a non-table object");
                            }
                        },
                        LuaValue::Nil => {
                            self.push(LuaValue::Nil);
                        }
                        _ => {
                            unreachable!("vararg is not a table or nil");
                        }
                    };
                    3
                }

                // Table
                Instruction::NewTable => {
                    let table = LuaObject::Table(Default::default());
                    self.push(table.into());
                    1
                }
                Instruction::SetTable => {
                    let value = self.pop();
                    let key = self.pop();
                    let table = self.peek().clone();
                    match table {
                        LuaValue::Object(o) => match &mut *o.write().unwrap() {
                            LuaObject::Table(t) => {
                                t.insert(key, value);
                            }
                            _ => {
                                return Err(miette!("attempt to index a non-table"));
                            }
                        },
                        _ => {
                            return Err(miette!("attempt to index a non-table"));
                        }
                    }
                    1
                }
                Instruction::GetTable => {
                    let key = self.pop();
                    let table = self.pop();
                    // TODO: Handle metatables
                    match table {
                        LuaValue::Object(o) => match &*o.read().unwrap() {
                            LuaObject::Table(t) => {
                                let value = t.get(&key).cloned().unwrap_or(LuaValue::Nil);
                                self.push(value);
                            }
                            _ => {
                                return Err(miette!("attempt to index a non-table"));
                            }
                        },
                        LuaValue::String(_) => {
                            // Maybe it's an stdlib string method
                            let string = stdlib::string();
                            assert_table!(string, string, {
                                if let Some(value) = string.get(&key) {
                                    self.push(value.clone());
                                } else {
                                    return Err(miette!("attempt to index a non-table"));
                                }
                            })
                        }
                        _ => {
                            return Err(miette!("attempt to index a non-table"));
                        }
                    }
                    1
                }
                Instruction::AppendToTable => {
                    // Find the latest marker, and append each value after it to the table at the
                    // stack right before the marker
                    let marker_index = self.stack[..self.stack_index]
                        .iter()
                        .rposition(|v| v == &LuaValue::Marker)
                        .expect("no marker found");
                    let table = &self.stack[marker_index - 1].clone();
                    let num_values = self.stack_index - marker_index - 1;

                    for _ in 0..num_values {
                        let value = self.pop();
                        match table {
                            LuaValue::Object(o) => match &mut *o.write().unwrap() {
                                LuaObject::Table(t) => {
                                    t.append(value);
                                }
                                _ => {
                                    return Err(miette!("attempt to index a non-table"));
                                }
                            },
                            _ => {
                                return Err(miette!("attempt to index a non-table"));
                            }
                        }
                    }

                    // Get rid of the marker
                    self.pop();

                    1
                }

                // Function
                Instruction::Call => {
                    // Pop the function off the stack, and dereference the upvalue if it's one
                    let function = match self.pop() {
                        LuaValue::UpValue(upval) => upval.read().unwrap().clone(),
                        function => function,
                    };

                    // If it's a metamethod, the function is the __call metamethod
                    // In that case, the object itself is (implicitly) the first argument
                    let mut is_metamethod = None;

                    // Find the underlying closure or native function, handling metamethods
                    // FIXME: I don't like all of this cloning.
                    let function = match &function {
                        LuaValue::Object(o) => match &*o.read().unwrap() {
                            closure @ LuaObject::Closure(_) => Some(closure.clone()),
                            n @ LuaObject::NativeFunction(_, _) => Some(n.clone()),
                            other => other.get_metavalue(&CALL_KEY).and_then(|__call| {
                                is_metamethod = Some(function.clone());
                                match __call {
                                    LuaValue::Object(__call) => match &*__call.read().unwrap() {
                                        closure @ LuaObject::Closure(_) => Some(closure.clone()),
                                        n @ LuaObject::NativeFunction(_, _) => Some(n.clone()),
                                        _ => None,
                                    },
                                    _ => None,
                                }
                            }),
                        },
                        _ => None,
                    };

                    // Look up the stack to find the marker, anything above that is
                    // arguments to the function
                    let marker_position = self.stack[..self.stack_index]
                        .iter()
                        .rposition(|v| v == &LuaValue::Marker)
                        .expect("no function call args marker found");
                    let mut num_args = self.stack_index - marker_position - 1;

                    if let Some(self_parameter) = is_metamethod {
                        // We can reuse the marker index as self parameter value
                        self.stack[marker_position] = self_parameter;
                        num_args += 1;
                    } else {
                        // Get rid of the marker
                        self.shift_left(num_args, 1);
                        self.pop();
                    }

                    let is_single_return = instr_param!() == 1;

                    match function {
                        Some(LuaObject::Closure(closure)) => {
                            self.push_call_frame(
                                closure.name.clone(),
                                closure.chunk,
                                false,
                                self.stack_index - num_args,
                                self.chunks[chunk_index].ip + 2,
                                Some(closure.upvalues),
                                !is_single_return,
                            );
                            self.chunks[closure.chunk].ip = closure.ip as usize;

                            continue;
                        }
                        Some(LuaObject::NativeFunction(name, f)) => {
                            // Push a call frame just for nicer stack traces
                            self.push_call_frame(
                                Some(format!("native function '{}'", name)),
                                0,
                                false,
                                self.stack_index - num_args,
                                self.chunks[chunk_index].ip + 2,
                                None,
                                !is_single_return,
                            );
                            let mut args = Vec::with_capacity(num_args);
                            for _ in 0..num_args {
                                args.push(self.pop());
                            }
                            args.reverse();
                            for value in f(self, args)? {
                                self.push(value);
                                if is_single_return {
                                    break;
                                }
                            }
                            self.pop_call_frame();
                            2
                        }
                        _ => {
                            return Err(miette!("attempt to call a non-function"));
                        }
                    }
                }
                Instruction::Return => {
                    let frame = self.pop_call_frame();

                    // Find the marker indicating the start of the return values
                    let marker_position = self.stack[..self.stack_index]
                        .iter()
                        .rposition(|v| v == &LuaValue::Marker)
                        .expect("no return values marker found");

                    // Collect the return values
                    let mut return_values: Vec<_> = (marker_position + 1..self.stack_index)
                        .map(|_| self.pop())
                        .collect();
                    return_values.reverse();

                    // Pop the marker
                    self.pop();

                    // Check if this is the root frame, or if we're leaving this chunk
                    if frame.border_frame {
                        return Ok(return_values);
                    }

                    let parent_call_frame = &self.call_stack[self.call_stack_index - 1];
                    self.chunks[parent_call_frame.chunk].ip = frame.return_addr;
                    // Discard any remaining values on the stack from the function call
                    self.stack_index = frame.frame_pointer;

                    // Put the return values back
                    for value in return_values {
                        self.push(value);
                    }

                    continue;
                }

                // Control
                Instruction::Jmp => {
                    let addr = jump_addr!();
                    if addr == JumpAddr::MAX {
                        // TODO: We probably want a nicer error here. Was this a break
                        // outside of a loop? Was it a goto statement with no matching
                        // label?
                        return Err(miette!("attempt to jump to an invalid address"));
                    }
                    self.chunks[chunk_index].ip = addr as usize;
                    continue;
                }
                Instruction::JmpTrue => {
                    let value = self.peek();
                    if value.as_boolean() {
                        let addr = jump_addr!();
                        self.chunks[chunk_index].ip = addr as usize;
                        continue;
                    }
                    1 + size_of::<JumpAddr>()
                }
                Instruction::JmpFalse => {
                    let value = self.peek();
                    if !value.as_boolean() {
                        let addr = jump_addr!();
                        self.chunks[chunk_index].ip = addr as usize;
                        continue;
                    }
                    1 + size_of::<JumpAddr>()
                }

                // Other
                Instruction::Error => {
                    let error_type = instr_param!();
                    return Err(RuntimeError::try_from(error_type)?).into_diagnostic();
                }
            };
            self.chunks[chunk_index].ip += instruction_increment;
        }
    }
}

impl<'source> Chunk<'source> {
    pub fn get_current_addr(&self) -> JumpAddr {
        self.instructions.len() as JumpAddr
    }

    // TODO: Can this be non-optional?
    pub fn push_instruction<T>(&mut self, instruction: T, span: Option<Span>)
    where
        T: Into<u8> + std::fmt::Debug,
    {
        let instruction_index = self.instructions.len();
        self.instructions.push(instruction.into());
        if let Some(span) = span {
            self.instruction_spans.insert(instruction_index, span);
        }
    }

    pub fn push_addr(&mut self, addr: JumpAddr) {
        self.instructions.extend_from_slice(&addr.to_be_bytes());
    }

    pub fn push_const_index(&mut self, index: ConstIndex) {
        self.instructions.extend(index.to_be_bytes());
    }

    pub fn push_addr_placeholder(&mut self) -> usize {
        let index = self.instructions.len();
        self.instructions
            .extend_from_slice(&JumpAddr::MAX.to_be_bytes());
        index
    }

    pub fn patch_addr_placeholder(&mut self, index: usize) {
        let addr = self.instructions.len() as JumpAddr;
        let addr_bytes = addr.to_be_bytes();
        self.instructions[index..index + size_of::<JumpAddr>()].copy_from_slice(&addr_bytes);
    }

    pub fn get_instructions(&self) -> &[u8] {
        &self.instructions
    }
}
