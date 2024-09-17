use std::{collections::HashMap, path::Path};

use miette::{miette, NamedSource};

use crate::{
    instruction::Instruction,
    stdlib,
    token::Span,
    value::{LuaConst, LuaNumber, LuaObject, LuaTable, LuaValue},
};

const MAX_STACK_SIZE: usize = 256;

pub type JumpAddr = u16;
pub type ConstIndex = u16;

#[derive(Debug)]
pub struct VM<'path, 'source> {
    filename: &'path Path,
    source: &'source str,
    instructions: Vec<u8>,
    instruction_spans: HashMap<usize, Span>,
    ip: usize,
    consts: Vec<LuaConst>,
    const_index: ConstIndex,
    stack: [LuaValue; MAX_STACK_SIZE],
    stack_index: usize,
    globals: HashMap<ConstIndex, LuaValue>,
    call_stack: [CallFrame; MAX_STACK_SIZE],
    call_stack_index: usize,
}

#[derive(Debug)]
struct CallFrame {
    name: Option<String>,
    frame_pointer: usize,
    return_addr: usize,
}

impl<'path, 'source> VM<'path, 'source> {
    pub fn new(filename: &'path Path, source: &'source str) -> Self {
        let top_frame_name = filename.file_name().unwrap().to_string_lossy().to_string();
        let mut result = Self {
            filename,
            source,
            instructions: vec![],
            instruction_spans: HashMap::new(),
            ip: 0,
            consts: vec![],
            const_index: 0,
            stack: [const { LuaValue::Nil }; MAX_STACK_SIZE],
            stack_index: 0,
            globals: HashMap::new(),
            call_stack: [const {
                CallFrame {
                    name: None,
                    frame_pointer: 0,
                    return_addr: 0,
                }
            }; MAX_STACK_SIZE],
            call_stack_index: 1,
        };
        result.call_stack[0].name = Some(top_frame_name);
        result
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

    fn push_call_frame(&mut self, frame: CallFrame) {
        self.call_stack[self.call_stack_index] = frame;
        self.call_stack_index += 1;
    }

    fn pop_call_frame(&mut self) -> CallFrame {
        self.call_stack_index -= 1;
        std::mem::replace(
            &mut self.call_stack[self.call_stack_index],
            CallFrame {
                name: None,
                frame_pointer: 0,
                return_addr: 0,
            },
        )
    }

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
        self.instructions[index..index + std::mem::size_of::<JumpAddr>()]
            .copy_from_slice(&addr_bytes);
    }

    pub fn get_consts(&self) -> &[LuaConst] {
        &self.consts
    }

    pub fn get_instructions(&self) -> &[u8] {
        &self.instructions
    }

    pub fn run(&mut self) {
        if let Err(err) = self.run_inner() {
            let labels = if let Some(span) = self.instruction_spans.get(&self.ip) {
                vec![span.labeled("here")]
            } else {
                vec![]
            };

            // FIXME: This is a workaround for the fact that miette doesn't support
            // adding labels ad-hoc, but it ends up printing the error message
            // kind of weirdly.
            let mut err = miette!(labels = labels, "{err:?}");
            // Attach stack trace
            for (i, frame) in self.call_stack[..self.call_stack_index].iter().enumerate() {
                err = err.wrap_err(format!(
                    "#{i} {}",
                    frame.name.as_deref().unwrap_or("<anonymous>"),
                    i = self.call_stack_index - i,
                ));
            }
            err = err.with_source_code(
                NamedSource::new(self.filename.to_string_lossy(), self.source.to_string())
                    .with_language("lua"),
            );

            eprintln!("{:?}", err);
            std::process::exit(1);
        }
    }

    fn run_inner(&mut self) -> miette::Result<()> {
        loop {
            let instruction = self.instructions[self.ip];
            let instruction_increment = match Instruction::from(instruction) {
                // Stack manipulation
                Instruction::LoadConst => {
                    let const_index_bytes = &self.instructions
                        [self.ip + 1..self.ip + 1 + std::mem::size_of::<ConstIndex>()];
                    let const_index =
                        ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
                    let constant = self.consts[const_index as usize].clone();
                    self.push(constant.into());
                    1 + std::mem::size_of::<ConstIndex>()
                }
                Instruction::Pop => {
                    self.pop();
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
                    let swap_offset = self.instructions[self.ip + 1];
                    let a = self.stack_index - 1;
                    let b = self.stack_index - 1 - swap_offset as usize;
                    self.stack.swap(a, b);
                    2
                }
                instr @ Instruction::Align | instr @ Instruction::AlignVararg => {
                    let collect_varargs = matches!(instr, Instruction::AlignVararg);
                    let align_amount = self.instructions[self.ip + 1];

                    assert!(self.stack_index > 0, "cannot align an empty stack");

                    // Find either the latest marker or the start of the stack
                    // from the current call frame
                    let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
                    let marker_index = (frame_pointer..self.stack_index)
                        .rev()
                        .find(|&i| self.stack[i] == LuaValue::Marker);
                    // let base_pointer = marker_index.unwrap_or(frame_pointer);

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
                            let mut index = num_varargs;
                            // Descending order!
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
                    let global_index_bytes = &self.instructions
                        [self.ip + 1..self.ip + 1 + std::mem::size_of::<ConstIndex>()];
                    let global_index =
                        ConstIndex::from_be_bytes(global_index_bytes.try_into().unwrap());
                    let value = self.pop();
                    self.globals.insert(global_index, value);
                    1 + std::mem::size_of::<ConstIndex>()
                }
                Instruction::GetGlobal => {
                    let global_index_bytes = &self.instructions
                        [self.ip + 1..self.ip + 1 + std::mem::size_of::<ConstIndex>()];
                    let global_index =
                        ConstIndex::from_be_bytes(global_index_bytes.try_into().unwrap());
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
                        .unwrap_or(LuaValue::Nil);
                    self.push(value);
                    1 + std::mem::size_of::<ConstIndex>()
                }
                Instruction::SetLocal => {
                    let local_index = self.instructions[self.ip + 1];
                    let frame_pointer = self.call_stack.last().unwrap().frame_pointer;
                    let value = self.pop();
                    self.stack[frame_pointer + local_index as usize] = value;
                    2
                }
                Instruction::GetLocal => {
                    let local_index = self.instructions[self.ip + 1];
                    let current_frame = &self.call_stack[self.call_stack_index - 1];
                    let value =
                        self.stack[current_frame.frame_pointer + local_index as usize].clone();
                    self.push(value);
                    2
                }
                Instruction::LoadVararg => {
                    let local_index = self.instructions[self.ip + 1];
                    let frame_pointer = self.call_stack[self.call_stack_index - 1].frame_pointer;
                    let vararg = self.stack[frame_pointer + local_index as usize].clone();
                    match vararg {
                        LuaValue::Object(o) => match &*o.read().unwrap() {
                            LuaObject::Table(table) => {
                                for i in 1..=table.len() {
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
                    2
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
                        _ => {
                            return Err(miette!("attempt to index a non-table"));
                        }
                    }
                    1
                }

                // Function
                Instruction::Call => {
                    let function = self.pop();
                    // Look up the stack to find the marker, anything above that is
                    // arguments to the function
                    let marker_position = self.stack[..self.stack_index]
                        .iter()
                        .rposition(|v| *v == LuaValue::Marker)
                        .expect("no function call args marker found");
                    let num_args = self.stack_index - marker_position - 1;

                    // Get rid of the marker
                    self.shift_left(num_args, 1);
                    self.pop();

                    match function {
                        LuaValue::Object(o) => match &*o.read().unwrap() {
                            LuaObject::Function(name, f) => {
                                self.push_call_frame(CallFrame {
                                    name: name.clone(),
                                    frame_pointer: self.stack_index - num_args,
                                    return_addr: self.ip + 1,
                                });
                                self.ip = *f as usize;
                                continue;
                            }
                            LuaObject::NativeFunction(f) => {
                                let mut args = Vec::with_capacity(num_args);
                                for _ in 0..num_args {
                                    args.push(self.pop());
                                }
                                args.reverse();
                                for value in f(args)? {
                                    self.push(value);
                                }
                                1
                            }
                            // TODO: Handle tables with a `__call` metamethod
                            _ => {
                                return Err(miette!("attempt to call a non-function"));
                            }
                        },
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
                        .rposition(|v| *v == LuaValue::Marker)
                        .expect("no return values marker found");

                    // Collect the return values
                    let mut return_values: Vec<_> = (marker_position + 1..self.stack_index)
                        .map(|_| self.pop())
                        .collect();
                    return_values.reverse();

                    // Pop the marker
                    self.pop();

                    // Check if this is the root frame
                    if self.call_stack_index == 0 {
                        break;
                    }

                    self.ip = frame.return_addr;
                    // Discard any remaining values on the stack from the function call
                    self.stack_index = frame.frame_pointer;

                    // Put the return values back
                    // TODO: Only push the first return value in certain contexts, like in the
                    // argument list of a function call
                    for value in return_values {
                        self.push(value);
                    }

                    continue;
                }

                // Control
                Instruction::Jmp => {
                    // Absolute jump
                    let addr_bytes = &self.instructions
                        [self.ip + 1..self.ip + 1 + std::mem::size_of::<JumpAddr>()];
                    let addr = JumpAddr::from_be_bytes(addr_bytes.try_into().unwrap());
                    if addr == JumpAddr::MAX {
                        // TODO: We probably want a nicer error here. Was this a break
                        // outside of a loop? Was it a goto statement with no matching
                        // label?
                        return Err(miette!("attempt to jump to an invalid address"));
                    }
                    self.ip = addr as usize;
                    continue;
                }
                Instruction::JmpTrue => {
                    let value = self.peek();
                    if value.as_boolean() {
                        // Absolute jump
                        let addr_bytes = &self.instructions
                            [self.ip + 1..self.ip + 1 + std::mem::size_of::<JumpAddr>()];
                        let addr = JumpAddr::from_be_bytes(addr_bytes.try_into().unwrap());
                        self.ip = addr as usize;
                        continue;
                    }
                    1 + std::mem::size_of::<JumpAddr>()
                }
                Instruction::JmpFalse => {
                    let value = self.peek();
                    if !value.as_boolean() {
                        // Absolute jump
                        let addr_bytes = &self.instructions
                            [self.ip + 1..self.ip + 1 + std::mem::size_of::<JumpAddr>()];
                        let addr = JumpAddr::from_be_bytes(addr_bytes.try_into().unwrap());
                        self.ip = addr as usize;
                        continue;
                    }
                    1 + std::mem::size_of::<JumpAddr>()
                }
            };
            self.ip += instruction_increment;
        }

        assert!(self.stack_index == 0, "stack is not empty");

        Ok(())
    }
}
