use std::{collections::HashMap, path::Path};

use miette::{miette, NamedSource};

use crate::{
    instruction::Instruction,
    token::Span,
    value::{LuaConst, LuaObject, LuaValue},
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
    stack: Vec<LuaValue>,
    stack_index: usize,
    globals: HashMap<ConstIndex, LuaValue>,
    call_stack: Vec<CallFrame>,
}

#[derive(Debug)]
struct CallFrame {
    name: Option<String>,
    frame_pointer: usize,
    return_addr: usize,
}

impl<'path, 'source> VM<'path, 'source> {
    pub fn new(filename: &'path Path, source: &'source str) -> Self {
        Self {
            filename,
            source,
            instructions: vec![],
            instruction_spans: HashMap::new(),
            ip: 0,
            consts: vec![],
            const_index: 0,
            stack: vec![LuaValue::Nil; MAX_STACK_SIZE],
            stack_index: 0,
            globals: HashMap::new(),
            // TODO: Allocate all of this up front like the stack.
            call_stack: vec![],
        }
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
        self.call_stack.push(CallFrame {
            name: Some(format!("<file> {}", self.filename.to_string_lossy())),
            frame_pointer: 0,
            return_addr: self.get_current_addr() as usize,
        });

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
            for (i, frame) in self.call_stack.iter().rev().enumerate() {
                err = err.wrap_err(format!(
                    "#{i} {}",
                    // TODO: Maybe anonymous is a better name?
                    frame.name.as_deref().unwrap_or("<unknown>")
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
                Instruction::Swap => {
                    let swap_offset = self.instructions[self.ip + 1];
                    let a = self.stack_index - 1;
                    let b = self.stack_index - 1 - swap_offset as usize;
                    self.stack.swap(a, b);
                    2
                }
                Instruction::Align => {
                    let align_amount = self.instructions[self.ip + 1];

                    // Find either the latest marker or the start of the stack
                    // from the current call frame
                    let current_frame = self.call_stack.last().unwrap();
                    let marker_index = (0..self.stack_index)
                        .rev()
                        .find(|&i| {
                            self.stack[i] == LuaValue::Marker || i == current_frame.frame_pointer
                        })
                        .expect("no marker found");

                    // Align the stack, so that there are <align_amount> values
                    // between the marker and the top of the stack
                    let num_values = self.stack_index - marker_index - 1;
                    if num_values < align_amount as usize {
                        for _ in 0..align_amount as usize - num_values {
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
                    self.push(!a);
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
                        // FIXME: Not everything can be freely cloned
                        .cloned()
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
                    let current_frame = self.call_stack.last().unwrap();
                    let value =
                        self.stack[current_frame.frame_pointer + local_index as usize].clone();
                    self.push(value);
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
                    let num_args = self.instructions[self.ip + 1];
                    match function {
                        LuaValue::Object(o) => match &*o.read().unwrap() {
                            LuaObject::Function(_, f) => {
                                self.call_stack.push(CallFrame {
                                    name: None,
                                    frame_pointer: self.stack_index - num_args as usize,
                                    return_addr: self.ip + 2,
                                });
                                self.ip = *f as usize;
                                continue;
                            }
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
                    // TODO: Handle multiple return values
                    let value = self.pop();
                    let frame = self.call_stack.pop().unwrap();
                    self.ip = frame.return_addr;
                    // Discard any remaining values on the stack from the function call
                    self.stack_index = frame.frame_pointer;
                    // Put the return value back
                    self.push(value);
                    // Check if this is the root frame
                    if self.call_stack.is_empty() {
                        break;
                    } else {
                        continue;
                    }
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

                // Debug
                Instruction::Print => {
                    let num_args = self.instructions[self.ip + 1];
                    let mut args = Vec::with_capacity(num_args as usize);
                    for _ in 0..num_args {
                        args.push(self.pop());
                    }
                    for (i, arg) in args.iter().rev().enumerate() {
                        if i > 0 {
                            print!("\t");
                        }
                        print!("{}", arg);
                    }
                    println!();
                    // Return value
                    self.push(LuaValue::Nil);
                    2
                }

                // Halt
                Instruction::Halt => break,
            };
            self.ip += instruction_increment;
        }

        assert!(self.stack_index == 0, "stack is not empty");

        Ok(())
    }
}
