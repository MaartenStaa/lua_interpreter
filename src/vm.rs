use std::{collections::HashMap, path::Path};

use miette::{miette, NamedSource};

use crate::{
    instruction::Instruction,
    token::Span,
    value::{LuaConst, LuaValue},
};

const MAX_STACK_SIZE: usize = 256;

pub type JumpAddr = u16;
pub type JumpRelOffset = i16;

#[derive(Debug)]
pub struct VM<'path, 'source> {
    filename: &'path Path,
    source: &'source str,
    instructions: Vec<u8>,
    instruction_spans: HashMap<usize, Span>,
    ip: usize,
    consts: Vec<LuaConst>,
    const_index: u8,
    stack: Vec<LuaValue>,
    stack_index: usize,
}

impl<'path, 'source> VM<'path, 'source> {
    pub fn new(filename: &'path Path, source: &'source str) -> Self {
        Self {
            filename,
            source,
            instructions: vec![],
            instruction_spans: HashMap::new(),
            ip: 0,
            consts: vec![LuaConst::Nil; u8::MAX as usize],
            const_index: 0,
            stack: vec![LuaValue::Nil; MAX_STACK_SIZE],
            stack_index: 0,
        }
    }

    pub fn register_const(&mut self, constant: LuaConst) -> u8 {
        let index = self.const_index;
        self.consts[index as usize] = constant;
        self.const_index += 1;
        index
    }

    fn push(&mut self, value: LuaValue) {
        let index = self.stack_index;
        self.stack[index] = value;
        self.stack_index += 1;
    }

    fn pop(&mut self) -> LuaValue {
        self.stack_index -= 1;
        // FIXME: This shrinks the stack by 1 if this was the last element
        self.stack.swap_remove(self.stack_index)
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
            let err = miette!(labels = labels, "{err:?}").with_source_code(
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
                    let const_index = self.instructions[self.ip + 1];
                    let constant = self.consts[const_index as usize].clone();
                    self.push(constant.into());
                    2
                }
                Instruction::Pop => {
                    self.pop();
                    1
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
                    let value = self.pop();
                    println!("{}", value);
                    1
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
