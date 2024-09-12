use crate::{
    instruction::Instruction,
    value::{LuaConst, LuaValue},
};

const MAX_STACK_SIZE: usize = 256;

pub type JumpAddr = u16;
pub type JumpRelOffset = i16;

pub struct Chunk {}

#[derive(Debug)]
pub struct VM {
    instructions: Vec<u8>,
    consts: Vec<LuaConst>,
    const_index: u8,
    stack: Vec<LuaValue>,
    stack_index: usize,
}

impl VM {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
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

    pub fn push_instruction<T>(&mut self, instruction: T)
    where
        T: Into<u8> + std::fmt::Debug,
    {
        self.instructions.push(instruction.into());
    }

    pub fn push_addr_placeholder(&mut self) -> usize {
        let index = self.instructions.len();
        self.instructions
            .extend_from_slice(&[0; std::mem::size_of::<JumpAddr>()]);
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
        let mut ip = 0;
        loop {
            let instruction = self.instructions[ip];
            let instruction_increment = match Instruction::from(instruction) {
                // Constants
                Instruction::LoadConst => {
                    let const_index = self.instructions[ip + 1];
                    let constant = self.consts[const_index as usize].clone();
                    self.push(constant.into());
                    2
                }

                // Binary operations
                // Arithmetic
                Instruction::Add => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a + b);
                    1
                }
                Instruction::Sub => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a - b);
                    1
                }
                Instruction::Mul => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a * b);
                    1
                }
                Instruction::Div => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a / b);
                    1
                }
                Instruction::Mod => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a % b);
                    1
                }
                Instruction::Pow => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a.pow(b));
                    1
                }
                Instruction::IDiv => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a.idiv(b));
                    1
                }
                Instruction::Band => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a & b);
                    1
                }
                Instruction::Bor => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a | b);
                    1
                }
                Instruction::Bxor => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a ^ b);
                    1
                }
                Instruction::Shl => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a << b);
                    1
                }
                Instruction::Shr => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a >> b);
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
                    self.push(a.concat(b));
                    1
                }

                // Unary operations
                Instruction::Neg => {
                    let a = self.pop();
                    self.push(-a);
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
                    let addr_bytes =
                        &self.instructions[ip + 1..ip + 1 + std::mem::size_of::<JumpAddr>()];
                    let addr = JumpAddr::from_be_bytes(addr_bytes.try_into().unwrap());
                    ip = addr as usize;
                    continue;
                }
                Instruction::JmpTrue => {
                    let value = self.peek();
                    if value.as_boolean() {
                        // Absolute jump
                        let addr_bytes =
                            &self.instructions[ip + 1..ip + 1 + std::mem::size_of::<JumpAddr>()];
                        let addr = JumpAddr::from_be_bytes(addr_bytes.try_into().unwrap());
                        ip = addr as usize;
                        continue;
                    }
                    1 + std::mem::size_of::<JumpAddr>()
                }
                Instruction::JmpFalse => {
                    let value = self.peek();
                    if !value.as_boolean() {
                        // Absolute jump
                        let addr_bytes =
                            &self.instructions[ip + 1..ip + 1 + std::mem::size_of::<JumpAddr>()];
                        let addr = JumpAddr::from_be_bytes(addr_bytes.try_into().unwrap());
                        ip = addr as usize;
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
            ip += instruction_increment;
        }
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}
