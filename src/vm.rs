use crate::{
    instruction::Instruction,
    value::{LuaConst, LuaValue},
};

const MAX_STACK_SIZE: usize = 256;

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

    pub fn push(&mut self, value: LuaValue) {
        let index = self.stack_index;
        self.stack[index] = value;
        self.stack_index += 1;
    }

    pub fn pop(&mut self) -> LuaValue {
        self.stack_index -= 1;
        // FIXME: This shrinks the stack by 1 if this was the last element
        self.stack.swap_remove(self.stack_index)
    }

    pub fn push_instruction<T>(&mut self, instruction: T)
    where
        T: Into<u8> + std::fmt::Debug,
    {
        self.instructions.push(instruction.into());
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
                Instruction::Halt => {
                    break;
                }
                Instruction::Print => {
                    let value = self.pop();
                    println!("{}", value);
                    1
                }
                Instruction::LoadConst => {
                    let const_index = self.instructions[ip + 1];
                    let constant = self.consts[const_index as usize].clone();
                    self.push(constant.into());
                    2
                }
                Instruction::Add => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a + b);
                    1
                }
                _ => unimplemented!(),
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
