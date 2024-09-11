use crate::{instruction::Instruction, vm::VM};

pub fn print_instructions(vm: &VM) {
    let instructions = vm.get_instructions();
    let consts = vm.get_consts();
    let mut instruction_pointer = 0;

    loop {
        let instruction = instructions[instruction_pointer];
        print!("{instruction_pointer:04}   ");

        let instruction_increment = match Instruction::from(instruction) {
            Instruction::Halt => {
                println!("HALT");
                break;
            }
            Instruction::Print => {
                println!("PRINT");
                1
            }
            // Constants
            Instruction::LoadConst => {
                let const_index = instructions[instruction_pointer + 1];
                print!("LOAD_CONST    ");
                print_const(&consts[const_index as usize]);
                println!();
                2
            }

            // Binary operations
            // Arithmetic
            Instruction::Add => {
                println!("ADD");
                1
            }
            Instruction::Sub => {
                println!("SUB");
                1
            }
            Instruction::Mul => {
                println!("MUL");
                1
            }
            Instruction::Div => {
                println!("DIV");
                1
            }
            Instruction::Mod => {
                println!("MOD");
                1
            }
            Instruction::Pow => {
                println!("POW");
                1
            }
            Instruction::IDiv => {
                println!("IDIV");
                1
            }

            // Concatenation
            Instruction::Concat => {
                println!("CONCAT");
                1
            }

            // Unary operations
            Instruction::Neg => {
                println!("NEG");
                1
            }

            _ => todo!("print_instructions {:?}", instruction),
        };
        instruction_pointer += instruction_increment;
    }
}

fn print_const(constant: &crate::value::LuaConst) {
    match constant {
        crate::value::LuaConst::Nil => print!("NIL"),
        crate::value::LuaConst::Boolean(b) => print!("{b}"),
        crate::value::LuaConst::Number(n) => print_number(n),
        crate::value::LuaConst::String(s) => print!("STRING    \"{}\"", String::from_utf8_lossy(s)),
    }
}

fn print_number(number: &crate::value::LuaNumber) {
    match number {
        crate::value::LuaNumber::Integer(i) => print!("INT       {}", i),
        crate::value::LuaNumber::Float(f) => print!("FLOAT     {}", f),
    }
}
