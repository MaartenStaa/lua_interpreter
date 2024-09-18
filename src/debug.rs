use crate::{
    instruction::Instruction,
    value::LuaConst,
    vm::{ConstIndex, JumpAddr, VM},
};

pub fn print_instructions(vm: &VM) {
    let instructions = vm.get_instructions();
    let consts = vm.get_consts();
    let mut instruction_pointer = 0;

    loop {
        if instruction_pointer >= instructions.len() {
            break;
        }

        let instruction = instructions[instruction_pointer];
        print!("{instruction_pointer:04}   ");

        let instruction_increment = match Instruction::from(instruction) {
            // Stack manipulation
            Instruction::LoadConst => {
                let const_index_bytes = &instructions[instruction_pointer + 1
                    ..instruction_pointer + 1 + std::mem::size_of::<ConstIndex>()];
                let const_index = ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
                print!("LOAD_CONST    ");
                print_const(&consts[const_index as usize]);
                println!();
                1 + std::mem::size_of::<ConstIndex>()
            }
            Instruction::Pop => {
                println!("POP");
                1
            }
            Instruction::Discard => {
                println!("DISCARD");
                1
            }
            Instruction::Swap => {
                let swap_offset = instructions[instruction_pointer + 1];
                println!("SWAP          {swap_offset}");
                2
            }
            Instruction::Align => {
                let align_offset = instructions[instruction_pointer + 1];
                println!("ALIGN         {align_offset}");
                2
            }
            Instruction::AlignVararg => {
                let align_offset = instructions[instruction_pointer + 1];
                println!("ALIGN_VARARG  {align_offset}");
                2
            }
            Instruction::DupFromMarker => {
                let offset = instructions[instruction_pointer + 1];
                println!("DUP_FROM_MARK {offset}");
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
            Instruction::Band => {
                println!("BAND");
                1
            }
            Instruction::Bor => {
                println!("BOR");
                1
            }
            Instruction::Bxor => {
                println!("BXOR");
                1
            }
            Instruction::Shl => {
                println!("SHL");
                1
            }
            Instruction::Shr => {
                println!("SHR");
                1
            }

            // Comparison
            Instruction::Eq => {
                println!("EQ");
                1
            }
            Instruction::Ne => {
                println!("NE");
                1
            }
            Instruction::Lt => {
                println!("LT");
                1
            }
            Instruction::Le => {
                println!("LE");
                1
            }
            Instruction::Gt => {
                println!("GT");
                1
            }
            Instruction::Ge => {
                println!("GE");
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
            Instruction::Not => {
                println!("NOT");
                1
            }
            Instruction::Len => {
                println!("LEN");
                1
            }
            Instruction::BNot => {
                println!("BNOT");
                1
            }

            // Variables
            Instruction::SetGlobal => {
                let name_index_bytes = &instructions[instruction_pointer + 1
                    ..instruction_pointer + 1 + std::mem::size_of::<ConstIndex>()];
                let name_index = ConstIndex::from_be_bytes(name_index_bytes.try_into().unwrap());
                print!("SET_GLOBAL    ");
                print_const(&consts[name_index as usize]);
                println!("   ({name_index})");
                1 + std::mem::size_of::<ConstIndex>()
            }
            Instruction::GetGlobal => {
                let name_index_bytes = &instructions[instruction_pointer + 1
                    ..instruction_pointer + 1 + std::mem::size_of::<ConstIndex>()];
                let name_index = ConstIndex::from_be_bytes(name_index_bytes.try_into().unwrap());
                print!("GET_GLOBAL    ");
                print_const(&consts[name_index as usize]);
                println!("   ({name_index})");
                1 + std::mem::size_of::<ConstIndex>()
            }
            Instruction::SetLocal => {
                let local_index = instructions[instruction_pointer + 1];
                println!("SET_LOCAL     {local_index}");
                2
            }
            Instruction::GetLocal => {
                let local_index = instructions[instruction_pointer + 1];
                println!("GET_LOCAL     {local_index}");
                2
            }
            Instruction::LoadVararg => {
                let local_index = instructions[instruction_pointer + 1];
                println!("LOAD_VARARG   {local_index}");
                2
            }

            // Table
            Instruction::NewTable => {
                println!("NEW_TABLE");
                1
            }
            Instruction::SetTable => {
                println!("SET_TABLE");
                1
            }
            Instruction::GetTable => {
                println!("GET_TABLE");
                1
            }

            // Function
            Instruction::Call => {
                println!("CALL");
                1
            }
            Instruction::Return => {
                println!("RETURN");
                1
            }

            // Control
            Instruction::Jmp => {
                let offset_bytes = &instructions[instruction_pointer + 1
                    ..instruction_pointer + 1 + std::mem::size_of::<JumpAddr>()];
                let offset = JumpAddr::from_be_bytes(offset_bytes.try_into().unwrap());
                println!("JMP           {offset:04}");
                1 + std::mem::size_of::<JumpAddr>()
            }
            Instruction::JmpTrue => {
                let offset_bytes = &instructions[instruction_pointer + 1
                    ..instruction_pointer + 1 + std::mem::size_of::<JumpAddr>()];
                let offset = JumpAddr::from_be_bytes(offset_bytes.try_into().unwrap());
                println!("JMP_TRUE      {offset:04}");
                1 + std::mem::size_of::<JumpAddr>()
            }
            Instruction::JmpFalse => {
                let offset_bytes = &instructions[instruction_pointer + 1
                    ..instruction_pointer + 1 + std::mem::size_of::<JumpAddr>()];
                let offset = JumpAddr::from_be_bytes(offset_bytes.try_into().unwrap());
                println!("JMP_FALSE     {offset:04}");
                1 + std::mem::size_of::<JumpAddr>()
            }
        };
        instruction_pointer += instruction_increment;
    }
}

fn print_const(constant: &LuaConst) {
    match constant {
        LuaConst::Marker => print!("MARKER"),
        LuaConst::Nil => print!("NIL"),
        LuaConst::Boolean(b) => print!("{b}"),
        LuaConst::Number(n) => print_number(n),
        LuaConst::String(s) => print!("STRING    \"{}\"", String::from_utf8_lossy(s)),
        LuaConst::Function(name, f) => print!("FUNCTION  {name:?} {f:04}"),
    }
}

fn print_number(number: &crate::value::LuaNumber) {
    match number {
        crate::value::LuaNumber::Integer(i) => print!("INT       {}", i),
        crate::value::LuaNumber::Float(f) => print!("FLOAT     {}", f),
    }
}
