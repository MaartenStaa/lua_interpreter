use std::mem::size_of;

use crate::{
    chunk::Chunk,
    error::RuntimeError,
    instruction::Instruction,
    value::LuaConst,
    vm::{ConstIndex, JumpAddr, VM},
};

const IP_WIDTH: usize = 4;
const IP_HEX_WIDTH: usize = 2;
const IP_TEXT_WIDTH: usize = 18;
const REG_TEXT_WIDTH: usize = 4;

macro_rules! instr {
    ($name:expr) => {
        print!("{:01$}", $name, IP_TEXT_WIDTH);
    };
}

pub fn print_instructions(vm: &VM, chunk: &Chunk<'_>) {
    let instructions = chunk.get_instructions();
    let consts = vm.get_consts();
    let mut instruction_pointer = 0;

    macro_rules! reg {
        ($offset:expr) => {{
            let r = instructions[instruction_pointer + $offset];
            print!("{:>1$} ", format!("R{}", r), REG_TEXT_WIDTH);
        }};
        () => {
            reg!(1);
        };
    }

    loop {
        if instruction_pointer >= instructions.len() {
            break;
        }

        let instruction = instructions[instruction_pointer];
        print!(
            "{instruction_pointer:00$}  0x{instruction:01$x}  ",
            IP_WIDTH, IP_HEX_WIDTH
        );

        let instruction_increment = match Instruction::from(instruction) {
            // Stack manipulation
            Instruction::LoadConst => {
                let const_index_bytes = &instructions
                    [instruction_pointer + 2..instruction_pointer + 2 + size_of::<ConstIndex>()];
                let const_index = ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
                instr!("LOAD_CONST");
                reg!();
                print_const(&consts[const_index as usize]);
                println!();
                2 + size_of::<ConstIndex>()
            }
            Instruction::LoadClosure => {
                let const_index_bytes = &instructions
                    [instruction_pointer + 2..instruction_pointer + 2 + size_of::<ConstIndex>()];
                let chunk_index = ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
                instr!("LOAD_CLOSURE");
                reg!();
                let chunk = vm.get_chunk(chunk_index).expect("valid chunk index");
                println!(
                    "FUNCTION<{name}:{chunk_index:04}>",
                    name = String::from_utf8_lossy(&chunk.name)
                );

                2 + size_of::<ConstIndex>()
            }
            Instruction::LoadNil => {
                instr!("LOAD_NIL");
                reg!(1);
                reg!(2);
                println!();
                3
            }

            // Binary operations
            // Arithmetic
            Instruction::Add => {
                instr!("ADD");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::Sub => {
                instr!("SUB");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::Mul => {
                instr!("MUL");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::Div => {
                instr!("DIV");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::Mod => {
                instr!("MOD");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::Pow => {
                instr!("POW");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::IDiv => {
                instr!("IDIV");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::Band => {
                instr!("BAND");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::Bor => {
                instr!("BOR");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::Bxor => {
                instr!("BXOR");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::Shl => {
                instr!("SHL");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::Shr => {
                instr!("SHR");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }

            // Comparison
            Instruction::Eq => {
                instr!("EQ");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::Ne => {
                instr!("NE");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::Lt => {
                instr!("LT");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::Le => {
                instr!("LE");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }

            // Concatenation
            Instruction::Concat => {
                instr!("CONCAT");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }

            // Unary operations
            Instruction::Neg => {
                instr!("NEG");
                reg!(1);
                reg!(2);
                println!();
                3
            }
            Instruction::Not => {
                instr!("NOT");
                reg!(1);
                reg!(2);
                println!();
                3
            }
            Instruction::Len => {
                instr!("LEN");
                reg!(1);
                reg!(2);
                println!();
                3
            }
            Instruction::BNot => {
                instr!("BNOT");
                reg!(1);
                reg!(2);
                println!();
                3
            }

            // Variables
            Instruction::Mov => {
                instr!("MOV");
                reg!(1);
                reg!(2);
                println!();
                3
            }
            Instruction::SetUpval => {
                let upval_index = instructions[instruction_pointer + 1];
                instr!("SET_UPVAL");
                print!("{upval_index}");
                reg!();
                println!();
                3
            }
            Instruction::GetUpval => {
                let upval_index = instructions[instruction_pointer + 2];
                instr!("GET_UPVAL");
                reg!();
                println!("{upval_index}");
                3
            }
            Instruction::LoadVararg => {
                let is_single_value = instructions[instruction_pointer + 2] == 1;
                instr!("LOAD_VARARG");
                reg!();
                println!("{}", if is_single_value { "single" } else { "multi" });
                3
            }
            Instruction::ToClose => {
                instr!("TO_CLOSE");
                reg!();
                println!();
                2
            }
            Instruction::Close => {
                instr!("CLOSE");
                reg!();
                println!();
                2
            }

            // Table
            Instruction::NewTable => {
                instr!("NEW_TABLE");
                reg!();
                println!();
                2
            }
            Instruction::SetTable => {
                instr!("SET_TABLE");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::SetTableConstKey => {
                let const_index_bytes = &instructions
                    [instruction_pointer + 2..instruction_pointer + 2 + size_of::<ConstIndex>()];
                let const_index = ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
                instr!("SET_TABLE_CK");
                reg!(1);
                print_const(&consts[const_index as usize]);
                reg!(2 + size_of::<ConstIndex>());
                println!();
                3 + size_of::<ConstIndex>()
            }
            Instruction::SetTableUpval => {
                let upval_index = instructions[instruction_pointer + 2];
                instr!("SET_TABLE_UP");
                reg!(1);
                print!("{upval_index}");
                reg!(3);
                println!();
                4
            }
            Instruction::SetTableUpvalConstKey => {
                let const_index_bytes = &instructions
                    [instruction_pointer + 2..instruction_pointer + 2 + size_of::<ConstIndex>()];
                let const_index = ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
                instr!("SET_TABLE_UP_CK");
                let upval_index = instructions[instruction_pointer + 1];
                print!("{upval_index} ");
                print_const(&consts[const_index as usize]);
                reg!(2 + size_of::<ConstIndex>());
                println!();
                3 + size_of::<ConstIndex>()
            }
            Instruction::GetTable => {
                instr!("GET_TABLE");
                reg!(1);
                reg!(2);
                reg!(3);
                println!();
                4
            }
            Instruction::GetTableConstKey => {
                let const_index_bytes = &instructions
                    [instruction_pointer + 3..instruction_pointer + 3 + size_of::<ConstIndex>()];
                let const_index = ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
                instr!("GET_TABLE_CK");
                reg!(1);
                reg!(2);
                print_const(&consts[const_index as usize]);
                println!();
                3 + size_of::<ConstIndex>()
            }
            Instruction::GetTableUpval => {
                let upval_index = instructions[instruction_pointer + 3];
                instr!("GET_TABLE_UP");
                reg!(1);
                print!("{upval_index}");
                reg!(3);
                println!();
                4
            }
            Instruction::GetTableUpvalConstKey => {
                let const_index_bytes = &instructions
                    [instruction_pointer + 3..instruction_pointer + 3 + size_of::<ConstIndex>()];
                let const_index = ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
                instr!("GET_TABLE_UP_CK");
                reg!(1);
                let upval_index = instructions[instruction_pointer + 2];
                print!("{upval_index} ");
                print_const(&consts[const_index as usize]);
                println!();
                3 + size_of::<ConstIndex>()
            }
            Instruction::AppendToTable => {
                instr!("APPEND_TO_TABLE");
                reg!();
                let num_values = instructions[instruction_pointer + 2];
                println!("{num_values}");
                3
            }
            Instruction::AppendToTableM => {
                instr!("APPEND_TO_TABLE_M");
                reg!();
                let num_values = instructions[instruction_pointer + 2];
                println!("{num_values}");
                3
            }

            // Function
            instr @ Instruction::Call
            | instr @ Instruction::CallM
            | instr @ Instruction::CallT
            | instr @ Instruction::CallTM => {
                let is_single_return = instructions[instruction_pointer + 2] == 1;
                let num_params = instructions[instruction_pointer + 3];
                instr!(match instr {
                    Instruction::Call => "CALL",
                    Instruction::CallM => "CALL_M",
                    Instruction::CallT => "CALL_T",
                    Instruction::CallTM => "CALL_T_M",
                    _ => unreachable!(),
                });
                reg!();
                println!(
                    "{:9} {num_params}",
                    if is_single_return { "single" } else { "multi" },
                );
                4
            }
            Instruction::Return => {
                instr!("RETURN");
                reg!();
                let n = instructions[instruction_pointer + 2];
                println!("{n}");
                3
            }
            Instruction::Return0 => {
                println!("RETURN0");
                1
            }
            Instruction::Return1 => {
                instr!("RETURN1");
                reg!();
                println!();
                2
            }
            Instruction::ReturnM => {
                instr!("RETURN_M");
                reg!();
                let n = instructions[instruction_pointer + 2];
                println!("{n}");
                3
            }

            // Control
            Instruction::Jmp => {
                let offset_bytes = &instructions
                    [instruction_pointer + 1..instruction_pointer + 1 + size_of::<JumpAddr>()];
                let offset = JumpAddr::from_be_bytes(offset_bytes.try_into().unwrap());
                instr!("JMP");
                println!("{offset:04}");
                1 + size_of::<JumpAddr>()
            }
            Instruction::JmpTrue => {
                let offset_bytes = &instructions
                    [instruction_pointer + 2..instruction_pointer + 2 + size_of::<JumpAddr>()];
                let offset = JumpAddr::from_be_bytes(offset_bytes.try_into().unwrap());
                instr!("JMP_TRUE");
                reg!();
                println!("{offset:04}");
                2 + size_of::<JumpAddr>()
            }
            Instruction::JmpFalse => {
                let offset_bytes = &instructions
                    [instruction_pointer + 2..instruction_pointer + 2 + size_of::<JumpAddr>()];
                let offset = JumpAddr::from_be_bytes(offset_bytes.try_into().unwrap());
                instr!("JMP_FALSE");
                reg!();
                println!("{offset:04}");
                2 + size_of::<JumpAddr>()
            }
            Instruction::JmpClose => {
                let offset_bytes = &instructions
                    [instruction_pointer + 2..instruction_pointer + 2 + size_of::<JumpAddr>()];
                let offset = JumpAddr::from_be_bytes(offset_bytes.try_into().unwrap());
                instr!("JMP_CLOSE");
                reg!();
                println!("{offset:04}");
                2 + size_of::<JumpAddr>()
            }

            // Other
            Instruction::Error => {
                let error_code = instructions[instruction_pointer + 1];
                let error = RuntimeError::try_from(error_code).expect("valid error code");
                instr!("ERROR");
                println!("{:?}", error);
                2
            }
        };
        instruction_pointer += instruction_increment;
    }
}

fn print_const(constant: &LuaConst) {
    match constant {
        LuaConst::Nil => print!("NIL"),
        LuaConst::Boolean(b) => print!("{b}"),
        LuaConst::Number(n) => print_number(n),
        LuaConst::String(s) => print!("STRING    \"{}\"", String::from_utf8_lossy(s)),
        LuaConst::Table(table) => print!("TABLE ({} fields)", table.keys().count()),
    }
}

fn print_number(number: &crate::value::LuaNumber) {
    match number {
        crate::value::LuaNumber::Integer(i) => print!("INT       {}", i),
        crate::value::LuaNumber::Float(f) => print!("FLOAT     {}", f),
    }
}
