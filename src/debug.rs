use std::mem::size_of;

use crate::{
    chunk::Chunk,
    error::RuntimeError,
    instruction::Instruction,
    value::LuaConst,
    vm::{ChunkIndex, ConstIndex, JumpAddr},
};

const IP_WIDTH: usize = 4;
const IP_HEX_WIDTH: usize = 2;
const IP_TEXT_WIDTH: usize = 18;
const REG_TEXT_WIDTH: usize = 4;

macro_rules! instr {
    ($name:expr) => {
        eprint!("\x1b[34m{:01$}\x1b[0m", $name, IP_TEXT_WIDTH);
    };
}

pub fn print_instructions(chunk: &Chunk) {
    let instructions = chunk.get_instructions();
    let mut instruction_pointer = 0;

    loop {
        if instruction_pointer >= instructions.len() {
            break;
        }

        let instruction = instructions[instruction_pointer];

        let instruction_increment = print_instruction(
            chunk,
            instructions,
            instruction_pointer,
            instruction,
        );
        instruction_pointer += instruction_increment;
    }
}

pub(crate) fn print_instruction(
    chunk: &Chunk,
    instruction_pointer: usize,
    instruction: u8,
) -> usize {
    let instructions = &chunk.instructions;
    macro_rules! reg {
        ($offset:expr) => {{
            let r = instructions[instruction_pointer + $offset];
            eprint!("\x1b[33m{:>1$}\x1b[0m ", format!("R{}", r), REG_TEXT_WIDTH);
        }};
        () => {
            reg!(1);
        };
    }

    eprint!(
        "{instruction_pointer:00$}  \x1b[90m0x{instruction:01$x}\x1b[0m  ",
        IP_WIDTH, IP_HEX_WIDTH
    );

    match Instruction::from(instruction) {
        // Stack manipulation
        Instruction::LoadConst => {
            let const_index_bytes = &instructions
                [instruction_pointer + 2..instruction_pointer + 2 + size_of::<ConstIndex>()];
            let const_index = ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
            instr!("LOAD_CONST");
            reg!();
            print_const(chunk.get_const(const_index));
            eprintln!();
            2 + size_of::<ConstIndex>()
        }
        Instruction::LoadClosure => {
            let const_index_bytes = &instructions
                [instruction_pointer + 2..instruction_pointer + 2 + size_of::<ConstIndex>()];
            let chunk_index = ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
            instr!("LOAD_CLOSURE");
            reg!();
            eprintln!("FUNCTION<{name}:{chunk_index:04}>", name = chunk.name);

            2 + size_of::<ChunkIndex>()
        }
        Instruction::LoadNil => {
            instr!("LOAD_NIL");
            reg!(1);
            reg!(2);
            eprintln!();
            3
        }
        Instruction::LoadShort => {
            instr!("LOAD_SHORT");
            reg!();
            let value_bytes =
                &instructions[instruction_pointer + 2..instruction_pointer + 2 + size_of::<i16>()];
            let value = i16::from_be_bytes(value_bytes.try_into().unwrap());
            eprintln!("SHORT {value}");
            4
        }
        Instruction::LoadShortFloat => {
            instr!("LOAD_SHORT_FLOAT");
            reg!();
            let value_bytes =
                &instructions[instruction_pointer + 2..instruction_pointer + 2 + size_of::<i16>()];
            let value = i16::from_be_bytes(value_bytes.try_into().unwrap());
            let value = value as f32;
            eprintln!("SHORT_FLOAT {value}");
            4
        }

        // Binary operations
        // Arithmetic
        Instruction::Add => {
            instr!("ADD");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::Sub => {
            instr!("SUB");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::Mul => {
            instr!("MUL");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::Div => {
            instr!("DIV");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::Mod => {
            instr!("MOD");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::Pow => {
            instr!("POW");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::IDiv => {
            instr!("IDIV");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::Band => {
            instr!("BAND");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::Bor => {
            instr!("BOR");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::Bxor => {
            instr!("BXOR");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::Shl => {
            instr!("SHL");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::Shr => {
            instr!("SHR");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }

        // Comparison
        Instruction::Eq => {
            instr!("EQ");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::Ne => {
            instr!("NE");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::Lt => {
            instr!("LT");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::Le => {
            instr!("LE");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }

        // Concatenation
        Instruction::Concat => {
            instr!("CONCAT");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }

        // Unary operations
        Instruction::Neg => {
            instr!("NEG");
            reg!(1);
            reg!(2);
            eprintln!();
            3
        }
        Instruction::Not => {
            instr!("NOT");
            reg!(1);
            reg!(2);
            eprintln!();
            3
        }
        Instruction::Len => {
            instr!("LEN");
            reg!(1);
            reg!(2);
            eprintln!();
            3
        }
        Instruction::BNot => {
            instr!("BNOT");
            reg!(1);
            reg!(2);
            eprintln!();
            3
        }

        // Variables
        Instruction::Mov => {
            instr!("MOV");
            reg!(1);
            reg!(2);
            eprintln!();
            3
        }
        Instruction::SetUpval => {
            let upval_index = instructions[instruction_pointer + 1];
            instr!("SET_UPVAL");
            eprint!("{upval_index}");
            reg!();
            eprintln!();
            3
        }
        Instruction::GetUpval => {
            let upval_index = instructions[instruction_pointer + 2];
            instr!("GET_UPVAL");
            reg!();
            eprintln!("{upval_index}");
            3
        }
        Instruction::LoadVararg => {
            let is_single_value = instructions[instruction_pointer + 2] == 1;
            instr!("LOAD_VARARG");
            reg!();
            eprintln!("{}", if is_single_value { "single" } else { "multi" });
            3
        }
        Instruction::ToClose => {
            instr!("TO_CLOSE");
            reg!();
            eprintln!();
            2
        }
        Instruction::Close => {
            instr!("CLOSE");
            reg!();
            eprintln!();
            2
        }

        // Table
        Instruction::NewTable => {
            instr!("NEW_TABLE");
            reg!();
            eprintln!();
            2
        }
        Instruction::SetTable => {
            instr!("SET_TABLE");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::SetTableConstKey => {
            let const_index_bytes = &instructions
                [instruction_pointer + 2..instruction_pointer + 2 + size_of::<ConstIndex>()];
            let const_index = ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
            instr!("SET_TABLE_CK");
            reg!(1);
            print_const(chunk.get_const(const_index));
            reg!(2 + size_of::<ConstIndex>());
            eprintln!();
            3 + size_of::<ConstIndex>()
        }
        Instruction::SetTableUpval => {
            let upval_index = instructions[instruction_pointer + 2];
            instr!("SET_TABLE_UP");
            reg!(1);
            eprint!("{upval_index}");
            reg!(3);
            eprintln!();
            4
        }
        Instruction::SetTableUpvalConstKey => {
            let const_index_bytes = &instructions
                [instruction_pointer + 2..instruction_pointer + 2 + size_of::<ConstIndex>()];
            let const_index = ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
            instr!("SET_TABLE_UP_CK");
            let upval_index = instructions[instruction_pointer + 1];
            eprint!("{upval_index} ");
            print_const(chunk.get_const(const_index));
            reg!(2 + size_of::<ConstIndex>());
            eprintln!();
            3 + size_of::<ConstIndex>()
        }
        Instruction::GetTable => {
            instr!("GET_TABLE");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        Instruction::GetTableConstKey => {
            let const_index_bytes = &instructions
                [instruction_pointer + 3..instruction_pointer + 3 + size_of::<ConstIndex>()];
            let const_index = ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
            instr!("GET_TABLE_CK");
            reg!(1);
            reg!(2);
            print_const(chunk.get_const(const_index));
            eprintln!();
            3 + size_of::<ConstIndex>()
        }
        Instruction::GetTableUpval => {
            let upval_index = instructions[instruction_pointer + 3];
            instr!("GET_TABLE_UP");
            reg!(1);
            eprint!("{upval_index}");
            reg!(3);
            eprintln!();
            4
        }
        Instruction::GetTableUpvalConstKey => {
            let const_index_bytes = &instructions
                [instruction_pointer + 3..instruction_pointer + 3 + size_of::<ConstIndex>()];
            let const_index = ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
            instr!("GET_TABLE_UP_CK");
            reg!(1);
            let upval_index = instructions[instruction_pointer + 2];
            eprint!("{upval_index} ");
            print_const(chunk.get_const(const_index));
            eprintln!();
            3 + size_of::<ConstIndex>()
        }
        Instruction::AppendToTable => {
            instr!("APPEND_TO_TABLE");
            reg!();
            let num_values = instructions[instruction_pointer + 2];
            eprintln!("{num_values}");
            3
        }
        Instruction::AppendToTableM => {
            instr!("APPEND_TO_TABLE_M");
            reg!();
            let num_values = instructions[instruction_pointer + 2];
            eprintln!("{num_values}");
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
            eprintln!(
                "{:9} {num_params}",
                if is_single_return { "single" } else { "multi" },
            );
            4
        }
        Instruction::Return => {
            instr!("RETURN");
            reg!();
            let n = instructions[instruction_pointer + 2];
            eprintln!("{n}");
            3
        }
        Instruction::Return0 => {
            instr!("RETURN0");
            eprintln!();
            1
        }
        Instruction::Return1 => {
            instr!("RETURN1");
            reg!();
            eprintln!();
            2
        }
        Instruction::ReturnM => {
            instr!("RETURN_M");
            reg!();
            let n = instructions[instruction_pointer + 2];
            eprintln!("{n}");
            3
        }

        // Control
        Instruction::Jmp => {
            let offset_bytes = &instructions
                [instruction_pointer + 1..instruction_pointer + 1 + size_of::<JumpAddr>()];
            let offset = JumpAddr::from_be_bytes(offset_bytes.try_into().unwrap());
            instr!("JMP");
            eprintln!("{offset:04}");
            1 + size_of::<JumpAddr>()
        }
        Instruction::JmpTrue => {
            let offset_bytes = &instructions
                [instruction_pointer + 2..instruction_pointer + 2 + size_of::<JumpAddr>()];
            let offset = JumpAddr::from_be_bytes(offset_bytes.try_into().unwrap());
            instr!("JMP_TRUE");
            reg!();
            eprintln!("{offset:04}");
            2 + size_of::<JumpAddr>()
        }
        Instruction::JmpFalse => {
            let offset_bytes = &instructions
                [instruction_pointer + 2..instruction_pointer + 2 + size_of::<JumpAddr>()];
            let offset = JumpAddr::from_be_bytes(offset_bytes.try_into().unwrap());
            instr!("JMP_FALSE");
            reg!();
            eprintln!("{offset:04}");
            2 + size_of::<JumpAddr>()
        }
        Instruction::JmpClose => {
            let offset_bytes = &instructions
                [instruction_pointer + 2..instruction_pointer + 2 + size_of::<JumpAddr>()];
            let offset = JumpAddr::from_be_bytes(offset_bytes.try_into().unwrap());
            instr!("JMP_CLOSE");
            reg!();
            eprintln!("{offset:04}");
            2 + size_of::<JumpAddr>()
        }

        // Other
        Instruction::Error => {
            let error_code = instructions[instruction_pointer + 1];
            let error = RuntimeError::try_from(error_code).expect("valid error code");
            instr!("ERROR");
            eprintln!("{:?}", error);
            2
        }
    }
}

fn print_const(constant: &LuaConst) {
    match constant {
        LuaConst::Nil => eprint!("NIL"),
        LuaConst::Boolean(b) => eprint!("{b}"),
        LuaConst::Number(n) => print_number(n),
        LuaConst::String(s) => eprint!("STRING    \"{}\"", String::from_utf8_lossy(s)),
        LuaConst::Table(table) => eprint!("TABLE ({} fields)", table.keys().count()),
    }
}

fn print_number(number: &crate::value::LuaNumber) {
    match number {
        crate::value::LuaNumber::Integer(i) => eprint!("INT       {}", i),
        crate::value::LuaNumber::Float(f) => eprint!("FLOAT     {}", f),
    }
}
