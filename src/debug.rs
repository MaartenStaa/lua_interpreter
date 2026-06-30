use std::mem::size_of;

use crate::{
    chunk::Chunk,
    error::RuntimeError,
    instruction::{Instruction, OpCode},
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

    for (ip, instruction) in instructions.iter().enumerate() {
        print_instruction(chunk, chunk_index, ip, *instruction);
    }
}

pub(crate) fn print_instruction(
    chunk: &Chunk,
    instruction_pointer: usize,
    instruction: Instruction,
) -> usize {
    macro_rules! reg {
        (1) => {{
            let r = instruction.a();
            reg!(_print r);
        }};
        (2) => {{
            let r = instruction.b();
            reg!(_print r);
        }};
        (3) => {{
            let r = instruction.c();
            reg!(_print r);
        }};
        () => {
            reg!(1);
        };
        (_print $r:expr) => {
            eprint!("\x1b[33m{:>1$}\x1b[0m ", format!("R{}", $r), REG_TEXT_WIDTH);
        }
    }

    eprint!(
        "{instruction_pointer:00$}  \x1b[90m0x{opcode:01$x}\x1b[0m  ",
        IP_WIDTH,
        IP_HEX_WIDTH,
        opcode = instruction.opcode().raw()
    );

    match instruction.opcode() {
        // Stack manipulation
        OpCode::LoadConst => {
            let const_index = instruction.b() as ConstIndex;
            dbg!(&const_index);
            instr!("LOAD_CONST");
            reg!();
            print_const(chunk.get_const(const_index));
            eprintln!();
            2 + size_of::<ConstIndex>()
        }
        OpCode::LoadClosure => {
            let chunk_index = instruction.bc() as ChunkIndex;
            instr!("LOAD_CLOSURE");
            reg!();
            eprintln!("FUNCTION<{name}:{chunk_index:04}>", name = chunk.name);

            2 + size_of::<ChunkIndex>()
        }
        OpCode::LoadNil => {
            instr!("LOAD_NIL");
            reg!(1);
            reg!(2);
            eprintln!();
            3
        }
        OpCode::LoadShort => {
            instr!("LOAD_SHORT");
            reg!();
            let bc = instruction.bc();
            let value = u16::cast_signed(bc);
            eprintln!("SHORT {value}");
            4
        }
        OpCode::LoadShortFloat => {
            instr!("LOAD_SHORT_FLOAT");
            reg!();
            let bc = instruction.bc();
            let value = u16::cast_signed(bc);
            let value = value as f32;
            eprintln!("SHORT_FLOAT {value}");
            4
        }

        // Binary operations
        // Arithmetic
        OpCode::Add => {
            instr!("ADD");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::Sub => {
            instr!("SUB");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::Mul => {
            instr!("MUL");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::Div => {
            instr!("DIV");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::Mod => {
            instr!("MOD");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::Pow => {
            instr!("POW");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::IDiv => {
            instr!("IDIV");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::Band => {
            instr!("BAND");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::Bor => {
            instr!("BOR");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::Bxor => {
            instr!("BXOR");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::Shl => {
            instr!("SHL");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::Shr => {
            instr!("SHR");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }

        // Comparison
        OpCode::Eq => {
            instr!("EQ");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::Ne => {
            instr!("NE");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::Lt => {
            instr!("LT");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::Le => {
            instr!("LE");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }

        // Concatenation
        OpCode::Concat => {
            instr!("CONCAT");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }

        // Unary operations
        OpCode::Neg => {
            instr!("NEG");
            reg!(1);
            reg!(2);
            eprintln!();
            3
        }
        OpCode::Not => {
            instr!("NOT");
            reg!(1);
            reg!(2);
            eprintln!();
            3
        }
        OpCode::Len => {
            instr!("LEN");
            reg!(1);
            reg!(2);
            eprintln!();
            3
        }
        OpCode::BNot => {
            instr!("BNOT");
            reg!(1);
            reg!(2);
            eprintln!();
            3
        }

        // Variables
        OpCode::Mov => {
            instr!("MOV");
            reg!(1);
            reg!(2);
            eprintln!();
            3
        }
        OpCode::SetUpval => {
            let upval_index = instruction.a();
            instr!("SET_UPVAL");
            eprint!("{upval_index}");
            reg!();
            eprintln!();
            3
        }
        OpCode::GetUpval => {
            let upval_index = instruction.b();
            instr!("GET_UPVAL");
            reg!();
            eprintln!("{upval_index}");
            3
        }
        OpCode::LoadVararg => {
            let is_single_value = instruction.b() == 1;
            instr!("LOAD_VARARG");
            reg!();
            eprintln!("{}", if is_single_value { "single" } else { "multi" });
            3
        }
        OpCode::ToClose => {
            instr!("TO_CLOSE");
            reg!();
            eprintln!();
            2
        }
        OpCode::Close => {
            instr!("CLOSE");
            reg!();
            eprintln!();
            2
        }

        // Table
        OpCode::NewTable => {
            instr!("NEW_TABLE");
            reg!();
            eprintln!();
            2
        }
        OpCode::SetTable => {
            instr!("SET_TABLE");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::SetTableConstKey => {
            instr!("SET_TABLE_CK");
            reg!(1);
            let const_index: ConstIndex = instruction.b();
            print_const(chunk.get_const(const_index));
            reg!(3);
            eprintln!();
            3 + size_of::<ConstIndex>()
        }
        OpCode::SetTableUpval => {
            instr!("SET_TABLE_UP");
            reg!(1);
            let upval_index = instruction.b();
            eprint!("{upval_index}");
            reg!(3);
            eprintln!();
            4
        }
        OpCode::SetTableUpvalConstKey => {
            instr!("SET_TABLE_UP_CK");
            let upval_index = instruction.b();
            eprint!("{upval_index} ");
            let const_index: ConstIndex = instruction.b();
            print_const(chunk.get_const(const_index));
            reg!(3);
            eprintln!();
            3 + size_of::<ConstIndex>()
        }
        OpCode::GetTable => {
            instr!("GET_TABLE");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
            4
        }
        OpCode::GetTableConstKey => {
            instr!("GET_TABLE_CK");
            reg!(1);
            reg!(2);
            let const_index: ConstIndex = instruction.c();
            print_const(chunk.get_const(const_index));
            eprintln!();
            3 + size_of::<ConstIndex>()
        }
        OpCode::GetTableUpval => {
            instr!("GET_TABLE_UP");
            reg!(1);
            let upval_index = instruction.b();
            eprint!("{upval_index}");
            reg!(3);
            eprintln!();
            4
        }
        OpCode::GetTableUpvalConstKey => {
            instr!("GET_TABLE_UP_CK");
            reg!(1);
            let upval_index = instruction.b();
            eprint!("{upval_index} ");
            let const_index: ConstIndex = instruction.c();
            print_const(chunk.get_const(const_index));
            eprintln!();
            3 + size_of::<ConstIndex>()
        }
        OpCode::AppendToTable => {
            instr!("APPEND_TO_TABLE");
            reg!();
            let num_values = instruction.b();
            eprintln!("{num_values}");
            3
        }
        OpCode::AppendToTableM => {
            instr!("APPEND_TO_TABLE_M");
            reg!();
            let num_values = instruction.b();
            eprintln!("{num_values}");
            3
        }

        // Function
        instr @ (OpCode::Call | OpCode::CallM | OpCode::CallT | OpCode::CallTM) => {
            let is_single_return = instruction.b() == 1;
            let num_params = instruction.c();
            instr!(match instr {
                OpCode::Call => "CALL",
                OpCode::CallM => "CALL_M",
                OpCode::CallT => "CALL_T",
                OpCode::CallTM => "CALL_T_M",
                _ => unreachable!(),
            });
            reg!();
            eprintln!(
                "{:9} {num_params}",
                if is_single_return { "single" } else { "multi" },
            );
            4
        }
        OpCode::Return => {
            instr!("RETURN");
            reg!();
            let n = instruction.b();
            eprintln!("{n}");
            3
        }
        OpCode::Return0 => {
            instr!("RETURN0");
            eprintln!();
            1
        }
        OpCode::Return1 => {
            instr!("RETURN1");
            reg!();
            eprintln!();
            2
        }
        OpCode::ReturnM => {
            instr!("RETURN_M");
            reg!();
            let n = instruction.b();
            eprintln!("{n}");
            3
        }

        // Control
        OpCode::Jmp => {
            // NOTE: The `JMP` instruction has nothing in slot `A`.
            let offset: JumpAddr = instruction.bc();
            instr!("JMP");
            eprintln!("{offset:04}");
            1 + size_of::<JumpAddr>()
        }
        OpCode::JmpTrue => {
            let offset: JumpAddr = instruction.bc();
            instr!("JMP_TRUE");
            reg!();
            eprintln!("{offset:04}");
            2 + size_of::<JumpAddr>()
        }
        OpCode::JmpFalse => {
            let offset: JumpAddr = instruction.bc();
            instr!("JMP_FALSE");
            reg!();
            eprintln!("{offset:04}");
            2 + size_of::<JumpAddr>()
        }
        OpCode::JmpClose => {
            let offset: JumpAddr = instruction.bc();
            instr!("JMP_CLOSE");
            reg!();
            eprintln!("{offset:04}");
            2 + size_of::<JumpAddr>()
        }

        // Other
        OpCode::Error => {
            let error_code = instruction.a();
            let error = RuntimeError::try_from(error_code).expect("valid error code");
            instr!("ERROR");
            eprintln!("{error:?}");
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
        crate::value::LuaNumber::Integer(i) => eprint!("INT       {i}"),
        crate::value::LuaNumber::Float(f) => eprint!("FLOAT     {f}"),
    }
}
