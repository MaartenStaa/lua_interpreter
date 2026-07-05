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

pub fn print_instructions(chunk: &Chunk, chunk_index: ChunkIndex) {
    let instructions = chunk.get_instructions();

    eprintln!("-- BYTECODE -- {} ({})", chunk.name, chunk_index);
    for (ip, instruction) in instructions.iter().enumerate() {
        print_instruction(chunk, chunk_index, ip, *instruction);
    }

    eprintln!();
}

pub(crate) fn print_instruction(
    chunk: &Chunk,
    chunk_index: ChunkIndex,
    instruction_pointer: usize,
    instruction: Instruction,
) {
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
    macro_rules! num_results {
        ($num:expr) => {{
            let n = $num;
            if n == 0 {
                eprint!("\x1b[36mMULTRES\x1b[0m");
            } else {
                eprint!("\x1b[36m{}\x1b[0m", n - 1);
            }
        }};
    }

    eprint!(
        "{chunk_index}:{instruction_pointer:00$}  \x1b[90m0x{opcode:01$x} {instruction:#010x}\x1b[0m  ",
        IP_WIDTH,
        IP_HEX_WIDTH,
        opcode = instruction.opcode().raw(),
        instruction = instruction.raw()
    );

    match instruction.opcode() {
        // Stack manipulation
        OpCode::LoadConst => {
            let const_index = instruction.b() as ConstIndex;
            instr!("LOAD_CONST");
            reg!();
            print_const(chunk.get_const(const_index));
            eprintln!();
        }
        OpCode::LoadClosure => {
            let chunk_index = instruction.bc() as ChunkIndex;
            instr!("LOAD_CLOSURE");
            reg!();
            eprintln!("FUNCTION<{name}:{chunk_index:04}>", name = chunk.name);
        }
        OpCode::LoadNil => {
            instr!("LOAD_NIL");
            reg!(1);
            reg!(2);
            eprintln!();
        }
        OpCode::LoadShort => {
            instr!("LOAD_SHORT");
            reg!();
            let bc = instruction.bc();
            let value = u16::cast_signed(bc);
            eprintln!("SHORT {value}");
        }
        OpCode::LoadShortFloat => {
            instr!("LOAD_SHORT_FLOAT");
            reg!();
            let bc = instruction.bc();
            let value = u16::cast_signed(bc);
            let value = value as f32;
            eprintln!("SHORT_FLOAT {value}");
        }

        // Binary operations
        // Arithmetic
        OpCode::Add => {
            instr!("ADD");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::Sub => {
            instr!("SUB");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::Mul => {
            instr!("MUL");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::Div => {
            instr!("DIV");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::Mod => {
            instr!("MOD");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::Pow => {
            instr!("POW");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::IDiv => {
            instr!("IDIV");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::Band => {
            instr!("BAND");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::Bor => {
            instr!("BOR");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::Bxor => {
            instr!("BXOR");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::Shl => {
            instr!("SHL");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::Shr => {
            instr!("SHR");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }

        // Comparison
        OpCode::Eq => {
            instr!("EQ");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::Ne => {
            instr!("NE");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::Lt => {
            instr!("LT");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::Le => {
            instr!("LE");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }

        // Concatenation
        OpCode::Concat => {
            instr!("CONCAT");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }

        // Unary operations
        OpCode::Neg => {
            instr!("NEG");
            reg!(1);
            reg!(2);
            eprintln!();
        }
        OpCode::Not => {
            instr!("NOT");
            reg!(1);
            reg!(2);
            eprintln!();
        }
        OpCode::Len => {
            instr!("LEN");
            reg!(1);
            reg!(2);
            eprintln!();
        }
        OpCode::BNot => {
            instr!("BNOT");
            reg!(1);
            reg!(2);
            eprintln!();
        }

        // Variables
        OpCode::Mov => {
            instr!("MOV");
            reg!(1);
            reg!(2);
            eprintln!();
        }
        OpCode::SetUpval => {
            let upval_index = instruction.a();
            instr!("SET_UPVAL");
            eprint!("{upval_index}");
            reg!();
            eprintln!();
        }
        OpCode::GetUpval => {
            let upval_index = instruction.b();
            instr!("GET_UPVAL");
            reg!();
            eprintln!("{upval_index}");
        }
        OpCode::LoadVararg => {
            let num = instruction.b();
            instr!("LOAD_VARARG");
            reg!();
            num_results!(num);
            eprintln!();
        }
        OpCode::ToClose => {
            instr!("TO_CLOSE");
            reg!();
            eprintln!();
        }
        OpCode::Close => {
            instr!("CLOSE");
            reg!();
            eprintln!();
        }

        // Table
        OpCode::NewTable => {
            instr!("NEW_TABLE");
            reg!();
            eprintln!();
        }
        OpCode::SetTable => {
            instr!("SET_TABLE");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::SetTableConstKey => {
            instr!("SET_TABLE_CK");
            reg!(1);
            let const_index: ConstIndex = instruction.b();
            print_const(chunk.get_const(const_index));
            reg!(3);
            eprintln!();
        }
        OpCode::SetTableUpval => {
            instr!("SET_TABLE_UP");
            reg!(1);
            let upval_index = instruction.b();
            eprint!("{upval_index}");
            reg!(3);
            eprintln!();
        }
        OpCode::SetTableUpvalConstKey => {
            instr!("SET_TABLE_UP_CK");
            let upval_index = instruction.b();
            eprint!("{upval_index} ");
            let const_index: ConstIndex = instruction.b();
            print_const(chunk.get_const(const_index));
            reg!(3);
            eprintln!();
        }
        OpCode::GetTable => {
            instr!("GET_TABLE");
            reg!(1);
            reg!(2);
            reg!(3);
            eprintln!();
        }
        OpCode::GetTableConstKey => {
            instr!("GET_TABLE_CK");
            reg!(1);
            reg!(2);
            let const_index: ConstIndex = instruction.c();
            print_const(chunk.get_const(const_index));
            eprintln!();
        }
        OpCode::GetTableUpval => {
            instr!("GET_TABLE_UP");
            reg!(1);
            let upval_index = instruction.b();
            eprint!("{upval_index}");
            reg!(3);
            eprintln!();
        }
        OpCode::GetTableUpvalConstKey => {
            instr!("GET_TABLE_UP_CK");
            reg!(1);
            let upval_index = instruction.b();
            eprint!("{upval_index} ");
            let const_index: ConstIndex = instruction.c();
            print_const(chunk.get_const(const_index));
            eprintln!();
        }
        OpCode::AppendToTable => {
            instr!("APPEND_TO_TABLE");
            reg!();
            let num_values = instruction.b();
            eprintln!("{num_values}");
        }
        OpCode::AppendToTableM => {
            instr!("APPEND_TO_TABLE_M");
            reg!();
            let num_values = instruction.b();
            eprintln!("{num_values}");
        }

        // Function
        instr @ (OpCode::Call | OpCode::CallM | OpCode::CallT | OpCode::CallTM) => {
            let num_results = instruction.b();
            let num_params = instruction.c();
            instr!(match instr {
                OpCode::Call => "CALL",
                OpCode::CallM => "CALL_M",
                OpCode::CallT => "CALL_T",
                OpCode::CallTM => "CALL_T_M",
                _ => unreachable!(),
            });
            reg!();
            num_results!(num_results);
            eprintln!(" {num_params}");
        }
        OpCode::Return => {
            instr!("RETURN");
            reg!();
            let n = instruction.b();
            eprintln!("{n}");
        }
        OpCode::Return0 => {
            instr!("RETURN0");
            eprintln!();
        }
        OpCode::Return1 => {
            instr!("RETURN1");
            reg!();
            eprintln!();
        }
        OpCode::ReturnM => {
            instr!("RETURN_M");
            reg!();
            let n = instruction.b();
            eprintln!("{n}");
        }

        // Control
        OpCode::Jmp => {
            // NOTE: The `JMP` instruction has nothing in slot `A`.
            let offset: JumpAddr = instruction.bc();
            instr!("JMP");
            eprintln!("{offset:04}");
        }
        OpCode::JmpTrue => {
            let offset: JumpAddr = instruction.bc();
            instr!("JMP_TRUE");
            reg!();
            eprintln!("{offset:04}");
        }
        OpCode::JmpFalse => {
            let offset: JumpAddr = instruction.bc();
            instr!("JMP_FALSE");
            reg!();
            eprintln!("{offset:04}");
        }
        OpCode::JmpClose => {
            let offset: JumpAddr = instruction.bc();
            instr!("JMP_CLOSE");
            reg!();
            eprintln!("{offset:04}");
        }

        // Other
        OpCode::Error => {
            let error_code = instruction.a();
            let error = RuntimeError::try_from(error_code).expect("valid error code");
            instr!("ERROR");
            eprintln!("{error:?}");
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
