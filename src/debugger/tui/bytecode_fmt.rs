use crate::{
    instruction::{Instruction, OpCode},
    value::LuaConst,
    vm::{ChunkIndex, ConstIndex},
};

use super::super::protocol::ChunkSnapshot;

#[derive(Debug, Clone)]
pub(crate) struct BytecodeLine {
    pub ip: usize,
    pub address: String,
    pub raw: String,
    pub opcode_name: String,
    pub operands: String,
    pub source_line: Option<usize>,
}

pub(crate) fn build_bytecode_lines(
    chunk: &ChunkSnapshot,
    chunk_index: ChunkIndex,
    ip_to_line: &std::collections::HashMap<usize, usize>,
) -> Vec<BytecodeLine> {
    chunk
        .instructions
        .iter()
        .enumerate()
        .map(|(ip, instr)| {
            let address = format!("{chunk_index}:{ip:04}");
            let raw = format!("{:#010x}", instr.raw());
            let opcode_name = opcode_display_name(instr.opcode());
            let operands = format_operands(chunk, *instr);
            BytecodeLine {
                ip,
                address,
                raw,
                opcode_name,
                operands,
                source_line: ip_to_line.get(&ip).copied(),
            }
        })
        .collect()
}

fn opcode_display_name(opcode: OpCode) -> String {
    match opcode {
        OpCode::LoadConst => "LOAD_CONST",
        OpCode::LoadClosure => "LOAD_CLOSURE",
        OpCode::LoadNil => "LOAD_NIL",
        OpCode::LoadShort => "LOAD_SHORT",
        OpCode::LoadShortFloat => "LOAD_SHORT_FLOAT",
        OpCode::Add => "ADD",
        OpCode::Sub => "SUB",
        OpCode::Mul => "MUL",
        OpCode::Div => "DIV",
        OpCode::Mod => "MOD",
        OpCode::Pow => "POW",
        OpCode::IDiv => "IDIV",
        OpCode::Band => "BAND",
        OpCode::Bor => "BOR",
        OpCode::Bxor => "BXOR",
        OpCode::Shl => "SHL",
        OpCode::Shr => "SHR",
        OpCode::Eq => "EQ",
        OpCode::Ne => "NE",
        OpCode::Lt => "LT",
        OpCode::Le => "LE",
        OpCode::Concat => "CONCAT",
        OpCode::Neg => "NEG",
        OpCode::Not => "NOT",
        OpCode::Len => "LEN",
        OpCode::BNot => "BNOT",
        OpCode::Mov => "MOV",
        OpCode::SetUpval => "SET_UPVAL",
        OpCode::GetUpval => "GET_UPVAL",
        OpCode::LoadVararg => "LOAD_VARARG",
        OpCode::ToClose => "TO_CLOSE",
        OpCode::Close => "CLOSE",
        OpCode::NewTable => "NEW_TABLE",
        OpCode::SetTable => "SET_TABLE",
        OpCode::SetTableConstKey => "SET_TABLE_CK",
        OpCode::SetTableUpval => "SET_TABLE_UP",
        OpCode::SetTableUpvalConstKey => "SET_TABLE_UP_CK",
        OpCode::GetTable => "GET_TABLE",
        OpCode::GetTableConstKey => "GET_TABLE_CK",
        OpCode::GetTableUpval => "GET_TABLE_UP",
        OpCode::GetTableUpvalConstKey => "GET_TABLE_UP_CK",
        OpCode::AppendToTable => "APPEND_TO_TABLE",
        OpCode::AppendToTableM => "APPEND_TO_TABLE_M",
        OpCode::Call => "CALL",
        OpCode::CallM => "CALL_M",
        OpCode::CallT => "CALL_T",
        OpCode::CallTM => "CALL_T_M",
        OpCode::Return => "RETURN",
        OpCode::Return0 => "RETURN0",
        OpCode::Return1 => "RETURN1",
        OpCode::ReturnM => "RETURN_M",
        OpCode::Jmp => "JMP",
        OpCode::JmpTrue => "JMP_TRUE",
        OpCode::JmpFalse => "JMP_FALSE",
        OpCode::JmpClose => "JMP_CLOSE",
        OpCode::Error => "ERROR",
    }
    .to_string()
}

fn format_operands(chunk: &ChunkSnapshot, instr: Instruction) -> String {
    let r = |n: u8| format!("R{n}");
    let num_results = |n: u8| {
        if n == 0 {
            "MULTRES".to_string()
        } else {
            format!("{}", n - 1)
        }
    };
    let fmt_const = |idx: ConstIndex| -> String {
        match &chunk.consts[idx as usize] {
            LuaConst::Nil => "nil".to_string(),
            LuaConst::Boolean(b) => format!("{b}"),
            LuaConst::Number(n) => format!("{n}"),
            LuaConst::String(s) => format!("\"{}\"", String::from_utf8_lossy(s)),
            LuaConst::Table(t) => format!("table({} fields)", t.keys().count()),
        }
    };

    match instr.opcode() {
        OpCode::LoadConst => format!("{} {}", r(instr.a()), fmt_const(instr.b())),
        OpCode::LoadClosure => format!("{} chunk:{}", r(instr.a()), instr.bc()),
        OpCode::LoadNil => format!("{} {}", r(instr.a()), r(instr.b())),
        OpCode::LoadShort => format!("{} {}", r(instr.a()), instr.bc().cast_signed()),
        OpCode::LoadShortFloat => {
            format!("{} {}", r(instr.a()), f32::from(instr.bc().cast_signed()))
        }
        OpCode::Add
        | OpCode::Sub
        | OpCode::Mul
        | OpCode::Div
        | OpCode::Mod
        | OpCode::Pow
        | OpCode::IDiv
        | OpCode::Band
        | OpCode::Bor
        | OpCode::Bxor
        | OpCode::Shl
        | OpCode::Shr
        | OpCode::Eq
        | OpCode::Ne
        | OpCode::Lt
        | OpCode::Le
        | OpCode::Concat
        | OpCode::SetTable
        | OpCode::GetTable => {
            format!("{} {} {}", r(instr.a()), r(instr.b()), r(instr.c()))
        }
        OpCode::Neg | OpCode::Not | OpCode::Len | OpCode::BNot | OpCode::Mov => {
            format!("{} {}", r(instr.a()), r(instr.b()))
        }
        OpCode::SetUpval => format!("U{} {}", instr.a(), r(instr.b())),
        OpCode::GetUpval => format!("{} U{}", r(instr.a()), instr.b()),
        OpCode::LoadVararg => format!("{} {}", r(instr.a()), num_results(instr.b())),
        OpCode::ToClose | OpCode::Close | OpCode::NewTable | OpCode::Return1 => r(instr.a()),
        OpCode::SetTableConstKey => {
            format!("{} {} {}", r(instr.a()), fmt_const(instr.b()), r(instr.c()))
        }
        OpCode::SetTableUpval => format!("{} U{} {}", r(instr.a()), instr.b(), r(instr.c())),
        OpCode::SetTableUpvalConstKey => {
            format!("U{} {} {}", instr.a(), fmt_const(instr.b()), r(instr.c()))
        }
        OpCode::GetTableConstKey => {
            format!("{} {} {}", r(instr.a()), r(instr.b()), fmt_const(instr.c()))
        }
        OpCode::GetTableUpval => format!("{} U{} {}", r(instr.a()), instr.b(), r(instr.c())),
        OpCode::GetTableUpvalConstKey => {
            format!("{} U{} {}", r(instr.a()), instr.b(), fmt_const(instr.c()))
        }
        OpCode::AppendToTable | OpCode::AppendToTableM => {
            format!("{} {}", r(instr.a()), instr.b())
        }
        OpCode::Call | OpCode::CallM | OpCode::CallT | OpCode::CallTM => {
            format!("{} {} {}", r(instr.a()), num_results(instr.b()), instr.c())
        }
        OpCode::Return | OpCode::ReturnM => format!("{} {}", r(instr.a()), instr.b()),
        OpCode::Return0 => String::new(),
        OpCode::Jmp => format!("{:04}", instr.bc()),
        OpCode::JmpTrue | OpCode::JmpFalse | OpCode::JmpClose => {
            format!("{} {:04}", r(instr.a()), instr.bc())
        }
        OpCode::Error => format!("{}", instr.a()),
    }
}
