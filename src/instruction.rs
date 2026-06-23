use std::mem::size_of;

use crate::vm::{ChunkIndex, ConstIndex, JumpAddr};

#[derive(Debug)]
#[repr(u8)]
pub enum Instruction {
    // Loading values
    LoadConst,
    LoadClosure,
    LoadNil,
    LoadShort,
    LoadShortFloat,

    // Binary operations
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    IDiv,
    Band,
    Bor,
    Bxor,
    Shl,
    Shr,

    // Comparison
    Eq,
    Ne,
    Lt,
    Le,

    // Concatenation
    Concat,

    // Unary operations
    Neg,
    Not,
    Len,
    BNot,

    // Variables
    Mov,
    SetUpval,
    GetUpval,
    LoadVararg,
    ToClose,
    Close,

    // Table
    NewTable,
    SetTable,
    SetTableConstKey,
    SetTableUpval,
    SetTableUpvalConstKey,
    GetTable,
    GetTableConstKey,
    GetTableUpval,
    GetTableUpvalConstKey,
    AppendToTable,
    AppendToTableM,

    // Function
    Call,
    CallM,
    CallT,
    CallTM,
    Return,
    Return0,
    Return1,
    ReturnM,

    // Control
    Jmp,
    JmpTrue,
    JmpFalse,
    JmpClose,

    // Other
    Error,
}

impl Instruction {
    pub(crate) fn size(self) -> usize {
        match self {
            Self::Return0 => 1,

            Self::ToClose | Self::Close | Self::NewTable | Self::Return1 | Self::Error => 2,

            Self::LoadNil
            | Self::LoadVararg
            | Self::Mov
            | Self::SetUpval
            | Self::GetUpval
            | Self::Neg
            | Self::Not
            | Self::Len
            | Self::BNot
            | Self::Return
            | Self::ReturnM
            | Self::AppendToTable
            | Self::AppendToTableM => 3,

            Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Mod
            | Self::Pow
            | Self::IDiv
            | Self::Band
            | Self::Bor
            | Self::Bxor
            | Self::Shl
            | Self::Shr
            | Self::Eq
            | Self::Ne
            | Self::Lt
            | Self::Le
            | Self::Concat
            | Self::SetTable
            | Self::SetTableUpval
            | Self::GetTable
            | Self::GetTableUpval
            | Self::Call
            | Self::CallM
            | Self::CallT
            | Self::CallTM => 4,

            Self::LoadConst => 2 + size_of::<ConstIndex>(),
            Self::LoadClosure => 2 + size_of::<ChunkIndex>(),
            Self::LoadShort | Self::LoadShortFloat => 2 + size_of::<i16>(),

            Self::Jmp => 1 + size_of::<JumpAddr>(),
            Self::JmpTrue | Self::JmpFalse | Self::JmpClose => 2 + size_of::<JumpAddr>(),

            Self::SetTableConstKey
            | Self::SetTableUpvalConstKey
            | Self::GetTableConstKey
            | Self::GetTableUpvalConstKey => 3 + size_of::<ConstIndex>(),
        }
    }
}

impl From<Instruction> for u8 {
    fn from(instruction: Instruction) -> u8 {
        instruction as u8
    }
}

impl From<u8> for Instruction {
    fn from(byte: u8) -> Instruction {
        unsafe { std::mem::transmute(byte) }
    }
}
