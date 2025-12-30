#[derive(Debug)]
#[repr(u8)]
pub enum Instruction {
    // Loading values
    LoadConst,
    LoadClosure,
    LoadNil,

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
    SetGlobal,
    GetGlobal,
    Mov,
    SetUpval,
    GetUpval,
    LoadVararg,
    ToClose,
    Close,

    // Table
    NewTable,
    SetTable,
    GetTable,
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
