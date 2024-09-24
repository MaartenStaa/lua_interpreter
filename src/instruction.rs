#[derive(Debug)]
#[repr(u8)]
pub enum Instruction {
    // Stack operations
    LoadConst,
    LoadClosure,
    Pop,
    Discard,
    Swap,
    Align,
    AlignVararg,
    DupFromMarker,

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

    // Logical
    And,
    Or,

    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

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
    SetLocal,
    GetLocal,
    SetLocalAttr,
    SetUpval,
    GetUpval,
    LoadVararg,

    // Table
    NewTable,
    SetTable,
    GetTable,
    AppendToTable,

    // Function
    Call,
    Return,

    // Control
    Jmp,
    JmpTrue,
    JmpFalse,
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
