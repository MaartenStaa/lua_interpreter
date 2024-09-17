#[derive(Debug)]
#[repr(u8)]
pub enum Instruction {
    // Stack operations
    LoadConst,
    Pop,
    Discard,
    Swap,
    Align,
    AlignVararg,

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
    // Unm,
    // BNot,
    // Not,

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
    // SetUpval,
    // GetUpval,
    LoadVararg,

    // Table
    NewTable,
    SetTable,
    GetTable,

    // // Extra
    // Move,
    // Self_,
    // // Function
    // Closure,
    Call,
    Return,

    // Control
    Jmp,
    JmpTrue,
    JmpFalse,
    // JmpRel,
    // Test,

    // // Vararg
    // Vararg,
    // // Extra
    // ExtraArg,
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
