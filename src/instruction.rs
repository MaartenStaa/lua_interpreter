#[derive(Debug)]
#[repr(u8)]
pub enum Instruction {
    // Constants
    LoadConst,

    // Binary operations
    // Arithmetic
    Add,
    Sub,
    Mul,
    Mod,
    Pow,
    Div,
    IDiv,
    // Band,
    // Bor,
    // Bxor,
    // Shl,
    // Shr,
    // Unm,
    // BNot,
    // Not,

    // Comparison
    // Eq,
    // Lt,
    // Le,

    // Concatenation
    Concat,

    // Unary operations
    Neg,
    // // Table
    // NewTable,
    // SetTable,
    // GetTable,
    // // Extra
    // Move,
    // Self_,
    // // Function
    // Closure,
    // Call,
    // Return,
    // // Control
    // Jmp,
    // Test,
    // TestSet,
    // // Upvalue
    // GetUpval,
    // SetUpval,
    // // Vararg
    // Vararg,
    // // Extra
    // ExtraArg,
    //

    // Debug
    Print,

    // Halt
    Halt,
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
