#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum OpCode {
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

impl From<OpCode> for u8 {
    fn from(instruction: OpCode) -> u8 {
        instruction as u8
    }
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> OpCode {
        unsafe { std::mem::transmute(byte) }
    }
}

impl OpCode {
    pub(crate) fn raw(&self) -> u8 {
        *self as u8
    }
}

/// Instructions are 32-bit values that encode an operation and its operands. The first byte is the
/// opcode, and the remaining bytes are the operands.
///
/// Layout A:
/// [opcode: u8, operand1: u8, operand2: u8, operand3: u8]
///
/// Layout B:
/// [opcode: u8, operand1: u8, operand2: u16]
#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Instruction(u32);

impl From<u32> for Instruction {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl Instruction {
    /// Returns the opcode of the instruction.
    #[inline(always)]
    pub(crate) fn opcode(&self) -> OpCode {
        let opcode_byte = (self.0 & 0xFF) as u8;
        OpCode::from(opcode_byte)
    }

    /// Returns the instruction as a 32-bit value.
    pub(crate) fn create(opcode: OpCode, a: u8, b: u8, c: u8) -> Self {
        let opcode_byte = opcode as u32;
        let instruction_value = (opcode_byte & 0xFF)
            | ((a as u32 & 0xFF) << 8)
            | ((b as u32 & 0xFF) << 16)
            | ((c as u32 & 0xFF) << 24);
        Self(instruction_value)
    }

    /// Returns the instruction as a 32-bit value.
    pub(crate) fn create_bc(opcode: OpCode, a: u8, bc: u16) -> Self {
        let opcode_byte = opcode as u32;
        let instruction_value =
            (opcode_byte & 0xFF) | ((a as u32 & 0xFF) << 8) | ((bc as u32 & 0xFFFF) << 16);
        Self(instruction_value)
    }

    /// Returns the first operand of the instruction.
    #[inline(always)]
    pub(crate) fn a(&self) -> u8 {
        ((self.0 >> 8) & 0xFF) as u8
    }

    /// Returns the second operand of the instruction.
    #[inline(always)]
    pub(crate) fn b(&self) -> u8 {
        ((self.0 >> 16) & 0xFF) as u8
    }

    /// Returns the third operand of the instruction.
    #[inline(always)]
    pub(crate) fn c(&self) -> u8 {
        ((self.0 >> 24) & 0xFF) as u8
    }

    /// Returns the second and third operands of the instruction as a single 16-bit value.
    #[inline(always)]
    pub(crate) fn bc(&self) -> u16 {
        ((self.0 >> 16) & 0xFFFF) as u16
    }

    /// Sets the second and third operands of the instruction as a single 16-bit value.
    /// Used for jump instructions, which get patched later on after the bytecode is generated.
    pub(crate) fn set_bc(&mut self, bc: u16) {
        self.0 = (self.0 & 0x0000FFFF) | ((bc as u32 & 0xFFFF) << 16);
    }

    pub(crate) fn raw(&self) -> u32 {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::Bytecode;

    #[test]
    fn test_instruction_encoding() {
        let instruction = Instruction::create(OpCode::Add, 1, 2, 3);
        assert_eq!(instruction.opcode(), OpCode::Add);
        assert_eq!(instruction.a(), 1);
        assert_eq!(instruction.b(), 2);
        assert_eq!(instruction.c(), 3);

        let instruction = Instruction::create(OpCode::LoadConst, 16, 12, 0);
        assert_eq!(instruction.opcode(), OpCode::LoadConst);
        assert_eq!(instruction.a(), 16);
        assert_eq!(instruction.b(), 12);
        assert_eq!(instruction.c(), 0);

        let instruction_bc = Instruction::create_bc(OpCode::Jmp, 4, 0x1234);
        assert_eq!(instruction_bc.opcode(), OpCode::Jmp);
        assert_eq!(instruction_bc.a(), 4);
        assert_eq!(instruction_bc.bc(), 0x1234);

        let mut instruction_patch = instruction_bc;
        instruction_patch.set_bc(0x5678);
        assert_eq!(instruction_patch.bc(), 0x5678);
    }

    #[test]
    fn test_instruction_from_bytecode() {
        let bytecode = Bytecode::LoadClosure {
            register: 12,
            chunk_index: 0x1234,
        };
        let instruction = bytecode.into_bytes();

        assert_eq!(instruction.opcode(), OpCode::LoadClosure);
        assert_eq!(instruction.a(), 12);
        assert_eq!(instruction.bc(), 0x1234);
    }
}
