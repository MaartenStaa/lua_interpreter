use crate::compiler::ExpressionResultMode;
use crate::instruction::Instruction;
use crate::vm::{ChunkIndex, ConstIndex, JumpAddr};

#[derive(Debug)]
pub(crate) enum Bytecode {
    // Loading values
    /// Load a constant known at compile time into a register.
    LoadConst {
        register: u8,
        const_index: ConstIndex,
    },
    /// Load a closure (function) into a register, and specify its upvalues.
    LoadClosure {
        register: u8,
        chunk_index: ChunkIndex,
    },
    /// Load `nil` into a range of registers.
    LoadNil {
        from_register: u8,
        to_register: u8,
    },
    /// Load an integer (16 bytes) into a register.
    LoadShort {
        register: u8,
        value: i16,
    },
    /// Load an integral float (16 bytes) into a register. Any floating point number that has an
    /// integer representation that fits in 16 bits can be loaded with this instruction, which is
    /// more compact than loading it as a constant.
    LoadShortFloat {
        register: u8,
        value: i16,
    },

    // Binary operations
    Add {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },
    Sub {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },
    Mul {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },
    Div {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },
    Mod {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },
    Pow {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },
    IDiv {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },
    Band {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },
    Bor {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },
    Bxor {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },
    Shl {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },
    Shr {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },

    // Comparison
    Eq {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },
    Ne {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },
    Lt {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },
    Le {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },

    // Concatenation
    Concat {
        dest_register: u8,
        left_register: u8,
        right_register: u8,
    },

    // Unary operations
    Neg {
        dest_register: u8,
        src_register: u8,
    },
    Not {
        dest_register: u8,
        src_register: u8,
    },
    Len {
        dest_register: u8,
        src_register: u8,
    },
    BNot {
        dest_register: u8,
        src_register: u8,
    },

    // Variables
    /// Move/copy a value from one register to another.
    Mov {
        dest_register: u8,
        src_register: u8,
    },
    /// Copy a local known never to be captured into a fresh temporary.
    MovDirect {
        dest_register: u8,
        src_register: u8,
    },

    /// Set an upvalue from a register.
    SetUpval {
        upval_index: u8,
        src_register: u8,
    },
    /// Load an upvalue into a register.
    GetUpval {
        dest_register: u8,
        upval_index: u8,
    },
    /// Load vararg values into registers. Can choose to either load a single value,
    /// or all of them.
    LoadVararg {
        dest_register: u8,
        result_mode: ExpressionResultMode,
    },
    /// Mark a register as having a <close> attribute.
    ToClose {
        register: u8,
    },
    /// Close all registers from a given register upwards that have the <close> attribute.
    Close {
        from_register: u8,
    },

    // Tables
    /// Create a new empty table and store it in a register.
    NewTable {
        dest_register: u8,
    },
    /// Set a key-value pair in a table.
    SetTable {
        table_register: u8,
        key_register: u8,
        value_register: u8,
    },
    /// Set a key-value pair in a table, where the key is a constant.
    SetTableConstKey {
        table_register: u8,
        key_const_index: ConstIndex,
        value_register: u8,
    },
    /// Set a key-value pair in an upvalue table.
    SetTableUpval {
        table_upval_index: u8,
        key_register: u8,
        value_register: u8,
    },
    /// Set a key-value pair in an upvalue table, where the key is a constant.
    SetTableUpvalConstKey {
        table_upval_index: u8,
        key_const_index: ConstIndex,
        value_register: u8,
    },
    /// Get a value from a table by key.
    GetTable {
        dest_register: u8,
        table_register: u8,
        key_register: u8,
    },
    /// Get a value from a table by a constant key
    GetTableConstKey {
        dest_register: u8,
        table_register: u8,
        key_const_index: ConstIndex,
    },
    /// Get a value from an upvalue table
    GetTableUpval {
        dest_register: u8,
        table_upval_index: u8,
        key_register: u8,
    },
    /// Get a value from an upvalue table by a constant key
    GetTableUpvalConstKey {
        dest_register: u8,
        table_upval_index: u8,
        key_const_index: ConstIndex,
    },
    /// Append values to the end of a table (array part).
    AppendToTable {
        table_register: u8,
        num_values: u8,
    },
    /// Append multiple values to the end of a table (array part).
    AppendToTableM {
        table_register: u8,
        num_values: u8,
    },

    // Functions
    Call {
        func_register: u8,
        result_mode: ExpressionResultMode,
        num_args: u8,
    },
    CallM {
        func_register: u8,
        result_mode: ExpressionResultMode,
        num_args: u8,
    },
    CallT {
        args_from_register: u8,
        result_mode: ExpressionResultMode,
        num_args: u8,
    },
    CallTM {
        args_from_register: u8,
        result_mode: ExpressionResultMode,
        num_args: u8,
    },
    Return {
        from_register: u8,
        num_values: u8,
    },
    Return0,
    Return1 {
        src_register: u8,
    },
    ReturnM {
        from_register: u8,
        num_fixed_values: u8,
    },

    // Loops
    /// Prepare a numeric for loop. Validates that all three values (initial, limit, step) in
    /// consecutive registers starting at `base_register` are numbers, coerces to float if any is
    /// a float, checks that step is not zero, and jumps to `address` if the initial value already
    /// exceeds the limit.
    ForPrep {
        base_register: u8,
        address: JumpAddr,
    },
    /// Numeric for loop step. Increments R[`base_register`] by R[`base_register`+2] (the step), then
    /// checks if the loop variable is still within the limit. If so, jumps to `address` (the loop
    /// body). Otherwise falls through (exits the loop).
    ForLoop {
        base_register: u8,
        address: JumpAddr,
    },

    // Control
    Jmp {
        address: JumpAddr,
    },
    JmpTrue {
        condition_register: u8,
        address: JumpAddr,
    },
    JmpFalse {
        condition_register: u8,
        address: JumpAddr,
    },
    JmpClose {
        close_from_register: u8,
        address: JumpAddr,
    },
}

macro_rules! bytes {
    ($instruction:ident) => {{ $crate::instruction::Instruction::create($crate::instruction::OpCode::$instruction, 0, 0, 0) }};
    ($instruction:ident, bytes = $bc:expr) => {{
        $crate::instruction::Instruction::create_bc(
            $crate::instruction::OpCode::$instruction,
            0,
            $bc,
        )
    }};
    ($instruction:ident, $a:expr, bytes = $bc:expr) => {{
        $crate::instruction::Instruction::create_bc(
            $crate::instruction::OpCode::$instruction,
            $a,
            $bc,
        )
    }};

    ($instruction:ident, $a:expr) => {{
        $crate::instruction::Instruction::create(
            $crate::instruction::OpCode::$instruction,
            $a,
            0,
            0,
        )
    }};
    ($instruction:ident, $a:expr, $b:expr) => {{
        $crate::instruction::Instruction::create(
            $crate::instruction::OpCode::$instruction,
            $a,
            $b,
            0,
        )
    }};
    ($instruction:ident, $a:expr, $b:expr, $c:expr) => {{
        $crate::instruction::Instruction::create(
            $crate::instruction::OpCode::$instruction,
            $a,
            $b,
            $c,
        )
    }};
}

impl Bytecode {
    fn expression_result_mode_to_byte(mode: ExpressionResultMode) -> u8 {
        match mode {
            ExpressionResultMode::Amount(n) => {
                debug_assert!(
                    n < u8::MAX as usize,
                    "ExpressionResultMode::Amount must be < 255"
                );
                (n + 1) as u8
            }
            ExpressionResultMode::Multres => 0,
        }
    }

    pub fn into_bytes(self) -> Instruction {
        match self {
            // Loading values
            Bytecode::LoadConst {
                register,
                const_index,
            } => {
                bytes!(LoadConst, register, const_index)
            }
            Bytecode::LoadClosure {
                register,
                chunk_index,
            } => {
                bytes!(LoadClosure, register, bytes = chunk_index)
            }
            Bytecode::LoadNil {
                from_register,
                to_register,
            } => {
                bytes!(LoadNil, from_register, to_register)
            }
            Bytecode::LoadShort { register, value } => {
                bytes!(LoadShort, register, bytes = i16::cast_unsigned(value))
            }
            Bytecode::LoadShortFloat { register, value } => {
                bytes!(LoadShortFloat, register, bytes = i16::cast_unsigned(value))
            }

            // Binary operations
            Self::Add {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Add, dest_register, left_register, right_register)
            }
            Self::Sub {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Sub, dest_register, left_register, right_register)
            }
            Self::Mul {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Mul, dest_register, left_register, right_register)
            }
            Self::Div {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Div, dest_register, left_register, right_register)
            }
            Self::Mod {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Mod, dest_register, left_register, right_register)
            }
            Self::Pow {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Pow, dest_register, left_register, right_register)
            }
            Self::IDiv {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(IDiv, dest_register, left_register, right_register)
            }
            Self::Band {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Band, dest_register, left_register, right_register)
            }
            Self::Bor {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Bor, dest_register, left_register, right_register)
            }
            Self::Bxor {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Bxor, dest_register, left_register, right_register)
            }
            Self::Shl {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Shl, dest_register, left_register, right_register)
            }
            Self::Shr {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Shr, dest_register, left_register, right_register)
            }

            // Comparison
            Self::Eq {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Eq, dest_register, left_register, right_register)
            }
            Self::Ne {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Ne, dest_register, left_register, right_register)
            }
            Self::Lt {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Lt, dest_register, left_register, right_register)
            }
            Self::Le {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Le, dest_register, left_register, right_register)
            }

            // Concatenation
            Self::Concat {
                dest_register,
                left_register,
                right_register,
            } => {
                bytes!(Concat, dest_register, left_register, right_register)
            }

            // Unary operations
            Self::Neg {
                dest_register,
                src_register,
            } => {
                bytes!(Neg, dest_register, src_register)
            }
            Self::Not {
                dest_register,
                src_register,
            } => {
                bytes!(Not, dest_register, src_register)
            }
            Self::Len {
                dest_register,
                src_register,
            } => {
                bytes!(Len, dest_register, src_register)
            }
            Self::BNot {
                dest_register,
                src_register,
            } => {
                bytes!(BNot, dest_register, src_register)
            }

            // Variables
            Self::MovDirect {
                dest_register,
                src_register,
            } => {
                bytes!(MovDirect, dest_register, src_register)
            }
            Self::Mov {
                dest_register,
                src_register,
            } => {
                bytes!(Mov, dest_register, src_register)
            }
            Self::SetUpval {
                upval_index,
                src_register,
            } => {
                bytes!(SetUpval, upval_index, src_register)
            }
            Self::GetUpval {
                dest_register,
                upval_index,
            } => {
                bytes!(GetUpval, dest_register, upval_index)
            }
            Self::LoadVararg {
                dest_register,
                result_mode,
            } => {
                bytes!(
                    LoadVararg,
                    dest_register,
                    Self::expression_result_mode_to_byte(result_mode)
                )
            }
            Self::ToClose { register } => {
                bytes!(ToClose, register)
            }
            Self::Close { from_register } => {
                bytes!(Close, from_register)
            }

            // Tables
            Self::NewTable { dest_register } => {
                bytes!(NewTable, dest_register)
            }
            Self::SetTable {
                table_register,
                key_register,
                value_register,
            } => {
                bytes!(SetTable, table_register, key_register, value_register)
            }
            Self::SetTableConstKey {
                table_register,
                key_const_index,
                value_register,
            } => {
                bytes!(
                    SetTableConstKey,
                    table_register,
                    key_const_index,
                    value_register
                )
            }
            Self::SetTableUpval {
                table_upval_index,
                key_register,
                value_register,
            } => {
                bytes!(
                    SetTableUpval,
                    table_upval_index,
                    key_register,
                    value_register
                )
            }
            Self::SetTableUpvalConstKey {
                table_upval_index,
                key_const_index,
                value_register,
            } => {
                bytes!(
                    SetTableUpvalConstKey,
                    table_upval_index,
                    key_const_index,
                    value_register
                )
            }
            Self::GetTable {
                dest_register,
                table_register,
                key_register,
            } => {
                bytes!(GetTable, dest_register, table_register, key_register)
            }
            Self::GetTableConstKey {
                dest_register,
                table_register,
                key_const_index,
            } => {
                bytes!(
                    GetTableConstKey,
                    dest_register,
                    table_register,
                    key_const_index
                )
            }
            Self::GetTableUpval {
                dest_register,
                table_upval_index,
                key_register,
            } => {
                bytes!(
                    GetTableUpval,
                    dest_register,
                    table_upval_index,
                    key_register
                )
            }
            Self::GetTableUpvalConstKey {
                dest_register,
                table_upval_index,
                key_const_index,
            } => {
                bytes!(
                    GetTableUpvalConstKey,
                    dest_register,
                    table_upval_index,
                    key_const_index
                )
            }
            Self::AppendToTable {
                table_register,
                num_values,
            } => {
                bytes!(AppendToTable, table_register, num_values)
            }
            Self::AppendToTableM {
                table_register,
                num_values,
            } => {
                bytes!(AppendToTableM, table_register, num_values)
            }

            // Functions
            Self::Call {
                func_register,
                result_mode,
                num_args,
            } => {
                bytes!(
                    Call,
                    func_register,
                    Self::expression_result_mode_to_byte(result_mode),
                    num_args
                )
            }
            Self::CallM {
                func_register,
                result_mode,
                num_args,
            } => {
                bytes!(
                    CallM,
                    func_register,
                    Self::expression_result_mode_to_byte(result_mode),
                    num_args
                )
            }
            Self::CallT {
                args_from_register,
                result_mode,
                num_args,
            } => {
                bytes!(
                    CallT,
                    args_from_register,
                    Self::expression_result_mode_to_byte(result_mode),
                    num_args
                )
            }
            Self::CallTM {
                args_from_register,
                result_mode,
                num_args,
            } => {
                bytes!(
                    CallTM,
                    args_from_register,
                    Self::expression_result_mode_to_byte(result_mode),
                    num_args
                )
            }
            Self::Return {
                from_register,
                num_values,
            } => {
                bytes!(Return, from_register, num_values)
            }
            Self::Return0 => {
                bytes!(Return0)
            }
            Self::Return1 { src_register } => {
                bytes!(Return1, src_register)
            }
            Self::ReturnM {
                from_register,
                num_fixed_values,
            } => {
                bytes!(ReturnM, from_register, num_fixed_values)
            }

            // Loops
            Self::ForPrep {
                base_register,
                address,
            } => {
                bytes!(ForPrep, base_register, bytes = address)
            }
            Self::ForLoop {
                base_register,
                address,
            } => {
                bytes!(ForLoop, base_register, bytes = address)
            }

            // Control
            Self::Jmp { address } => {
                bytes!(Jmp, bytes = address)
            }
            Self::JmpTrue {
                condition_register,
                address,
            } => {
                bytes!(JmpTrue, condition_register, bytes = address)
            }
            Self::JmpFalse {
                condition_register,
                address,
            } => {
                bytes!(JmpFalse, condition_register, bytes = address)
            }
            Self::JmpClose {
                close_from_register,
                address,
            } => {
                bytes!(JmpClose, close_from_register, bytes = address)
            }
        }
    }
}

#[derive(Debug)]
pub(crate) enum JumpToUndecidedAddress {
    Jmp,
    JmpTrue { condition_register: u8 },
    JmpFalse { condition_register: u8 },
    JmpClose { close_from_register: u8 },
    ForPrep { base_register: u8 },
}

impl JumpToUndecidedAddress {
    pub(crate) fn into_bytecode(self, address: JumpAddr) -> Bytecode {
        match self {
            JumpToUndecidedAddress::Jmp => Bytecode::Jmp { address },
            JumpToUndecidedAddress::JmpTrue { condition_register } => Bytecode::JmpTrue {
                condition_register,
                address,
            },
            JumpToUndecidedAddress::JmpFalse { condition_register } => Bytecode::JmpFalse {
                condition_register,
                address,
            },
            JumpToUndecidedAddress::JmpClose {
                close_from_register,
            } => Bytecode::JmpClose {
                close_from_register,
                address,
            },
            JumpToUndecidedAddress::ForPrep { base_register } => Bytecode::ForPrep {
                base_register,
                address,
            },
        }
    }
}
