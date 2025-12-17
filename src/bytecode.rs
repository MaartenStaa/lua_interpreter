use crate::compiler::{ExpressionResultMode, Upvalue};
use crate::error::RuntimeError;
use crate::instruction::Instruction;
use crate::value::LuaVariableAttribute;
use crate::vm::{ConstIndex, JumpAddr};

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
        const_index: ConstIndex,
        upvalues: Vec<Upvalue>,
    },
    /// Load `nil` into a range of registers.
    LoadNil {
        from_register: u8,
        to_register: u8,
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
    /// Set a global variable from a register, using the name as specified in the constants table.
    SetGlobal {
        src_register: u8,
        name_index: ConstIndex,
    },
    /// Get a global variable into a register, using the name as specified in the constants table.
    GetGlobal {
        dest_register: u8,
        name_index: ConstIndex,
    },
    /// Move/copy a value from one register to another.
    Mov {
        dest_register: u8,
        src_register: u8,
    },
    /// Set an attribute for a local variable in a register.
    SetLocalAttr {
        register: u8,
        attr_index: LuaVariableAttribute,
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
        single_value: bool,
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
    /// Get a value from a table by key.
    GetTable {
        dest_register: u8,
        table_register: u8,
        key_register: u8,
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

    // Other
    Error {
        code: RuntimeError,
    },
}

macro_rules! bytes {
    ($instruction:ident $(,$($rest:tt)*)?) => {{
        #[allow(unused_mut)]
        let mut result = vec![Instruction::$instruction as u8];
        bytes!(@append result; $($($rest)*)?);
        result
    }};

    // Termination
    (@append $result:ident;) => {};

    // Typed element: `bytes = expr`
    (@append $result:ident; bytes = $item:expr) => {{
        $result.extend_from_slice(&$item.to_be_bytes());
    }};
    (@append $result:ident; bytes = $item:expr, $($rest:tt)*) => {{
        $result.extend_from_slice(&$item.to_be_bytes());
        bytes!(@append $result; $($rest)*);
    }};

    // Untyped element: anything else
    (@append $result:ident; $item:expr) => {{
        $result.push($item);
    }};
    (@append $result:ident; $item:expr, $($rest:tt)*) => {{
        $result.push($item);
        bytes!(@append $result; $($rest)*);
    }};
}

impl Bytecode {
    fn expression_result_mode_to_byte(mode: ExpressionResultMode) -> u8 {
        match mode {
            ExpressionResultMode::Single => 1,
            ExpressionResultMode::Multiple => 0,
        }
    }

    pub fn into_bytes(self) -> Vec<u8> {
        match self {
            // Loading values
            Bytecode::LoadConst {
                register,
                const_index,
            } => {
                bytes!(LoadConst, register, bytes = const_index)
            }
            Bytecode::LoadClosure {
                register,
                const_index,
                upvalues,
            } => {
                let mut b = bytes!(LoadClosure, register, bytes = const_index);
                for u in upvalues {
                    b.push(u.is_local as u8);
                    b.push(u.index);
                }
                b
            }
            Bytecode::LoadNil {
                from_register,
                to_register,
            } => {
                bytes!(LoadNil, from_register, to_register)
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
            Self::SetGlobal {
                src_register,
                name_index,
            } => {
                bytes!(SetGlobal, src_register, bytes = name_index)
            }
            Self::GetGlobal {
                dest_register,
                name_index,
            } => {
                bytes!(GetGlobal, dest_register, bytes = name_index)
            }
            Self::Mov {
                dest_register,
                src_register,
            } => {
                bytes!(Mov, dest_register, src_register)
            }
            Self::SetLocalAttr {
                register,
                attr_index,
            } => {
                bytes!(SetLocalAttr, register, bytes = attr_index as u8)
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
                single_value,
            } => {
                bytes!(LoadVararg, dest_register, single_value as u8)
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
            Self::GetTable {
                dest_register,
                table_register,
                key_register,
            } => {
                bytes!(GetTable, dest_register, table_register, key_register)
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

            // Other
            Self::Error { code } => {
                bytes!(Error, code as u8)
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
}

impl JumpToUndecidedAddress {
    pub(crate) fn instruction_size_without_address(&self) -> usize {
        match self {
            JumpToUndecidedAddress::Jmp => 1,
            JumpToUndecidedAddress::JmpTrue { .. }
            | JumpToUndecidedAddress::JmpFalse { .. }
            | JumpToUndecidedAddress::JmpClose { .. } => 2,
        }
    }

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
        }
    }
}
