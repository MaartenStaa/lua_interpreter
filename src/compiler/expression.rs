#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum VariableMode {
    Ref,
    Read,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ExpressionMode {
    /// When possible, just return a register reference to the value. This will
    /// mainly benefit references to variables, in usages like `SetTable`, where
    /// you need the register holding the table value. Expressions such as
    /// calls or operations that produce new values will always return a new
    /// register.
    Ref,
    /// Always copy the value into a new register, even if we have a register
    /// already holding the value. This is useful e.g. for function calls,
    /// where the layout is expected to be [..., function, arg1, arg2, ...].
    Copy,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ExpressionResultMode {
    /// The expression may produce no more than 1 result.
    Single,
    /// The expression being evaluated may produce multiple results, if applicable.
    Multiple,
}

impl ExpressionResultMode {
    pub(crate) fn into_result(self, register: u8) -> ExpressionResult {
        match self {
            ExpressionResultMode::Single => ExpressionResult::Single { register },
            ExpressionResultMode::Multiple => ExpressionResult::Multiple {
                from_register: register,
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ExpressionResult {
    /// The compiled expression will result in one value, in the given register.
    Single { register: u8 },
    /// The compiled expression may result in multiple values, starting at the given register.
    Multiple { from_register: u8 },
}

impl ExpressionResult {
    pub(crate) fn get_register(&self) -> u8 {
        match self {
            ExpressionResult::Single { register } => *register,
            ExpressionResult::Multiple { from_register } => *from_register,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ExpressionListLength {
    Constant {
        from_register: u8,
    },
    /// Expression list ends in an expression that _may_ result in multiple values.
    MultRes {
        from_register: u8,
    },
}

impl ExpressionListLength {
    pub(crate) fn get_register(&self) -> u8 {
        match self {
            Self::Constant { from_register } => *from_register,
            Self::MultRes { from_register } => *from_register,
        }
    }
}
