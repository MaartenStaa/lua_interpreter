use std::{fmt::Display, hash::Hash};

use crate::error::lua_error;

#[derive(Debug, Clone)]
pub enum LuaNumber {
    Integer(i64),
    Float(f64),
}

impl Hash for LuaNumber {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            LuaNumber::Integer(i) => i.hash(state),
            LuaNumber::Float(f) => {
                if f.is_nan() {
                    "#!LuaFloatNaN!#".hash(state);
                } else {
                    f.to_bits().hash(state);
                }
            }
        }
    }
}

impl From<LuaNumber> for f32 {
    fn from(value: LuaNumber) -> Self {
        match value {
            LuaNumber::Integer(i) => i as f32,
            LuaNumber::Float(f) => f as f32,
        }
    }
}

impl From<LuaNumber> for f64 {
    fn from(value: LuaNumber) -> Self {
        match value {
            LuaNumber::Integer(i) => i as f64,
            LuaNumber::Float(f) => f,
        }
    }
}

impl Display for LuaNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaNumber::Integer(i) => write!(f, "{i}"),
            LuaNumber::Float(fl) => {
                // Ensure we always print the decimal point
                if fl.fract() == 0.0 {
                    write!(f, "{fl:.1}")
                } else {
                    write!(f, "{fl}")
                }
            }
        }
    }
}

impl PartialEq for LuaNumber {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => a == b,
            (LuaNumber::Float(a), LuaNumber::Float(b)) => a == b,
            (LuaNumber::Integer(a), LuaNumber::Float(b)) => (*a as f64) == *b,
            (LuaNumber::Float(a), LuaNumber::Integer(b)) => *a == (*b as f64),
        }
    }
}

pub trait IntegerRepr {
    /// Convert this value to an integer representation, if possible.
    ///
    /// # Errors
    ///
    /// If the value cannot be represented as an integer, an error is returned.
    fn integer_repr(&self) -> crate::Result<i64>;
}

impl IntegerRepr for LuaNumber {
    fn integer_repr(&self) -> crate::Result<i64> {
        match self {
            LuaNumber::Integer(i) => Ok(*i),
            LuaNumber::Float(f) => f.integer_repr(),
        }
    }
}

impl IntegerRepr for f64 {
    fn integer_repr(&self) -> crate::Result<i64> {
        if self.is_nan() {
            return Err(lua_error!("NaN has no integer representation",));
        }

        if self.is_infinite() {
            return Err(lua_error!("infinity has no integer representation",));
        }

        if self.fract() != 0.0 {
            return Err(lua_error!("float has no integer representation",));
        }

        Ok(*self as i64)
    }
}
