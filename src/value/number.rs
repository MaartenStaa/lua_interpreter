use miette::miette;
use std::{fmt::Display, hash::Hash};

#[derive(Debug, Clone, PartialEq)]
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

impl LuaNumber {
    pub fn integer_repr(&self) -> miette::Result<i64> {
        match self {
            LuaNumber::Integer(i) => Ok(*i),
            LuaNumber::Float(f) => {
                if f.is_nan() {
                    return Err(miette!("NaN has no integer representation",));
                }

                if f.is_infinite() {
                    return Err(miette!("infinity has no integer representation",));
                }

                if f.fract() != 0.0 {
                    return Err(miette!("float has no integer representation",));
                }

                Ok(*f as i64)
            }
        }
    }
}

impl Display for LuaNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaNumber::Integer(i) => write!(f, "{}", i),
            LuaNumber::Float(fl) => {
                // Ensure we always print the decimal point
                if fl.fract() == 0.0 {
                    write!(f, "{:.1}", fl)
                } else {
                    write!(f, "{}", fl)
                }
            }
        }
    }
}
