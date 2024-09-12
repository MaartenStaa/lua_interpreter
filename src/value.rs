use miette::miette;
use std::fmt::Display;

use crate::ast;

#[derive(Debug, Clone, PartialEq)]
pub enum LuaConst {
    Nil,
    Boolean(bool),
    Number(LuaNumber),
    String(Vec<u8>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LuaValue {
    Nil,
    Boolean(bool),
    Number(LuaNumber),
    String(Vec<u8>),

    // TODO: Implement these
    Table(LuaTable),
    Function(ast::FunctionDef),
    UserData,
    Thread,
}

impl From<LuaConst> for LuaValue {
    fn from(constant: LuaConst) -> Self {
        match constant {
            LuaConst::Nil => LuaValue::Nil,
            LuaConst::Boolean(b) => LuaValue::Boolean(b),
            LuaConst::Number(n) => LuaValue::Number(n),
            LuaConst::String(s) => LuaValue::String(s),
        }
    }
}

impl LuaValue {
    pub fn type_name(&self) -> &'static str {
        match self {
            LuaValue::Nil => "nil",
            LuaValue::Boolean(_) => "boolean",
            LuaValue::Number(_) => "number",
            LuaValue::String(_) => "string",
            LuaValue::Table(_) => "table",
            LuaValue::Function(_) => "function",
            LuaValue::UserData => "userdata",
            LuaValue::Thread => "thread",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LuaNumber {
    Integer(i64),
    Float(f64),
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

#[derive(Debug, Clone, PartialEq)]
pub struct LuaTable {
    // pub fields: Vec<LuaTableField>,
}

impl Display for LuaValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaValue::Nil => write!(f, "nil"),
            LuaValue::Boolean(b) => write!(f, "{}", b),
            LuaValue::Number(n) => write!(f, "{}", n),
            LuaValue::String(s) => write!(f, "{}", String::from_utf8_lossy(s)),
            _ => todo!("formatting a {self:?}"),
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
