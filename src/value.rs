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
    Table(LuaTable),
    Function(ast::FunctionDef),

    // TODO: Implement these
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

#[derive(Debug, Clone, PartialEq)]
pub enum LuaNumber {
    Integer(i64),
    Float(f64),
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
            LuaNumber::Float(fl) => write!(f, "{}", fl),
        }
    }
}
