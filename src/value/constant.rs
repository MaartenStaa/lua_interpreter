use crate::value::LuaString;

use super::{LuaNumber, LuaTable};

#[derive(Debug, Clone)]
pub enum LuaConst {
    Nil,
    Boolean(bool),
    Number(LuaNumber),
    String(LuaString),
    Table(LuaTable),
}

impl PartialEq for LuaConst {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LuaConst::Nil, LuaConst::Nil) => true,
            (LuaConst::Boolean(a), LuaConst::Boolean(b)) => a == b,
            (LuaConst::Number(a), LuaConst::Number(b)) => match (a, b) {
                (LuaNumber::Integer(a), LuaNumber::Integer(b)) => a == b,
                (LuaNumber::Float(a), LuaNumber::Float(b)) => a == b,
                // NOTE: While the values may be equal, these are distinct constants!
                (LuaNumber::Integer(_), LuaNumber::Float(_)) => false,
                (LuaNumber::Float(_), LuaNumber::Integer(_)) => false,
            },
            (LuaConst::String(a), LuaConst::String(b)) => a == b,
            (LuaConst::Table(a), LuaConst::Table(b)) => a == b,
            _ => false,
        }
    }
}
