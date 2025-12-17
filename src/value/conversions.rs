use std::sync::{Arc, RwLock};

use crate::ast;

use super::{
    closure::LuaClosure,
    constant::{LuaConst, LuaFunctionDefinition},
    number::LuaNumber,
    object::LuaObject,
    table::LuaTable,
    LuaValue, UserData,
};

impl From<ast::Literal> for LuaValue {
    fn from(literal: ast::Literal) -> Self {
        match literal {
            ast::Literal::Nil => LuaValue::Nil,
            ast::Literal::Boolean(b) => LuaValue::Boolean(b),
            ast::Literal::Number(n) => LuaValue::Number(n.into()),
            ast::Literal::String(s) => LuaValue::String(s),
        }
    }
}

impl From<bool> for LuaValue {
    fn from(b: bool) -> Self {
        LuaValue::Boolean(b)
    }
}

impl From<&str> for LuaValue {
    fn from(s: &str) -> Self {
        LuaValue::String(s.as_bytes().to_vec())
    }
}

impl From<String> for LuaValue {
    fn from(s: String) -> Self {
        LuaValue::String(s.as_bytes().to_vec())
    }
}

impl From<Vec<u8>> for LuaValue {
    fn from(s: Vec<u8>) -> Self {
        LuaValue::String(s)
    }
}

impl From<i64> for LuaValue {
    fn from(i: i64) -> Self {
        LuaValue::Number(LuaNumber::Integer(i))
    }
}

impl From<f64> for LuaValue {
    fn from(f: f64) -> Self {
        LuaValue::Number(LuaNumber::Float(f))
    }
}

impl From<LuaNumber> for LuaValue {
    fn from(number: LuaNumber) -> Self {
        LuaValue::Number(number)
    }
}

impl From<LuaTable> for LuaValue {
    fn from(table: LuaTable) -> Self {
        LuaValue::Object(Arc::new(RwLock::new(LuaObject::Table(table))))
    }
}

impl From<UserData> for LuaValue {
    fn from(user_data: UserData) -> Self {
        LuaValue::Object(Arc::new(RwLock::new(LuaObject::UserData(user_data))))
    }
}

impl From<LuaConst> for LuaValue {
    fn from(constant: LuaConst) -> Self {
        match constant {
            LuaConst::Nil => LuaValue::Nil,
            LuaConst::Boolean(b) => LuaValue::Boolean(b),
            LuaConst::Number(n) => LuaValue::Number(n),
            LuaConst::String(s) => LuaValue::String(s),
            LuaConst::Function(LuaFunctionDefinition {
                name,
                chunk,
                ip,
                upvalues,
                num_params,
                has_varargs,
                max_registers,
            }) => LuaValue::Object(Arc::new(RwLock::new(LuaObject::Closure(LuaClosure {
                name,
                chunk,
                ip,
                upvalues: vec![None; upvalues],
                num_params,
                has_varargs,
                max_registers,
            })))),
            LuaConst::Table(t) => LuaValue::Object(Arc::new(RwLock::new(LuaObject::Table(t)))),
        }
    }
}

impl From<LuaObject> for LuaValue {
    fn from(object: LuaObject) -> Self {
        LuaValue::Object(Arc::new(RwLock::new(object)))
    }
}

impl From<ast::Number> for LuaNumber {
    fn from(number: ast::Number) -> Self {
        match number {
            ast::Number::Integer(i) => LuaNumber::Integer(i),
            ast::Number::Float(f) => LuaNumber::Float(f),
        }
    }
}
