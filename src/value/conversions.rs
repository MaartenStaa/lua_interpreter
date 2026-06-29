use std::{
    borrow::Cow,
    sync::{Arc, RwLock},
};

use crate::{LuaState, ast, value::LuaString};

use super::{
    LuaValue, UserData, closure::LuaClosure, constant::LuaConst, number::LuaNumber,
    object::LuaObject, table::LuaTable,
};

impl From<ast::Literal> for LuaValue {
    fn from(literal: ast::Literal) -> Self {
        match literal {
            ast::Literal::Nil => LuaValue::NIL,
            ast::Literal::Boolean(b) => LuaValue::from_bool(b),
            ast::Literal::Number(n) => LuaValue::from_number(n.into()),
            ast::Literal::String(s) => LuaValue::from_string(s),
        }
    }
}

impl From<bool> for LuaValue {
    fn from(b: bool) -> Self {
        LuaValue::from_bool(b)
    }
}

impl From<&str> for LuaValue {
    fn from(s: &str) -> Self {
        LuaValue::from_string(s.as_bytes().to_vec())
    }
}

impl From<String> for LuaValue {
    fn from(s: String) -> Self {
        LuaValue::from_string(s.into_bytes())
    }
}

impl From<Cow<'_, str>> for LuaValue {
    fn from(s: Cow<'_, str>) -> Self {
        LuaValue::from_string(s.into_owned().into_bytes())
    }
}

impl From<Vec<u8>> for LuaValue {
    fn from(s: Vec<u8>) -> Self {
        LuaValue::from_string(LuaString::from_bytes(s))
    }
}

impl From<LuaString> for LuaValue {
    fn from(s: LuaString) -> Self {
        LuaValue::from_string(s)
    }
}

impl From<i64> for LuaValue {
    fn from(i: i64) -> Self {
        LuaValue::from_integer(i)
    }
}

impl From<f64> for LuaValue {
    fn from(f: f64) -> Self {
        LuaValue::from_float(f)
    }
}

impl From<LuaNumber> for LuaValue {
    fn from(number: LuaNumber) -> Self {
        LuaValue::from_number(number)
    }
}

impl<T> From<T> for LuaValue
where
    T: Into<LuaObject>,
{
    fn from(object: T) -> Self {
        LuaValue::from_object(Arc::new(RwLock::new(object.into())))
    }
}

impl From<LuaTable> for LuaObject {
    fn from(table: LuaTable) -> Self {
        LuaObject::Table(table)
    }
}

impl From<LuaClosure> for LuaValue {
    fn from(closure: LuaClosure) -> Self {
        LuaValue::from_object(Arc::new(RwLock::new(LuaObject::Closure(closure))))
    }
}

impl From<LuaState> for LuaValue {
    fn from(state: LuaState) -> Self {
        LuaValue::from_object(Arc::new(RwLock::new(LuaObject::Thread(Box::new(state)))))
    }
}

impl From<LuaConst> for LuaValue {
    fn from(constant: LuaConst) -> Self {
        match constant {
            LuaConst::Nil => LuaValue::NIL,
            LuaConst::Boolean(b) => LuaValue::from_bool(b),
            LuaConst::Number(n) => LuaValue::from_number(n),
            LuaConst::String(s) => LuaValue::from_string(s),
            LuaConst::Table(t) => LuaValue::from_object(Arc::new(RwLock::new(LuaObject::Table(t)))),
        }
    }
}

impl From<Arc<RwLock<LuaObject>>> for LuaValue {
    fn from(object: Arc<RwLock<LuaObject>>) -> Self {
        LuaValue::from_object(object)
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

impl From<UserData> for LuaObject {
    fn from(user_data: UserData) -> Self {
        LuaObject::UserData(user_data)
    }
}
