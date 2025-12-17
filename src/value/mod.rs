mod attribute;
pub(crate) mod callable;
mod closure;
mod constant;
mod conversions;
pub(crate) mod metatables;
mod number;
mod object;
mod string;
mod table;
mod upvalue;
mod userdata;

pub use attribute::LuaVariableAttribute;
pub use closure::LuaClosure;
pub use constant::{LuaConst, LuaFunctionDefinition};
pub use number::LuaNumber;
pub use object::LuaObject;
pub use table::LuaTable;
pub use upvalue::UpValue;
pub use userdata::UserData;

use std::{
    fmt::{Debug, Display},
    hash::Hash,
    sync::{Arc, RwLock},
};

#[derive(Clone)]
pub enum LuaValue {
    Nil,
    Boolean(bool),
    Number(number::LuaNumber),
    String(Vec<u8>),
    Object(Arc<RwLock<LuaObject>>),
    UpValue(Arc<RwLock<UpValue>>),
}

impl Debug for LuaValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaValue::Nil => write!(f, "nil"),
            LuaValue::Boolean(b) => write!(f, "{}", b),
            LuaValue::Number(n) => write!(f, "{}", n),
            LuaValue::String(s) => write!(f, "{:?}", String::from_utf8_lossy(s)),
            LuaValue::Object(o) => {
                let o = o.read().unwrap();
                write!(f, "{:?}", o)
            }
            LuaValue::UpValue(u) => {
                let inner = u.read().unwrap();
                write!(f, "upvalue<0x{:x}>: {:?}", u as *const _ as usize, inner)
            }
        }
    }
}

impl PartialEq for LuaValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LuaValue::Marker, LuaValue::Marker) => true,
            (LuaValue::Nil, LuaValue::Nil) => true,
            (LuaValue::Boolean(a), LuaValue::Boolean(b)) => a == b,
            (LuaValue::Number(a), LuaValue::Number(b)) => a == b,
            (LuaValue::String(a), LuaValue::String(b)) => a == b,
            (LuaValue::Object(a), LuaValue::Object(b)) => *a.read().unwrap() == *b.read().unwrap(),
            (LuaValue::UpValue(a), LuaValue::UpValue(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Hash for LuaValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            LuaValue::Nil => 0.hash(state),
            LuaValue::Boolean(b) => b.hash(state),
            LuaValue::Number(n) => n.hash(state),
            LuaValue::String(s) => s.hash(state),
            LuaValue::Object(o) => Arc::as_ptr(o).hash(state),
            LuaValue::UpValue(u) => Arc::as_ptr(u).hash(state),
        }
    }
}

impl Eq for LuaValue {}

impl LuaValue {
    pub fn type_name(&self) -> &'static str {
        match self {
            LuaValue::Nil => "nil",
            LuaValue::Boolean(_) => "boolean",
            LuaValue::Number(_) => "number",
            LuaValue::String(_) => "string",
            LuaValue::Object(o) => o.read().unwrap().type_name(),
            LuaValue::UpValue(u) => u.read().unwrap().type_name(),
        }
    }
}

impl Display for LuaValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaValue::Nil => write!(f, "nil"),
            LuaValue::Boolean(b) => write!(f, "{}", b),
            LuaValue::Number(n) => write!(f, "{}", n),
            LuaValue::String(s) => write!(f, "{}", String::from_utf8_lossy(s)),
            LuaValue::Object(o) => write!(f, "{}", o.read().unwrap()),
            LuaValue::UpValue(u) => write!(f, "{}", u.read().unwrap()),
        }
    }
}

impl LuaValue {
    pub fn is_table(&self) -> bool {
        match self {
            LuaValue::Object(o) => {
                matches!(*o.read().unwrap(), LuaObject::Table(_))
            }
            _ => false,
        }
    }
}
