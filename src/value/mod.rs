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
            // NOTE: Here and for UpValue, lock by write to prevent deadlock/stack overflow, in the
            // case where values are self-referential.
            LuaValue::Object(o) => match o.try_write() {
                Ok(o) => {
                    write!(f, "{:?}", o)
                }
                Err(e) => {
                    write!(f, "object<0x{:x}>: <locked: {e}>", o as *const _ as usize)
                }
            },
            LuaValue::UpValue(u) => match u.try_write() {
                Ok(inner) => write!(f, "upvalue<0x{:x}>: {:?}", u as *const _ as usize, inner),
                Err(e) => {
                    write!(f, "upvalue<0x{:x}>: <locked: {e}>", u as *const _ as usize)
                }
            },
        }
    }
}

impl PartialEq for LuaValue {
    fn eq(&self, other: &Self) -> bool {
        let lhs = match self {
            LuaValue::UpValue(u) => &*u.read().unwrap(),
            v => v,
        };
        let rhs = match other {
            LuaValue::UpValue(u) => &*u.read().unwrap(),
            v => v,
        };

        match (lhs, rhs) {
            (LuaValue::Nil, LuaValue::Nil) => true,
            (LuaValue::Boolean(a), LuaValue::Boolean(b)) => a == b,
            (LuaValue::Number(a), LuaValue::Number(b)) => a == b,
            (LuaValue::String(a), LuaValue::String(b)) => a == b,
            (LuaValue::Object(a), LuaValue::Object(b)) => *a.read().unwrap() == *b.read().unwrap(),
            // NOTE: Don't need to account for upvalues here since they are dereferenced above.
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
    pub const MAX_STRING_LENGTH: usize = const {
        if size_of::<usize>() < size_of::<i32>() {
            usize::MAX
        } else {
            i32::MAX as usize
        }
    };

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
