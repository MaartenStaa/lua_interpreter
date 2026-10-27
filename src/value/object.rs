use crate::{LuaState, value::NativeLuaFunction};
use std::{fmt::Display, hash::Hash};

use super::{UserData, closure::LuaClosure, table::LuaTable};

#[derive(Debug)]
pub enum LuaObject {
    Table(LuaTable),
    Closure(LuaClosure),
    NativeFunction(&'static str, NativeLuaFunction),
    UserData(UserData),
    Thread(Box<LuaState>),
}

impl PartialEq for LuaObject {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LuaObject::Table(a), LuaObject::Table(b)) => std::ptr::eq(a, b),
            (LuaObject::Closure(a), LuaObject::Closure(b)) => std::ptr::eq(a, b),
            (LuaObject::NativeFunction(_, a), LuaObject::NativeFunction(_, b)) => {
                std::ptr::fn_addr_eq(*a, *b)
            }
            (LuaObject::Thread(a), LuaObject::Thread(b)) => std::ptr::eq(a, b),

            // TODO: Implement these
            (LuaObject::UserData(a), LuaObject::UserData(b)) => a == b,

            _ => false,
        }
    }
}

impl Hash for LuaObject {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::from_ref(self).hash(state);
    }
}

impl LuaObject {
    #[must_use]
    pub fn type_name(&self) -> &'static str {
        match self {
            LuaObject::Table(_) => "table",
            LuaObject::Closure(LuaClosure { .. }) | LuaObject::NativeFunction(_, _) => "function",
            LuaObject::UserData(_) => "userdata",
            LuaObject::Thread(_) => "thread",
        }
    }

    pub(crate) fn is_table(&self) -> bool {
        matches!(self, LuaObject::Table(_))
    }
}

impl Display for LuaObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaObject::Table(_) => write!(f, "table: {self:012p}"),
            LuaObject::Closure(LuaClosure { chunk, .. }) => {
                write!(f, "function: {self:012p} ({chunk:04})")
            }
            LuaObject::NativeFunction(name, func) => {
                write!(f, "function: {func:012p} ({name})")
            }
            LuaObject::UserData(u) => {
                write!(f, "{u}")
            }
            LuaObject::Thread(_) => write!(f, "thread: {self:012p}"),
        }
    }
}
