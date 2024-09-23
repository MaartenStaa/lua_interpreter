use crate::vm::VM;
use std::{fmt::Display, hash::Hash};

use super::{closure::LuaClosure, table::LuaTable, LuaValue};

#[derive(Debug, Clone)]
pub enum LuaObject {
    Table(LuaTable),
    Closure(LuaClosure),
    NativeFunction(
        &'static str,
        fn(&mut VM, Vec<LuaValue>) -> miette::Result<Vec<LuaValue>>,
    ),

    // TODO: Implement these
    Thread,
    UserData,
}

impl PartialEq for LuaObject {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LuaObject::Table(a), LuaObject::Table(b)) => a == b,
            (LuaObject::Closure(LuaClosure { .. }), LuaObject::Closure(LuaClosure { .. })) => false,
            (LuaObject::NativeFunction(_, a), LuaObject::NativeFunction(_, b)) => a == b,

            // TODO: Implement these
            (LuaObject::Thread, LuaObject::Thread) => true,
            (LuaObject::UserData, LuaObject::UserData) => true,

            _ => false,
        }
    }
}

impl Hash for LuaObject {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Self).hash(state);
    }
}

impl LuaObject {
    pub fn type_name(&self) -> &'static str {
        match self {
            LuaObject::Table(_) => "table",
            LuaObject::Closure(LuaClosure { .. }) => "function",
            LuaObject::NativeFunction(_, _) => "function",
            LuaObject::UserData => "userdata",
            LuaObject::Thread => "thread",
        }
    }
}

impl Display for LuaObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaObject::Table(_) => write!(f, "table: 0x{:x}", self as *const _ as usize),
            LuaObject::Closure(LuaClosure { name, .. }) => {
                write!(f, "function: 0x{:x}", self as *const _ as usize)?;
                if let Some(name) = name {
                    write!(f, " ({name})")
                } else {
                    Ok(())
                }
            }
            LuaObject::NativeFunction(name, func) => {
                write!(f, "function: 0x{:x} ({name})", func as *const _ as usize)
            }
            _ => todo!("formatting a {self:?}"),
        }
    }
}
