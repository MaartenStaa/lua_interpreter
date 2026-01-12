use crate::vm::VM;
use std::{fmt::Display, hash::Hash};

use super::{LuaValue, UserData, closure::LuaClosure, table::LuaTable};

#[derive(Debug)]
pub enum LuaObject {
    Table(LuaTable),
    Closure(LuaClosure),
    NativeFunction(
        &'static str,
        fn(&mut VM, Vec<LuaValue>) -> crate::Result<Vec<LuaValue>>,
    ),
    UserData(UserData),

    // TODO: Implement this
    Thread,
}

impl PartialEq for LuaObject {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LuaObject::Table(a), LuaObject::Table(b)) => std::ptr::eq(a, b),
            (
                LuaObject::Closure(LuaClosure {
                    chunk: chunk_a,
                    ip: ip_a,
                    ..
                }),
                LuaObject::Closure(LuaClosure {
                    chunk: chunk_b,
                    ip: ip_b,
                    ..
                }),
            ) => chunk_a == chunk_b && ip_a == ip_b,
            (LuaObject::NativeFunction(_, a), LuaObject::NativeFunction(_, b)) => {
                std::ptr::fn_addr_eq(*a, *b)
            }

            // TODO: Implement these
            (LuaObject::Thread, LuaObject::Thread) => true,
            (LuaObject::UserData(a), LuaObject::UserData(b)) => a == b,

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
            LuaObject::UserData(_) => "userdata",
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
                    write!(f, " ({name})", name = String::from_utf8_lossy(name))
                } else {
                    Ok(())
                }
            }
            LuaObject::NativeFunction(name, func) => {
                write!(f, "function: 0x{:x} ({name})", func as *const _ as usize)
            }
            LuaObject::UserData(u) => {
                write!(f, "{}", u)
            }
            _ => todo!("formatting a {self:?}"),
        }
    }
}
