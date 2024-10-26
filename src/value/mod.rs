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
use callable::{Callable, Method};
pub use closure::LuaClosure;
pub use constant::{LuaConst, LuaFunctionDefinition};
pub use number::LuaNumber;
pub use object::LuaObject;
pub use table::LuaTable;
pub use upvalue::UpValue;
pub use userdata::UserData;

use miette::miette;
use std::{
    fmt::{Debug, Display},
    hash::Hash,
    sync::{Arc, RwLock},
};

use crate::vm::VM;

#[derive(Clone)]
pub enum LuaValue {
    Marker,
    Nil,
    Boolean(bool),
    Number(number::LuaNumber),
    String(Vec<u8>),
    Object(Arc<RwLock<LuaObject>>),
    UpValue(Arc<RwLock<UpValue>>),
}

impl LuaValue {
    /// Turn the value into a string, using the `__tostring` metamethod if it exists.
    /// Falls back on this type's `Display` implementation if the metamethod does not exist.
    pub fn as_string(&self, vm: &mut VM) -> miette::Result<Vec<u8>> {
        let __tostring = self.get_metavalue(&metatables::TOSTRING_KEY);
        if let Some(__tostring) = __tostring {
            let __tostring: Callable = (&__tostring).try_into().map_err(|_| {
                miette!(
                    "invalid value for __tostring metamethod in {}, expected function, got {}",
                    self.type_name(),
                    __tostring.type_name()
                )
            })?;

            let mut result = match __tostring.method {
                Method::Closure(closure) => vm.run_closure(closure, vec![self.clone()])?,
                Method::NativeFunction(_, f) => f(vm, vec![self.clone()])?,
            };
            result.resize(1, LuaValue::Nil);

            return match result.swap_remove(0) {
                LuaValue::String(s) => Ok(s),
                v => Err(miette!(
                    "__tostring metamethod must return a string, got {}",
                    v.type_name()
                )),
            };
        }

        Ok(format!("{}", self).into_bytes())
    }
}

impl Debug for LuaValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaValue::Marker => write!(f, "<marker>"),
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
            LuaValue::Marker => 0.hash(state),
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
            LuaValue::Marker => "<marker>",
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
            LuaValue::Marker => write!(f, "<marker>"),
            LuaValue::Nil => write!(f, "nil"),
            LuaValue::Boolean(b) => write!(f, "{}", b),
            LuaValue::Number(n) => write!(f, "{}", n),
            LuaValue::String(s) => write!(f, "{}", String::from_utf8_lossy(s)),
            LuaValue::Object(o) => write!(f, "{}", o.read().unwrap()),
            LuaValue::UpValue(u) => write!(f, "{}", u.read().unwrap()),
        }
    }
}
