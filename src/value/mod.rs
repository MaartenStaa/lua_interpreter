mod attribute;
pub(crate) mod callable;
mod closure;
mod constant;
mod conversions;
mod function;
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
pub use constant::LuaConst;
pub use function::NativeLuaFunction;
pub use number::LuaNumber;
pub use object::LuaObject;
pub use string::LuaString;
pub use table::LuaTable;
pub use upvalue::UpValue;
pub use userdata::UserData;

use std::{
    fmt::{Debug, Display},
    hash::Hash,
    sync::{Arc, RwLock},
};

use crate::{LuaState, error::lua_error};

#[derive(Clone)]
pub enum LuaValue {
    Nil,
    Boolean(bool),
    Number(number::LuaNumber),
    String(LuaString),
    Object(Arc<RwLock<LuaObject>>),
    UpValue(Arc<RwLock<UpValue>>),
}

impl LuaValue {
    /// Turn the value into a string, using the `__tostring` metamethod if it exists.
    /// Falls back on this type's `Display` implementation if the metamethod does not exist.
    pub fn as_string(&self, state: &mut LuaState) -> crate::Result<LuaString> {
        let __tostring = self.get_metavalue(&metatables::TOSTRING_KEY);
        if let Some(__tostring) = __tostring {
            let __tostring: Callable = (&__tostring).try_into().map_err(|_| {
                lua_error!(
                    "invalid value for __tostring metamethod in {}, expected function, got {}",
                    self.type_name(),
                    __tostring.type_name()
                )
            })?;

            let mut result = match __tostring.method {
                Method::Closure(closure) => state
                    .run_closure(closure, vec![self.clone()])?
                    .unwrap_completed(),
                Method::NativeFunction(name, f) => {
                    let args_r = state.extend_stack(vec![self.clone()]);
                    state.run_native_function(name, f, args_r, 1)?
                }
            };
            result.resize(1, LuaValue::Nil);

            return match result.swap_remove(0) {
                LuaValue::String(s) => Ok(s),
                v => Err(lua_error!(
                    "__tostring metamethod must return a string, got {}",
                    v.type_name()
                )),
            };
        }

        Ok(format!("{}", self).into_bytes().into())
    }
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
                Ok(inner) => write!(f, "{:?}", inner),
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

    pub fn size_bytes(&self) -> u64 {
        (size_of::<Self>() as u64)
            + match self {
                Self::String(s) => size_of_val(s) as u64,
                Self::Object(o) => {
                    let o = o.read().unwrap();
                    match &*o {
                        LuaObject::Table(t) => t.size_bytes(),
                        // TODO: Once we have threads or userdata, this probably needs to change.
                        _ => 0,
                    }
                }
                _ => 0,
            }
    }

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
