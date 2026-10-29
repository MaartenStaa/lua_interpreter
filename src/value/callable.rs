use crate::value::{NativeLuaFunction, function::JitLuaFunction};

use super::{LuaClosure, LuaObject, LuaValue, metatables};

#[derive(Debug)]
pub struct Callable {
    pub(crate) method: Method,
    pub(crate) is_metamethod: bool,
}

#[derive(Debug, Clone)]
pub(crate) enum Method {
    Closure(LuaClosure),
    JitFunction(&'static str, JitLuaFunction),
    NativeFunction(&'static str, NativeLuaFunction),
}

impl TryFrom<LuaValue> for Callable {
    type Error = &'static str;

    fn try_from(value: LuaValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LuaValue> for Callable {
    type Error = &'static str;

    fn try_from(value: &LuaValue) -> Result<Self, Self::Error> {
        let result = match value {
            v if v.is_object() => (&*unsafe { v.as_object() }.read().unwrap()).try_into(),
            v if v.is_upvalue() => (&unsafe { v.as_upvalue() }.read().unwrap().0).try_into(),
            _ => Err("expected closure or native function"),
        };

        if result.is_err()
            && let Some(__call) = value.get_metavalue(&metatables::CALL_KEY)
        {
            let __call: Result<Callable, _> = __call.try_into();
            if let Ok(callable) = __call {
                return Ok(Callable {
                    method: callable.method,
                    is_metamethod: true,
                });
            }
        }

        result
    }
}

// FIXME: I don't like all of this cloning.
impl TryFrom<&LuaObject> for Callable {
    type Error = &'static str;

    fn try_from(object: &LuaObject) -> Result<Self, Self::Error> {
        match object {
            LuaObject::Closure(closure) => Ok(Callable {
                method: Method::Closure(closure.clone()),
                is_metamethod: false,
            }),
            LuaObject::NativeFunction(name, f) => Ok(Callable {
                method: Method::NativeFunction(name, *f),
                is_metamethod: false,
            }),
            _ => Err("expected closure or native function"),
        }
    }
}

impl From<Callable> for LuaObject {
    fn from(value: Callable) -> Self {
        LuaObject::from(value.method)
    }
}

impl From<Method> for LuaObject {
    fn from(value: Method) -> Self {
        match value {
            Method::Closure(closure) => LuaObject::Closure(closure),
            Method::JitFunction(name, f) => LuaObject::JitFunction(name, f),
            Method::NativeFunction(name, f) => LuaObject::NativeFunction(name, f),
        }
    }
}
