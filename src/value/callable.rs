use crate::vm::VM;

use super::{LuaClosure, LuaObject, LuaValue, metatables};

#[derive(Debug)]
pub(crate) struct Callable {
    pub(crate) method: Method,
    pub(crate) is_metamethod: bool,
}

#[derive(Debug)]
pub(crate) enum Method {
    Closure(LuaClosure),
    NativeFunction(
        #[allow(unused)] String,
        fn(&mut VM, Vec<LuaValue>) -> crate::Result<Vec<LuaValue>>,
    ),
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
            LuaValue::Object(o) => (&*o.read().unwrap()).try_into(),
            LuaValue::UpValue(u) => (&u.read().unwrap().0).try_into(),
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
                method: Method::NativeFunction(name.to_string(), *f),
                is_metamethod: false,
            }),
            _ => Err("expected closure or native function"),
        }
    }
}
