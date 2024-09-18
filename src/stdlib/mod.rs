use crate::value::{LuaObject, LuaValue};

mod globals;
mod math;
mod string;

pub fn lookup_global(name: &str) -> Option<LuaValue> {
    match name {
        // Globals
        "_VERSION" => Some(LuaValue::String(globals::_VERSION.bytes().collect())),
        "assert" => Some(LuaObject::NativeFunction(globals::assert).into()),
        "print" => Some(LuaObject::NativeFunction(globals::print).into()),
        "tostring" => Some(LuaObject::NativeFunction(globals::tostring).into()),
        "type" => Some(LuaObject::NativeFunction(globals::r#type).into()),
        "warn" => Some(LuaObject::NativeFunction(globals::warn).into()),

        // Namespaced modules
        "math" => Some(math::MATH.clone()),
        "string" => Some(string()),

        _ => None,
    }
}

pub fn string() -> LuaValue {
    string::STRING.clone()
}
