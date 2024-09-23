use crate::value::{LuaObject, LuaValue};

mod globals;
mod math;
mod string;

use globals::*;

pub fn lookup_global(name: &str) -> Option<LuaValue> {
    match name {
        // Globals
        "_VERSION" => Some(LuaValue::String(globals::_VERSION.bytes().collect())),
        "assert" => Some(LuaObject::NativeFunction("assert", assert).into()),
        "getmetatable" => Some(LuaObject::NativeFunction("getmetatable", getmetatable).into()),
        "ipairs" => Some(LuaObject::NativeFunction("ipairs", ipairs).into()),
        "print" => Some(LuaObject::NativeFunction("print", print).into()),
        "require" => Some(LuaObject::NativeFunction("require", require).into()),
        "select" => Some(LuaObject::NativeFunction("select", select).into()),
        "setmetatable" => Some(LuaObject::NativeFunction("setmetatable", setmetatable).into()),
        "tostring" => Some(LuaObject::NativeFunction("tostring", tostring).into()),
        "type" => Some(LuaObject::NativeFunction("type", r#type).into()),
        "warn" => Some(LuaObject::NativeFunction("warn", warn).into()),

        // Namespaced modules
        "math" => Some(math::MATH.clone()),
        "string" => Some(string()),

        _ => None,
    }
}

pub fn string() -> LuaValue {
    string::STRING.clone()
}
