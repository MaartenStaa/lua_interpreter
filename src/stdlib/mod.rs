use crate::value::{LuaObject, LuaValue};

pub mod globals;

pub fn lookup_global(name: &str) -> Option<LuaValue> {
    match name {
        "_VERSION" => Some(LuaValue::String(globals::_VERSION.bytes().collect())),
        "assert" => Some(LuaObject::NativeFunction(globals::assert).into()),
        "print" => Some(LuaObject::NativeFunction(globals::print).into()),
        "tostring" => Some(LuaObject::NativeFunction(globals::tostring).into()),
        "type" => Some(LuaObject::NativeFunction(globals::r#type).into()),
        "warn" => Some(LuaObject::NativeFunction(globals::warn).into()),
        _ => None,
    }
}
