use crate::value::LuaObject;

pub mod globals;

pub fn lookup_global(name: &str) -> Option<LuaObject> {
    match name {
        "print" => Some(LuaObject::NativeFunction(globals::print)),
        _ => None,
    }
}
