use crate::{
    stdlib,
    value::{LuaObject, LuaTable},
};

pub fn create_global_env() -> LuaTable {
    let mut env = LuaTable::new();

    // Global functions
    env.insert(
        "assert".into(),
        LuaObject::NativeFunction("assert", stdlib::globals::assert).into(),
    );
    env.insert(
        "error".into(),
        LuaObject::NativeFunction("error", stdlib::globals::error).into(),
    );
    env.insert(
        "getmetatable".into(),
        LuaObject::NativeFunction("getmetatable", stdlib::globals::getmetatable).into(),
    );
    env.insert(
        "ipairs".into(),
        LuaObject::NativeFunction("ipairs", stdlib::globals::ipairs).into(),
    );
    env.insert(
        "load".into(),
        LuaObject::NativeFunction("load", stdlib::globals::load).into(),
    );
    env.insert(
        "pcall".into(),
        LuaObject::NativeFunction("pcall", stdlib::globals::pcall).into(),
    );
    env.insert(
        "print".into(),
        LuaObject::NativeFunction("print", stdlib::globals::print).into(),
    );
    env.insert(
        "rawget".into(),
        LuaObject::NativeFunction("rawget", stdlib::globals::rawget).into(),
    );
    env.insert(
        "rawset".into(),
        LuaObject::NativeFunction("rawset", stdlib::globals::rawset).into(),
    );
    env.insert(
        "require".into(),
        LuaObject::NativeFunction("require", stdlib::globals::require).into(),
    );
    env.insert(
        "select".into(),
        LuaObject::NativeFunction("select", stdlib::globals::select).into(),
    );
    env.insert(
        "setmetatable".into(),
        LuaObject::NativeFunction("setmetatable", stdlib::globals::setmetatable).into(),
    );
    env.insert(
        "tonumber".into(),
        LuaObject::NativeFunction("tonumber", stdlib::globals::tonumber).into(),
    );
    env.insert(
        "tostring".into(),
        LuaObject::NativeFunction("tostring", stdlib::globals::tostring).into(),
    );
    env.insert(
        "type".into(),
        LuaObject::NativeFunction("type", stdlib::globals::r#type).into(),
    );
    env.insert(
        "warn".into(),
        LuaObject::NativeFunction("warn", stdlib::globals::warn).into(),
    );

    // Global constants
    env.insert("_VERSION".into(), stdlib::globals::_VERSION.into());

    // Namespaced modules
    env.insert("debug".into(), stdlib::debug::DEBUG.clone());
    env.insert("io".into(), stdlib::io::IO.clone());
    env.insert("math".into(), stdlib::math::MATH.clone());
    env.insert("package".into(), stdlib::package::PACKAGE.clone());
    env.insert("os".into(), stdlib::os::OS.clone());
    env.insert("string".into(), stdlib::string::STRING.clone());
    env.insert("table".into(), stdlib::table::TABLE.clone());

    env
}
