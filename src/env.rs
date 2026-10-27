use std::sync::{Arc, RwLock};

use crate::{
    macros::{assert_table, native_function},
    stdlib::{self, globals::*},
    value::{LuaObject, LuaTable, LuaValue, UpValue},
};

pub fn create_global_env() -> Arc<RwLock<UpValue>> {
    let mut env = LuaTable::new();

    // Global functions
    env.insert("assert".into(), native_function!(assert));
    env.insert("collectgarbage".into(), native_function!(collectgarbage));
    env.insert("dofile".into(), native_function!(dofile));
    env.insert("error".into(), native_function!(error));
    env.insert("getmetatable".into(), native_function!(getmetatable));
    env.insert("ipairs".into(), native_function!(ipairs));
    env.insert("load".into(), native_function!(load));
    env.insert("next".into(), native_function!(next));
    env.insert("pairs".into(), native_function!(pairs));
    env.insert("pcall".into(), native_function!(pcall));
    env.insert("print".into(), native_function!(print));
    env.insert("rawequal".into(), native_function!(rawequal));
    env.insert("rawget".into(), native_function!(rawget));
    env.insert("rawlen".into(), native_function!(rawlen));
    env.insert("rawset".into(), native_function!(rawset));
    env.insert("require".into(), native_function!(require));
    env.insert("select".into(), native_function!(select));
    env.insert("setmetatable".into(), native_function!(setmetatable));
    env.insert("tonumber".into(), native_function!(tonumber));
    env.insert("tostring".into(), native_function!(tostring));
    env.insert("type".into(), native_function!("type", r#type));
    env.insert("warn".into(), native_function!(warn));
    env.insert("xpcall".into(), native_function!(xpcall));

    // Global constants
    env.insert("_VERSION".into(), _VERSION.into());

    // Namespaced modules
    env.insert("coroutine".into(), stdlib::coroutine::COROUTINE.clone());
    env.insert("debug".into(), stdlib::debug::DEBUG.clone());
    env.insert("io".into(), stdlib::io::IO.clone());
    env.insert("math".into(), stdlib::math::MATH.clone());
    env.insert("package".into(), stdlib::package::PACKAGE.clone());
    env.insert("os".into(), stdlib::os::OS.clone());
    env.insert("string".into(), stdlib::string::STRING.clone());
    env.insert("table".into(), stdlib::table::TABLE.clone());
    env.insert("utf8".into(), stdlib::utf8::UTF8.clone());

    let mut metatable = LuaTable::new();
    metatable.insert("__debug_repr".into(), "<global object _G>".into());
    env.set_metatable(Some(LuaValue::from_object(Arc::new(RwLock::new(
        LuaObject::Table(metatable),
    )))));

    let object = LuaValue::from_object(Arc::new(RwLock::new(LuaObject::Table(env))));
    let g = object.clone();
    assert_table!(write, &object, t, {
        t.insert("_G".into(), g);
    });

    Arc::new(RwLock::new(UpValue(object)))
}
