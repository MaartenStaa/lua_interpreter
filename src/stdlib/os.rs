use cpu_time::ProcessTime;
use std::{ffi::OsString, sync::LazyLock};

use crate::{
    error::lua_error,
    value::{LuaObject, LuaTable, LuaValue},
    vm::VM,
};

pub static OS: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut os = LuaTable::new();

    os.insert(
        "clock".into(),
        LuaObject::NativeFunction("os.clock", clock).into(),
    );
    os.insert(
        "getenv".into(),
        LuaObject::NativeFunction("os.getenv", getenv).into(),
    );
    os.insert(
        "time".into(),
        LuaObject::NativeFunction("os.time", time).into(),
    );

    os.into()
});

pub static CLOCK_START: LazyLock<ProcessTime> = LazyLock::new(ProcessTime::now);

fn clock(_: &mut VM) -> crate::Result<Vec<LuaValue>> {
    Ok(vec![CLOCK_START.elapsed().as_secs_f64().into()])
}

fn getenv(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let name = vm.arg_as_string(0)?;
    let name_os_str = OsString::from(String::from_utf8_lossy(name.as_slice()).to_string());

    match std::env::var(name_os_str) {
        Ok(value) => Ok(vec![value.into()]),
        Err(_) => Ok(vec![LuaValue::Nil]),
    }
}

fn time(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    if vm.num_args() > 0 {
        return Err(lua_error!(
            "passing arguments to 'os.time' is not yet supported"
        ));
    }

    let epoch = chrono::DateTime::UNIX_EPOCH;
    let now = chrono::Utc::now();

    Ok(vec![(now - epoch).num_seconds().into()])
}
