use cpu_time::ProcessTime;
use rand::{Rng, distr};
use std::{ffi::OsString, sync::LazyLock};

use crate::{
    error::lua_error,
    macros::{lua_return, native_function},
    state::LuaState,
    value::{LuaTable, LuaValue},
};

pub static OS: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut os = LuaTable::new();

    os.insert("clock".into(), native_function!("os"; clock));
    os.insert("getenv".into(), native_function!("os"; getenv));
    os.insert("setlocale".into(), native_function!("os"; setlocale));
    os.insert("time".into(), native_function!("os"; time));
    os.insert("tmpname".into(), native_function!("os"; tmpname));
    os.insert("tmpname".into(), native_function!("os"; tmpname));

    os.into()
});

pub static CLOCK_START: LazyLock<ProcessTime> = LazyLock::new(ProcessTime::now);

fn clock(state: &mut LuaState) -> crate::Result<u16> {
    lua_return!(state; CLOCK_START.elapsed().as_secs_f64().into())
}

fn getenv(state: &mut LuaState) -> crate::Result<u16> {
    let name = state.arg_as_string(0)?;
    let name_os_str = OsString::from(String::from_utf8_lossy(name.as_slice()).to_string());

    match std::env::var(name_os_str) {
        Ok(value) => lua_return!(state; value.into()),
        Err(_) => lua_return!(state),
    }
}

fn setlocale(state: &mut LuaState) -> crate::Result<u16> {
    lua_return!(state)
}

fn time(state: &mut LuaState) -> crate::Result<u16> {
    if state.num_args() > 0 {
        return Err(lua_error!(
            "passing arguments to 'os.time' is not yet supported"
        ));
    }

    let epoch = chrono::DateTime::UNIX_EPOCH;
    let now = chrono::Utc::now();

    lua_return!(state; (now - epoch).num_seconds().into())
}

fn tmpname(state: &mut LuaState) -> crate::Result<u16> {
    let rng = rand::rng();
    let random_string: String = rng
        .sample_iter(distr::Alphanumeric)
        .map(char::from)
        .take(8)
        .collect();

    let tmpdir = std::env::temp_dir();
    let tmpname = tmpdir.join(format!("lua-{}", random_string));

    lua_return!(state; tmpname.to_string_lossy().to_string().into())
}
