use cpu_time::ProcessTime;
use miette::miette;
use rand::{distributions, Rng};
use std::{ffi::OsString, sync::LazyLock};

use crate::{
    macros::require_string,
    value::{LuaObject, LuaTable, LuaValue},
    vm::VM,
};

pub static OS: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut os = LuaTable::new();

    os.insert(
        "clock".into(),
        LuaObject::NativeFunction("clock", clock).into(),
    );
    os.insert(
        "getenv".into(),
        LuaObject::NativeFunction("getenv", getenv).into(),
    );
    os.insert(
        "setlocale".into(),
        LuaObject::NativeFunction("setlocale", setlocale).into(),
    );
    os.insert(
        "time".into(),
        LuaObject::NativeFunction("time", time).into(),
    );
    os.insert(
        "tmpname".into(),
        LuaObject::NativeFunction("tmpname", tmpname).into(),
    );

    os.into()
});

pub static CLOCK_START: LazyLock<ProcessTime> = LazyLock::new(ProcessTime::now);

fn clock(_: &mut VM, _: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    Ok(vec![CLOCK_START.elapsed().as_secs_f64().into()])
}

fn getenv(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let name = require_string!(input, "os.getenv");
    let name_os_str = OsString::from(String::from_utf8_lossy(name.as_slice()).to_string());

    match std::env::var(name_os_str) {
        Ok(value) => Ok(vec![value.into()]),
        Err(_) => Ok(vec![LuaValue::Nil]),
    }
}

fn setlocale(_: &mut VM, _: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    Ok(vec![LuaValue::Nil])
}

fn time(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    if !input.is_empty() {
        return Err(miette!(
            "passing arguments to 'os.time' is not yet supported"
        ));
    }

    let epoch = chrono::DateTime::UNIX_EPOCH;
    let now = chrono::Utc::now();

    Ok(vec![(now - epoch).num_seconds().into()])
}

fn tmpname(_: &mut VM, _: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let mut rng = rand::thread_rng();
    let random_string: String = rng
        .sample_iter(distributions::Alphanumeric)
        .map(char::from)
        .take(8)
        .collect();

    let tmpdir = std::env::temp_dir();
    let tmpname = tmpdir.join(format!("lua-{}", random_string));

    Ok(vec![tmpname.to_string_lossy().to_string().into()])
}
