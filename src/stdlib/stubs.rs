use std::sync::{LazyLock, Mutex};

use crate::{
    error::lua_error,
    macros::get_string,
    value::{LuaTable, LuaValue},
    vm::VM,
};

static GC_MODE: LazyLock<Mutex<LuaValue>> = LazyLock::new(|| Mutex::new(LuaValue::Nil));
static GC_RUNNING: LazyLock<Mutex<bool>> = LazyLock::new(|| Mutex::new(true));
static GC_PARAMS: LazyLock<Mutex<LuaTable>> = LazyLock::new(|| Mutex::new(LuaTable::new()));

pub(crate) fn collectgarbage(_: &mut VM, values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let opt = get_string!(values, "collectgarbage", 0)
        .map(|s| s.as_slice())
        .unwrap_or(b"collect");

    Ok(match opt {
        b"collect" => {
            vec![LuaValue::Nil]
        }
        b"stop" => {
            *GC_RUNNING.lock().unwrap() = false;
            vec![LuaValue::Nil]
        }
        b"restart" => {
            *GC_RUNNING.lock().unwrap() = true;
            vec![LuaValue::Nil]
        }
        b"count" => {
            // TODO: Should we try to return a real number here?
            vec![0.into()]
        }
        b"step" => {
            vec![true.into()]
        }
        b"isrunning" => {
            vec![(*GC_RUNNING.lock().unwrap()).into()]
        }
        b"incremental" => {
            let mut mode = GC_MODE.lock().unwrap();
            let previous_mode = mode.clone();
            *mode = LuaValue::String(opt.to_vec().into());
            vec![previous_mode]
        }
        b"generational" => {
            let mut mode = GC_MODE.lock().unwrap();
            let previous_mode = mode.clone();
            *mode = LuaValue::String(opt.to_vec().into());
            vec![previous_mode]
        }
        b"param" => {
            let name = values.get(1).ok_or_else(|| {
                lua_error!("bad argument #1 to 'collectgarbage' (param name expected, got nil)")
            })?;
            let value = values.get(2);
            let mut params = GC_PARAMS.lock().unwrap();

            if let Some(value) = value {
                let old_value = params.get(name).cloned();
                params.insert(name.clone(), value.clone());
                vec![old_value.unwrap_or(LuaValue::Nil)]
            } else {
                let value = params.get(name).cloned();
                vec![value.unwrap_or(LuaValue::Nil)]
            }
        }
        _ => {
            return Err(lua_error!(
                "bad argument #1 to 'collectgarbage' (invalid option '{opt}')",
                opt = String::from_utf8_lossy(opt)
            ));
        }
    })
}

pub(crate) fn gcinfo(_: &mut VM, _: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    // Same as collectgarbage("count") ?
    Ok(vec![0.into()])
}
