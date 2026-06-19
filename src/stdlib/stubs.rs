use std::sync::{LazyLock, Mutex};

use crate::{
    LuaState,
    error::lua_error,
    value::{LuaTable, LuaValue},
};

static GC_MODE: LazyLock<Mutex<LuaValue>> = LazyLock::new(|| Mutex::new(LuaValue::NIL));
static GC_RUNNING: LazyLock<Mutex<bool>> = LazyLock::new(|| Mutex::new(true));
static GC_PARAMS: LazyLock<Mutex<LuaTable>> = LazyLock::new(|| Mutex::new(LuaTable::new()));

pub(crate) fn collectgarbage(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let opt = state.try_arg_as_string(0)?.unwrap_or(b"collect".into());

    Ok(match opt.as_slice() {
        b"collect" => {
            vec![LuaValue::NIL]
        }
        b"stop" => {
            *GC_RUNNING.lock().unwrap() = false;
            vec![LuaValue::NIL]
        }
        b"restart" => {
            *GC_RUNNING.lock().unwrap() = true;
            vec![LuaValue::NIL]
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
            *mode = LuaValue::from_string(opt.to_vec());
            vec![previous_mode]
        }
        b"generational" => {
            let mut mode = GC_MODE.lock().unwrap();
            let previous_mode = mode.clone();
            *mode = LuaValue::from_string(opt.to_vec());
            vec![previous_mode]
        }
        b"param" => {
            let name = state.ensure_arg(1)?;
            let value = state.try_arg(2);
            let mut params = GC_PARAMS.lock().unwrap();

            if let Some(value) = value {
                let old_value = params.get(&name).cloned();
                params.insert(name, value.clone());
                vec![old_value.unwrap_or(LuaValue::NIL)]
            } else {
                let value = params.get(&name).cloned();
                vec![value.unwrap_or(LuaValue::NIL)]
            }
        }
        _ => {
            return Err(lua_error!(
                "bad argument #1 to 'collectgarbage' (invalid option '{opt}')"
            ));
        }
    })
}

pub(crate) fn gcinfo(_: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    // Same as collectgarbage("count") ?
    Ok(vec![0.into()])
}
