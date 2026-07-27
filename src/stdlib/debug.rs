use std::sync::LazyLock;

use crate::{
    error::lua_error,
    macros::{lua_return, native_function, require_closure},
    state::LuaState,
    value::{
        IntegerRepr, LuaObject, LuaTable, LuaValue,
        callable::{Callable, Method},
    },
};

pub static DEBUG: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut debug = LuaTable::new();

    debug.insert("getinfo".into(), native_function!("debug"; getinfo));
    debug.insert("getupvalue".into(), native_function!("debug"; getupvalue));
    debug.insert(
        "setmetatable".into(),
        native_function!("debug"; setmetatable),
    );
    debug.insert("upvalueid".into(), native_function!("debug"; upvalueid));

    debug.into()
});

fn getinfo(state: &mut LuaState) -> crate::Result<usize> {
    // TODO: First argument can optionally refer to the thread
    // TODO: Last argument can refer to which fields to fill in the table

    let mut info = LuaTable::new();
    match state.ensure_arg(0)? {
        v if v.is_number() => {
            let call_frame_offset = unsafe { v.as_number() }.integer_repr()?;
            if call_frame_offset < 0 || call_frame_offset as usize > state.call_stack_index {
                lua_return!(state)
            }

            // let call_frame = &vm.call_stack[vm.call_stack_index - call_frame_offset as usize];
            // if let Some(name) = &call_frame.name {
            //     info.insert("name".into(), LuaValue::String(name.clone()));
            // }
        }
        v if v.is_object() => {
            let o = unsafe { v.as_object() };
            match &*o.read().unwrap() {
                LuaObject::Closure(closure) => {
                    if let Some(name) = state.get_chunk_name(closure.chunk) {
                        info.insert("name".into(), LuaValue::from_string(name.clone()));
                    }
                }
                LuaObject::NativeFunction(name, _) => {
                    info.insert("name".into(), (*name).into());
                }
                _ => {
                    return Err(lua_error!(
                        "bad argument #1 to 'getinfo' (number or function expected, got {type_name})",
                        type_name = o.read().unwrap().type_name()
                    ));
                }
            }
        }
        v => {
            return Err(lua_error!(
                "bad argument #1 to 'getinfo' (number or function expected, got {type_name})",
                type_name = v.type_name()
            ));
        }
    }

    lua_return!(state; info.into())
}

fn getupvalue(state: &mut LuaState) -> crate::Result<usize> {
    // TODO: Will produce error "value expected" rather than "function expected, got no value"
    let value = state.ensure_arg(0)?;
    let callable: Result<Callable, _> = value.clone().try_into();
    let Ok(callable) = callable else {
        return Err(crate::error::lua_error!(
            "bad argument #1 to 'debug.getupvalue' (function expected, got {type_name})",
            type_name = value.type_name()
        ));
    };

    let n = state.arg_as_integer(1)? - 1;

    let Method::Closure(closure) = callable.method else {
        return Ok(0);
    };

    if n < 0 || n as usize >= closure.upvalues.len() {
        return Ok(0);
    }

    if let Some(upvalue) = &closure.upvalues.get(&(n as usize)) {
        let chunk = closure.chunk;
        let name = state
            .with_chunk(chunk, |c| c.upvalues[n as usize].name.clone())
            .expect("invalid chunk")
            .unwrap_or_else(|| b"?".into());

        lua_return!(state;
            name.into(),
            upvalue.read().unwrap().clone(),
        );
    }

    Ok(0)
}

fn setmetatable(state: &mut LuaState) -> crate::Result<usize> {
    let mut value = state.ensure_arg(0)?;
    let mt = state.try_arg(1);

    value.set_metatable(mt)?;
    lua_return!(state; value)
}

fn upvalueid(state: &mut LuaState) -> crate::Result<usize> {
    require_closure!(state.try_arg(0), "debug.upvalueid", closure, {
        let n = state.arg_as_integer(1)? - 1;
        if n < 0 || n as usize >= closure.upvalues.len() {
            lua_return!(state)
        } else if let Some(upvalue) = &closure.upvalues.get(&(n as usize)) {
            let id = (&*upvalue.read().unwrap()) as *const _ as i64;
            // TODO: This should be a "light userdata" value
            lua_return!(state; id.into());
        }

        lua_return!(state)
    })
}
