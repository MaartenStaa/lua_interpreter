use std::sync::LazyLock;

use crate::{
    macros::{require_closure, require_number},
    value::{
        LuaObject, LuaTable, LuaValue,
        callable::{Callable, Method},
    },
    vm::VM,
};

pub static DEBUG: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut debug = LuaTable::new();

    debug.insert(
        "getupvalue".into(),
        LuaObject::NativeFunction("getupvalue", getupvalue).into(),
    );
    debug.insert(
        "upvalueid".into(),
        LuaObject::NativeFunction("upvalueid", upvalueid).into(),
    );

    debug.into()
});

fn getupvalue(vm: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    match input.first() {
        Some(value) => {
            let callable: Result<Callable, _> = value.try_into();
            let Ok(callable) = callable else {
                return Err(crate::error::lua_error!(
                    "bad argument #1 to 'debug.getupvalue' (function expected, got {type_name})",
                    type_name = value.type_name()
                ));
            };

            let n = require_number!(input, "debug.getupvalue", 1).integer_repr()? - 1;

            let Method::Closure(closure) = callable.method else {
                return Ok(vec![]);
            };

            if n < 0 || n as usize >= closure.upvalues.len() {
                return Ok(vec![]);
            }

            if let Some(upvalue) = &closure.upvalues.get(&(n as usize)) {
                let chunk = closure.chunk;
                let name = vm.get_chunk(chunk).expect("invalid chunk").upvalues[n as usize]
                    .name
                    .clone()
                    .unwrap_or_else(|| b"?".into());

                Ok(vec![name.into(), upvalue.read().unwrap().clone()])
            } else {
                Ok(vec![])
            }
        }
        None => Err(crate::error::lua_error!(
            "bad argument #1 to 'debug.getupvalue' (function expected, got nil)"
        )),
    }
}

fn upvalueid(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    require_closure!(input, "debug.upvalueid", closure, {
        let n = require_number!(input, "debug.upvalueid", 1).integer_repr()? - 1;
        if n < 0 || n as usize >= closure.upvalues.len() {
            return Ok(vec![LuaValue::Nil]);
        }

        if let Some(upvalue) = &closure.upvalues.get(&(n as usize)) {
            let id = (&*upvalue.read().unwrap()) as *const _ as i64;
            // TODO: This should be a "light userdata" value
            Ok(vec![id.into()])
        } else {
            Ok(vec![LuaValue::Nil])
        }
    })
}
