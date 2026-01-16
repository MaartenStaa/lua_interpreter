use std::sync::LazyLock;

use crate::{
    macros::require_closure,
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
        LuaObject::NativeFunction("debug.getupvalue", getupvalue).into(),
    );
    debug.insert(
        "upvalueid".into(),
        LuaObject::NativeFunction("debug.upvalueid", upvalueid).into(),
    );

    debug.into()
});

fn getupvalue(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    // TODO: Will produce error "value expected" rather than "function expected, got no value"
    let value = vm.ensure_arg(0)?;
    let callable: Result<Callable, _> = value.clone().try_into();
    let Ok(callable) = callable else {
        return Err(crate::error::lua_error!(
            "bad argument #1 to 'debug.getupvalue' (function expected, got {type_name})",
            type_name = value.type_name()
        ));
    };

    let n = vm.arg_as_integer(1)? - 1;

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

fn upvalueid(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    require_closure!(vm.try_arg(0), "debug.upvalueid", closure, {
        let n = vm.arg_as_integer(1)? - 1;
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
