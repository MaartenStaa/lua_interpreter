use std::sync::{Arc, LazyLock};

use crate::{
    state::LuaState,
    value::{LuaObject, LuaTable, LuaValue},
};

pub static COROUTINE: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut coroutine = LuaTable::new();

    coroutine.insert(
        "create".into(),
        LuaObject::NativeFunction("coroutine.create", create).into(),
    );
    coroutine.insert(
        "resume".into(),
        LuaObject::NativeFunction("coroutine.resume", resume).into(),
    );
    coroutine.insert(
        "yield".into(),
        LuaObject::NativeFunction("coroutine.yield", yield_fn).into(),
    );

    coroutine.into()
});

fn create(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let callable = state.arg_as_callable(0)?;

    // Native functions cannot be used as coroutines directly
    // They would need special handling to store the function pointer
    if matches!(
        callable.method,
        crate::value::callable::Method::NativeFunction(_, _)
    ) {
        return Err(crate::error::lua_error!(
            "cannot create coroutine from C function"
        ));
    }

    let state = LuaState::for_callable(Arc::clone(&state.global), callable)
        .with_tracer(state.tracer.is_some());

    Ok(vec![state.into()])
}

fn resume(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let mut args = state.args();
    args.remove(0); // remove the thread argument

    state.with_arg_as_mut_thread(0, |co| match co.resume(args) {
        Ok(mut result) => {
            result.insert(0, LuaValue::from_bool(true));
            Ok(result)
        }
        Err(e) => {
            let msg = e.message.into_bytes();
            Ok(vec![LuaValue::from_bool(false), msg.into()])
        }
    })
}

fn yield_fn(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    if state.is_main_thread {
        return Err(crate::error::lua_error!(
            "attempt to yield from outside a coroutine"
        ));
    }

    if !state.is_yieldable() {
        return Err(crate::error::lua_error!(
            "attempt to yield across a C-call boundary"
        ));
    }

    let values = state.args();
    state.yield_values(values);

    // Return empty vec - the actual yield values will be picked up by pending_yield
    Ok(vec![])
}
