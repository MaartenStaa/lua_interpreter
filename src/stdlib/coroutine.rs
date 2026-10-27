use std::sync::{Arc, LazyLock};

use crate::{
    error::lua_error,
    macros::{lua_return, native_function},
    state::LuaState,
    value::{LuaObject, LuaTable, LuaValue},
};

pub static COROUTINE: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut coroutine = LuaTable::new();

    coroutine.insert("create".into(), native_function!("coroutine"; create));
    coroutine.insert("resume".into(), native_function!("coroutine"; resume));
    coroutine.insert("wrap".into(), native_function!("coroutine"; wrap));
    coroutine.insert(
        "yield".into(),
        native_function!("coroutine"; "yield", yield_fn),
    );

    coroutine.into()
});

fn create(state: &mut LuaState) -> crate::Result<usize> {
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

    let thread = LuaState::for_callable(Arc::clone(&state.global), callable)
        .with_tracer(state.tracer.is_some());
    state.set(0usize, thread.into());

    Ok(1)
}

fn resume(state: &mut LuaState) -> crate::Result<usize> {
    let mut args = state.args();
    args.remove(0); // remove the thread argument

    match state.with_arg_as_mut_thread(0, |co| co.resume(args)) {
        Ok(result) => {
            state.set(0usize, LuaValue::from_bool(true));
            let num_results = result.len();
            for (i, value) in result.into_iter().enumerate() {
                state.set(i + 1, value);
            }
            Ok(num_results + 1)
        }
        Err(e) => {
            let msg = e.message.into_bytes();
            lua_return!(state; LuaValue::from_bool(false), msg.into())
        }
    }
}

fn wrap(state: &mut LuaState) -> crate::Result<usize> {
    let callable = state.arg_as_callable(0)?;

    if matches!(
        callable.method,
        crate::value::callable::Method::NativeFunction(_, _)
    ) {
        return Err(lua_error!("cannot create coroutine from C function"));
    }

    let co = LuaState::for_callable(Arc::clone(&state.global), callable)
        .with_tracer(state.tracer.is_some());

    let mut holder = LuaTable::new();
    holder.insert(LuaValue::from_integer(1), co.into());

    let mut metatable = LuaTable::new();
    metatable.insert(
        LuaValue::from_string(b"__call"),
        native_function!("coroutine"; wrap_resume),
    );
    holder.set_metatable(Some(metatable.into()));

    lua_return!(state; holder.into())
}

fn wrap_resume(state: &mut LuaState) -> crate::Result<usize> {
    let mut args = state.args();
    args.remove(0); // remove self (the table)

    let result = state.with_arg_as_mut_table(0, |table| {
        let thread_val = match table.get(&LuaValue::from_integer(1)) {
            Some(v) => v.clone(),
            None => return Err(lua_error!("cannot resume dead coroutine")),
        };
        if !thread_val.is_object() {
            return Err(lua_error!("cannot resume dead coroutine"));
        }
        let obj = unsafe { thread_val.as_object() };
        let mut obj = obj.write().unwrap();
        match &mut *obj {
            LuaObject::Thread(co) => co.resume(args),
            _ => Err(lua_error!("cannot resume dead coroutine")),
        }
    })?;

    lua_return!(state; from = result)
}

fn yield_fn(state: &mut LuaState) -> crate::Result<usize> {
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

    // Return no values - the actual yield values will be picked up by pending_yield
    lua_return!(state)
}
