use crate::{
    ExecutionResult, LuaState,
    value::{LuaValue, metatables},
};

pub(super) extern "C" fn jit_get_local(state: *mut LuaState, register: u8) -> i64 {
    let state = unsafe { &mut *state };
    state.get(register).into_raw()
    // state.stack[state.frame_base + register as usize]
}

pub(super) extern "C" fn jit_set_local(state: *mut LuaState, register: u8, value: i64) {
    let state = unsafe { &mut *state };
    state.set(register, LuaValue::from_raw(value))
    // state.stack[state.frame_base + register as usize] = value;
}

pub(super) extern "C" fn jit_return_one(state: *mut LuaState, value: i64) {
    let state = unsafe { &mut *state };
    state.set(0usize, LuaValue::from_raw(value));
    // state.stack[state.frame_base] = value;
    state.multres = 1;
}

pub(super) extern "C" fn jit_binary_add(state: *mut LuaState, a: i64, b: i64) -> i64 {
    let a = LuaValue::from_raw(a);
    let b = LuaValue::from_raw(b);
    let state = unsafe { &mut *state };

    if a.is_number() && b.is_number() {
        // Safety: We've verified that both a and b are numbers.
        let a = unsafe { a.as_number() };
        let b = unsafe { b.as_number() };

        LuaValue::from_number(a + b).into_raw()
    } else {
        match metatables::handle(state, &metatables::ADD_KEY, vec![a, b]) {
            Some(result) => match result.unwrap() {
                ExecutionResult::Completed(mut values) => {
                    if values.is_empty() {
                        LuaValue::NIL.into_raw()
                    } else {
                        values.swap_remove(0).into_raw()
                    }
                }
                ExecutionResult::Yielded(lua_values) => todo!(),
            },
            None => {
                todo!()
                // return Err(self.op_err($op_name, self.get_ref(lhs_r).as_ref(), Some(self.get_ref(rhs_r).as_ref())));
            }
        }
    }
}
