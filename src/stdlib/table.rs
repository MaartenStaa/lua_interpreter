use std::sync::LazyLock;

use crate::{
    error::lua_error,
    macros::{lua_return, native_function, require_table},
    state::LuaState,
    value::{IntegerRepr, LuaObject, LuaTable, LuaValue},
};

pub static TABLE: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut table = LuaTable::new();

    table.insert("concat".into(), native_function!("table"; concat));
    table.insert("insert".into(), native_function!("table"; insert));
    table.insert("pack".into(), native_function!("table"; pack));
    table.insert("unpack".into(), native_function!("table"; unpack));

    table.into()
});

fn concat(state: &mut LuaState) -> crate::Result<usize> {
    // Temporary storage for number conversion, avoids needing to clone a string separator to match
    // the Vec<u8> type.
    let temp;
    let sep = state.arg(1);
    let sep = match sep {
        v if v.is_string() => {
            temp = Some(unsafe { v.into_string() }.into_inner());
            temp.as_deref()
        }
        v if v.is_number() => {
            temp = Some(unsafe { v.as_number() }.to_string().into_bytes());
            temp.as_deref()
        }
        v if v.is_nil() => None,
        v => {
            return Err(lua_error!(
                "bad argument #1 to 'table.concat' (string expected, got {type_name})",
                type_name = v.type_name()
            ));
        }
    };
    let i = state.try_arg_as_integer(2)?.unwrap_or(1);
    let j = state.try_arg_as_integer(3)?;

    let mut result = Vec::new();

    require_table!(state.try_arg(0), "table.concat", 0, t, {
        let j = j.unwrap_or_else(|| t.len() as i64);
        for k in i..=j {
            if k > i
                && let Some(sep) = sep.as_ref()
            {
                result.extend(*sep);
            }

            match t.get(&k.into()).unwrap_or(&LuaValue::NIL) {
                v if v.is_string() => result.extend(unsafe { v.as_string() }),
                v if v.is_number() => {
                    result.extend(unsafe { v.as_number() }.to_string().as_bytes());
                }
                v => {
                    return Err(lua_error!(
                        "invalid value ({type_name}) at index {index} in table for 'table.concat'",
                        type_name = v.type_name(),
                        index = k
                    ));
                }
            }
        }
    });

    lua_return!(state; result.into())
}

fn insert(state: &mut LuaState) -> crate::Result<usize> {
    require_table!(write, state.try_arg(0), "table.insert", 0, table, {
        let len = table.len() as i64;

        match state.try_arg(1) {
            Some(v) if v.is_number() => {
                let n = unsafe { v.as_number() };
                let i = n.integer_repr()?;
                let value = state.ensure_arg(2)?;

                if i < 1 || i > len + 1 {
                    return state.arg_err(1, "position out of bounds");
                }

                for k in (i..=len).rev() {
                    let v = table.get(&k.into()).cloned().unwrap_or(LuaValue::NIL);
                    table.insert((k + 1).into(), v);
                }

                table.insert(i.into(), value);
            }
            Some(value) => {
                table.insert((len + 1).into(), value.clone());
            }
            None => {
                return Err(lua_error!("wrong number of arguments to 'table.insert'"));
            }
        }

        lua_return!(state)
    })
}

fn pack(state: &mut LuaState) -> crate::Result<usize> {
    let mut result = LuaTable::new();

    let len = state.num_args();
    for i in 0..len {
        let v = state.arg(i);
        result.insert((i as i64 + 1).into(), v);
    }

    result.insert("n".into(), (len as i64).into());

    lua_return!(state; result.into())
}

fn unpack(state: &mut LuaState) -> crate::Result<usize> {
    require_table!(read, state.try_arg(0), "table.unpack", 0, table, {
        let i = state.try_arg_as_integer(1)?.unwrap_or(1);
        let j = state.try_arg_as_integer(2)?.unwrap_or(table.len() as i64);
        let num_values = (j - i + 1) as usize;
        state.ensure_stack_space(num_values);

        for (register, k) in (i..=j).enumerate() {
            state.set(
                register,
                table.get(&k.into()).unwrap_or(&LuaValue::NIL).clone(),
            );
        }

        Ok(num_values)
    })
}
