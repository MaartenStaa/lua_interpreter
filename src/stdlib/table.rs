use std::sync::LazyLock;

use crate::{
    error::lua_error,
    macros::require_table,
    state::LuaState,
    value::{LuaObject, LuaTable, LuaValue},
};

pub static TABLE: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut table = LuaTable::new();

    table.insert(
        "concat".into(),
        LuaObject::NativeFunction("table.concat", concat).into(),
    );
    table.insert(
        "insert".into(),
        LuaObject::NativeFunction("table.insert", insert).into(),
    );
    table.insert(
        "pack".into(),
        LuaObject::NativeFunction("table.pack", pack).into(),
    );
    table.insert(
        "unpack".into(),
        LuaObject::NativeFunction("table.unpack", unpack).into(),
    );

    table.into()
});

fn concat(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    // Temporary storage for number conversion, avoids needing to clone a string separator to match
    // the Vec<u8> type.
    let temp;
    let sep = state.try_arg(1);
    let sep = match sep.as_ref() {
        Some(LuaValue::String(s)) => Some(s.as_slice()),
        Some(LuaValue::Number(n)) => {
            temp = Some(n.to_string());
            temp.as_ref().map(|s| s.as_bytes())
        }
        None | Some(LuaValue::Nil) => None,
        Some(v) => {
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

            match t.get(&k.into()).unwrap_or(&LuaValue::Nil) {
                LuaValue::String(s) => result.extend(s),
                LuaValue::Number(n) => result.extend(n.to_string().as_bytes()),
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

    Ok(vec![LuaValue::String(result.into())])
}

fn insert(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    require_table!(write, state.try_arg(0), "table.insert", 0, table, {
        let len = table.len() as i64;

        match state.try_arg(1) {
            Some(LuaValue::Number(n)) => {
                let i = n.integer_repr()?;
                let value = state.ensure_arg(2)?;

                if i < 1 || i > len + 1 {
                    return state.arg_err(1, "position out of bounds");
                }

                for k in (i..=len).rev() {
                    let v = table.get(&k.into()).cloned().unwrap_or(LuaValue::Nil);
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

        Ok(vec![])
    })
}

fn pack(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let mut result = LuaTable::new();

    let len = state.num_args();
    for i in 0..len {
        let v = state.arg(i);
        result.insert((i as i64 + 1).into(), v);
    }

    result.insert("n".into(), (len as i64).into());

    Ok(vec![result.into()])
}

fn unpack(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    require_table!(read, state.try_arg(0), "table.unpack", 0, table, {
        let i = state.try_arg_as_integer(1)?.unwrap_or(1);
        let j = state.try_arg_as_integer(2)?.unwrap_or(table.len() as i64);

        let mut result = Vec::new();
        for k in i..=j {
            result.push(table.get(&k.into()).unwrap_or(&LuaValue::Nil).clone());
        }

        Ok(result)
    })
}
