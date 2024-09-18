use std::sync::LazyLock;

use crate::value::{LuaNumber, LuaObject, LuaTable, LuaValue};

pub static STRING: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut string = LuaTable::new();

    string.insert("len".into(), LuaObject::NativeFunction(len).into());
    string.insert("reverse".into(), LuaObject::NativeFunction(reverse).into());

    string.into()
});

macro_rules! require_string {
    ($input:expr, $name:expr, $i:expr) => {
        match $input.get($i) {
            Some(LuaValue::String(s)) => s,
            Some(v) => {
                return Err(miette::miette!(
                    "bad argument #{n} to '{name}' (string expected, got {type_name})",
                    n = $i + 1,
                    name = $name,
                    type_name = v.type_name()
                ));
            }
            None => {
                return Err(miette::miette!(
                    "bad argument #{n} to '{name}' (value expected)",
                    n = $i + 1,
                    name = $name
                ));
            }
        }
    };
    ($input:expr, $name:expr) => {
        require_string!($input, $name, 0)
    };
}

fn len(input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let input = require_string!(input, "len");

    Ok(vec![LuaValue::Number(LuaNumber::Integer(
        input.len() as i64
    ))])
}

fn reverse(input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let input = require_string!(input, "reverse");

    let mut reversed = input.clone();
    reversed.reverse();
    Ok(vec![LuaValue::String(reversed)])
}
