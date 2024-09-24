use std::sync::LazyLock;

use crate::{
    value::{LuaNumber, LuaObject, LuaTable, LuaValue},
    vm::VM,
};

pub static STRING: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut string = LuaTable::new();

    string.insert(
        "format".into(),
        LuaObject::NativeFunction("format", format).into(),
    );
    string.insert("len".into(), LuaObject::NativeFunction("len", len).into());
    string.insert(
        "reverse".into(),
        LuaObject::NativeFunction("reverse", reverse).into(),
    );

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

fn format(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let mut format_string = require_string!(input, "format").clone();

    for (i, v) in input.iter().skip(1).enumerate() {
        let Some((index, format)) = format_string.iter().enumerate().find_map(|(i, c)| {
            if *c == b'%' {
                match format_string.get(i + 1) {
                    Some(b's') => Some((i, "s")),
                    Some(b'%') => None,
                    Some(f) => todo!("format specifier: {:?}", f),
                    None => None,
                }
            } else {
                None
            }
        }) else {
            break;
        };

        let formatted_value = match format {
            "s" => v.to_string(),
            _ => unreachable!(),
        };

        format_string.splice(index..index + 2, formatted_value.bytes());
    }

    Ok(vec![LuaValue::String(format_string)])
}

fn len(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let input = require_string!(input, "len");

    Ok(vec![LuaValue::Number(LuaNumber::Integer(
        input.len() as i64
    ))])
}

fn reverse(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let input = require_string!(input, "reverse");

    let mut reversed = input.clone();
    reversed.reverse();
    Ok(vec![LuaValue::String(reversed)])
}
