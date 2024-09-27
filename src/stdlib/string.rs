use std::sync::LazyLock;

use crate::{
    macros::{get_string, require_number, require_string},
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
    string.insert("rep".into(), LuaObject::NativeFunction("rep", rep).into());
    string.insert(
        "reverse".into(),
        LuaObject::NativeFunction("reverse", reverse).into(),
    );

    string.into()
});


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

fn rep(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let s = require_string!(input, "rep");
    let n = require_number!(input, "rep", 1).integer_repr()?;
    let sep = get_string!(input, "rep", 2)
        .map(|s| s.as_slice())
        .unwrap_or_else(|| b"");
    if n < 1 {
        return Ok(vec![LuaValue::String(vec![])]);
    }

    let mut result = s.clone();
    for _ in 1..n {
        result.extend_from_slice(sep);
        result.extend_from_slice(s);
    }

    Ok(vec![LuaValue::String(result)])
}

fn reverse(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let input = require_string!(input, "reverse");

    let mut reversed = input.clone();
    reversed.reverse();
    Ok(vec![LuaValue::String(reversed)])
}
