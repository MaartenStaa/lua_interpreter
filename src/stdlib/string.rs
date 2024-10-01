use std::{cmp::Ordering, sync::LazyLock};

use crate::{
    macros::{get_string, require_number, require_string},
    value::{LuaNumber, LuaObject, LuaTable, LuaValue},
    vm::VM,
};

pub static STRING: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut string = LuaTable::new();

    string.insert(
        "find".into(),
        LuaObject::NativeFunction("find", find).into(),
    );
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

fn find(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let s = require_string!(input, "find");
    let pattern = require_string!(input, "find", 1);
    let mut init = match input.get(2) {
        Some(LuaValue::Number(LuaNumber::Integer(i))) => *i,
        Some(LuaValue::Number(f @ LuaNumber::Float(_))) => f.integer_repr()?,
        Some(v) => {
            return Err(miette::miette!(
                "bad argument #3 to 'find' (number expected, got {type_name})",
                type_name = v.type_name()
            ));
        }
        None => 1,
    };

    if init == 0 {
        init = 1;
    }

    if pattern.is_empty() {
        return Ok(vec![
            LuaValue::Number(LuaNumber::Integer(init)),
            LuaValue::Number(LuaNumber::Integer(init - 1)),
        ]);
    }

    let s = match init.cmp(&0) {
        Ordering::Less => &s[(s.len() as i64 + init) as usize..],
        Ordering::Greater => &s[(init - 1) as usize..],
        Ordering::Equal => unreachable!(),
    };

    // TODO: Handle actual patterns

    let start = s.windows(pattern.len()).position(|w| w == pattern);
    Ok(match start {
        Some(start) => {
            vec![
                LuaValue::Number(LuaNumber::Integer(start as i64 + init)),
                LuaValue::Number(LuaNumber::Integer(
                    start as i64 + init - 1 + pattern.len() as i64,
                )),
            ]
        }
        None => vec![LuaValue::Nil],
    })
}

fn format(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let mut format_string = require_string!(input, "format").clone();

    let mut format_from = 0;
    for v in input.iter().skip(1) {
        let Some((index, format)) =
            format_string
                .iter()
                .enumerate()
                .skip(format_from)
                .find_map(|(i, c)| {
                    if *c == b'%' {
                        match format_string.get(i + 1) {
                            Some(b'%') | None => None,
                            Some(f) => Some((i, *f as char)),
                        }
                    } else {
                        None
                    }
                })
        else {
            break;
        };

        let formatted_value = match format {
            's' => v.to_string(),
            'd' => match v {
                LuaValue::Number(n) => n.to_string(),
                _ => {
                    return Err(miette::miette!(
                        "bad argument #{} to 'format' (number expected, got {type_name})",
                        input.iter().position(|x| x == v).unwrap() + 1,
                        type_name = v.type_name()
                    ));
                }
            },
            other => return Err(miette::miette!("unsupported format specifier: {:?}", other)),
        };

        format_string.splice(index..index + 2, formatted_value.bytes());
        format_from = index + formatted_value.len();
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
