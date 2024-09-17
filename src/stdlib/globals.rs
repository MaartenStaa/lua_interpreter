use std::sync::atomic::{AtomicBool, Ordering};

use crate::value::LuaValue;

use miette::miette;

pub const _VERSION: &str = "LuaRust 5.4";

pub(crate) fn assert(input: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let Some(value) = input.first() else {
        return Err(miette::miette!(
            "bad argument #1 to 'assert' (value expected)"
        ));
    };

    if !value {
        match input.get(1) {
            Some(LuaValue::String(s)) => {
                return Err(miette!("{}", String::from_utf8_lossy(s)));
            }
            Some(LuaValue::Number(n)) => {
                return Err(miette!("{}", n));
            }
            Some(value) => {
                return Err(miette!("(error object is a {} value)", value.type_name()));
            }
            _ => return Err(miette!("assertion failed!")),
        }
    }

    Ok(LuaValue::Nil)
}

pub(crate) fn print(input: Vec<LuaValue>) -> miette::Result<LuaValue> {
    for (i, value) in input.into_iter().enumerate() {
        if i != 0 {
            print!("\t");
        }
        print!("{}", value);
    }
    println!();

    Ok(LuaValue::Nil)
}

pub(crate) fn tostring(input: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let value = input.first().unwrap_or(&LuaValue::Nil);

    Ok(LuaValue::String(value.to_string().into_bytes()))
}

pub(crate) fn r#type(input: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let value = input.first().unwrap_or(&LuaValue::Nil);
    Ok(LuaValue::String(value.type_name().bytes().collect()))
}

static CONTROL_MESSAGES_ENABLED: AtomicBool = AtomicBool::new(false);

pub(crate) fn warn(input: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let strings: Vec<_> = input
        .iter()
        .enumerate()
        .map(|(i, v)| match v {
            LuaValue::String(s) => Ok(String::from_utf8_lossy(s).to_string()),
            _ => Err(miette!(
                "bad argument #{i} to 'warn' (string expected, got {})",
                v.type_name()
            )),
        })
        .collect::<Result<_, _>>()?;

    // Check for control messages
    let first = strings
        .first()
        .ok_or_else(|| miette!("bad argument #1 to 'warn' (string expected)"))?;
    if first.starts_with('@') {
        match first.as_str() {
            "@on" => CONTROL_MESSAGES_ENABLED.store(true, Ordering::Relaxed),
            "@off" => CONTROL_MESSAGES_ENABLED.store(false, Ordering::Relaxed),
            _ => {
                // Unknown control messages are ignored
            }
        }

        return Ok(LuaValue::Nil);
    }

    if CONTROL_MESSAGES_ENABLED.load(Ordering::Relaxed) {
        eprint!("Lua warning: ");
        for s in strings {
            eprint!("{}", s);
        }
        eprintln!();
    }

    Ok(LuaValue::Nil)
}
