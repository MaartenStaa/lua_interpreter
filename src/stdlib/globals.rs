use std::{
    ffi::OsString,
    fs,
    path::Path,
    sync::atomic::{AtomicBool, Ordering},
};

use crate::{
    compiler::Compiler,
    value::{LuaNumber, LuaValue},
    vm::VM,
};

use miette::miette;

pub const _VERSION: &str = "LuaRust 5.4";

pub(crate) fn assert(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let Some(value) = input.first() else {
        return Err(miette::miette!(
            "bad argument #1 to 'assert' (value expected)"
        ));
    };

    if !value {
        return match input.get(1) {
            Some(LuaValue::String(s)) => Err(miette!("{}", String::from_utf8_lossy(s))),
            Some(LuaValue::Number(n)) => Err(miette!("{}", n)),
            Some(value) => Err(miette!("(error object is a {} value)", value.type_name())),
            _ => Err(miette!("assertion failed!")),
        };
    }

    Ok(vec![LuaValue::Nil])
}

pub(crate) fn print(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    for (i, value) in input.into_iter().enumerate() {
        if i != 0 {
            print!("\t");
        }
        print!("{}", value);
    }
    println!();

    Ok(vec![LuaValue::Nil])
}

pub(crate) fn require(vm: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    // TODO: Check if the module is already loaded

    let name = match input.first() {
        Some(LuaValue::String(s)) => s,
        Some(value) => {
            return Err(miette!(
                "bad argument #1 to 'require' (string expected, got {})",
                value.type_name()
            ));
        }
        None => {
            return Err(miette!(
                "bad argument #1 to 'require' (string expected, got no value)"
            ));
        }
    };

    // TODO: Use `package.path` to search for modules
    let name = String::from_utf8_lossy(name);
    let mut name_os = OsString::from(name.as_ref());
    name_os.push(".lua");

    let path = Path::new(&name_os);
    // TODO: Check how to handle `path` being a directory
    if !path.exists() {
        return Err(miette!("module '{name}' not found",));
    }

    let source = fs::read_to_string(path).map_err(|_| miette!("unable to read module {name}",))?;

    let compiler = Compiler::new(vm, path.to_owned(), source.into());
    let chunk_index = compiler.compile(None)?;
    let result = vm.run_chunk(chunk_index)?;

    Ok(vec![result, LuaValue::Nil])
}

pub(crate) fn select(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let index = match input.first() {
        Some(LuaValue::Number(n)) => n,
        Some(LuaValue::String(s)) => {
            if s == b"#" {
                return Ok(vec![LuaValue::Number(LuaNumber::Integer(
                    input.len() as i64 - 1,
                ))]);
            } else {
                return Err(miette!(
                    "bad argument #1 to 'select' (number expected, got string)"
                ));
            }
        }
        Some(value) => {
            return Err(miette!(
                "bad argument #1 to 'select' (number expected, got {})",
                value.type_name()
            ));
        }
        None => {
            return Err(miette!(
                "bad argument #1 to 'select' (number expected, got no value)"
            ));
        }
    };

    let index = index.integer_repr()?;
    if index < 1 {
        return Err(miette!("bad argument #1 to 'select' (index out of range)"));
    }

    Ok(input.into_iter().skip(index as usize).collect())
}

pub(crate) fn tostring(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = input.first().unwrap_or(&LuaValue::Nil);

    Ok(vec![LuaValue::String(value.to_string().into_bytes())])
}

pub(crate) fn r#type(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = input.first().unwrap_or(&LuaValue::Nil);
    Ok(vec![LuaValue::String(value.type_name().bytes().collect())])
}

static CONTROL_MESSAGES_ENABLED: AtomicBool = AtomicBool::new(false);

pub(crate) fn warn(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
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

        return Ok(vec![LuaValue::Nil]);
    }

    if CONTROL_MESSAGES_ENABLED.load(Ordering::Relaxed) {
        eprint!("Lua warning: ");
        for s in strings {
            eprint!("{}", s);
        }
        eprintln!();
    }

    Ok(vec![LuaValue::Nil])
}
