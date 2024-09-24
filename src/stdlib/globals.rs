use std::{
    ffi::OsString,
    fs,
    path::Path,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, RwLock,
    },
};

use crate::{
    compiler::Compiler,
    value::{
        metatables::{GLOBAL_BOOLEAN_METATABLE, GLOBAL_NUMBER_METATABLE, GLOBAL_STRING_METATABLE},
        LuaClosure, LuaNumber, LuaObject, LuaValue,
    },
    vm::VM,
};

use miette::miette;

use super::{math::MATH, string::STRING, table::TABLE};

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

    Ok(input)
}

pub(crate) fn getmetatable(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = match input.first() {
        Some(value) => value,
        None => {
            return Err(miette::miette!(
                "bad argument #1 to 'getmetatable' (value expected)"
            ));
        }
    };

    let metatable = match value {
        LuaValue::Object(o) => match &*o.read().unwrap() {
            LuaObject::Table(t) => t.get(&LuaValue::String(b"__metatable".to_vec())).cloned(),
            _ => None,
        },
        LuaValue::String(_) => Some(GLOBAL_STRING_METATABLE.clone()),
        LuaValue::Number(_) => Some(GLOBAL_NUMBER_METATABLE.clone()),
        LuaValue::Boolean(_) => Some(GLOBAL_BOOLEAN_METATABLE.clone()),
        _ => None,
    };

    Ok(vec![metatable.unwrap_or(LuaValue::Nil)])
}

fn iter(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let a = match input.first() {
        Some(value) => value,
        None => {
            return Err(miette::miette!(
                "bad argument #1 to 'iter' (value expected)"
            ));
        }
    };
    let i = match input.get(1) {
        Some(LuaValue::Number(n)) => n,
        Some(value) => {
            return Err(miette::miette!(
                "bad argument #2 to 'iter' (number expected, got {})",
                value.type_name()
            ));
        }
        None => {
            return Err(miette::miette!(
                "bad argument #2 to 'iter' (number expected, got no value)"
            ));
        }
    };

    let i = i.integer_repr()?;
    let i = (i + 1).into();

    let v = match a {
        LuaValue::Object(o) => match &*o.read().unwrap() {
            LuaObject::Table(t) => t.get(&i).cloned(),
            _ => None,
        },
        _ => None,
    };

    match v {
        Some(v) if v != LuaValue::Nil => Ok(vec![i, v]),
        _ => Ok(vec![LuaValue::Nil]),
    }
}

pub(crate) fn ipairs(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let Some(value) = input.first() else {
        return Err(miette::miette!(
            "bad argument #1 to 'ipairs' (value expected)"
        ));
    };

    // Return three values: the iterator function, the target value, and the initial index
    Ok(vec![
        LuaObject::NativeFunction("iter", iter).into(),
        value.clone(),
        LuaValue::Number(LuaNumber::Integer(0)),
    ])
}

pub(crate) fn load(vm: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let source = match input.first() {
        Some(LuaValue::String(s)) => s,
        // TODO: Support functions, returning chunks of code
        Some(value) => {
            return Err(miette!(
                "bad argument #1 to 'load' (string expected, got {})",
                value.type_name()
            ));
        }
        None => {
            return Err(miette!(
                "bad argument #1 to 'load' (string expected, got no value)"
            ));
        }
    };

    let source = String::from_utf8_lossy(source).to_string();
    let name = match input.get(1) {
        Some(LuaValue::String(s)) => String::from_utf8_lossy(s).to_string(),
        Some(value) => {
            return Err(miette!(
                "bad argument #2 to 'load' (string expected, got {})",
                value.type_name()
            ));
        }
        None => "chunk".to_string(),
    };

    let compiler = Compiler::new(vm, None, name.clone(), source.into());
    let Ok(chunk_index) = compiler.compile(None) else {
        return Ok(vec![
            LuaValue::Nil,
            LuaValue::String("compilation error".into()),
        ]);
    };

    // Return a function that runs the chunk
    Ok(vec![LuaValue::Object(Arc::new(RwLock::new(
        LuaObject::Closure(LuaClosure {
            name: Some(name),
            chunk: chunk_index,
            ip: 0,
            upvalues: vec![],
        }),
    )))])
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

    let name = String::from_utf8_lossy(name);

    // Allow loading stdlib modules via `require`
    match name.as_ref() {
        "math" => {
            return Ok(vec![MATH.clone()]);
        }
        "string" => {
            return Ok(vec![STRING.clone()]);
        }
        "table" => {
            return Ok(vec![TABLE.clone()]);
        }
        _ => {}
    }

    // TODO: Use `package.path` to search for modules
    let mut name_os = OsString::from(name.as_ref());
    name_os.push(".lua");

    let path = Path::new(&name_os);
    // TODO: Check how to handle `path` being a directory
    if !path.exists() {
        return Err(miette!("module '{name}' not found",));
    }

    let source = fs::read_to_string(path).map_err(|_| miette!("unable to read module {name}",))?;

    let compiler = Compiler::new(vm, Some(path.to_owned()), name.to_string(), source.into());
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

pub(crate) fn setmetatable(_: &mut VM, mut input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    match (input.first(), input.get(1)) {
        (Some(LuaValue::Object(target)), Some(LuaValue::Object(metatable))) => {
            match (&mut *target.write().unwrap(), &*metatable.read().unwrap()) {
                (LuaObject::Table(table), LuaObject::Table(_)) => {
                    table.insert(
                        LuaValue::String(b"__metatable".to_vec()),
                        LuaValue::Object(metatable.clone()),
                    );
                }
                (LuaObject::Table(_), _) => {
                    return Err(miette!(
                        "bad argument #2 to 'setmetatable' (table expected)"
                    ));
                }
                _ => {
                    return Err(miette!(
                        "bad argument #1 to 'setmetatable' (table expected)"
                    ));
                }
            }
        }
        (Some(LuaValue::Object(target)), Some(LuaValue::Nil)) => {
            match &mut *target.write().unwrap() {
                LuaObject::Table(table) => {
                    table.remove(&LuaValue::String(b"__metatable".to_vec()));
                }
                _ => {
                    return Err(miette!(
                        "bad argument #1 to 'setmetatable' (table expected)"
                    ));
                }
            }
        }
        (Some(LuaValue::Object(target)), _) => {
            if matches!(&*target.read().unwrap(), LuaObject::Table(_)) {
                return Err(miette!(
                    "bad argument #2 to 'setmetatable' (table expected)"
                ));
            } else {
                return Err(miette!(
                    "bad argument #1 to 'setmetatable' (table expected)"
                ));
            }
        }
        (_, _) => {
            return Err(miette!(
                "bad argument #1 to 'setmetatable' (table expected)"
            ));
        }
    }

    Ok(vec![input.swap_remove(0)])
}

pub(crate) fn tonumber(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = match input.first() {
        Some(value) => value,
        None => {
            return Err(miette::miette!(
                "bad argument #1 to 'tonumber' (value expected)"
            ));
        }
    };

    let value = match value {
        n @ LuaValue::Number(_) => n.clone(),
        LuaValue::String(s) => {
            let s = String::from_utf8_lossy(s);
            match s.parse::<i64>() {
                Ok(n) => n.into(),
                Err(_) => match s.parse::<f64>() {
                    Ok(n) => n.into(),
                    Err(_) => LuaValue::Nil,
                },
            }
        }
        _ => LuaValue::Nil,
    };

    Ok(vec![value])
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
