use std::{
    fs,
    path::Path,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, RwLock,
    },
};

use crate::{
    compiler::Compiler,
    macros::{assert_string, assert_table, require_table},
    stdlib::package,
    value::{metatables::METATABLE_KEY, LuaClosure, LuaNumber, LuaObject, LuaValue},
    vm::VM,
};

use miette::miette;

use super::{
    debug::DEBUG, io::IO, math::MATH, os::OS, package::PACKAGE, string::STRING, table::TABLE,
};

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

    let metatable = value.get_metatable();

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
        Some(LuaValue::String(s)) => s.clone(),
        Some(LuaValue::Object(o)) => match o.read().unwrap().clone() {
            LuaObject::Closure(closure) => {
                let mut result = Vec::new();
                loop {
                    let mut piece_result = vm.run_closure(closure.clone(), vec![])?;
                    let piece_result = piece_result.swap_remove(0);
                    match piece_result {
                        LuaValue::String(s) => {
                            result.extend(s);
                        }
                        LuaValue::Nil => {
                            break;
                        }
                        _ => {
                            return Err(miette!(
                                    "unexpected return value from chunk loader (string or nil expected, got {})",
                                    piece_result.type_name()
                                ));
                        }
                    }
                }

                result
            }
            LuaObject::NativeFunction(_, f) => {
                let result = f(vm, vec![])?;
                return Ok(result);
            }
            _ => {
                return Err(miette!(
                    "bad argument #1 to 'load' (string or function expected, got {})",
                    o.read().unwrap().type_name()
                ));
            }
        },
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

    let source = String::from_utf8_lossy(&source).to_string();
    let name = match input.get(1) {
        Some(LuaValue::String(s)) => String::from_utf8_lossy(s).to_string(),
        Some(LuaValue::Nil) | None => "chunk".to_string(),
        Some(value) => {
            return Err(miette!(
                "bad argument #2 to 'load' (string expected, got {})",
                value.type_name()
            ));
        }
    };

    let env = match input.get(3) {
        Some(LuaValue::Object(o)) => Some(o.clone()),
        Some(LuaValue::Nil) | None => None,
        Some(value) => {
            return Err(miette!(
                "bad argument #4 to 'load' (table expected, got {})",
                value.type_name()
            ));
        }
    };

    let compiler = Compiler::new(vm, env, None, name.clone(), source.into());
    let chunk_index = match compiler.compile(None) {
        Ok(chunk_index) => chunk_index,
        Err(e) => {
            return Ok(vec![
                LuaValue::Nil,
                LuaValue::String(format!("{}", e).into_bytes()),
            ]);
        }
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

pub(crate) fn pcall(vm: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let mut input = input.into_iter();
    let function = match input.next() {
        Some(LuaValue::Object(o)) => o,
        Some(value) => {
            return Err(miette!(
                "bad argument #1 to 'pcall' (function expected, got {})",
                value.type_name()
            ));
        }
        None => {
            return Err(miette!(
                "bad argument #1 to 'pcall' (function expected, got no value)"
            ));
        }
    };

    let args = input.collect();
    let result = match function.read().unwrap().clone() {
        LuaObject::Closure(closure) => vm.run_closure(closure, args),
        LuaObject::NativeFunction(_, f) => f(vm, args),
        _ => {
            return Err(miette!(
                "bad argument #1 to 'pcall' (function expected, got {})",
                function.read().unwrap().type_name()
            ));
        }
    };

    Ok(match result {
        Ok(mut values) => {
            values.insert(0, LuaValue::Boolean(true));
            values
        }
        Err(e) => vec![
            LuaValue::Boolean(false),
            LuaValue::String(e.to_string().into_bytes()),
        ],
    })
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

pub(crate) fn rawget(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    require_table!(input, "rawget", 0, table, {
        let key = input
            .get(1)
            .ok_or_else(|| miette!("bad argument #2 to 'rawget' (value expected)"))?;
        let value = table.get(key).cloned().unwrap_or(LuaValue::Nil);

        Ok(vec![value])
    })
}

pub(crate) fn rawset(_: &mut VM, mut input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let key = input
        .get(1)
        .cloned()
        .ok_or_else(|| miette!("bad argument #2 to 'rawset' (value expected)"))?;
    match key {
        LuaValue::Nil => {
            return Err(miette!("bad argument #2 to 'rawset' (value expected)"));
        }
        LuaValue::Number(LuaNumber::Float(f)) if f.is_nan() => {
            return Err(miette!("bad argument #2 to 'rawset' (key cannot be NaN)"));
        }
        _ => {}
    }

    if input.len() < 3 {
        return Err(miette!("bad argument #3 to 'rawset' (value expected)"));
    }
    let value = input.swap_remove(2);

    require_table!(write, input, "rawset", 0, table, {
        table.insert(key.clone(), value);
    });

    Ok(vec![input.swap_remove(0)])
}

pub(crate) fn require(vm: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
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

    let name_str = String::from_utf8_lossy(name);

    // Allow loading stdlib modules via `require`
    match name_str.as_ref() {
        "debug" => {
            return Ok(vec![DEBUG.clone()]);
        }
        "io" => {
            return Ok(vec![IO.clone()]);
        }
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

    let package = PACKAGE.clone();
    let package_path = assert_string!(assert_table!(&package, package_table, {
        package_table
            .get(&"path".into())
            .expect("package.path exists")
            .clone()
    }));

    // Loop through each package path option
    let package_config = &package::CONFIG;
    for mut option in package_path
        .split(|c| *c == package_config.template_separator)
        .map(|s| s.to_vec())
    {
        while let Some(index) = option
            .iter()
            .position(|c| *c == package_config.substitution_character)
        {
            let (prefix, suffix) = option.split_at(index);
            option = prefix
                .iter()
                .chain(name.iter())
                .chain(suffix.iter().skip(1))
                .cloned()
                .collect();
        }

        // TODO: Do we need to resolve this relative to the current file?
        let path_str = String::from_utf8_lossy(&option);
        // let path_os_str = OsString::from_(&option);
        let path = Path::new(path_str.as_ref());
        if !path.exists() {
            continue;
        }
        if path.is_dir() {
            return Err(
                miette!(
                    "error loading module '{name_str}' from file '{source}': cannot read '{dir_name}': is a directory",
                    source = "<unknown>",
                    dir_name = path.file_name().map(|s| s.to_string_lossy()).unwrap_or_else(|| "<unknown>".into())
                )
            );
        }

        // Check if it's already loaded
        let path_value = LuaValue::String(path_str.as_bytes().to_vec());
        assert_table!(&package, package_table, {
            let loaded = package_table
                .get(&"loaded".into())
                .expect("package.loaded exists");
            assert_table!(loaded, loaded_table, {
                if let Some(value) = loaded_table.get(&path_value) {
                    return Ok(vec![value.clone()]);
                }
            })
        });

        let source =
            fs::read_to_string(path).map_err(|_| miette!("unable to read module {name_str}",))?;

        let compiler = Compiler::new(
            vm,
            None,
            Some(path.to_owned()),
            name_str.to_string(),
            source.into(),
        );
        let chunk_index = compiler.compile(None)?;
        let mut result = vm.run_chunk(chunk_index)?;
        let result = result.swap_remove(0);

        assert_table!(write, package, package_table, {
            let loaded = package_table
                .get_mut(&"loaded".into())
                .expect("package.loaded exists");
            assert_table!(write, loaded, loaded_table, {
                loaded_table.insert(path_value.clone(), result.clone());
            });
        });

        return Ok(vec![result, path_value]);
    }

    Err(miette!("module '{name_str}' not found",))
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
                    table.insert(METATABLE_KEY.clone(), LuaValue::Object(metatable.clone()));
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
                    table.remove(&METATABLE_KEY);
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
