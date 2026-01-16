use std::{
    borrow::Cow,
    fs,
    path::{Path, PathBuf},
    sync::{
        Arc, LazyLock, RwLock,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
};

use crate::{
    compiler::Compiler,
    error::lua_error,
    macros::{assert_string, assert_table, require_table},
    stdlib::package,
    value::{
        LuaClosure, LuaNumber, LuaObject, LuaValue,
        callable::{Callable, Method},
        metatables,
    },
    vm::VM,
};

use super::{
    debug::DEBUG, io::IO, math::MATH, os::OS, package::PACKAGE, string::STRING, table::TABLE,
    utf8::UTF8,
};

pub const _VERSION: &str = "Lua 5.4";

pub(crate) fn assert(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let Some(value) = vm.try_arg(0) else {
        return Err(lua_error!("bad argument #1 to 'assert' (value expected)"));
    };

    if !value {
        return match vm.try_arg(1) {
            Some(LuaValue::String(s)) => Err(lua_error!("{s}")),
            Some(LuaValue::Number(n)) => Err(lua_error!("{n}")),
            Some(value) => Err(lua_error!(
                "(error object is a {} value)",
                value.type_name()
            )),
            _ => Err(lua_error!("assertion failed!")),
        };
    }

    Ok(vm.args())
}

static GC_STATUS: LazyLock<AtomicBool> = LazyLock::new(|| AtomicBool::new(true));
static GC_MODE: LazyLock<RwLock<&'static str>> = LazyLock::new(|| RwLock::new("incremental"));
static GC_PAUSE: LazyLock<AtomicUsize> = LazyLock::new(|| AtomicUsize::new(200));
static GC_STEP_MULTIPLIER: LazyLock<AtomicUsize> = LazyLock::new(|| AtomicUsize::new(100));

const GC_PAUSE_MAX: usize = 1020;
const GC_STEP_MULTIPLIER_MAX: usize = 572;

pub(crate) fn collectgarbage(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let result = match vm.try_arg(0) {
        Some(LuaValue::String(s)) if &s == b"collect" => {
            // Performs a full garbage-collection cycle. This is the default option.
            // We don't use a garbage collector, so do nothing.
            vec![LuaValue::Nil]
        }
        Some(LuaValue::String(s)) if &s == b"stop" => {
            // Stops automatic execution of the garbage collector. The collector will run only when explicitly invoked, until a call to restart it.
            // We don't use a garbage collector, so do nothing.
            GC_STATUS.store(false, Ordering::Relaxed);
            vec![LuaValue::Nil]
        }
        Some(LuaValue::String(s)) if &s == b"restart" => {
            // Restarts automatic execution of the garbage collector.
            // We don't use a garbage collector, so do nothing.
            GC_STATUS.store(true, Ordering::Relaxed);
            vec![LuaValue::Nil]
        }
        Some(LuaValue::String(s)) if &s == b"count" => {
            // Returns the total memory in use by Lua (in Kbytes).
            // We don't use a garbage collector, so return 0.
            // FIXME: Account for upvalues held only by call frames.
            vec![
                ((vm.get_stack().iter().map(|v| v.size_bytes()).sum::<u64>() as f64) / 1024.)
                    .into(),
            ]
            // TODO: Maybe we can give an approximation of memory used by the VM?
            // vec![LuaValue::Number(LuaNumber::Float(0.0))]
        }
        Some(LuaValue::String(s)) if &s == b"step" => {
            // Performs a garbage-collection step. The step "size" is controlled by arg. With a zero value, the collector will perform one basic (indivisible) step. For non-zero values, the collector will perform as if that amount of memory (in Kbytes) had been allocated by Lua. Returns true if the step finished a collection cycle.
            // We don't use a garbage collector, so return false.
            vec![LuaValue::Boolean(false)]
        }
        Some(LuaValue::String(s)) if &s == b"isrunning" => {
            // Returns a boolean that tells whether the collector is running (i.e., not stopped).
            vec![LuaValue::Boolean(GC_STATUS.load(Ordering::Relaxed))]
        }
        // Changes the GC mode, no-op in our case. Just return the previous mode.
        Some(LuaValue::String(s)) if &s == b"incremental" => {
            let mut mode = GC_MODE.write().unwrap();
            let previous_mode = *mode;
            *mode = "incremental";
            vec![LuaValue::String(previous_mode.as_bytes().into())]
        }
        Some(LuaValue::String(s)) if &s == b"generational" => {
            let mut mode = GC_MODE.write().unwrap();
            let previous_mode = *mode;
            *mode = "generational";
            vec![LuaValue::String(previous_mode.as_bytes().into())]
        }
        Some(LuaValue::String(s)) if &s == b"setpause" => {
            let val = vm
                .try_arg_as_integer(1)?
                .map(|n| n as usize)
                .unwrap_or(0)
                .min(GC_PAUSE_MAX);
            let previous_value = GC_PAUSE.load(Ordering::Relaxed);
            GC_PAUSE.store(val, Ordering::Relaxed);
            vec![LuaValue::Number(LuaNumber::Integer(previous_value as i64))]
        }
        Some(LuaValue::String(s)) if &s == b"setstepmul" => {
            let val = vm
                .try_arg_as_integer(1)?
                .map(|n| n as usize)
                .unwrap_or(0)
                .min(GC_STEP_MULTIPLIER_MAX);
            let previous_value = GC_STEP_MULTIPLIER.load(Ordering::Relaxed);
            GC_STEP_MULTIPLIER.store(val, Ordering::Relaxed);
            vec![LuaValue::Number(LuaNumber::Integer(previous_value as i64))]
        }
        Some(LuaValue::String(s)) => {
            return Err(lua_error!(
                "bad argument #1 to 'collectgarbage' (invalid option '{s}')",
            ));
        }
        Some(LuaValue::Number(n)) => {
            return Err(lua_error!(
                "bad argument #1 to 'collectgarbage' (invalid option '{}')",
                n.to_string()
            ));
        }
        None => vec![LuaValue::Nil],
        v => {
            return Err(lua_error!(
                "bad argument #1 to 'collectgarbage' (string expected, got {})",
                v.unwrap().type_name(),
            ));
        }
    };

    Ok(result)
}

pub(crate) fn error(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    // TODO: You can call error with no arguments, in which case the error value is nil. How can we
    // do that with miette?
    let message = vm.arg_as_string(0)?;
    if vm.num_args() > 1 {
        // TODO: Second argument is level, which specifies the offset in the call stack at which to
        // raise the error.
    }

    Err(lua_error!("{}", message.to_string()))
}

pub(crate) fn getmetatable(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let value = match vm.try_arg(0) {
        Some(value) => value,
        None => {
            return vm.arg_err(0, "value expected");
        }
    };

    let metatable = value.get_metatable();

    Ok(vec![metatable.unwrap_or(LuaValue::Nil)])
}

fn iter(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let Some(a) = vm.try_arg(0) else {
        return vm.arg_err(0, "value expected");
    };
    let i = vm.arg_as_integer(1)?;

    let i = (i.overflowing_add(1).0).into();

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

pub(crate) fn ipairs(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let value = vm.ensure_arg(0)?;

    // Return three values: the iterator function, the target value, and the initial index
    Ok(vec![
        LuaObject::NativeFunction("iter", iter).into(),
        value.clone(),
        LuaValue::Number(LuaNumber::Integer(0)),
    ])
}

pub(crate) fn load(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let mut load_from_closure = false;
    let source: Vec<u8> = match vm.try_arg(0) {
        Some(LuaValue::String(s)) => s.clone().into(),
        Some(LuaValue::Object(o)) => {
            let object = &*o.read().unwrap();
            let callable: Result<Callable, _> = object.try_into();
            let Ok(callable) = callable else {
                return Err(lua_error!(
                    "bad argument #1 to 'load' (string or function expected, got {})",
                    o.read().unwrap().type_name(),
                ));
            };

            let mut result = vec![];
            loop {
                let mut piece_result = match &callable.method {
                    Method::Closure(closure) => {
                        load_from_closure = true;
                        vm.run_closure(closure.clone(), vec![])
                    }
                    Method::NativeFunction(name, f) => vm.run_native_function(name, *f, 0, 0),
                }?;

                if piece_result.is_empty() {
                    break;
                }

                let piece_result = piece_result.swap_remove(0);
                match piece_result {
                    LuaValue::String(s) => {
                        result.extend(s);
                    }
                    LuaValue::Nil => {
                        break;
                    }
                    _ => {
                        return Err(lua_error!(
                            "unexpected return value from chunk loader (string or nil expected, got {})",
                            piece_result.type_name()
                        ));
                    }
                }
            }

            result
        }
        Some(value) => {
            return vm.arg_err(0, format!("function expected, got {}", value.type_name()));
        }
        None => {
            return vm.arg_err(0, "function expected, got no value");
        }
    };

    let name = match vm.try_arg(1) {
        Some(LuaValue::String(s)) => s.clone(),
        Some(LuaValue::Nil) | None => {
            if load_from_closure {
                b"=(load)".into()
            } else {
                b"chunk".into()
            }
        }
        Some(value) => {
            return vm.arg_err(1, format!("string expected, got {}", value.type_name()));
        }
    };

    let filename: Option<PathBuf> = None;
    let compiler = Compiler::new(
        vm,
        name,
        filename,
        None,
        Arc::new(Cow::Owned(source)),
        vec![],
        true,
    );
    let compiled_chunk = match compiler.compile_chunk(None) {
        Ok(compiled_chunk) => compiled_chunk,
        Err(e) => {
            return Ok(vec![
                LuaValue::Nil,
                LuaValue::String(format!("{}", e).into_bytes().into()),
            ]);
        }
    };

    // Return a function that runs the chunk
    let mut closure = LuaClosure::new(compiled_chunk.chunk_index);

    let env = if vm.num_args() > 3 {
        vm.capture_upvalue(3)
    } else {
        vm.get_global_env()
    };
    closure.upvalues.insert(0, env);

    Ok(vec![closure.into()])
}

pub(crate) fn next(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    require_table!(read, vm.try_arg(0), "next", 0, table, {
        let kv = match vm.try_arg(1) {
            None | Some(LuaValue::Nil) => table.iter().next(),
            Some(key) => {
                // Find the key-value _after_ the given key
                let iter = table.iter();
                let mut key_found = false;
                let mut kv = None;
                for (k, v) in iter {
                    if key_found {
                        kv = Some((k, v));
                        break;
                    }
                    if k == &key {
                        key_found = true;
                    }
                }

                kv
            }
        };

        match kv {
            Some((k, v)) => Ok(vec![k.clone(), v.clone()]),
            None => Ok(vec![LuaValue::Nil]),
        }
    })
}

pub(crate) fn pairs(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let t = vm.ensure_arg(0)?;

    // If t has a metamethod __pairs, calls it with t as argument and returns the first three results from the call.
    if let Some(Callable { method, .. }) = t
        .get_metavalue(&metatables::PAIRS_KEY)
        .and_then(|v| v.try_into().ok())
    {
        let mut result = match method {
            Method::Closure(closure) => {
                let args = vec![t.clone()];
                vm.run_closure(closure, args)?
            }
            Method::NativeFunction(name, f) => vm.run_native_function(name, f, 0, 1)?,
        };
        result.resize(3, LuaValue::Nil);

        return Ok(result);
    }

    Ok(vec![
        LuaObject::NativeFunction("next", next).into(),
        t,
        LuaValue::Nil,
    ])
}

pub(crate) fn pcall(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    // TODO: This will produce error "value expected" rather than "function expected, got no value"
    let function = vm.ensure_arg(0)?;

    let callable_function: Callable = (&function).try_into().map_err(|_| {
        lua_error!(
            "bad argument #1 to 'pcall' (function expected, got {})",
            function.type_name(),
        )
    })?;

    let result = match callable_function.method {
        Method::Closure(closure) => {
            let args = (if callable_function.is_metamethod {
                0
            } else {
                1
            }..vm.num_args())
                .map(|i| vm.arg(i))
                .collect();
            vm.run_closure(closure.clone(), args)
        }
        Method::NativeFunction(name, f) => {
            let (args_r, num_args) = if callable_function.is_metamethod {
                (0, vm.num_args())
            } else {
                (1, vm.num_args() - 1)
            };
            vm.run_native_function(name, f, args_r, num_args as u16)
        }
    };

    Ok(match result {
        Ok(mut values) => {
            values.insert(0, LuaValue::Boolean(true));
            values
        }
        Err(e) => vec![
            LuaValue::Boolean(false),
            LuaValue::String(e.to_string().into_bytes().into()),
        ],
    })
}

pub(crate) fn print(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    for (i, value) in vm.args_ref().into_iter().enumerate() {
        if i != 0 {
            print!("\t");
        }
        print!("{}", value);
    }
    println!();

    Ok(vec![LuaValue::Nil])
}

pub(crate) fn rawequal(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let lhs = vm.ensure_arg(0)?;
    let rhs = vm.ensure_arg(1)?;

    Ok(vec![LuaValue::Boolean(lhs == rhs)])
}

pub(crate) fn rawget(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    require_table!(vm.try_arg(0), "rawget", 0, table, {
        let key = vm.ensure_arg(1)?;
        let value = table.get(&key).cloned().unwrap_or(LuaValue::Nil);

        Ok(vec![value])
    })
}

pub(crate) fn rawlen(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let result = match vm.try_arg(0) {
        Some(LuaValue::Object(o)) => match &*o.read().unwrap() {
            LuaObject::Table(t) => t.len(),
            _ => {
                return Err(lua_error!(
                    "bad argument #1 to 'rawlen' (table or string expected, got {})",
                    o.read().unwrap().type_name(),
                ));
            }
        },
        Some(LuaValue::String(s)) => s.len(),
        Some(v) => {
            return Err(lua_error!(
                "bad argument #1 to 'rawlen' (table or string expected, got {})",
                v.type_name(),
            ));
        }
        None => {
            return Err(lua_error!(
                "bad argument #1 to 'rawlen' (table or string expected, got no value)"
            ));
        }
    } as i64;

    Ok(vec![result.into()])
}

pub(crate) fn rawset(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let key = vm.ensure_arg(1)?;
    match key {
        LuaValue::Nil => {
            return Err(lua_error!("bad argument #2 to 'rawset' (value expected)"));
        }
        LuaValue::Number(LuaNumber::Float(f)) if f.is_nan() => {
            return Err(lua_error!(
                "bad argument #2 to 'rawset' (key cannot be NaN)"
            ));
        }
        _ => {}
    }

    let value = vm.ensure_arg(2)?;

    require_table!(write, vm.try_arg(0), "rawset", 0, table, {
        table.insert(key.clone(), value);
    });

    Ok(vec![vm.ensure_arg(0)?])
}

pub(crate) fn require(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let name = vm.arg_as_string(0)?;

    // Allow loading stdlib modules via `require`
    match name.as_slice() {
        b"debug" => {
            return Ok(vec![DEBUG.clone()]);
        }
        b"io" => {
            return Ok(vec![IO.clone()]);
        }
        b"math" => {
            return Ok(vec![MATH.clone()]);
        }
        b"os" => {
            return Ok(vec![OS.clone()]);
        }
        b"string" => {
            return Ok(vec![STRING.clone()]);
        }
        b"table" => {
            return Ok(vec![TABLE.clone()]);
        }
        b"utf8" => {
            return Ok(vec![UTF8.clone()]);
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
    let called_from = vm
        .get_current_chunk()
        .expect("don't call require from native function")
        .get_filename();
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

        let mut path_str = String::from_utf8_lossy(&option);
        let mut path = Path::new(path_str.as_ref()).to_owned();
        if path.is_relative() {
            // Resolve to the current file's directory
            if let Some(filename) = called_from {
                path = filename
                    .parent()
                    .map(|parent| parent.join(&path).canonicalize())
                    .transpose()
                    .map_err(|err| {
                        lua_error!(
                            "error loading module '{name}' from file '{source}': cannot resolve path '{}': {err}",
                            path.display(),
                            source = &filename.to_string_lossy(),
                        )
                    })?
                    .unwrap_or(path);
                path_str = path.to_string_lossy();
            }
        }
        if !path.exists() {
            continue;
        }
        if path.is_dir() {
            return Err(lua_error!(
                "error loading module '{name}' from file '{source}': cannot read '{dir_name}': is a directory",
                source = "<unknown>",
                dir_name = path
                    .file_name()
                    .map(|s| s.to_string_lossy())
                    .unwrap_or_else(|| "<unknown>".into())
            ));
        }

        // Check if it's already loaded
        let path_value = LuaValue::String(path_str.as_bytes().into());
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

        let source = fs::read(&path).map_err(|_| lua_error!("unable to read module {name}"))?;

        let compiler = Compiler::new(
            vm,
            name.clone(),
            Some(path),
            None,
            Arc::new(source.into()),
            vec![],
            false,
        );
        let compiled_chunk = compiler.compile_chunk(None)?;
        let mut result = vm.run_chunk(compiled_chunk.chunk_index)?;
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

    Err(lua_error!("module '{name}' not found"))
}

pub(crate) fn select(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let index = match vm.try_arg(0) {
        Some(LuaValue::Number(n)) => n,
        Some(LuaValue::String(s)) => {
            if &s == b"#" {
                return Ok(vec![LuaValue::Number(LuaNumber::Integer(
                    vm.num_args() as i64 - 1,
                ))]);
            } else {
                return Err(lua_error!(
                    "bad argument #1 to 'select' (number expected, got string)"
                ));
            }
        }
        Some(value) => {
            return Err(lua_error!(
                "bad argument #1 to 'select' (number expected, got {})",
                value.type_name(),
            ));
        }
        None => {
            return Err(lua_error!(
                "bad argument #1 to 'select' (number expected, got no value)"
            ));
        }
    };

    let index = index.integer_repr()?;
    if index < 1 {
        return Err(lua_error!(
            "bad argument #1 to 'select' (index out of range)"
        ));
    }

    Ok(vm.args().into_iter().skip(index as usize).collect())
}

pub(crate) fn setmetatable(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    match (vm.try_arg(0), vm.try_arg(1)) {
        (Some(LuaValue::Object(target)), Some(LuaValue::Object(metatable))) => {
            match (&mut *target.write().unwrap(), &*metatable.read().unwrap()) {
                (LuaObject::Table(table), LuaObject::Table(_)) => {
                    table.set_metatable(Some(LuaValue::Object(metatable.clone())));
                }
                (LuaObject::Table(_), _) => {
                    return Err(lua_error!(
                        "bad argument #2 to 'setmetatable' (table expected)"
                    ));
                }
                _ => {
                    return Err(lua_error!(
                        "bad argument #1 to 'setmetatable' (table expected)"
                    ));
                }
            }
        }
        (Some(LuaValue::Object(target)), Some(LuaValue::Nil)) => {
            match &mut *target.write().unwrap() {
                LuaObject::Table(table) => {
                    table.set_metatable(None);
                }
                _ => {
                    return Err(lua_error!(
                        "bad argument #1 to 'setmetatable' (table expected)"
                    ));
                }
            }
        }
        (Some(LuaValue::Object(target)), _) => {
            if matches!(&*target.read().unwrap(), LuaObject::Table(_)) {
                return Err(lua_error!(
                    "bad argument #2 to 'setmetatable' (table expected)"
                ));
            } else {
                return Err(lua_error!(
                    "bad argument #1 to 'setmetatable' (table expected)"
                ));
            }
        }
        (_, _) => {
            return Err(lua_error!(
                "bad argument #1 to 'setmetatable' (table expected)"
            ));
        }
    }

    Ok(vec![vm.arg(0)])
}

pub(crate) fn tonumber(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let value = match vm.ensure_arg(0)? {
        n @ LuaValue::Number(_) => n.clone(),
        LuaValue::String(s) => {
            let s = String::from_utf8_lossy(&s);
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

pub(crate) fn tostring(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let value = vm.ensure_arg(0)?;

    Ok(vec![LuaValue::String(
        value.to_string().into_bytes().into(),
    )])
}

pub(crate) fn r#type(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let value = vm.ensure_arg(0)?;

    Ok(vec![LuaValue::String(
        value.type_name().bytes().collect::<Vec<_>>().into(),
    )])
}

static CONTROL_MESSAGES_ENABLED: AtomicBool = AtomicBool::new(false);

pub(crate) fn warn(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    let strings: Vec<_> = vm
        .args_ref()
        .iter()
        .enumerate()
        .map(|(i, v)| match &**v {
            LuaValue::String(s) => Ok(s.to_string()),
            _ => Err(lua_error!(
                "bad argument #{i} to 'warn' (string expected, got {})",
                v.type_name(),
            )),
        })
        .collect::<Result<_, _>>()?;

    // Check for control messages
    let first = strings
        .first()
        .ok_or_else(|| lua_error!("bad argument #1 to 'warn' (string expected)"))?;
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

pub(crate) fn xpcall(vm: &mut VM) -> crate::Result<Vec<LuaValue>> {
    // TODO: Will produce error "value expected" rather than "function expected, got no value"
    let function = vm.ensure_arg(0)?;
    let msgh = vm.ensure_arg(1)?;

    let function_callable: Callable = (&function).try_into().map_err(|_| {
        lua_error!(
            "bad argument #1 to 'xpcall' (function expected, got {})",
            function.type_name(),
        )
    })?;
    let msgh: Callable = (&msgh).try_into().map_err(|_| {
        lua_error!(
            "bad argument #2 to 'xpcall' (function expected, got {})",
            msgh.type_name(),
        )
    })?;

    let mut result = match function_callable.method {
        Method::Closure(closure) => {
            let mut args = vec![function];
            args.extend((2..vm.num_args()).map(|i| vm.arg(i)));
            vm.run_closure(closure.clone(), args)
        }
        Method::NativeFunction(name, f) => {
            let (args_r, num_args) = if function_callable.is_metamethod {
                let mut args = vec![function];
                args.extend((2..vm.num_args()).map(|i| vm.arg(i)));
                let num_args = args.len();
                let args_r = vm.extend_stack(args);
                (args_r, num_args)
            } else {
                (1, vm.num_args() - 1)
            };

            vm.run_native_function(name, f, args_r, num_args as u16)
        }
    };

    const MAX_MESSAGE_HANDLER_DEPTH: usize = 200;
    for _ in 0..MAX_MESSAGE_HANDLER_DEPTH {
        match result {
            Ok(_) => break,
            Err(e) => {
                result = match &msgh.method {
                    Method::Closure(closure) => vm.run_closure(
                        closure.clone(),
                        vec![LuaValue::String(e.to_string().into_bytes().into())],
                    ),
                    Method::NativeFunction(name, f) => {
                        let args_r =
                            vm.extend_stack([LuaValue::String(e.to_string().into_bytes().into())]);
                        vm.run_native_function(name, *f, args_r, 1)
                    }
                };
            }
        }
    }

    Ok(match result {
        Ok(mut values) => {
            values.insert(0, LuaValue::Boolean(true));
            values
        }
        Err(e) => vec![
            LuaValue::Boolean(false),
            LuaValue::String(e.to_string().into_bytes().into()),
        ],
    })
}
