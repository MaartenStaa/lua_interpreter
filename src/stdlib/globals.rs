use std::{
    ffi::OsStr,
    fs::{self, File},
    io::{Read, Write},
    path::{Path, PathBuf},
    sync::{
        Arc, LazyLock, RwLock,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
};

use crate::{
    ExecutionResult, LuaState,
    compiler::Compiler,
    error::{IntoLuaError, lua_error},
    macros::{assert_string, assert_table, lua_return, native_function, require_table},
    stdlib::package,
    value::{
        IntegerRepr, LuaClosure, LuaObject, LuaString, LuaValue,
        callable::{Callable, Method},
        metatables,
    },
};

use super::{
    debug::DEBUG, io::IO, math::MATH, os::OS, package::PACKAGE, string::STRING, table::TABLE,
    utf8::UTF8,
};

pub const _VERSION: &str = "Lua 5.4";

pub(crate) fn assert(state: &mut LuaState) -> crate::Result<usize> {
    let Some(value) = state.try_arg(0) else {
        return Err(lua_error!("bad argument #1 to 'assert' (value expected)"));
    };

    if !value {
        return match state.try_arg(1) {
            Some(v) if v.is_string() => Err(lua_error!(
                "{}",
                String::from_utf8_lossy(unsafe { v.as_string() })
            )),
            Some(v) if v.is_number() => Err(lua_error!("{}", unsafe { v.as_number() })),
            Some(value) => Err(lua_error!(
                "(error object is a {} value)",
                value.type_name()
            )),
            _ => Err(lua_error!("assertion failed!")),
        };
    }

    for i in (0..state.num_args()).rev() {
        state.swap(i, i + 1);
    }

    Ok(state.num_args())
}

static GC_STATUS: LazyLock<AtomicBool> = LazyLock::new(|| AtomicBool::new(true));
static GC_MODE: LazyLock<RwLock<&'static str>> = LazyLock::new(|| RwLock::new("incremental"));
static GC_PAUSE: LazyLock<AtomicUsize> = LazyLock::new(|| AtomicUsize::new(200));
static GC_STEP_MULTIPLIER: LazyLock<AtomicUsize> = LazyLock::new(|| AtomicUsize::new(100));

const GC_PAUSE_MAX: usize = 1020;
const GC_STEP_MULTIPLIER_MAX: usize = 572;

pub(crate) fn collectgarbage(state: &mut LuaState) -> crate::Result<usize> {
    let Some(operation) = state.try_arg_as_string(0)? else {
        lua_return!(state)
    };

    match operation.as_slice() {
        b"collect" => {
            // Performs a full garbage-collection cycle. This is the default option.
            // We don't use a garbage collector, so do nothing.
            lua_return!(state)
        }
        b"stop" => {
            // Stops automatic execution of the garbage collector. The collector will run only when explicitly invoked, until a call to restart it.
            // We don't use a garbage collector, so do nothing.
            GC_STATUS.store(false, Ordering::Relaxed);
            lua_return!(state)
        }
        b"restart" => {
            // Restarts automatic execution of the garbage collector.
            // We don't use a garbage collector, so do nothing.
            GC_STATUS.store(true, Ordering::Relaxed);
            lua_return!(state)
        }
        b"count" => {
            // Returns the total memory in use by Lua (in Kbytes).
            // We don't use a garbage collector, so return 0.
            // FIXME: Account for upvalues held only by call frames.
            lua_return!(state;
                ((state
                    .get_stack()
                    .iter()
                    .map(LuaValue::size_bytes)
                    .sum::<u64>() as f64)
                    / 1024.)
                    .into(),
            )
            // TODO: Maybe we can give an approximation of memory used by the VM?
            // vec![LuaValue::Number(LuaNumber::Float(0.0))]
        }
        b"step" => {
            // Performs a garbage-collection step. The step "size" is controlled by arg. With a zero value, the collector will perform one basic (indivisible) step. For non-zero values, the collector will perform as if that amount of memory (in Kbytes) had been allocated by Lua. Returns true if the step finished a collection cycle.
            // We don't use a garbage collector, so return false.
            lua_return!(state; LuaValue::from_bool(false))
        }
        b"isrunning" => {
            // Returns a boolean that tells whether the collector is running (i.e., not stopped).
            lua_return!(state; LuaValue::from_bool(GC_STATUS.load(Ordering::Relaxed)))
        }
        // Changes the GC mode, no-op in our case. Just return the previous mode.
        b"incremental" => {
            let mut mode = GC_MODE.write().unwrap();
            let previous_mode = *mode;
            *mode = "incremental";
            lua_return!(state; LuaValue::from_string(previous_mode.as_bytes()))
        }
        b"generational" => {
            let mut mode = GC_MODE.write().unwrap();
            let previous_mode = *mode;
            *mode = "generational";
            lua_return!(state; LuaValue::from_string(previous_mode.as_bytes()))
        }
        b"setpause" => {
            let val = state
                .try_arg_as_number(1)?
                .map(|v| v.integer_repr())
                .transpose()?
                .map_or(0, |n| n as usize)
                .min(GC_PAUSE_MAX);
            let previous_value = GC_PAUSE.load(Ordering::Relaxed);
            GC_PAUSE.store(val, Ordering::Relaxed);
            lua_return!(state; LuaValue::from_integer(previous_value as i64))
        }
        b"setstepmul" => {
            let val = state
                .try_arg_as_number(1)?
                .map(|v| v.integer_repr())
                .transpose()?
                .map_or(0, |n| n as usize)
                .min(GC_STEP_MULTIPLIER_MAX);
            let previous_value = GC_STEP_MULTIPLIER.load(Ordering::Relaxed);
            GC_STEP_MULTIPLIER.store(val, Ordering::Relaxed);
            lua_return!(state; LuaValue::from_integer(previous_value as i64))
        }
        _ => Err(lua_error!(
            "bad argument #1 to 'collectgarbage' (invalid option '{operation}')",
        )),
    }
}

pub(crate) fn dofile(state: &mut LuaState) -> crate::Result<usize> {
    let (name, path, source) = if let Some(s) = state.try_arg_as_string(0)? {
        // This is a file path. Read the file and execute it as a chunk.
        let str = s.as_str();
        let path = PathBuf::from(&*str);
        let mut f = File::open(&path).map_err(|_| lua_error!("cannot open file '{s}'"))?;
        let mut buffer = Vec::new();
        f.read_to_end(&mut buffer).into_lua_error()?;
        (
            path.file_prefix()
                .map(OsStr::to_string_lossy)
                .map(LuaString::from)
                .unwrap_or(s),
            Some(path),
            buffer,
        )
    } else {
        // Read the source code from stdin
        let mut buffer = Vec::new();
        std::io::stdin().read_to_end(&mut buffer).into_lua_error()?;
        (b"[stdin]".into(), None, buffer)
    };

    let compiler = Compiler::new(
        Arc::clone(&state.global),
        name.clone(),
        path,
        None,
        Arc::new(source),
        vec![],
        false,
        false,
    );
    let mut compiled_state = compiler.compile(None)?;
    let result = compiled_state.run()?;
    let num_return_values = result.len();
    for (i, value) in result.into_iter().enumerate() {
        state.set(i, value);
    }

    Ok(num_return_values)
}

pub(crate) fn error(state: &mut LuaState) -> crate::Result<usize> {
    // TODO: You can call error with no arguments, in which case the error value is nil. How can we
    // do that with miette?
    let message = state.arg_as_string(0)?;
    if state.num_args() > 1 {
        // TODO: Second argument is level, which specifies the offset in the call stack at which to
        // raise the error.
    }

    Err(lua_error!("{}", message.to_string()))
}

pub(crate) fn getmetatable(state: &mut LuaState) -> crate::Result<usize> {
    let Some(value) = state.try_arg(0) else {
        return state.arg_err(0, "value expected");
    };

    // NOTE: Only tables and full user data objects have metatables. Under the hood there are
    // metatables for other values as well, but these are only accessible in native code, or via
    // `debug.getmetatable`.
    let metatable = if value.is_object() {
        let o = unsafe { value.as_object() };
        let o = o.read().unwrap();

        // "if the object's metatable has a __metatable field, returns the associated value"
        o.get_metavalue(&metatables::METATABLE_KEY)
            .or_else(|| o.get_metatable())
    } else {
        None
    };

    lua_return!(state; metatable.unwrap_or(LuaValue::NIL))
}

fn iter(state: &mut LuaState) -> crate::Result<usize> {
    let Some(a) = state.try_arg(0) else {
        return state.arg_err(0, "value expected");
    };
    let i = state.arg_as_integer(1)?;

    let i = (i.overflowing_add(1).0).into();

    let v = if a.is_object()
        && let o = unsafe { a.as_object() }
        && let LuaObject::Table(t) = &*o.read().unwrap()
    {
        t.get(&i).cloned()
    } else {
        None
    };

    match v {
        Some(v) if v != LuaValue::NIL => {
            lua_return!(state; i, v)
        }
        _ => {
            lua_return!(state; LuaValue::NIL)
        }
    }
}

pub(crate) fn ipairs(state: &mut LuaState) -> crate::Result<usize> {
    let value = state.ensure_arg(0)?;

    // Return three values: the iterator function, the target value, and the initial index
    lua_return!(state;
        native_function!(iter),
        value,
        LuaValue::from_integer(0),
    )
}

pub(crate) fn load(state: &mut LuaState) -> crate::Result<usize> {
    let mut load_from_closure = false;
    let source: Vec<u8> = match state.try_arg(0) {
        Some(v) if v.is_string() => unsafe { v.as_string() }.clone().into(),
        Some(v) if v.is_object() => {
            let o = unsafe { v.as_object() };
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
                let piece_result = match &callable.method {
                    Method::Closure(closure) => {
                        load_from_closure = true;
                        let r = state.extend_stack(callable.method.clone(), []);
                        state
                            .run_closure(closure, r, 0, 2)?
                            .unwrap_completed()
                            .into_iter()
                            .next()
                    }
                    Method::NativeFunction(name, f) => {
                        let r = state.extend_stack(callable.method.clone(), []);
                        state.run_native_function(name, *f, r, 0, 2)?;
                        // NOTE: native functions MUST return at least one value
                        Some(state.get(r as usize))
                    }
                };

                let Some(piece_result) = piece_result else {
                    break;
                };

                if piece_result.is_nil() {
                    break;
                }
                if piece_result.is_string() {
                    result.extend(unsafe { piece_result.as_string() });
                } else {
                    return Err(lua_error!(
                        "unexpected return value from chunk loader (string or nil expected, got {})",
                        piece_result.type_name()
                    ));
                }
            }

            result
        }
        Some(value) => {
            return state.arg_err(0, format!("function expected, got {}", value.type_name()));
        }
        None => {
            return state.arg_err(0, "function expected, got no value");
        }
    };

    macro_rules! default_name {
        () => {
            if load_from_closure {
                b"=(load)".into()
            } else {
                b"chunk".into()
            }
        };
    }
    let name = match state.try_arg(1) {
        Some(v) if v.is_string() => unsafe { v.as_string() }.clone(),
        Some(v) if v.is_nil() => {
            default_name!()
        }
        None => {
            default_name!()
        }
        Some(value) => {
            return state.arg_err(1, format!("string expected, got {}", value.type_name()));
        }
    };

    let filename: Option<PathBuf> = None;
    let compiler = Compiler::new(
        Arc::clone(&state.global),
        name,
        filename,
        None,
        Arc::new(source),
        vec![],
        true,
        false,
    );
    let compiled_chunk = match compiler.compile_chunk(None) {
        Ok(compiled_chunk) => compiled_chunk,
        Err(e) => {
            lua_return!(state; LuaValue::NIL, LuaValue::from_string(format!("{e}").into_bytes()))
        }
    };

    // Return a function that runs the chunk
    let mut closure = LuaClosure::new(compiled_chunk.chunk_index);

    let env = if state.num_args() > 3 {
        state.capture_upvalue(4)
    } else {
        state.get_global_env()
    };
    closure.upvalues.insert(0, env);

    lua_return!(state; closure.into())
}

pub(crate) fn next(state: &mut LuaState) -> crate::Result<usize> {
    require_table!(read, state.try_arg(0), "next", 0, table, {
        let start_index = match state.arg(1) {
            v if v.is_nil() => 0,
            key => match table.get_index_of(&key) {
                Some(i) => i + 1,
                None => {
                    return Err(lua_error!("invalid key to 'next'"));
                }
            },
        };

        let mut i = start_index;
        loop {
            match table.get_index(i) {
                Some((k, v)) if !v.is_nil() => {
                    lua_return!(state; k.clone(), v.clone());
                }
                Some(_) => i += 1,
                None => {
                    lua_return!(state; LuaValue::NIL);
                }
            }
        }
    })
}

pub(crate) fn pairs(state: &mut LuaState) -> crate::Result<usize> {
    let t = state.ensure_arg(0)?;

    // If t has a metamethod __pairs, calls it with t as argument and returns the first three results from the call.
    if let Some(Callable { method, .. }) = t
        .get_metavalue(&metatables::PAIRS_KEY)
        .and_then(|v| v.try_into().ok())
    {
        match method {
            Method::Closure(closure) => {
                // TODO: Is this ok
                let args_r = state.extend_stack(LuaValue::NIL, [t.clone()]);
                let mut result = state
                    .run_closure(&closure, args_r, 1, 4)?
                    .unwrap_completed();
                result.resize(3, LuaValue::NIL);

                for (i, v) in result.into_iter().enumerate() {
                    state.set(i, v);
                }
            }
            Method::NativeFunction(name, f) => {
                let num_results = state.run_native_function(name, f, 0, 1, 4)?;
                if num_results < 3 {
                    (num_results..3).for_each(|i| state.set(i, LuaValue::NIL));
                }
            }
        }

        return Ok(3);
    }

    lua_return!(state;
        native_function!(next),
        t,
        LuaValue::NIL,
    )
}

pub(crate) fn pcall(state: &mut LuaState) -> crate::Result<usize> {
    enum ResultKind {
        Vector(Vec<LuaValue>),
        Register { r: u8, num_values: usize },
    }

    // TODO: This will produce error "value expected" rather than "function expected, got no value"
    let function = state.ensure_arg(0)?;

    let callable_function: Callable = (&function).try_into().map_err(|_| {
        lua_error!(
            "bad argument #1 to 'pcall' (function expected, got {})",
            function.type_name(),
        )
    })?;

    let result = match callable_function.method {
        Method::Closure(closure) => {
            let args: Vec<_> = (usize::from(!callable_function.is_metamethod)..state.num_args())
                .map(|i| state.arg(i))
                .collect();
            let num_args = args.len();
            // TODO: Is this okay?
            let args_r = state.extend_stack(LuaValue::NIL, args);
            state
                .run_closure(&closure, args_r, num_args, 0)
                .map(ExecutionResult::unwrap_completed)
                .map(ResultKind::Vector)
        }
        Method::NativeFunction(name, f) => {
            let (args_r, num_args) = if callable_function.is_metamethod {
                (0, state.num_args())
            } else {
                (1, state.num_args() - 1)
            };
            state
                .run_native_function(name, f, args_r, num_args, 0)
                .map(|num_values| ResultKind::Register {
                    r: args_r,
                    num_values,
                })
        }
    };

    match result {
        Ok(ResultKind::Vector(values)) => {
            lua_return!(state;
                from = std::iter::once(LuaValue::from_bool(true)).chain(values.into_iter())
            )
        }
        Ok(ResultKind::Register { r, num_values }) => {
            // We need to put the boolean `true` in register one, so let's move
            // all other values over by one.
            for i in (0..num_values).rev() {
                let value = state.get(r as usize + i);
                state.set(i + 1, value);
            }
            state.set(0usize, LuaValue::from_bool(true));
            Ok(num_values + 1)
        }
        Err(e) => lua_return!(state;
            LuaValue::from_bool(false),
            LuaValue::from_string(e.to_string().into_bytes()),
        ),
    }
}

pub(crate) fn print(state: &mut LuaState) -> crate::Result<usize> {
    let mut output = std::io::stdout();

    for (i, value) in state.args().into_iter().enumerate() {
        if i != 0 {
            output.write_all(b"\t").into_lua_error()?;
        }
        output
            .write_all(&value.cast_to_string(state)?)
            .into_lua_error()?;
    }

    output.write_all(b"\n").into_lua_error()?;
    output.flush().into_lua_error()?;

    lua_return!(state)
}

pub(crate) fn rawequal(state: &mut LuaState) -> crate::Result<usize> {
    let lhs = state.ensure_arg(0)?;
    let rhs = state.ensure_arg(1)?;

    lua_return!(state; LuaValue::from_bool(lhs == rhs))
}

pub(crate) fn rawget(state: &mut LuaState) -> crate::Result<usize> {
    require_table!(state.try_arg(0), "rawget", 0, table, {
        let key = state.ensure_arg(1)?;
        let value = table.get(&key).cloned().unwrap_or(LuaValue::NIL);

        lua_return!(state; value)
    })
}

pub(crate) fn rawlen(state: &mut LuaState) -> crate::Result<usize> {
    let result = match state.try_arg(0) {
        Some(v) if v.is_object() => match &*unsafe { v.as_object() }.read().unwrap() {
            LuaObject::Table(t) => t.len(),
            o => {
                return Err(lua_error!(
                    "bad argument #1 to 'rawlen' (table or string expected, got {})",
                    o.type_name(),
                ));
            }
        },
        Some(v) if v.is_string() => unsafe { v.as_string() }.len(),
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

    lua_return!(state; result.into())
}

pub(crate) fn rawset(state: &mut LuaState) -> crate::Result<usize> {
    let key = state.ensure_arg(1)?;
    if key.is_nil() {
        return Err(lua_error!("bad argument #2 to 'rawset' (value expected)"));
    }
    if key.is_float() && unsafe { key.as_float() }.is_nan() {
        return Err(lua_error!(
            "bad argument #2 to 'rawset' (key cannot be NaN)"
        ));
    }

    let value = state.ensure_arg(2)?;

    require_table!(write, state.try_arg(0), "rawset", 0, table, {
        table.insert(key.clone(), value);
    });

    lua_return!(state; state.ensure_arg(0)?)
}

pub(crate) fn require(state: &mut LuaState) -> crate::Result<usize> {
    let name = state.arg_as_string(0)?;

    // Allow loading stdlib modules via `require`
    match name.as_slice() {
        b"debug" => {
            lua_return!(state; DEBUG.clone());
        }
        b"io" => {
            lua_return!(state; IO.clone());
        }
        b"math" => {
            lua_return!(state; MATH.clone());
        }
        b"os" => {
            lua_return!(state; OS.clone());
        }
        b"string" => {
            lua_return!(state; STRING.clone());
        }
        b"table" => {
            lua_return!(state; TABLE.clone());
        }
        b"utf8" => {
            lua_return!(state; UTF8.clone());
        }
        _ => {}
    }

    let package = PACKAGE.clone();
    let x = assert_table!(&package, package_table, {
        package_table
            .get(&"path".into())
            .expect("package.path exists")
            .clone()
    });
    let package_path = assert_string!(x);

    // Loop through each package path option
    let package_config = &package::CONFIG;
    let called_from = match state.call_frame().kind {
        crate::vm::CallFrameKind::Lua { chunk, .. } => state
            .with_chunk(chunk, |c| c.get_filename().map(Path::to_path_buf))
            .expect("valid chunk"),
        crate::vm::CallFrameKind::Native { .. } => {
            panic!("don't call require from native function")
        }
    };
    for mut option in package_path
        .split(|c| *c == package_config.template_separator)
        .map(<[u8]>::to_vec)
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
                .copied()
                .collect();
        }

        let mut path_str = String::from_utf8_lossy(&option);
        let mut path = Path::new(path_str.as_ref()).to_owned();
        if path.is_relative() {
            // Resolve to the current file's directory
            if let Some(ref filename) = called_from {
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
                    .map_or_else(|| "<unknown>".into(), |s| s.to_string_lossy())
            ));
        }

        // Check if it's already loaded
        let path_value = LuaValue::from_string(path_str.as_bytes());
        assert_table!(&package, package_table, {
            let loaded = package_table
                .get(&"loaded".into())
                .expect("package.loaded exists");
            assert_table!(loaded, loaded_table, {
                if let Some(value) = loaded_table.get(&path_value) {
                    lua_return!(state; value.clone());
                }
            });
        });

        let source = fs::read(&path).map_err(|_| lua_error!("unable to read module {name}"))?;

        let compiler = Compiler::new(
            Arc::clone(&state.global),
            name.clone(),
            Some(path),
            None,
            Arc::new(source),
            vec![],
            false,
            false,
        );
        let mut state = compiler.compile(None)?;
        let mut result = state.run()?;
        let result = result.swap_remove(0);

        assert_table!(write, package, package_table, {
            let loaded = package_table
                .get_mut(&LuaValue::from_string(b"loaded"))
                .expect("package.loaded exists");
            assert_table!(write, loaded, loaded_table, {
                loaded_table.insert(path_value.clone(), result.clone());
            });
        });

        lua_return!(state; result, path_value);
    }

    Err(lua_error!("module '{name}' not found"))
}

pub(crate) fn select(state: &mut LuaState) -> crate::Result<usize> {
    let index = match state.try_arg(0) {
        Some(value) if value.is_number() => unsafe { value.as_number() },
        Some(value) if value.is_string() => {
            let s = unsafe { value.as_string() };
            if s == b"#" {
                lua_return!(state; LuaValue::from_integer(state.num_args() as i64 - 1));
            }

            return Err(lua_error!(
                "bad argument #1 to 'select' (number expected, got string)"
            ));
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

    lua_return!(state; from = state.args().into_iter().skip(index as usize))
}

pub(crate) fn setmetatable(state: &mut LuaState) -> crate::Result<usize> {
    match (state.try_arg(0), state.try_arg(1)) {
        (Some(target), Some(metatable)) if target.is_object() && metatable.is_object() => {
            let metatable = unsafe { metatable.into_object() };
            match (
                &mut *unsafe { target.as_object() }.write().unwrap(),
                &*metatable.read().unwrap(),
            ) {
                (LuaObject::Table(table), LuaObject::Table(_)) => {
                    table.set_metatable(Some(LuaValue::from_object(metatable.clone())));
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
        (Some(target), Some(metatable)) if target.is_object() && metatable.is_nil() => {
            match &mut *unsafe { target.as_object() }.write().unwrap() {
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
        (Some(target), _) if target.is_object() => {
            if matches!(
                &*unsafe { target.as_object() }.read().unwrap(),
                LuaObject::Table(_)
            ) {
                return Err(lua_error!(
                    "bad argument #2 to 'setmetatable' (table expected)"
                ));
            }
            return Err(lua_error!(
                "bad argument #1 to 'setmetatable' (table expected)"
            ));
        }
        (_, _) => {
            return Err(lua_error!(
                "bad argument #1 to 'setmetatable' (table expected)"
            ));
        }
    }

    lua_return!(state; state.arg(0))
}

pub(crate) fn tonumber(state: &mut LuaState) -> crate::Result<usize> {
    let value = match state.ensure_arg(0)? {
        n if n.is_number() => n.clone(),
        v if v.is_string() => {
            let s = unsafe { v.as_string() };
            let s = String::from_utf8_lossy(s);
            match s.parse::<i64>() {
                Ok(n) => n.into(),
                Err(_) => match s.parse::<f64>() {
                    Ok(n) => n.into(),
                    Err(_) => LuaValue::NIL,
                },
            }
        }
        _ => LuaValue::NIL,
    };

    lua_return!(state; value)
}

pub(crate) fn tostring(state: &mut LuaState) -> crate::Result<usize> {
    let value = state.arg(0);
    let result = LuaValue::from_string(value.cast_to_string(state)?);

    lua_return!(state; result)
}

pub(crate) fn r#type(state: &mut LuaState) -> crate::Result<usize> {
    let value = state.ensure_arg(0)?;

    lua_return!(state; LuaValue::from_string(value.type_name().bytes().collect::<Vec<_>>()))
}

static CONTROL_MESSAGES_ENABLED: AtomicBool = AtomicBool::new(false);

pub(crate) fn warn(state: &mut LuaState) -> crate::Result<usize> {
    let strings: Vec<_> = state
        .args_ref()
        .iter()
        .enumerate()
        .map(|(i, v)| {
            if v.is_string() {
                Ok(String::from_utf8_lossy(unsafe { v.as_string() }).to_string())
            } else {
                Err(lua_error!(
                    "bad argument #{i} to 'warn' (string expected, got {})",
                    v.type_name(),
                ))
            }
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

        lua_return!(state)
    }

    if CONTROL_MESSAGES_ENABLED.load(Ordering::Relaxed) {
        eprint!("Lua warning: ");
        for s in strings {
            eprint!("{s}");
        }
        eprintln!();
    }

    lua_return!(state)
}

pub(crate) fn xpcall(state: &mut LuaState) -> crate::Result<usize> {
    enum ResultKind {
        Vector(Vec<LuaValue>),
        Register { r: u8, num_values: usize },
    }

    // TODO: Will produce error "value expected" rather than "function expected, got no value"
    let function = state.ensure_arg(0)?;
    let msgh = state.ensure_arg(1)?;

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
            args.extend((2..state.num_args()).map(|i| state.arg(i)));
            let num_args = args.len();
            let args_r = state.extend_stack(closure.clone(), args);
            state
                .run_closure(&closure, args_r, num_args, 0)
                .map(ExecutionResult::unwrap_completed)
                .map(ResultKind::Vector)
        }
        nf @ Method::NativeFunction(name, f) => {
            let (args_r, num_args) = if function_callable.is_metamethod {
                let mut args = vec![function];
                args.extend((2..state.num_args()).map(|i| state.arg(i)));
                let num_args = args.len();
                let args_r = state.extend_stack(nf, args);
                (args_r, num_args)
            } else {
                (1, state.num_args() - 1)
            };

            state
                .run_native_function(name, f, args_r, num_args, 0)
                .map(|num_values| ResultKind::Register {
                    r: args_r,
                    num_values,
                })
        }
    };

    const MAX_MESSAGE_HANDLER_DEPTH: usize = 200;
    for _ in 0..MAX_MESSAGE_HANDLER_DEPTH {
        match result {
            Ok(_) => break,
            Err(e) => {
                result = match &msgh.method {
                    Method::Closure(closure) => {
                        let args_r = state.extend_stack(
                            msgh.method.clone(),
                            [LuaValue::from_string(e.to_string().into_bytes())],
                        );
                        state
                            .run_closure(closure, args_r, 1, 0)
                            .map(ExecutionResult::unwrap_completed)
                            .map(ResultKind::Vector)
                    }
                    Method::NativeFunction(name, f) => {
                        let args_r = state.extend_stack(
                            msgh.method.clone(),
                            [LuaValue::from_string(e.to_string().into_bytes())],
                        );
                        state
                            .run_native_function(name, *f, args_r, 1, 0)
                            .map(|num_values| ResultKind::Register {
                                r: args_r,
                                num_values,
                            })
                    }
                };
            }
        }
    }

    match result {
        Ok(ResultKind::Vector(values)) => {
            lua_return!(state;
                from = std::iter::once(LuaValue::from_bool(true)).chain(values.into_iter())
            )
        }
        Ok(ResultKind::Register { r, num_values }) => {
            state.set(0usize, LuaValue::from_bool(true));
            for i in 0..num_values {
                let value = state.get(r as usize + i);
                state.set(i + 1, value);
            }
            Ok(num_values + 1)
        }
        Err(e) => lua_return!(state;
            LuaValue::from_bool(false),
            LuaValue::from_string(e.to_string().into_bytes()),
        ),
    }
}
