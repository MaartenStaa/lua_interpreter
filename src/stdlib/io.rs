use std::{
    fs::OpenOptions,
    io::{Read, Write, stderr, stdin, stdout},
    path::Path,
    sync::{Arc, LazyLock, RwLock},
};

use crate::{
    error::{IntoLuaError, lua_error},
    macros::{lua_return, native_function, require_userdata, require_userdata_type},
    state::LuaState,
    value::{IntegerRepr, LuaObject, LuaTable, LuaValue, UserData, metatables},
};

static FILE_METATABLE: LazyLock<LuaTable> = LazyLock::new(|| {
    let mut file_metatable = LuaTable::new();

    file_metatable.insert(metatables::INDEX_KEY.clone(), native_function!(file_index));
    file_metatable.insert(metatables::NAME_KEY.clone(), "FILE*".into());

    file_metatable
});

static STDIN: LazyLock<Arc<RwLock<LuaObject>>> =
    LazyLock::new(|| create_file(FileHandle::Stdin(stdin())));
static STDOUT: LazyLock<Arc<RwLock<LuaObject>>> =
    LazyLock::new(|| create_file(FileHandle::Stdout(stdout())));

static DEFAULT_INPUT: LazyLock<RwLock<Arc<RwLock<LuaObject>>>> =
    LazyLock::new(|| RwLock::new(STDIN.clone()));
static DEFAULT_OUTPUT: LazyLock<RwLock<Arc<RwLock<LuaObject>>>> =
    LazyLock::new(|| RwLock::new(STDOUT.clone()));

pub static IO: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut io = LuaTable::new();

    // Predefined file handles
    io.insert("stdin".into(), LuaValue::from_object(STDIN.clone()));
    io.insert("stdout".into(), LuaValue::from_object(STDOUT.clone()));
    io.insert(
        "stderr".into(),
        LuaValue::from_object(create_file(FileHandle::Stderr(stderr()))),
    );

    // Functions
    io.insert("close".into(), native_function!(close));
    io.insert("input".into(), native_function!(input));
    io.insert("open".into(), native_function!(open));
    io.insert("output".into(), native_function!(output));
    io.insert("read".into(), native_function!(read));
    io.insert("type".into(), native_function!("type", r#type));
    io.insert("write".into(), native_function!(write));

    io.into()
});

#[derive(Debug)]
pub struct File {
    methods: LuaTable,
    handle: FileHandle,
}

#[derive(Debug)]
pub enum FileHandle {
    Stdin(std::io::Stdin),
    Stdout(std::io::Stdout),
    Stderr(std::io::Stderr),
    File(std::fs::File),
    Closed,
}

#[derive(Debug, Copy, Clone)]
pub enum ReadFormat {
    Numeral,
    All,
    Line,
    LineIncluding,
    N(usize),
}

impl FileHandle {
    fn close(&mut self) -> crate::Result<()> {
        match self {
            Self::Closed => Err(lua_error!("file is already closed")),
            Self::Stdin(_) | Self::Stdout(_) | Self::Stderr(_) => {
                Err(lua_error!("cannot close standard file"))
            }
            Self::File(handle) => {
                handle.sync_all().into_lua_error()?;
                *self = FileHandle::Closed;
                Ok(())
            }
        }
    }

    fn read(&mut self, format: ReadFormat) -> crate::Result<LuaValue> {
        match self {
            Self::Stdin(stdin) => match format {
                ReadFormat::Numeral => todo!(),
                ReadFormat::All => {
                    let mut buffer = Vec::new();
                    stdin.read_to_end(&mut buffer).into_lua_error()?;
                    Ok(LuaValue::from_string(buffer))
                }
                ReadFormat::Line => {
                    let mut buffer = String::new();
                    stdin.read_line(&mut buffer).into_lua_error()?;
                    Ok(buffer.into())
                }
                ReadFormat::LineIncluding => todo!(),
                ReadFormat::N(n) => {
                    let mut buffer = vec![0; n];
                    stdin.read_exact(&mut buffer).into_lua_error()?;
                    Ok(LuaValue::from_string(buffer))
                }
            },
            _ => todo!(),
        }
    }

    fn write_all(&mut self, data: &[u8]) -> crate::Result<()> {
        match self {
            Self::Stdin(_) => {
                return Err(lua_error!("cannot write to stdin"));
            }
            FileHandle::Stdout(handle) => handle.write_all(data).into_lua_error()?,
            FileHandle::Stderr(handle) => handle.write_all(data).into_lua_error()?,
            FileHandle::File(handle) => handle.write_all(data).into_lua_error()?,
            FileHandle::Closed => {
                return Err(lua_error!("attempt to use a closed file"));
            }
        }

        Ok(())
    }
}

fn create_file(handle: FileHandle) -> Arc<RwLock<LuaObject>> {
    let mut methods = LuaTable::new();

    methods.insert("close".into(), native_function!(file_close));
    methods.insert("read".into(), native_function!(file_read));
    methods.insert("write".into(), native_function!(file_write));

    Arc::new(RwLock::new(
        UserData::new_full(File { methods, handle }, FILE_METATABLE.clone()).into(),
    ))
}

fn file_close(state: &mut LuaState) -> crate::Result<usize> {
    require_userdata!(write, state.try_arg(0), "file:write", 0, userdata, {
        require_userdata_type!(write, userdata, "file:write", 0, File, file, _metatable, {
            file.handle.close()?;
            lua_return!(state; LuaValue::from_bool(true))
        })
    })
}

fn file_index(state: &mut LuaState) -> crate::Result<usize> {
    require_userdata!(state.try_arg(0), "file:__index", 0, userdata, {
        require_userdata_type!(userdata, "file:__index", 0, File, file, _metatable, {
            lua_return!(state;
                file.methods
                    .get(&state.arg_ref(1))
                    .cloned()
                    .unwrap_or(LuaValue::NIL)
            )
        })
    })
}

fn file_read(state: &mut LuaState) -> crate::Result<usize> {
    require_userdata!(write, state.try_arg(0), "file:read", 0, userdata, {
        require_userdata_type!(write, userdata, "file:read", 0, File, file, _metatable, {
            let mut result = Vec::new();
            if state.num_args() == 1 {
                result.push(if let Ok(s) = file.handle.read(ReadFormat::Line) {
                    s
                } else {
                    lua_return!(state)
                });
            } else {
                for (i, value) in state.args_ref().iter().enumerate().skip(1) {
                    match value.as_ref() {
                        v if v.is_string() => {
                            let s = unsafe { v.as_string() };
                            result.push(
                                if let Ok(s) = file.handle.read(match s.as_slice() {
                                    b"n" => ReadFormat::Numeral,
                                    b"a" => ReadFormat::All,
                                    b"l" => ReadFormat::Line,
                                    b"L" => ReadFormat::LineIncluding,
                                    _ => {
                                        return Err(lua_error!(
                                            "bad argument #{} to 'read' (invalid format '{}')",
                                            i + 1,
                                            String::from_utf8_lossy(s)
                                        ));
                                    }
                                }) {
                                    s
                                } else {
                                    lua_return!(state)
                                },
                            );
                        }
                        v if v.is_number() => {
                            let n = unsafe { v.as_number() };
                            result.push(
                                if let Ok(s) =
                                    file.handle.read(ReadFormat::N(n.integer_repr()? as usize))
                                {
                                    s
                                } else {
                                    lua_return!(state)
                                },
                            );
                        }
                        _ => {
                            return Err(lua_error!(
                                "bad argument #{} to 'write' (string or number expected, got {})",
                                i + 1,
                                value.type_name()
                            ));
                        }
                    }
                }
            }

            lua_return!(state; state.arg(0))
        })
    })
}

fn file_write(state: &mut LuaState) -> crate::Result<usize> {
    require_userdata!(write, state.try_arg(0), "file:write", 0, userdata, {
        require_userdata_type!(write, userdata, "file:write", 0, File, file, _metatable, {
            for (i, value) in state.args().iter().enumerate().skip(1) {
                if value.is_string() {
                    file.handle.write_all(unsafe { value.as_string() }).unwrap();
                } else if value.is_number() {
                    file.handle
                        .write_all(unsafe { value.as_number() }.to_string().as_bytes())
                        .unwrap();
                } else {
                    return Err(lua_error!(
                        "bad argument #{} to 'write' (string or number expected, got {})",
                        i + 1,
                        value.type_name()
                    ));
                }
            }

            lua_return!(state; state.ensure_arg(0)?)
        })
    })
}

fn close(state: &mut LuaState) -> crate::Result<usize> {
    if state
        .try_with_arg_as_mut_user_data(0, |userdata| {
            require_userdata_type!(write, userdata, "io.close", 0, File, file, _metatable, {
                file.handle.close()?;
                Ok(())
            })
        })?
        .is_some()
    {
        lua_return!(state; LuaValue::from_bool(true))
    }

    let default = (*DEFAULT_OUTPUT.write().unwrap()).clone();
    // FIXME: We shouldn't have to construct the `LuaValue` here.
    require_userdata!(
        write,
        Some(LuaValue::from_object(default)),
        "io.close",
        0,
        userdata,
        {
            require_userdata_type!(write, userdata, "io.close", 0, File, file, _metatable, {
                file.handle.close()?;
                lua_return!(state; LuaValue::from_bool(true))
            })
        }
    )
}

fn input(state: &mut LuaState) -> crate::Result<usize> {
    let result = match state.arg(0) {
        v if v.is_nil() => LuaValue::from_object((*DEFAULT_INPUT.read().unwrap()).clone()),
        v if v.is_object() => {
            let object = unsafe { v.into_object() };
            match &*object.read().unwrap() {
                LuaObject::UserData(UserData::Full { data, .. }) => {
                    data.read()
                        .unwrap()
                        .downcast_ref::<FileHandle>()
                        .ok_or_else(|| {
                            lua_error!("bad argument #1 to 'io.input' (file expected)")
                        })?;

                    let result = v;
                    let mut default_input = DEFAULT_INPUT.write().unwrap();
                    *default_input = object.clone();

                    result
                }
                _ => return Err(lua_error!("bad argument #1 to 'io.input' (file expected)")),
            }
        }
        _ => return Err(lua_error!("bad argument #1 to 'io.input' (file expected)")),
    };

    lua_return!(state; result)
}

fn open(state: &mut LuaState) -> crate::Result<usize> {
    let filename = state.arg_as_string(0)?;
    let mode = state.try_arg_as_string(1)?.unwrap_or(b"r".into());

    let mut options = std::fs::OpenOptions::new();
    match mode.as_slice() {
        b"r" | b"rb" => {
            options.read(true);
        }
        b"w" | b"wb" => {
            options.write(true).create(true);
        }
        b"a" | b"ab" => {
            options.create(true).append(true);
        }
        b"r+" | b"r+b" => {
            options.read(true).write(true);
        }
        b"w+" | b"w+b" => {
            options.read(true).write(true).create(true);
        }
        b"a+" | b"a+b" => {
            options.read(true).write(true).create(true).append(true);
        }
        _ => {
            return Err(lua_error!(
                "bad argument #2 to 'io.open' (invalid mode '{mode}')"
            ));
        }
    }

    let filename_str = String::from_utf8_lossy(&filename);
    let filename_path = Path::new(filename_str.as_ref());
    let file = match options.open(filename_path) {
        Ok(file) => file,
        Err(e) => {
            lua_return!(state;
                LuaValue::NIL,
                format!("{e}: No such file or directory").into(),
                // TODO: What is this value??
                2.into(),
            );
        }
    };

    lua_return!(state; LuaValue::from_object(create_file(FileHandle::File(file))))
}

fn output(state: &mut LuaState) -> crate::Result<usize> {
    let result = match state.arg(0) {
        v if v.is_nil() => LuaValue::from_object((*DEFAULT_OUTPUT.read().unwrap()).clone()),
        v if v.is_string() => {
            let filename = unsafe { v.into_string() };
            let filename = filename.to_string();
            let file = OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(&filename)
                .map_err(|e| lua_error!("{}: {}", filename, e))?;
            let value = create_file(FileHandle::File(file));
            *DEFAULT_OUTPUT.write().unwrap() = value.clone();
            LuaValue::from_object(value)
        }
        v if v.is_object() => {
            let object = unsafe { v.into_object() };
            match &*object.read().unwrap() {
                LuaObject::UserData(UserData::Full { data, .. }) => {
                    data.read()
                        .unwrap()
                        .downcast_ref::<FileHandle>()
                        .ok_or_else(|| {
                            lua_error!("bad argument #1 to 'io.output' (file expected)")
                        })?;

                    let output = v;
                    let mut default_output = DEFAULT_OUTPUT.write().unwrap();
                    *default_output = object.clone();

                    output
                }
                _ => return Err(lua_error!("bad argument #1 to 'io.output' (file expected)")),
            }
        }
        _ => return Err(lua_error!("bad argument #1 to 'io.output' (file expected)")),
    };

    lua_return!(state; result)
}

fn read(state: &mut LuaState) -> crate::Result<usize> {
    // Equivalent to `io.input():read(...)`
    let file = LuaValue::from_object((*DEFAULT_INPUT.write().unwrap()).clone());
    // TODO: Should we navigate through the metatable lookup?
    let mut args = vec![file];
    args.extend(state.args());
    let f = native_function!(file_read);
    let args_r = state.extend_stack(f, args);
    state.run_native_function(
        "file_read",
        file_read,
        args_r,
        state.num_args() + 1,
        state.call_frame().num_return_values,
    )
}

fn r#type(state: &mut LuaState) -> crate::Result<usize> {
    let result = state.with_arg_as_user_data(0, |u| {
        Ok(match u {
            UserData::Full { data, .. } => {
                if let Some(file) = data.read().unwrap().downcast_ref::<FileHandle>() {
                    match file {
                        FileHandle::Closed => LuaValue::from_string(b"closed file"),
                        _ => LuaValue::from_string(b"file"),
                    }
                } else {
                    LuaValue::NIL
                }
            }
            UserData::Light(_) => LuaValue::NIL,
        })
    })?;

    lua_return!(state; result)
}

fn write(state: &mut LuaState) -> crate::Result<usize> {
    let default = LuaValue::from_object(DEFAULT_OUTPUT.read().unwrap().clone());
    let mut args = vec![default];
    args.extend(state.args());
    let f = native_function!(file_write);
    let args_r = state.extend_stack(f, args);
    state.run_native_function(
        "file:write",
        file_write,
        args_r,
        state.num_args() + 1,
        state.call_frame().num_return_values,
    )
}
