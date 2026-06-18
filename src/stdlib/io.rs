use std::{
    fs::OpenOptions,
    io::{Read, Write, stderr, stdin, stdout},
    path::Path,
    sync::{LazyLock, RwLock},
};

use crate::{
    error::{IntoLuaError, lua_error},
    macros::{require_userdata, require_userdata_type},
    state::LuaState,
    value::{LuaObject, LuaTable, LuaValue, UserData, metatables},
};

static FILE_METATABLE: LazyLock<LuaTable> = LazyLock::new(|| {
    let mut file_metatable = LuaTable::new();

    file_metatable.insert(
        metatables::INDEX_KEY.clone(),
        LuaObject::NativeFunction("file_index", file_index).into(),
    );
    file_metatable.insert(metatables::NAME_KEY.clone(), "FILE*".into());

    file_metatable
});

static STDIN: LazyLock<LuaValue> = LazyLock::new(|| create_file(FileHandle::Stdin(stdin())));
static STDOUT: LazyLock<LuaValue> = LazyLock::new(|| create_file(FileHandle::Stdout(stdout())));

static DEFAULT_INPUT: LazyLock<RwLock<LuaValue>> = LazyLock::new(|| RwLock::new(STDIN.clone()));
static DEFAULT_OUTPUT: LazyLock<RwLock<LuaValue>> = LazyLock::new(|| RwLock::new(STDOUT.clone()));

pub static IO: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut io = LuaTable::new();

    // Predefined file handles
    io.insert("stdin".into(), STDIN.clone());
    io.insert("stdout".into(), STDOUT.clone());
    io.insert("stderr".into(), create_file(FileHandle::Stderr(stderr())));

    // Functions
    io.insert(
        "close".into(),
        LuaObject::NativeFunction("close", close).into(),
    );
    io.insert(
        "input".into(),
        LuaObject::NativeFunction("input", input).into(),
    );
    io.insert(
        "open".into(),
        LuaObject::NativeFunction("open", open).into(),
    );
    io.insert(
        "output".into(),
        LuaObject::NativeFunction("output", output).into(),
    );
    io.insert(
        "read".into(),
        LuaObject::NativeFunction("read", read).into(),
    );
    io.insert(
        "type".into(),
        LuaObject::NativeFunction("type", r#type).into(),
    );
    io.insert(
        "write".into(),
        LuaObject::NativeFunction("write", write).into(),
    );

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
                    Ok(LuaValue::String(buffer.into()))
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
                    Ok(LuaValue::String(buffer.into()))
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

fn create_file(handle: FileHandle) -> LuaValue {
    let mut methods = LuaTable::new();

    methods.insert(
        "close".into(),
        LuaObject::NativeFunction("file_close", file_close).into(),
    );
    methods.insert(
        "read".into(),
        LuaObject::NativeFunction("file_read", file_read).into(),
    );
    methods.insert(
        "write".into(),
        LuaObject::NativeFunction("file_write", file_write).into(),
    );

    UserData::new_full(File { methods, handle }, FILE_METATABLE.clone()).into()
}

fn file_close(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    require_userdata!(write, state.try_arg(0), "file:write", 0, userdata, {
        require_userdata_type!(write, userdata, "file:write", 0, File, file, _metatable, {
            file.handle.close()?;
            Ok(vec![LuaValue::Boolean(true)])
        })
    })
}

fn file_index(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    require_userdata!(state.try_arg(0), "file:__index", 0, userdata, {
        require_userdata_type!(userdata, "file:__index", 0, File, file, _metatable, {
            Ok(vec![
                file.methods
                    .get(&state.arg_ref(1))
                    .cloned()
                    .unwrap_or(LuaValue::Nil),
            ])
        })
    })
}

fn file_read(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    require_userdata!(write, state.try_arg(0), "file:read", 0, userdata, {
        require_userdata_type!(write, userdata, "file:read", 0, File, file, _metatable, {
            let mut result = Vec::new();
            if state.num_args() == 1 {
                result.push(match file.handle.read(ReadFormat::Line) {
                    Ok(s) => s,
                    Err(_) => return Ok(vec![LuaValue::Nil]),
                });
            } else {
                for (i, value) in state.args_ref().iter().enumerate().skip(1) {
                    match value.as_ref() {
                        LuaValue::String(s) => {
                            result.push(
                                match file.handle.read(match s.as_slice() {
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
                                    Ok(s) => s,
                                    Err(_) => return Ok(vec![LuaValue::Nil]),
                                },
                            );
                        }
                        LuaValue::Number(n) => {
                            result.push(
                                match file.handle.read(ReadFormat::N(n.integer_repr()? as usize)) {
                                    Ok(s) => s,
                                    Err(_) => return Ok(vec![LuaValue::Nil]),
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

            Ok(vec![state.arg(0)])
        })
    })
}

fn file_write(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    require_userdata!(write, state.try_arg(0), "file:write", 0, userdata, {
        require_userdata_type!(write, userdata, "file:write", 0, File, file, _metatable, {
            for (i, value) in state.args().iter().enumerate().skip(1) {
                match value {
                    LuaValue::String(s) => {
                        file.handle.write_all(s)?;
                    }
                    LuaValue::Number(n) => {
                        file.handle.write_all(n.to_string().as_bytes())?;
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

            Ok(vec![state.ensure_arg(0)?])
        })
    })
}

fn close(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    match state.try_arg(0) {
        Some(value) if value != LuaValue::Nil => {
            require_userdata!(write, Some(value), "io.close", 0, userdata, {
                require_userdata_type!(write, userdata, "io.close", 0, File, file, _metatable, {
                    file.handle.close()?;
                    Ok(vec![LuaValue::Boolean(true)])
                })
            })
        }
        _ => {
            let default = (*DEFAULT_OUTPUT.write().unwrap()).clone();
            require_userdata!(write, Some(default), "io.close", 0, userdata, {
                require_userdata_type!(write, userdata, "io.close", 0, File, file, _metatable, {
                    file.handle.close()?;
                    Ok(vec![LuaValue::Boolean(true)])
                })
            })
        }
    }
}

fn input(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let result = match state.try_arg(0) {
        Some(LuaValue::Nil) | None => (*DEFAULT_INPUT.read().unwrap()).clone(),
        Some(LuaValue::Object(o)) => match &*o.read().unwrap() {
            LuaObject::UserData(UserData::Full { data, .. }) => {
                data.read()
                    .unwrap()
                    .downcast_ref::<FileHandle>()
                    .ok_or_else(|| lua_error!("bad argument #1 to 'io.input' (file expected)"))?;

                let output = LuaValue::Object(o.clone());
                let mut default_input = DEFAULT_INPUT.write().unwrap();
                *default_input = output.clone();

                output
            }
            _ => return Err(lua_error!("bad argument #1 to 'io.input' (file expected)")),
        },
        _ => return Err(lua_error!("bad argument #1 to 'io.input' (file expected)")),
    };

    Ok(vec![result])
}

fn open(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
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
            return Ok(vec![
                LuaValue::Nil,
                format!("{}: No such file or directory", e).into(),
                // TODO: What is this value??
                2.into(),
            ]);
        }
    };

    Ok(vec![create_file(FileHandle::File(file))])
}

fn output(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let result = match state.arg(0) {
        LuaValue::Nil => (*DEFAULT_OUTPUT.read().unwrap()).clone(),
        LuaValue::String(filename) => {
            let filename = filename.to_string();
            let file = OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(&filename)
                .map_err(|e| lua_error!("{}: {}", filename, e))?;
            let value = create_file(FileHandle::File(file));
            *DEFAULT_OUTPUT.write().unwrap() = value.clone();
            value
        }
        LuaValue::Object(o) => match &*o.read().unwrap() {
            LuaObject::UserData(UserData::Full { data, .. }) => {
                data.read()
                    .unwrap()
                    .downcast_ref::<FileHandle>()
                    .ok_or_else(|| lua_error!("bad argument #1 to 'io.output' (file expected)"))?;

                let output = LuaValue::Object(o.clone());
                let mut default_output = DEFAULT_OUTPUT.write().unwrap();
                *default_output = output.clone();

                output
            }
            _ => return Err(lua_error!("bad argument #1 to 'io.output' (file expected)")),
        },
        _ => return Err(lua_error!("bad argument #1 to 'io.output' (file expected)")),
    };

    Ok(vec![result])
}

fn read(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    // Equivalent to `io.input():read(...)`
    let file = (*DEFAULT_INPUT.write().unwrap()).clone();
    // TODO: Should we navigate through the metatable lookup?
    let mut args = vec![file];
    args.extend(state.args());
    let args_r = state.extend_stack(args);
    state.run_native_function("file_read", file_read, args_r, state.num_args() as u16 + 1)
}

fn r#type(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    Ok(vec![match state.try_arg(0) {
        Some(LuaValue::Object(o)) => match &*o.read().unwrap() {
            LuaObject::UserData(UserData::Full { data, .. }) => {
                if let Some(file) = data.read().unwrap().downcast_ref::<FileHandle>() {
                    match file {
                        FileHandle::Closed => LuaValue::String(b"closed file".into()),
                        _ => LuaValue::String(b"file".into()),
                    }
                } else {
                    LuaValue::Nil
                }
            }
            _ => LuaValue::Nil,
        },
        _ => LuaValue::Nil,
    }])
}

fn write(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let default = DEFAULT_OUTPUT.read().unwrap().clone();
    let mut args = vec![default];
    args.extend(state.args());
    let args_r = state.extend_stack(args);
    state.run_native_function(
        "file:write",
        file_write,
        args_r,
        state.num_args() as u16 + 1,
    )
}
