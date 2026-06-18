use std::{
    io::{Read, Write, stderr, stdin, stdout},
    path::Path,
    sync::{LazyLock, RwLock},
};

use crate::{
    error::{IntoLuaError, lua_error},
    macros::{get_string, require_string, require_userdata, require_userdata_type},
    value::{LuaObject, LuaTable, LuaValue, UserData, metatables},
    vm::VM,
};

static FILE_METATABLE: LazyLock<LuaTable> = LazyLock::new(|| {
    let mut file_metatable = LuaTable::new();

    file_metatable.insert(metatables::INDEX_KEY.clone(), {
        let mut index = LuaTable::new();

        index.insert(
            "close".into(),
            LuaObject::NativeFunction("file_close", file_close).into(),
        );
        index.insert(
            "read".into(),
            LuaObject::NativeFunction("file_read", file_read).into(),
        );
        index.insert(
            "write".into(),
            LuaObject::NativeFunction("file_write", file_write).into(),
        );

        index.into()
    });
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
        "read".into(),
        LuaObject::NativeFunction("read", read).into(),
    );
    io.insert(
        "type".into(),
        LuaObject::NativeFunction("type", r#type).into(),
    );
    io.insert(
        "output".into(),
        LuaObject::NativeFunction("output", output).into(),
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
            Self::Closed => {
                return Err(lua_error!("file is already closed"));
            }
            Self::Stdin(_) | Self::Stdout(_) | Self::Stderr(_) => {
                return Err(lua_error!("cannot close standard file"));
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
            Self::Stdout(handle) => handle.write_all(data).into_lua_error()?,
            Self::Stderr(handle) => handle.write_all(data).into_lua_error()?,
            Self::File(handle) => handle.write_all(data).into_lua_error()?,
            Self::Closed => {
                return Err(lua_error!("cannot write to closed file"));
            }
        }

        Ok(())
    }
}

fn create_file(file: FileHandle) -> LuaValue {
    UserData::new_full(file, FILE_METATABLE.clone()).into()
}

fn file_close(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    require_userdata!(write, input, "file:write", 0, userdata, {
        require_userdata_type!(write, userdata, "file:write", 0, File, file, _metatable, {
            file.handle.close()?;
            Ok(vec![LuaValue::Boolean(true)])
        })
    })
}

fn file_index(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    require_userdata!(input, "file:__index", 0, userdata, {
        require_userdata_type!(userdata, "file:__index", 0, File, file, _metatable, {
            Ok(vec![
                file.methods
                    .get(input.get(1).unwrap_or(&LuaValue::Nil))
                    .cloned()
                    .unwrap_or(LuaValue::Nil),
            ])
        })
    })
}

fn file_read(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    require_userdata!(write, input, "file:read", 0, userdata, {
        require_userdata_type!(write, userdata, "file:read", 0, File, file, _metatable, {
            let mut result = Vec::new();
            if input.len() == 1 {
                result.push(match file.handle.read(ReadFormat::Line) {
                    Ok(s) => s,
                    Err(_) => return Ok(vec![LuaValue::Nil]),
                });
            } else {
                for (i, value) in input.iter().enumerate().skip(1) {
                    match value {
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

            Ok(vec![input.first().unwrap().clone()])
        })
    })
}

fn file_write(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    require_userdata!(write, input, "file:write", 0, userdata, {
        require_userdata_type!(write, userdata, "file:write", 0, File, file, _metatable, {
            for (i, value) in input.iter().enumerate().skip(1) {
                match value {
                    LuaValue::String(s) => {
                        file.handle.write_all(s).unwrap();
                    }
                    LuaValue::Number(n) => {
                        file.handle.write_all(n.to_string().as_bytes()).unwrap();
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

            Ok(vec![input.first().unwrap().clone()])
        })
    })
}

fn close(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    match input.first() {
        Some(value) if *value != LuaValue::Nil => {
            require_userdata!(write, input, "io.close", 0, userdata, {
                require_userdata_type!(write, userdata, "io.close", 0, File, file, _metatable, {
                    file.handle.close()?;
                    Ok(vec![LuaValue::Boolean(true)])
                })
            })
        }
        _ => {
            let default = (*DEFAULT_OUTPUT.write().unwrap()).clone();
            require_userdata!(write, [default], "io.close", 0, userdata, {
                require_userdata_type!(write, userdata, "io.close", 0, File, file, _metatable, {
                    file.handle.close()?;
                    Ok(vec![LuaValue::Boolean(true)])
                })
            })
        }
    }
}

fn input(_: &mut VM, values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let result = match values.into_iter().next() {
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

fn read(vm: &mut VM, mut input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    // Equivalent to `io.input():read(...)`
    let file = (*DEFAULT_INPUT.write().unwrap()).clone();
    // TODO: Should we navigate through the metatable lookup?
    input.insert(0, file);
    file_read(vm, input)
}

fn r#type(_: &mut VM, values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    Ok(vec![match values.into_iter().next() {
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

fn open(_: &mut VM, values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let filename = require_string!(values, "io.open", 0);
    let mode = get_string!(values, "io.open", 1)
        .map(|s| s.as_slice())
        .unwrap_or(b"r");

    let mut options = std::fs::OpenOptions::new();
    match mode {
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
                "bad argument #2 to 'io.open' (invalid mode '{}')",
                String::from_utf8_lossy(mode)
            ));
        }
    }

    let filename_str = String::from_utf8_lossy(filename);
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

fn output(_: &mut VM, values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let result = match values.into_iter().next() {
        Some(LuaValue::Nil) | None => (*DEFAULT_OUTPUT.read().unwrap()).clone(),
        Some(LuaValue::Object(o)) => match &*o.read().unwrap() {
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
