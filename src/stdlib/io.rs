use std::{
    fs::OpenOptions,
    io::{Write, stderr, stdin, stdout},
    sync::{LazyLock, RwLock},
};

use crate::{
    error::{IntoLuaError, lua_error},
    macros::{require_userdata, require_userdata_type},
    value::{LuaObject, LuaTable, LuaValue, UserData},
    vm::VM,
};

static FILE_METATABLE: LazyLock<LuaTable> = LazyLock::new(|| {
    let mut file_metatable = LuaTable::new();

    file_metatable.insert(
        "__index".into(),
        LuaObject::NativeFunction("file_index", file_index).into(),
    );

    file_metatable
});

static STDOUT: LazyLock<LuaValue> = LazyLock::new(|| create_file(FileHandle::Stdout(stdout())));

static DEFAULT_OUTPUT: LazyLock<RwLock<LuaValue>> = LazyLock::new(|| RwLock::new(STDOUT.clone()));

pub static IO: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut io = LuaTable::new();

    io.insert(
        "close".into(),
        LuaObject::NativeFunction("io_close", close).into(),
    );
    io.insert(
        "output".into(),
        LuaObject::NativeFunction("io_output", output).into(),
    );
    io.insert(
        "write".into(),
        LuaObject::NativeFunction("io_write", write).into(),
    );

    io.insert("stdin".into(), create_file(FileHandle::Stdin(stdin())));
    io.insert("stdout".into(), STDOUT.clone());
    io.insert("stderr".into(), create_file(FileHandle::Stderr(stderr())));

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

impl FileHandle {
    fn write_all(&mut self, data: &[u8]) -> crate::Result<()> {
        match self {
            FileHandle::Stdin(_) => {
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

    fn close(&mut self) -> crate::Result<()> {
        match self {
            FileHandle::Closed => {
                return Err(lua_error!("attempt to use a closed file"));
            }
            FileHandle::Stdin(_) | FileHandle::Stdout(_) | FileHandle::Stderr(_) => {
                return Err(lua_error!("cannot close standard file"));
            }
            FileHandle::File(_) => {
                *self = FileHandle::Closed;
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
        "write".into(),
        LuaObject::NativeFunction("file_write", file_write).into(),
    );

    UserData::new_full(File { methods, handle }, FILE_METATABLE.clone()).into()
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

fn file_close(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    require_userdata!(write, input, "file:close", 0, userdata, {
        require_userdata_type!(write, userdata, "file:close", 0, File, file, _metatable, {
            file.handle.close()?;
            Ok(vec![LuaValue::Boolean(true)])
        })
    })
}

fn file_write(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    require_userdata!(write, input, "file:write", 0, userdata, {
        require_userdata_type!(write, userdata, "file:write", 0, File, file, _metatable, {
            for (i, value) in input.iter().enumerate().skip(1) {
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
            let default = DEFAULT_OUTPUT.read().unwrap().clone();
            require_userdata!(write, [default], "io.close", 0, userdata, {
                require_userdata_type!(write, userdata, "io.close", 0, File, file, _metatable, {
                    file.handle.close()?;
                    Ok(vec![LuaValue::Boolean(true)])
                })
            })
        }
    }
}

fn output(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    match input.first() {
        None | Some(LuaValue::Nil) => Ok(vec![DEFAULT_OUTPUT.read().unwrap().clone()]),
        Some(LuaValue::String(filename)) => {
            let filename = String::from_utf8_lossy(filename).into_owned();
            let file = OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(&filename)
                .map_err(|e| lua_error!("{}: {}", filename, e))?;
            let value = create_file(FileHandle::File(file));
            *DEFAULT_OUTPUT.write().unwrap() = value.clone();
            Ok(vec![value])
        }
        Some(LuaValue::Object(_)) => {
            let value = input.into_iter().next().unwrap();
            *DEFAULT_OUTPUT.write().unwrap() = value.clone();
            Ok(vec![value])
        }
        Some(v) => Err(lua_error!(
            "bad argument #1 to 'output' (string or file expected, got {})",
            v.type_name()
        )),
    }
}

fn write(vm: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let default = DEFAULT_OUTPUT.read().unwrap().clone();
    let mut args = vec![default];
    args.extend(input);
    file_write(vm, args)
}
