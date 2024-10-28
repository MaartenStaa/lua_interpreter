use miette::{miette, IntoDiagnostic};
use std::{
    cell::RefCell,
    io::{stderr, stdin, stdout, Read, Write},
    path::Path,
    sync::{LazyLock, Mutex},
};

use crate::{
    macros::{assert_table, get_string, require_string, require_userdata, require_userdata_type},
    value::{metatables, LuaObject, LuaTable, LuaValue, UserData},
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

pub static IO: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut io = LuaTable::new();

    // Predefined file handles
    io.insert("stdin".into(), create_file(File::Stdin(stdin())));
    io.insert("stdout".into(), create_file(File::Stdout(stdout())));
    io.insert("stderr".into(), create_file(File::Stderr(stderr())));

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

static DEFAULT_INPUT: LazyLock<Mutex<RefCell<LuaValue>>> = LazyLock::new(|| {
    Mutex::new(RefCell::new(assert_table!(
        read,
        &*IO,
        io,
        io.get(&"stdin".into()).cloned().unwrap()
    )))
});
static DEFAULT_OUTPUT: LazyLock<Mutex<RefCell<LuaValue>>> = LazyLock::new(|| {
    Mutex::new(RefCell::new(assert_table!(
        read,
        &*IO,
        io,
        io.get(&"stdout".into()).cloned().unwrap()
    )))
});

#[derive(Debug)]
pub enum File {
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

impl File {
    fn read(&mut self, format: ReadFormat) -> miette::Result<LuaValue> {
        match self {
            File::Stdin(stdin) => match format {
                ReadFormat::Numeral => todo!(),
                ReadFormat::All => {
                    let mut buffer = Vec::new();
                    stdin.read_to_end(&mut buffer).into_diagnostic()?;
                    Ok(LuaValue::String(buffer))
                }
                ReadFormat::Line => {
                    let mut buffer = String::new();
                    stdin.read_line(&mut buffer).into_diagnostic()?;
                    Ok(buffer.into())
                }
                ReadFormat::LineIncluding => todo!(),
                ReadFormat::N(n) => {
                    let mut buffer = vec![0; n];
                    stdin.read_exact(&mut buffer).into_diagnostic()?;
                    Ok(LuaValue::String(buffer))
                }
            },
            _ => todo!(),
        }
    }

    fn write_all(&mut self, data: &[u8]) -> miette::Result<()> {
        match self {
            File::Stdin(_) => {
                return Err(miette!("cannot write to stdin"));
            }
            File::Stdout(handle) => handle.write_all(data).into_diagnostic()?,
            File::Stderr(handle) => handle.write_all(data).into_diagnostic()?,
            File::File(handle) => handle.write_all(data).into_diagnostic()?,
            File::Closed => {
                return Err(miette!("file is closed"));
            }
        }

        Ok(())
    }
}

fn create_file(file: File) -> LuaValue {
    UserData::new_full(file, FILE_METATABLE.clone()).into()
}

fn file_close(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    require_userdata!(write, input, "file:write", 0, userdata, {
        require_userdata_type!(write, userdata, "file:write", 0, File, file, _metatable, {
            match &file {
                File::File(handle) => {
                    handle.sync_all().into_diagnostic()?;
                    *file = File::Closed;

                    Ok(vec![LuaValue::Boolean(true)])
                }
                _ => Ok(vec![LuaValue::Boolean(false)]),
            }
        })
    })
}

fn file_read(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    require_userdata!(write, input, "file:read", 0, userdata, {
        require_userdata_type!(write, userdata, "file:read", 0, File, file, _metatable, {
            for (i, value) in input.iter().enumerate().skip(1) {
                match value {
                    LuaValue::String(s) => {
                        file.write_all(s).unwrap();
                    }
                    LuaValue::Number(n) => {
                        file.write_all(n.to_string().as_bytes()).unwrap();
                    }
                    _ => {
                        return Err(miette!(
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

fn file_write(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    require_userdata!(write, input, "file:write", 0, userdata, {
        require_userdata_type!(write, userdata, "file:write", 0, File, file, _metatable, {
            for (i, value) in input.iter().enumerate().skip(1) {
                match value {
                    LuaValue::String(s) => {
                        file.write_all(s).unwrap();
                    }
                    LuaValue::Number(n) => {
                        file.write_all(n.to_string().as_bytes()).unwrap();
                    }
                    _ => {
                        return Err(miette!(
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

fn close(vm: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let file = match input.into_iter().next() {
        None | Some(LuaValue::Nil) => (*DEFAULT_OUTPUT.lock().unwrap()).borrow().clone(),
        Some(LuaValue::Object(o)) => match &*o.read().unwrap() {
            LuaObject::UserData(UserData::Full { data, .. }) => {
                let mut file = data.write().unwrap();
                let file = file
                    .downcast_mut::<File>()
                    .ok_or_else(|| miette!("bad argument #1 to 'io.close' (file expected)"))?;
                *file = File::Closed;

                LuaValue::Object(o.clone())
            }
            _ => return Err(miette!("bad argument #1 to 'io.close' (file expected)")),
        },
        _ => return Err(miette!("bad argument #1 to 'io.close' (file expected)")),
    };

    file_close(vm, vec![file])
}

fn input(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let result = match values.into_iter().next() {
        Some(LuaValue::Nil) | None => (*DEFAULT_INPUT.lock().unwrap()).borrow().clone(),
        Some(LuaValue::Object(o)) => match &*o.read().unwrap() {
            LuaObject::UserData(UserData::Full { data, .. }) => {
                data.read()
                    .unwrap()
                    .downcast_ref::<File>()
                    .ok_or_else(|| miette!("bad argument #1 to 'io.input' (file expected)"))?;

                let output = LuaValue::Object(o.clone());
                DEFAULT_INPUT.lock().unwrap().replace(output.clone());

                output
            }
            _ => return Err(miette!("bad argument #1 to 'io.input' (file expected)")),
        },
        _ => return Err(miette!("bad argument #1 to 'io.input' (file expected)")),
    };

    Ok(vec![result])
}

fn read(vm: &mut VM, mut input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    // Equivalent to `io.input():read(...)`
    let file = (*DEFAULT_INPUT.lock().unwrap()).borrow().clone();
    // TODO: Should we navigate through the metatable lookup?
    input.insert(0, file);
    file_read(vm, input)
}

fn r#type(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    Ok(vec![match values.into_iter().next() {
        Some(LuaValue::Object(o)) => match &*o.read().unwrap() {
            LuaObject::UserData(UserData::Full { data, .. }) => {
                if let Some(file) = data.read().unwrap().downcast_ref::<File>() {
                    match file {
                        File::Closed => LuaValue::String("closed file".into()),
                        _ => LuaValue::String("file".into()),
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

fn open(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
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
            return Err(miette!(
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

    Ok(vec![create_file(File::File(file))])
}

fn output(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let result = match values.into_iter().next() {
        Some(LuaValue::Nil) | None => (*DEFAULT_OUTPUT.lock().unwrap()).borrow().clone(),
        Some(LuaValue::Object(o)) => match &*o.read().unwrap() {
            LuaObject::UserData(UserData::Full { data, .. }) => {
                data.read()
                    .unwrap()
                    .downcast_ref::<File>()
                    .ok_or_else(|| miette!("bad argument #1 to 'io.output' (file expected)"))?;

                let output = LuaValue::Object(o.clone());
                DEFAULT_OUTPUT.lock().unwrap().replace(output.clone());

                output
            }
            _ => return Err(miette!("bad argument #1 to 'io.output' (file expected)")),
        },
        _ => return Err(miette!("bad argument #1 to 'io.output' (file expected)")),
    };

    Ok(vec![result])
}
