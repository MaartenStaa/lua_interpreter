use miette::{miette, IntoDiagnostic};
use std::{
    io::{stderr, stdin, stdout, Write},
    sync::LazyLock,
};

use crate::{
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

pub static IO: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut io = LuaTable::new();

    io.insert("stdin".into(), create_file(FileHandle::Stdin(stdin())));
    io.insert("stdout".into(), create_file(FileHandle::Stdout(stdout())));
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
}

impl FileHandle {
    fn write_all(&mut self, data: &[u8]) -> miette::Result<()> {
        match self {
            FileHandle::Stdin(_) => {
                return Err(miette!("cannot write to stdin"));
            }
            FileHandle::Stdout(handle) => handle.write_all(data).into_diagnostic()?,
            FileHandle::Stderr(handle) => handle.write_all(data).into_diagnostic()?,
            FileHandle::File(handle) => handle.write_all(data).into_diagnostic()?,
        }

        Ok(())
    }
}

fn create_file(handle: FileHandle) -> LuaValue {
    let mut methods = LuaTable::new();

    methods.insert(
        "write".into(),
        LuaObject::NativeFunction("file_write", file_write).into(),
    );

    UserData::new_full(File { methods, handle }, FILE_METATABLE.clone()).into()
}

fn file_index(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    require_userdata!(input, "file:__index", 0, userdata, {
        require_userdata_type!(userdata, "file:__index", 0, File, file, _metatable, {
            let result = file
                .methods
                .get(input.get(1).unwrap_or(&LuaValue::Nil))
                .cloned()
                .unwrap_or(LuaValue::Nil);
            dbg!(&result);
            Ok(vec![result])
        })
    })
}

fn file_write(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
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
