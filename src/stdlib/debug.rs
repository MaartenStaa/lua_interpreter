use std::sync::LazyLock;

use crate::{
    macros::{require_closure, require_number},
    value::{LuaObject, LuaTable, LuaValue},
    vm::VM,
};
use miette::miette;

pub static DEBUG: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut debug = LuaTable::new();

    debug.insert(
        "getinfo".into(),
        LuaObject::NativeFunction("getinfo", getinfo).into(),
    );
    debug.insert(
        "setmetatable".into(),
        LuaObject::NativeFunction("setmetatable", setmetatable).into(),
    );
    debug.insert(
        "upvalueid".into(),
        LuaObject::NativeFunction("upvalueid", upvalueid).into(),
    );

    debug.into()
});

fn getinfo(vm: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    // TODO: First argument can optionally refer to the thread
    // TODO: Last argument can refer to which fields to fill in the table

    let mut info = LuaTable::new();
    match input.first() {
        Some(LuaValue::Number(n)) => {
            let call_frame_offset = n.integer_repr()?;
            if call_frame_offset < 0 || call_frame_offset as usize > vm.call_stack_index {
                return Ok(vec![LuaValue::Nil]);
            }

            let call_frame = &vm.call_stack[vm.call_stack_index - call_frame_offset as usize];
            if let Some(name) = &call_frame.name {
                info.insert("name".into(), LuaValue::String(name.clone()));
            }
        }
        Some(LuaValue::Object(o)) => match &*o.read().unwrap() {
            LuaObject::Closure(closure) => {
                if let Some(name) = &closure.name {
                    info.insert("name".into(), LuaValue::String(name.clone()));
                }
            }
            LuaObject::NativeFunction(name, _) => {
                info.insert("name".into(), LuaValue::String(name.as_bytes().to_vec()));
            }
            _ => {
                return Err(miette!(
                    "bad argument #1 to 'getinfo' (number or function expected, got {type_name})",
                    type_name = o.read().unwrap().type_name()
                ));
            }
        },
        Some(v) => {
            return Err(miette!(
                "bad argument #1 to 'getinfo' (number or function expected, got {type_name})",
                type_name = v.type_name()
            ));
        }
        None => {
            return Err(miette!("bad argument #1 to 'getinfo' (value expected)"));
        }
    }

    Ok(vec![info.into()])
}

fn setmetatable(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let mut input = input.into_iter();
    let mut value = match input.next() {
        Some(value) => value,
        None => {
            return Err(miette!(
                "bad argument #1 to 'setmetatable' (value expected)"
            ))
        }
    };
    let mt = match input.next() {
        Some(LuaValue::Nil) | None => None,
        Some(mt) => Some(mt),
    };

    value.set_metatable(mt)?;
    Ok(vec![value])
}

fn upvalueid(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    require_closure!(input, "debug.upvalueid", closure, {
        let n = require_number!(input, "debug.upvalueid", 1).integer_repr()? - 1;
        if n < 0 || n as usize >= closure.upvalues.len() {
            return Ok(vec![LuaValue::Nil]);
        }

        if let Some(upvalue) = &closure.upvalues[n as usize] {
            let id = (&*upvalue.read().unwrap()) as *const _ as i64;
            // TODO: This should be a "light userdata" value
            Ok(vec![id.into()])
        } else {
            Ok(vec![LuaValue::Nil])
        }
    })
}
