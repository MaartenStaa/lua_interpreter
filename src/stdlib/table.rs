use miette::miette;
use std::sync::LazyLock;

use crate::{
    value::{LuaObject, LuaTable, LuaValue},
    vm::VM,
};

pub static TABLE: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut table = LuaTable::new();

    table.insert(
        "concat".into(),
        LuaObject::NativeFunction("concat", concat).into(),
    );

    table.into()
});

fn concat(_: &mut VM, input: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    // Temporary storage for number conversion, avoids needing to clone a string separator to match
    // the Vec<u8> type.
    let temp;
    let sep = match input.get(1) {
        Some(LuaValue::String(s)) => Some(s.as_slice()),
        Some(LuaValue::Number(n)) => {
            temp = Some(n.to_string());
            temp.as_ref().map(|s| s.as_bytes())
        }
        Some(LuaValue::Nil) => None,
        Some(v) => {
            return Err(miette!(
                "bad argument #1 to 'concat' (string expected, got {type_name})",
                type_name = v.type_name()
            ))
        }
        None => return Err(miette!("bad argument #1 to 'concat' (value expected)")),
    };
    let i = match input.get(2) {
        Some(LuaValue::Number(n)) => n.integer_repr()?,
        Some(v) => {
            return Err(miette!(
                "bad argument #2 to 'concat' (number expected, got {type_name})",
                type_name = v.type_name()
            ))
        }
        None => 1,
    };
    let j = match input.get(3) {
        Some(LuaValue::Number(n)) => Some(n.integer_repr()?),
        Some(v) => {
            return Err(miette!(
                "bad argument #3 to 'concat' (number expected, got {type_name})",
                type_name = v.type_name()
            ))
        }
        None => None,
    };

    let mut result = Vec::new();

    match input.first() {
        Some(LuaValue::Object(o)) => match &*o.read().unwrap() {
            LuaObject::Table(t) => {
                let j = j.unwrap_or_else(|| t.len() as i64);
                for k in i..=j {
                    if k > i {
                        if let Some(sep) = sep.as_ref() {
                            result.extend(*sep);
                        }
                    }

                    match t.get(&k.into()).unwrap_or(&LuaValue::Nil) {
                        LuaValue::String(s) => result.extend(s),
                        LuaValue::Number(n) => result.extend(n.to_string().as_bytes()),
                        v => {
                            return Err(miette!(
                            "invalid value ({type_name}) at index {index} in table for 'concat'",
                            type_name = v.type_name(),
                            index = k
                        ))
                        }
                    }
                }
            }
            _ => return Err(miette!("bad argument #1 to 'concat' (table expected)")),
        },
        Some(v) => {
            return Err(miette!(
                "bad argument #1 to 'concat' (table expected, got {type_name})",
                type_name = v.type_name()
            ))
        }
        None => return Err(miette!("bad argument #1 to 'concat' (value expected)")),
    }

    Ok(vec![LuaValue::String(result)])
}
