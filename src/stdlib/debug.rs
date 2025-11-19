use std::sync::LazyLock;

use crate::{
    macros::{require_closure, require_number},
    value::{LuaObject, LuaTable, LuaValue},
    vm::VM,
};

pub static DEBUG: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut debug = LuaTable::new();

    debug.insert(
        "upvalueid".into(),
        LuaObject::NativeFunction("upvalueid", upvalueid).into(),
    );

    debug.into()
});

fn upvalueid(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
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
