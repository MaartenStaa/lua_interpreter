use crate::{value::LuaValue, vm::VM};

pub(crate) fn collectgarbage(_: &mut VM, _: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    Ok(vec![])
}
