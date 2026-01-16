use crate::{value::LuaValue, vm::VM};

pub type NativeLuaFunction = fn(&mut VM) -> crate::Result<Vec<LuaValue>>;
