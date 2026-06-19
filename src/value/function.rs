use crate::{LuaState, value::LuaValue};

pub type NativeLuaFunction = fn(&mut LuaState) -> crate::Result<Vec<LuaValue>>;
