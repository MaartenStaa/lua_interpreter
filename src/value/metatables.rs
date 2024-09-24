use std::sync::LazyLock;

use super::{LuaTable, LuaValue};

pub(crate) static GLOBAL_BOOLEAN_METATABLE: LazyLock<LuaValue> =
    LazyLock::new(|| LuaTable::new().into());
pub(crate) static GLOBAL_NUMBER_METATABLE: LazyLock<LuaValue> =
    LazyLock::new(|| LuaTable::new().into());
pub(crate) static GLOBAL_STRING_METATABLE: LazyLock<LuaValue> =
    LazyLock::new(|| LuaTable::new().into());
