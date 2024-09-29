use std::sync::LazyLock;

use crate::macros::assert_table;

use super::{LuaObject, LuaTable, LuaValue, UserData};

pub(crate) static GLOBAL_BOOLEAN_METATABLE: LazyLock<LuaValue> =
    LazyLock::new(|| LuaTable::new().into());
pub(crate) static GLOBAL_NUMBER_METATABLE: LazyLock<LuaValue> =
    LazyLock::new(|| LuaTable::new().into());
pub(crate) static GLOBAL_STRING_METATABLE: LazyLock<LuaValue> =
    LazyLock::new(|| LuaTable::new().into());

pub(crate) static METATABLE_KEY: LazyLock<LuaValue> =
    LazyLock::new(|| LuaValue::String(b"__metatable".into()));

pub(crate) static CALL_KEY: LazyLock<LuaValue> =
    LazyLock::new(|| LuaValue::String(b"__call".into()));
pub(crate) static CLOSE_KEY: LazyLock<LuaValue> =
    LazyLock::new(|| LuaValue::String(b"__close".into()));

impl LuaValue {
    pub fn get_metatable(&self) -> Option<LuaValue> {
        match self {
            LuaValue::Boolean(_) => Some(GLOBAL_BOOLEAN_METATABLE.clone()),
            LuaValue::Number(_) => Some(GLOBAL_NUMBER_METATABLE.clone()),
            LuaValue::String(_) => Some(GLOBAL_STRING_METATABLE.clone()),
            LuaValue::Object(o) => o.read().unwrap().get_metatable(),
            _ => None,
        }
    }

    pub fn get_metavalue(&self, key: &LuaValue) -> Option<LuaValue> {
        self.get_metatable()
            .and_then(|mt| assert_table!(mt, table, table.get(key).cloned()))
    }
}

impl LuaObject {
    pub fn get_metatable(&self) -> Option<LuaValue> {
        match self {
            LuaObject::Table(t) => t.get(&METATABLE_KEY).cloned(),
            LuaObject::UserData(UserData::Full { metatable, .. }) => metatable.clone(),
            _ => None,
        }
    }

    pub fn get_metavalue(&self, key: &LuaValue) -> Option<LuaValue> {
        self.get_metatable()
            .and_then(|mt| assert_table!(mt, table, table.get(key).cloned()))
    }
}
