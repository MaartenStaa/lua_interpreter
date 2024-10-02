use std::sync::LazyLock;

use crate::value::{LuaTable, LuaValue};

pub static UTF8: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut package = LuaTable::new();

    // Global constants
    package.insert(
        "charpattern".into(),
        LuaValue::String(b"[\0-\x7F\xC2-\xFD][\x80-\xBF]*".into()),
    );

    package.into()
});
