use std::sync::LazyLock;

use crate::value::{LuaTable, LuaValue};

pub(crate) static CONFIG: LazyLock<PackageConfig> = LazyLock::new(|| PackageConfig {
    directory_separator: std::path::MAIN_SEPARATOR,
    template_separator: b';',
    substitution_character: b'?',
});

pub static PACKAGE: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut package = LuaTable::new();

    // Global state variables
    package.insert("config".into(), CONFIG.clone().into());
    package.insert("loaded".into(), LuaTable::new().into());
    package.insert(
        "path".into(),
        LuaValue::String(b"./?.lua;./?/init.lua".into()),
    );

    package.into()
});

#[derive(Clone)]
pub(crate) struct PackageConfig {
    pub(crate) directory_separator: char,
    pub(crate) template_separator: u8,
    pub(crate) substitution_character: u8,
    // TODO:
    // The fourth line is a string that, in a path in Windows, is replaced by the executable's directory. Default is '!'.
    // The fifth line is a mark to ignore all text after it when building the luaopen_ function name. Default is '-'.
}

impl From<PackageConfig> for LuaValue {
    fn from(config: PackageConfig) -> Self {
        let str = vec![
            config.directory_separator as u8,
            b'\n',
            config.template_separator,
            b'\n',
            config.substitution_character,
        ];

        LuaValue::String(str)
    }
}
