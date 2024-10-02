use crate::value::LuaValue;

pub(crate) mod debug;
pub(crate) mod globals;
pub(crate) mod io;
pub(crate) mod math;
pub(crate) mod os;
pub(crate) mod package;
pub(crate) mod string;
pub(crate) mod table;
pub(crate) mod utf8;

pub fn string() -> LuaValue {
    string::STRING.clone()
}
