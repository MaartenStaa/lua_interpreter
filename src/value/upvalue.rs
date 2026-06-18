use super::LuaValue;
use std::{
    fmt::{self, Debug, Display, Formatter},
    ops::{Deref, DerefMut},
};

pub struct UpValue(pub(crate) LuaValue);

impl Deref for UpValue {
    type Target = LuaValue;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for UpValue {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<LuaValue> for UpValue {
    fn from(value: LuaValue) -> Self {
        Self(value)
    }
}

impl Debug for UpValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "upvalue<0x{:x}>: {:?}",
            self as *const _ as usize, self.0
        )
    }
}

impl Display for UpValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
