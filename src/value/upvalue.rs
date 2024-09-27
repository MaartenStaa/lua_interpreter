use crate::value::metatables;

use super::LuaValue;
use std::{
    fmt::{self, Debug, Display, Formatter},
    ops::Deref,
};

pub struct UpValue(pub(crate) LuaValue);

/// This here is the key point of having an `UpValue` type: Implementing `Drop` so we can "close"
/// it when it goes out of scope.
impl Drop for UpValue {
    fn drop(&mut self) {
        let __close = self.0.get_metavalue(&metatables::CLOSE_KEY);
        if let Some(__close) = __close {
            todo!("call __close metamethod for upvalue (need access to vm!)");
        }
    }
}

impl Deref for UpValue {
    type Target = LuaValue;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<LuaValue> for UpValue {
    fn from(value: LuaValue) -> Self {
        Self(value)
    }
}

impl Debug for UpValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl Display for UpValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
