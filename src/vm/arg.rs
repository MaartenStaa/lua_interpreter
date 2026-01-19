use std::borrow::Cow;

use crate::{
    LuaState,
    error::lua_error,
    value::{LuaNumber, LuaString, LuaValue},
    vm::CallFrameKind,
};

impl LuaState {
    /// Get the number of arguments passed to the current function call.
    pub fn num_args(&self) -> usize {
        self.call_stack[self.call_stack_index - 1].num_args as usize
    }

    /// Get all arguments for the current function call.
    pub fn args(&self) -> Vec<LuaValue> {
        let num_args = self.call_stack[self.call_stack_index - 1].num_args as usize;
        (0..num_args).map(|i| self.get(i)).collect()
    }

    /// Get references to all arguments for the current function call.
    pub fn args_ref(&self) -> Vec<Cow<'_, LuaValue>> {
        let num_args = self.call_stack[self.call_stack_index - 1].num_args as usize;
        (0..num_args).map(|i| self.get_ref(i)).collect()
    }

    /// Get the argument at the given index for the current function call. Returns
    /// [`LuaValue::Nil`] if the index is out of bounds (>= the number of passed arguments).
    pub fn arg(&self, index: usize) -> LuaValue {
        let num_args = self.call_stack[self.call_stack_index - 1].num_args as usize;
        if index >= num_args {
            return LuaValue::Nil;
        }

        self.get(index)
    }

    /// Try to get the argument at the given index for the current function call. Returns
    /// [`Option::None`] if the index is out of bounds (>= the number of passed arguments).
    pub fn try_arg(&self, index: usize) -> Option<LuaValue> {
        let num_args = self.call_stack[self.call_stack_index - 1].num_args as usize;
        if index >= num_args {
            return None;
        }

        Some(self.get(index))
    }

    /// Ensure that the argument at the given index for the current function call exists. Like
    /// [`Self::arg`], but returns an error if the index is out of bounds.
    pub fn ensure_arg(&self, index: usize) -> crate::Result<LuaValue> {
        let num_args = self.call_stack[self.call_stack_index - 1].num_args as usize;
        if index >= num_args {
            return self.arg_err(index, "value expected");
        }

        Ok(self.get(index))
    }

    /// Get a reference to the argument at the given index for the current function call. Like
    /// [`Self::arg`], returns [`LuaValue::Nil`] if the index is out of bounds.
    pub fn arg_ref(&self, index: usize) -> Cow<'_, LuaValue> {
        let num_args = self.call_stack[self.call_stack_index - 1].num_args as usize;
        if index >= num_args {
            return std::borrow::Cow::Borrowed(&LuaValue::Nil);
        }

        self.get_ref(index)
    }

    pub fn arg_as_number(&self, index: usize) -> crate::Result<LuaNumber> {
        match self.try_arg(index) {
            Some(LuaValue::Number(n)) => Ok(n),
            Some(value) => {
                self.arg_err(index, format!("number expected, got {}", value.type_name()))
            }
            None => self.arg_err(index, "number expected, got no value"),
        }
    }

    pub fn try_arg_as_number(&self, index: usize) -> crate::Result<Option<LuaNumber>> {
        match self.try_arg(index) {
            Some(LuaValue::Number(n)) => Ok(Some(n)),
            Some(LuaValue::Nil) | None => Ok(None),
            Some(value) => {
                self.arg_err(index, format!("number expected, got {}", value.type_name()))
            }
        }
    }

    pub fn arg_as_integer(&self, index: usize) -> crate::Result<i64> {
        self.arg_as_number(index)?
            .integer_repr()
            .map_err(|err| self.arg_err::<i64>(index, err.message).unwrap_err())
    }

    pub fn try_arg_as_integer(&self, index: usize) -> crate::Result<Option<i64>> {
        match self.try_arg_as_number(index)? {
            Some(n) => Ok(Some(n.integer_repr().map_err(|err| {
                self.arg_err::<i64>(index, err.message).unwrap_err()
            })?)),
            None => Ok(None),
        }
    }

    pub fn arg_as_string(&self, index: usize) -> crate::Result<LuaString> {
        match self.arg(index) {
            LuaValue::String(s) => Ok(s),
            value => self.arg_err(index, format!("string expected, got {}", value.type_name())),
        }
    }

    pub fn try_arg_as_string(&self, index: usize) -> crate::Result<Option<LuaString>> {
        match self.try_arg(index) {
            Some(LuaValue::String(s)) => Ok(Some(s)),
            Some(LuaValue::Nil) | None => Ok(None),
            Some(value) => {
                self.arg_err(index, format!("string expected, got {}", value.type_name()))
            }
        }
    }

    pub(crate) fn arg_err<T>(&self, index: usize, message: impl AsRef<str>) -> crate::Result<T> {
        let frame = &self.call_stack[self.call_stack_index - 1];
        let name: Cow<'_, str> = match frame.kind {
            CallFrameKind::Lua { chunk, .. } => {
                Cow::Owned(self.get_chunk(chunk).unwrap().name.to_string())
            }
            CallFrameKind::Native { name } => Cow::Borrowed(name),
        };

        Err(lua_error!(
            "bad argument #{} to '{name}' ({})",
            index + 1,
            message.as_ref()
        ))
    }
}
