use std::{
    borrow::Cow,
    sync::{Arc, RwLock},
};

use crate::{
    LuaState,
    error::lua_error,
    value::{
        IntegerRepr, LuaClosure, LuaNumber, LuaObject, LuaString, LuaTable, LuaValue, UpValue,
        UserData, callable::Callable,
    },
    vm::CallFrameKind,
};

/// Utility functions for working with function arguments in the Lua state.
impl LuaState {
    /// Get the number of arguments passed to the current function call.
    pub fn num_args(&self) -> u16 {
        self.call_frame().num_args
    }

    /// Get all arguments for the current function call.
    pub fn args(&self) -> Vec<LuaValue> {
        let num_args = self.call_frame().num_args as usize;
        (0..num_args).map(|i| self.get(i + 1)).collect()
    }

    /// Get references to all arguments for the current function call.
    pub fn args_ref(&self) -> Vec<Cow<'_, LuaValue>> {
        let num_args = self.call_frame().num_args as usize;
        (0..num_args).map(|i| self.get_ref(i + 1)).collect()
    }

    /// Get the argument at the given index for the current function call. Returns
    /// [`LuaValue::NIL`] if the index is out of bounds (>= the number of passed arguments).
    pub fn arg(&self, index: usize) -> LuaValue {
        let num_args = self.call_frame().num_args as usize;
        if index >= num_args {
            return LuaValue::NIL;
        }

        self.get(index + 1)
    }

    /// Try to get the argument at the given index for the current function call. Returns
    /// [`Option::None`] if the index is out of bounds (>= the number of passed arguments).
    pub fn try_arg(&self, index: usize) -> Option<LuaValue> {
        let num_args = self.call_frame().num_args as usize;
        if index >= num_args {
            return None;
        }

        Some(self.get(index + 1))
    }

    /// Ensure that the argument at the given index for the current function call exists. Like
    /// [`Self::arg`], but returns an error if the index is out of bounds.
    pub fn ensure_arg(&self, index: usize) -> crate::Result<LuaValue> {
        let num_args = self.call_frame().num_args as usize;
        if index >= num_args {
            return self.arg_err(index, "value expected");
        }

        Ok(self.get(index + 1))
    }

    /// Get a reference to the argument at the given index for the current function call. Like
    /// [`Self::arg`], returns [`LuaValue::NIL`] if the index is out of bounds.
    pub fn arg_ref(&self, index: usize) -> Cow<'_, LuaValue> {
        let num_args = self.call_frame().num_args as usize;
        if index >= num_args {
            return std::borrow::Cow::Owned(LuaValue::NIL);
        }

        self.get_ref(index + 1)
    }

    pub fn arg_as_callable(&self, index: usize) -> crate::Result<Callable> {
        match self.try_arg(0) {
            Some(v) if v.is_object() => {
                let o = &*unsafe { v.as_object() }.read().unwrap();
                let r: Result<Callable, _> = o.try_into();
                Ok(r.map_err(|_| {
                    self.arg_err::<()>(index, format!("function expected, got {}", o.type_name()))
                        .unwrap_err()
                })?)
            }
            Some(value) => self.arg_err(
                index,
                format!("function expected, got {}", value.type_name()),
            ),
            None => self.arg_err(index, "function expected, got no value"),
        }
    }

    pub(crate) fn arg_err<T>(&self, index: usize, message: impl AsRef<str>) -> crate::Result<T> {
        let frame = self.call_frame();
        let name: Cow<'_, str> = match frame.kind {
            CallFrameKind::Lua { chunk, .. } => {
                Cow::Owned(self.with_chunk(chunk, |c| c.name.to_string()).unwrap())
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

/// Utility functions for working with integral function arguments in the Lua state.
///
/// NOTE: These are not implemented as part of the `impl_arg_type!` macro because they require
/// special handling for float arguments.
impl LuaState {
    /// Try to get the argument at the given index for the current function call as an integer. If
    /// the argument is a float, it will be converted to an integer if possible. Returns an error
    /// if the argument is not a number or cannot be converted to an integer.
    pub fn arg_as_integer(&self, index: usize) -> crate::Result<i64> {
        self.arg_as_number(index)?
            .integer_repr()
            .map_err(|err| self.arg_err::<i64>(index, err.message).unwrap_err())
    }

    /// Try to get the argument at the given index for the current function call as an integer. If
    /// the argument is a float, it will be converted to an integer if possible. Returns
    /// [`Option::None`] if the index is out of bounds (>= the number of passed arguments), or if
    /// the argument is [`LuaValue::NIL`]. Returns an error if the argument is not a number or
    /// cannot be converted to an integer.
    pub fn try_arg_as_integer(&self, index: usize) -> crate::Result<Option<i64>> {
        match self.try_arg_as_number(index)? {
            Some(n) => Ok(Some(n.integer_repr().map_err(|err| {
                self.arg_err::<i64>(index, err.message).unwrap_err()
            })?)),
            None => Ok(None),
        }
    }
}

macro_rules! impl_arg_type {
    ($type:ident, $owned_result:ty, $conversion:ident) => {
        paste::paste! {
            impl LuaState {
                /// Get the argument at the given index for the current function call, and convert
                /// it to the given type. Returns an error if the argument is not of the expected
                /// type.
                pub fn [<arg_as_ $type>](&self, index: usize) -> crate::Result<$owned_result> {
                    match self.try_arg(index) {
                        Some(v) if v.[<is_ $type>]() => Ok(unsafe { v.[<$conversion _ $type>]() }),
                        Some(value) => {
                            self.arg_err(index, format!("{} expected, got {}", stringify!($type), value.type_name()))
                        }
                        None => self.arg_err(index, format!("{} expected, got no value", stringify!($type))),
                    }
                }

                /// Try to get the argument at the given index for the current function call, and
                /// convert it to the given type. Returns [`Option::None`] if the index is out of
                /// bounds (>= the number of passed arguments), or if the argument is
                /// [`LuaValue::NIL`]. Returns an error if the argument is not of the expected type.
                pub fn [<try_arg_as_ $type>](&self, index: usize) -> crate::Result<Option<$owned_result>> {
                    match self.try_arg(index) {
                        Some(v) if v.[<is_ $type>]() => Ok(Some(unsafe { v.[<$conversion _ $type>]() })),
                        None => Ok(None),
                        Some(v) if v.is_nil() => Ok(None),
                        Some(value) => self.arg_err(
                            index,
                            format!("{} expected, got {}", stringify!($type), value.type_name()),
                        ),
                    }
                }
            }
        }
    };
}

macro_rules! arg_primary_type {
    ($type:ident) => {
        impl_arg_type!($type, $type, as);
    };
    ($type:ident, $owned_result:ty) => {
        impl_arg_type!($type, $owned_result, as);
    };
    ($type:ident, $owned_result:ty, $conversion:ident) => {
        impl_arg_type!($type, $owned_result, $conversion);
    };
}

arg_primary_type!(bool);
arg_primary_type!(float, f64);
arg_primary_type!(number, LuaNumber);
arg_primary_type!(string, LuaString, into);
arg_primary_type!(object, Arc<RwLock<LuaObject>>, into);
arg_primary_type!(upvalue, Arc<RwLock<UpValue>>, into);

macro_rules! impl_arg_object_type {
    ($name:ident, $variant:ident, $owned_result:ty) => {
        paste::paste! {
            impl LuaState {
                /// Get the argument at the given index for the current function call, and convert
                /// it to the given type. Returns an error if the argument is not of the expected
                /// type.
                pub fn [<with_arg_as_ $name>]<F, R>(&self, index: usize, callback: F) -> crate::Result<R>
                where
                    F: FnOnce(&$owned_result) -> crate::Result<R>
                {
                    match self.try_arg(index) {
                        Some(v) if v.is_object() => match &*unsafe { v.as_object() }.read().unwrap() {
                            LuaObject::$variant(item) => callback(item),
                            _ => self.arg_err(index, format!("{} expected, got {}", stringify!($name), v.type_name())),
                        },
                        Some(value) => {
                            self.arg_err(index, format!("{} expected, got {}", stringify!($name), value.type_name()))
                        }
                        None => self.arg_err(index, format!("{} expected, got no value", stringify!($name))),
                    }
                }

                /// Get the argument at the given index for the current function call, and convert
                /// it to the given type. Returns an error if the argument is not of the expected
                /// type.
                pub fn [<with_arg_as_mut_ $name>]<F, R>(&self, index: usize, callback: F) -> crate::Result<R>
                where
                    F: FnOnce(&mut $owned_result) -> crate::Result<R>
                {
                    match self.try_arg(index) {
                        Some(v) if v.is_object() => match &mut *unsafe { v.as_object() }.write().unwrap() {
                            LuaObject::$variant(item) => callback(item),
                            _ => self.arg_err(index, format!("{} expected, got {}", stringify!($name), v.type_name())),
                        },
                        Some(value) => {
                            self.arg_err(index, format!("{} expected, got {}", stringify!($name), value.type_name()))
                        }
                        None => self.arg_err(index, format!("{} expected, got no value", stringify!($name))),
                    }
                }

                /// Get the argument at the given index for the current function call, and convert
                /// it to the given type. Returns an error if the argument is not of the expected
                /// type.
                pub fn [<try_with_arg_as_ $name>]<F, R>(&self, index: usize, callback: F) -> crate::Result<Option<R>>
                where
                    F: FnOnce(&$owned_result) -> crate::Result<R>
                {
                    match self.try_arg(index) {
                        Some(v) if v.is_object() => match &*unsafe { v.as_object() }.read().unwrap() {
                            LuaObject::$variant(item) => callback(item).map(Some),
                            _ => self.arg_err(index, format!("{} expected, got {}", stringify!($name), v.type_name())),
                        },
                        Some(v) if v.is_nil() => Ok(None),
                        None => Ok(None),
                        Some(value) => {
                            self.arg_err(index, format!("{} expected, got {}", stringify!($name), value.type_name()))
                        }
                    }
                }

                /// Get the argument at the given index for the current function call, and convert
                /// it to the given type. Returns an error if the argument is not of the expected
                /// type.
                pub fn [<try_with_arg_as_mut_ $name>]<F, R>(&self, index: usize, callback: F) -> crate::Result<Option<R>>
                where
                    F: FnOnce(&mut $owned_result) -> crate::Result<R>
                {
                    match self.try_arg(index) {
                        Some(v) if v.is_object() => match &mut *unsafe { v.as_object() }.write().unwrap() {
                            LuaObject::$variant(item) => callback(item).map(Some),
                            _ => self.arg_err(index, format!("{} expected, got {}", stringify!($name), v.type_name())),
                        },
                        Some(v) if v.is_nil() => Ok(None),
                        None => Ok(None),
                        Some(value) => {
                            self.arg_err(index, format!("{} expected, got {}", stringify!($name), value.type_name()))
                        }
                    }
                }
            }
        }
    };
}

impl_arg_object_type!(table, Table, LuaTable);
impl_arg_object_type!(closure, Closure, LuaClosure);
impl_arg_object_type!(user_data, UserData, UserData);
impl_arg_object_type!(thread, Thread, LuaState);
