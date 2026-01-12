macro_rules! assert_closure(
    (read, $value:expr, $closure:ident, $tt:stmt) => {
        match $value {
            LuaValue::Object(o) => match &*o.read().unwrap() {
                LuaObject::Closure($closure) => {
                    $tt
                },
                _ => unreachable!("assert_closure!() called on non-function object"),
            },
            _ => unreachable!("assert_closure!() called on non-function value"),
        }
    };
    (write, $value:expr, $closure:ident, $tt:stmt) => {
        match $value {
            LuaValue::Object(o) => match &mut*o.write().unwrap() {
                LuaObject::Closure($closure) => {
                    $tt
                },
                _ => unreachable!("assert_closure!() called on non-function object"),
            },
            _ => unreachable!("assert_closure!() called on non-function value"),
        }
    };
    ($value:expr, $closure:ident, $tt:stmt) => {
        assert_closure!(read, $value, $closure,  $tt)
    }
);

macro_rules! assert_string {
    ($value:expr) => {
        match $value {
            LuaValue::String(s) => s,
            _ => unreachable!("assert_string!() called on non-string value"),
        }
    };
}

macro_rules! assert_table_object(
    (read, $value:expr, $table:ident, $tt:stmt) => {
        match &*$value.read().unwrap() {
            LuaObject::Table($table) => {
                $tt
            },
            _ => unreachable!("assert_object!() called on non-table object"),
        }
    };
    (write, $value:expr, $table:ident, $tt:stmt) => {
        match &mut *$value.write().unwrap() {
            LuaObject::Table($table) => {
                $tt
            },
            _ => unreachable!("assert_object!() called on non-table object"),
        }
    };
    ($value:expr, $table:ident, $tt:stmt) => {
        assert_table!(read, $value, $table, $tt)
    }
);

macro_rules! assert_table(
    (read, $value:expr, $table:ident, $tt:stmt) => {
        match $value {
            LuaValue::Object(o) => $crate::macros::assert_table_object!(read, o, $table, $tt),
            _ => unreachable!("assert_object!() called on non-object value"),
        }
    };
    (write, $value:expr, $table:ident, $tt:stmt) => {
        match $value {
            LuaValue::Object(o) => $crate::macros::assert_table_object!(write, o, $table, $tt),
            _ => unreachable!("assert_object!() called on non-object value"),
        }
    };
    ($value:expr, $table:ident, $tt:stmt) => {
        assert_table!(read, $value, $table, $tt)
    }
);

macro_rules! require_closure {
    ($values:expr, $name:expr, $index:expr, $closure:ident, $tt:tt) => {
        match $values.get($index) {
            Some(LuaValue::Object(o)) => match &*o.read().unwrap() {
                LuaObject::Closure($closure) => $tt,
                _ => {
                    return Err($crate::error::lua_error!(
                        "bad argument #{} to '{}', expected function, got {}",
                        $index + 1,
                        $name,
                        o.read().unwrap().type_name()
                    ))
                }
            },
            Some(v) => {
                return Err($crate::error::lua_error!(
                    "bad argument #{} to '{}', expected function, got {}",
                    $index + 1,
                    $name,
                    v.type_name()
                ))
            }
            _ => {
                return Err($crate::error::lua_error!(
                    "bad argument #{} to '{}', expected function, got no value",
                    $index + 1,
                    $name
                ))
            }
        }
    };
    ($values:expr, $name:expr, $closure:ident, $tt:tt) => {
        require_closure!($values, $name, 0, $closure, $tt)
    };
}

macro_rules! get_number {
    ($values:expr, $name:expr, $index:expr) => {
        match $values.get($index) {
            Some(LuaValue::Number(n)) => Some(n),
            Some(LuaValue::Nil) | None => None,
            Some(v) => {
                return Err($crate::error::lua_error!(
                    "bad argument #{} to '{}', expected number, got {}",
                    $index + 1,
                    $name,
                    v.type_name()
                ))
            }
        }
    };
    ($values:expr, $name:expr) => {
        get_number!($values, $name, 0)
    };
}

macro_rules! require_number {
    ($values:expr, $name:expr, $index:expr) => {
        match $values.get($index) {
            Some(LuaValue::Number(n)) => n,
            Some(v) => {
                return Err($crate::error::lua_error!(
                    "bad argument #{} to '{}', expected number, got {}",
                    $index + 1,
                    $name,
                    v.type_name()
                ))
            }
            _ => {
                return Err($crate::error::lua_error!(
                    "bad argument #{} to '{}', expected number, got no value",
                    $index + 1,
                    $name
                ))
            }
        }
    };
    ($values:expr, $name:expr) => {
        require_number!($values, $name, 0)
    };
}

macro_rules! get_string {
    ($values:expr, $name:expr, $index:expr) => {
        match $values.get($index) {
            Some(LuaValue::String(s)) => Some(s),
            Some(v) => {
                return Err($crate::error::lua_error!(
                    "bad argument #{} to '{}', expected string, got {}",
                    $index + 1,
                    $name,
                    v.type_name()
                ))
            }
            _ => None,
        }
    };
    ($values:expr, $name:expr) => {
        get_string!($values, $name, 0)
    };
}

macro_rules! require_string {
    ($input:expr, $name:expr, $i:expr) => {
        match $input.get($i) {
            Some(LuaValue::String(s)) => s,
            Some(v) => {
                return Err($crate::error::lua_error!(
                    "bad argument #{n} to '{name}' (string expected, got {type_name})",
                    n = $i + 1,
                    name = $name,
                    type_name = v.type_name()
                ));
            }
            None => {
                return Err($crate::error::lua_error!(
                    "bad argument #{n} to '{name}' (value expected)",
                    n = $i + 1,
                    name = $name
                ));
            }
        }
    };
    ($input:expr, $name:expr) => {
        require_string!($input, $name, 0)
    };
}

macro_rules! require_table {
    (read, $values:expr, $name:expr, $index:expr, $table:ident, $tt:tt) => {
        match $values.get($index) {
            Some(LuaValue::Object(o)) => match &*o.read().unwrap() {
                LuaObject::Table($table) => $tt,
                _ => {
                    return Err($crate::error::lua_error!(
                        "bad argument #{n} to '{name}' (table expected, got {actual_type})",
                        n = $index + 1,
                        name = $name,
                        actual_type = o.read().unwrap().type_name()
                    ));
                }
            },
            Some(v) => {
                return Err($crate::error::lua_error!(
                    "bad argument #{n} to '{name}' (table expected, got {actual_type})",
                    n = $index + 1,
                    name = $name,
                    actual_type = v.type_name()
                ));
            }
            None => {
                return Err($crate::error::lua_error!(
                    "bad argument #{n} to '{name}' (table expected, got no value)",
                    n = $index + 1,
                    name = $name,
                ));
            }
        }
    };
    (write, $values:expr, $name:expr, $index:expr, $table:ident, $tt:tt) => {
        match $values.get($index) {
            Some(LuaValue::Object(o)) => match &mut *o.write().unwrap() {
                LuaObject::Table($table) => $tt,
                _ => {
                    return Err($crate::error::lua_error!(
                        "bad argument #{n} to '{name}' (table expected, got {actual_type})",
                        n = $index + 1,
                        name = $name,
                        actual_type = o.read().unwrap().type_name()
                    ));
                }
            },
            Some(v) => {
                return Err($crate::error::lua_error!(
                    "bad argument #{n} to '{name}' (table expected, got {actual_type})",
                    n = $index + 1,
                    name = $name,
                    actual_type = v.type_name()
                ));
            }
            None => {
                return Err($crate::error::lua_error!(
                    "bad argument #{n} to '{name}' (table expected, got no value)",
                    n = $index + 1,
                    name = $name,
                ));
            }
        }
    };
    ($values:expr, $name:expr, $index:expr, $userdata:ident, $tt:tt) => {
        require_table!(read, $values, $name, $index, $userdata, $tt)
    };
}

// Disable formatting here, rustfmt insists on removing the braces around `$tt`
// which is invalid.
#[rustfmt::skip]
macro_rules! require_userdata {
    (read, $values:expr, $name:expr, $index:expr, $userdata:ident, $tt:stmt) => {
        match $values.get($index) {
            Some(LuaValue::Object(o)) => match &*o.read().unwrap() {
                LuaObject::UserData($userdata) => {
                    $tt
                }
                _ => {
                    return Err($crate::error::lua_error!(
                        "bad argument #{n} to '{name}' ({type_name} expected, got {actual_type})",
                        n = $index + 1,
                        name = $name,
                        type_name = stringify!($type),
                        actual_type = o.read().unwrap().type_name()
                    ));
                }
            },
            Some(v) => {
                return Err($crate::error::lua_error!(
                    "bad argument #{n} to '{name}' ({type_name} expected, got {actual_type})",
                    n = $index + 1,
                    name = $name,
                    type_name = stringify!($type),
                    actual_type = v.type_name()
                ));
            }
            None => {
                return Err($crate::error::lua_error!(
                    "bad argument #{n} to '{name}' ({type_name} expected, got no value)",
                    n = $index + 1,
                    name = $name,
                    type_name = stringify!($type)
                ));
            }
        }
    };
    (write, $values:expr, $name:expr, $index:expr, $userdata:ident, $tt:stmt) => {
        match $values.get($index) {
            Some(LuaValue::Object(o)) => match &mut *o.write().unwrap() {
                LuaObject::UserData($userdata) => {
                    $tt
                },
                _ => {
                    return Err($crate::error::lua_error!(
                        "bad argument #{n} to '{name}' ({type_name} expected, got {actual_type})",
                        n = $index + 1,
                        name = $name,
                        type_name = stringify!($type),
                        actual_type = o.read().unwrap().type_name()
                    ));
                }
            },
            Some(v) => {
                return Err($crate::error::lua_error!(
                    "bad argument #{n} to '{name}' ({type_name} expected, got {actual_type})",
                    n = $index + 1,
                    name = $name,
                    type_name = stringify!($type),
                    actual_type = v.type_name()
                ));
            }
            None => {
                return Err($crate::error::lua_error!(
                    "bad argument #{n} to '{name}' ({type_name} expected, got no value)",
                    n = $index + 1,
                    name = $name,
                    type_name = stringify!($type)
                ));
            }
        }
    };
    ($values:expr, $name:expr, $index:expr, $userdata:ident, $tt:stmt) => {
        require_userdata!(read, $values, $name, $index, $userdata, $tt)
    };
}

macro_rules! require_userdata_type {
    (read, $userdata:ident, $name:expr, $index:expr, $type:ty, $value:ident, $metatable:ident, $tt:stmt) => {
        match $userdata {
            $crate::value::UserData::Full {
                data: $value,
                metatable: $metatable,
                type_name,
            } => {
                if let Some($value) = $value.read().unwrap().downcast_ref::<$type>() {
                    $tt
                } else {
                    return Err($crate::error::lua_error!(
                        "bad argument #{n} to '{name}' ({type_name} expected, got {actual_type})",
                        n = $index + 1,
                        name = $name,
                        type_name = stringify!($type),
                        actual_type = type_name
                    ));
                }
            }
            $crate::value::UserData::Light { .. } => {
                return Err($crate::error::lua_error!(
                    "bad argument #{n} to '{name}' ({type_name} expected, got light userdata)",
                    n = $index + 1,
                    name = $name,
                    type_name = stringify!($type)
                ));
            }
        }
    };
    (write, $userdata:ident, $name:expr, $index:expr, $type:ty, $value:ident, $metatable:ident, $tt:stmt) => {
        match $userdata {
            $crate::value::UserData::Full {
                data: $value,
                metatable: $metatable,
                type_name,
            } => {
                if let Some($value) = $value.write().unwrap().downcast_mut::<$type>() {
                    $tt
                } else {
                    return Err($crate::error::lua_error!(
                        "bad argument #{n} to '{name}' ({type_name} expected, got {actual_type})",
                        n = $index + 1,
                        name = $name,
                        type_name = stringify!($type),
                        actual_type = type_name
                    ));
                }
            }
            $crate::value::UserData::Light { .. } => {
                return Err($crate::error::lua_error!(
                    "bad argument #{n} to '{name}' ({type_name} expected, got light userdata)",
                    n = $index + 1,
                    name = $name,
                    type_name = stringify!($type)
                ));
            }
        }
    };
    ($userdata:ident, $name:expr, $index:expr, $type:ty, $value:ident, $metatable:ident, $tt:stmt) => {
        require_userdata_type!(
            read, $userdata, $name, $index, $type, $value, $metatable, $tt
        )
    };
}

pub(crate) use {
    assert_closure, assert_string, assert_table, assert_table_object, get_number, get_string,
    require_closure, require_number, require_string, require_table, require_userdata,
    require_userdata_type,
};
