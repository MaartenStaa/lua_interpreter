macro_rules! assert_closure(
    (read, $value:expr, $closure:ident, $tt:stmt) => {
        match $value {
            LuaValue::Object(o) => match &*o.read().unwrap() {
                LuaObject::Closure($closure) => {
                    $tt
                },
                _ => unreachable!("assert_object!() called on non-function object"),
            },
            _ => unreachable!("assert_object!() called on non-function value"),
        }
    };
    (write, $value:expr, $closure:ident, $tt:stmt) => {
        match $value {
            LuaValue::Object(o) => match &mut*o.write().unwrap() {
                LuaObject::Closure($closure) => {
                    $tt
                },
                _ => unreachable!("assert_object!() called on non-function object"),
            },
            _ => unreachable!("assert_object!() called on non-function value"),
        }
    };
    ($value:expr, $closure:ident, $tt:stmt) => {
        assert_closure!(read, $value, $closure,  $tt)
    }
);

macro_rules! assert_function_const(
    ($constant:expr) => {
        match $constant {
            LuaConst::Function(f) => f,
            _ => unreachable!("assert_function_const!() called on non-function constant"),
        }
    }
);

macro_rules! assert_table(
    ($value:expr, $table:ident, $tt:stmt) => {
        match $value {
            LuaValue::Object(o) => match &*o.read().unwrap() {
                LuaObject::Table($table) => {
                    $tt
                },
                _ => unreachable!("assert_object!() called on non-table object"),
            },
            _ => unreachable!("assert_object!() called on non-object value"),
        }
    }
);

macro_rules! get_number {
    ($values:expr, $name:expr, $index:expr) => {
        match $values.get($index) {
            Some(LuaValue::Number(n)) => Some(n),
            Some(v) => {
                return Err(miette!(
                    "bad argument #{} to '{}', expected number, got {}",
                    $index + 1,
                    $name,
                    v.type_name()
                ))
            }
            _ => None,
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
                return Err(::miette::miette!(
                    "bad argument #{} to '{}', expected number, got {}",
                    $index + 1,
                    $name,
                    v.type_name()
                ))
            }
            _ => {
                return Err(::miette::miette!(
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
                return Err(::miette::miette!(
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
                return Err(::miette::miette!(
                    "bad argument #{n} to '{name}' (string expected, got {type_name})",
                    n = $i + 1,
                    name = $name,
                    type_name = v.type_name()
                ));
            }
            None => {
                return Err(::miette::miette!(
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

pub(crate) use {
    assert_closure, assert_function_const, assert_table, get_number, get_string, require_number,
    require_string,
};
