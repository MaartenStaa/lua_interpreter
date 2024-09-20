macro_rules! assert_closure(
    (read, $value:expr, $closure:ident, $tt:tt) => {
        match $value {
            LuaValue::Object(o) => match $ref*o.read().unwrap() {
                LuaObject::Closure($closure) => {
                    $tt
                },
                _ => unreachable!("assert_object!() called on non-function object"),
            },
            _ => unreachable!("assert_object!() called on non-function value"),
        }
    };
    (write, $value:expr, $closure:ident, $tt:tt) => {
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
    ($value:expr, $closure:ident, $tt:tt) => {
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
    ($value:expr, $table:ident, $tt:tt) => {
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

pub(crate) use {assert_closure, assert_function_const, assert_table};
