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

pub(crate) use assert_table;
