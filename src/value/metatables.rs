use miette::miette;
use std::sync::LazyLock;

use crate::{macros::assert_table, vm::VM};

use super::{LuaObject, LuaTable, LuaValue, UserData};

pub(crate) static GLOBAL_BOOLEAN_METATABLE: LazyLock<LuaValue> =
    LazyLock::new(|| LuaTable::new().into());
pub(crate) static GLOBAL_NUMBER_METATABLE: LazyLock<LuaValue> =
    LazyLock::new(|| LuaTable::new().into());
pub(crate) static GLOBAL_STRING_METATABLE: LazyLock<LuaValue> =
    LazyLock::new(|| super::string::get_string_metatable().into());

pub(crate) static METATABLE_KEY: LazyLock<LuaValue> =
    LazyLock::new(|| LuaValue::String(b"__metatable".into()));

pub(crate) static ADD_KEY: LazyLock<LuaValue> = LazyLock::new(|| LuaValue::String(b"__add".into()));
pub(crate) static SUB_KEY: LazyLock<LuaValue> = LazyLock::new(|| LuaValue::String(b"__sub".into()));
pub(crate) static MUL_KEY: LazyLock<LuaValue> = LazyLock::new(|| LuaValue::String(b"__mul".into()));
pub(crate) static DIV_KEY: LazyLock<LuaValue> = LazyLock::new(|| LuaValue::String(b"__div".into()));
pub(crate) static MOD_KEY: LazyLock<LuaValue> = LazyLock::new(|| LuaValue::String(b"__mod".into()));
pub(crate) static POW_KEY: LazyLock<LuaValue> = LazyLock::new(|| LuaValue::String(b"__pow".into()));
pub(crate) static UNM_KEY: LazyLock<LuaValue> = LazyLock::new(|| LuaValue::String(b"__unm".into()));
pub(crate) static IDIV_KEY: LazyLock<LuaValue> =
    LazyLock::new(|| LuaValue::String(b"__idiv".into()));
pub(crate) static BAND_KEY: LazyLock<LuaValue> =
    LazyLock::new(|| LuaValue::String(b"__band".into()));
pub(crate) static BOR_KEY: LazyLock<LuaValue> = LazyLock::new(|| LuaValue::String(b"__bor".into()));
pub(crate) static BXOR_KEY: LazyLock<LuaValue> =
    LazyLock::new(|| LuaValue::String(b"__bxor".into()));
pub(crate) static BNOT_KEY: LazyLock<LuaValue> =
    LazyLock::new(|| LuaValue::String(b"__bnot".into()));
pub(crate) static SHL_KEY: LazyLock<LuaValue> = LazyLock::new(|| LuaValue::String(b"__shl".into()));
pub(crate) static SHR_KEY: LazyLock<LuaValue> = LazyLock::new(|| LuaValue::String(b"__shr".into()));
pub(crate) static CONCAT_KEY: LazyLock<LuaValue> =
    LazyLock::new(|| LuaValue::String(b"__concat".into()));
pub(crate) static LEN_KEY: LazyLock<LuaValue> = LazyLock::new(|| LuaValue::String(b"__len".into()));
pub(crate) static EQ_KEY: LazyLock<LuaValue> = LazyLock::new(|| LuaValue::String(b"__eq".into()));
pub(crate) static LT_KEY: LazyLock<LuaValue> = LazyLock::new(|| LuaValue::String(b"__lt".into()));
pub(crate) static LE_KEY: LazyLock<LuaValue> = LazyLock::new(|| LuaValue::String(b"__le".into()));
pub(crate) static INDEX_KEY: LazyLock<LuaValue> =
    LazyLock::new(|| LuaValue::String(b"__index".into()));
pub(crate) static NEWINDEX_KEY: LazyLock<LuaValue> =
    LazyLock::new(|| LuaValue::String(b"__newindex".into()));
pub(crate) static CALL_KEY: LazyLock<LuaValue> =
    LazyLock::new(|| LuaValue::String(b"__call".into()));
pub(crate) static CLOSE_KEY: LazyLock<LuaValue> =
    LazyLock::new(|| LuaValue::String(b"__close".into()));

impl LuaValue {
    pub fn get_metatable(&self) -> Option<LuaValue> {
        match self {
            LuaValue::Boolean(_) => Some(GLOBAL_BOOLEAN_METATABLE.clone()),
            LuaValue::Number(_) => Some(GLOBAL_NUMBER_METATABLE.clone()),
            LuaValue::String(_) => Some(GLOBAL_STRING_METATABLE.clone()),
            LuaValue::Object(o) => o.read().unwrap().get_metatable(),
            _ => None,
        }
    }

    pub fn get_metavalue(&self, key: &LuaValue) -> Option<LuaValue> {
        self.get_metatable()
            .and_then(|mt| assert_table!(mt, table, table.get(key).cloned()))
    }
}

impl LuaObject {
    pub fn get_metatable(&self) -> Option<LuaValue> {
        match self {
            LuaObject::Table(t) => t.get(&METATABLE_KEY).cloned(),
            LuaObject::UserData(UserData::Full { metatable, .. }) => metatable.clone(),
            _ => None,
        }
    }

    pub fn get_metavalue(&self, key: &LuaValue) -> Option<LuaValue> {
        self.get_metatable()
            .and_then(|mt| assert_table!(mt, table, table.get(key).cloned()))
    }
}

pub fn handle(
    vm: &mut VM,
    key: &LuaValue,
    op_name: &'static str,
    input: Vec<LuaValue>,
) -> miette::Result<Vec<LuaValue>> {
    assert!(!input.is_empty());

    for value in input.iter() {
        if let Some(LuaValue::Object(o)) = value.get_metavalue(key) {
            match &*o.read().unwrap() {
                LuaObject::Closure(c) => {
                    return vm.run_closure(c.clone(), input);
                }
                LuaObject::NativeFunction(_, f) => return f(vm, input),
                _ => {}
            }
        }
    }

    let mut message = format!("cannot {} a '{}'", op_name, &input[0].type_name());
    if let Some(b) = input.get(1) {
        message.push_str(&format!(" and a '{}'", b.type_name()));
    }

    Err(miette!("{}", message))
}
