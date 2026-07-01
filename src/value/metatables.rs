use std::sync::LazyLock;

use crate::{ExecutionResult, LuaState, error::lua_error, macros::assert_table};

use super::{LuaObject, LuaTable, LuaValue, UserData};

pub(crate) static GLOBAL_BOOLEAN_METATABLE: LazyLock<LuaValue> =
    LazyLock::new(|| LuaTable::new().into());
pub(crate) static GLOBAL_NUMBER_METATABLE: LazyLock<LuaValue> =
    LazyLock::new(|| LuaTable::new().into());
pub(crate) static GLOBAL_STRING_METATABLE: LazyLock<LuaValue> =
    LazyLock::new(|| super::string::get_string_metatable().into());

pub(crate) static METATABLE_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__metatable".into());
pub(crate) static ADD_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__add".into());
pub(crate) static SUB_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__sub".into());
pub(crate) static MUL_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__mul".into());
pub(crate) static DIV_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__div".into());
pub(crate) static MOD_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__mod".into());
pub(crate) static POW_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__pow".into());
pub(crate) static UNM_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__unm".into());
pub(crate) static IDIV_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__idiv".into());
pub(crate) static BAND_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__band".into());
pub(crate) static BOR_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__bor".into());
pub(crate) static BXOR_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__bxor".into());
pub(crate) static BNOT_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__bnot".into());
pub(crate) static SHL_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__shl".into());
pub(crate) static SHR_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__shr".into());
pub(crate) static CONCAT_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__concat".into());
pub(crate) static LEN_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__len".into());
pub(crate) static EQ_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__eq".into());
pub(crate) static LT_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__lt".into());
pub(crate) static LE_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__le".into());
pub(crate) static INDEX_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__index".into());
pub(crate) static NEWINDEX_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__newindex".into());
pub(crate) static CALL_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__call".into());
pub(crate) static CLOSE_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__close".into());
pub(crate) static PAIRS_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__pairs".into());
pub(crate) static NAME_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__name".into());
pub(crate) static TOSTRING_KEY: LazyLock<LuaValue> = LazyLock::new(|| "__tostring".into());

impl LuaValue {
    pub fn get_metatable(&self) -> Option<LuaValue> {
        if self.is_bool() {
            Some(GLOBAL_BOOLEAN_METATABLE.clone())
        } else if self.is_number() {
            Some(GLOBAL_NUMBER_METATABLE.clone())
        } else if self.is_string() {
            Some(GLOBAL_STRING_METATABLE.clone())
        } else if self.is_object() {
            unsafe { self.as_object() }.read().unwrap().get_metatable()
        } else {
            None
        }
    }

    pub fn set_metatable(&mut self, metatable: Option<LuaValue>) -> crate::Result<()> {
        match self {
            v if v.is_object() => unsafe { v.as_object() }
                .write()
                .unwrap()
                .set_metatable(metatable),
            v if v.is_upvalue() => unsafe { v.as_upvalue() }
                .write()
                .unwrap()
                .set_metatable(metatable),
            _ => Err(lua_error!("cannot set metatable on a non-object")),
        }
    }

    #[must_use]
    pub fn get_metavalue(&self, key: &LuaValue) -> Option<LuaValue> {
        self.get_metatable()
            .and_then(|mt| assert_table!(mt, table, table.get(key).cloned()))
    }
}

impl LuaObject {
    #[must_use]
    pub fn get_metatable(&self) -> Option<LuaValue> {
        match self {
            LuaObject::Table(t) => t.metatable().cloned(),
            LuaObject::UserData(UserData::Full { metatable, .. }) => metatable.clone(),
            _ => None,
        }
    }

    pub fn set_metatable(&mut self, metatable: Option<LuaValue>) -> crate::Result<()> {
        match self {
            LuaObject::Table(t) => {
                t.set_metatable(metatable);
                Ok(())
            }
            LuaObject::UserData(UserData::Full { metatable: mt, .. }) => {
                *mt = metatable;
                Ok(())
            }
            _ => Err(lua_error!("cannot set metatable on a non-table")),
        }
    }

    #[must_use]
    pub fn get_metavalue(&self, key: &LuaValue) -> Option<LuaValue> {
        self.get_metatable()
            .and_then(|mt| assert_table!(mt, table, table.get(key).cloned()))
    }
}

pub fn handle(
    state: &mut LuaState,
    key: &LuaValue,
    input: Vec<LuaValue>,
) -> Option<crate::Result<ExecutionResult>> {
    assert!(!input.is_empty());

    for value in &input {
        if let Some(mv) = value.get_metavalue(key)
            && mv.is_object()
        {
            match &*unsafe { mv.as_object() }.read().unwrap() {
                LuaObject::Closure(c) => {
                    let num_args = input.len();
                    let args_r = state.extend_stack(mv.clone(), input);
                    let result = match state.run_closure(c, args_r, num_args) {
                        Ok(result) => result,
                        Err(e) => return Some(Err(e)),
                    };

                    return Some(Ok(result));
                }
                LuaObject::NativeFunction(name, f) => {
                    let num_args = input.len();
                    let args_r = state.extend_stack(mv.clone(), input);
                    let num_results =
                        match state.run_native_function(name, *f, args_r, num_args) {
                            Ok(value) => value,
                            Err(e) => return Some(Err(e)),
                        };

                    let results = (0..num_results)
                        .map(|i| state.arg(args_r as usize + i))
                        .collect();
                    return Some(Ok(ExecutionResult::Completed(results)));
                }
                _ => {}
            }
        }
    }

    None
}
