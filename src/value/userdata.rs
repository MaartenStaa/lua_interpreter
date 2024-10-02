use std::{
    any::{type_name_of_val, Any},
    fmt::Display,
    sync::{Arc, RwLock},
};

use super::{LuaTable, LuaValue};

#[derive(Debug)]
pub enum UserData {
    Light(Arc<RwLock<Box<dyn Any + Sync + Send>>>),
    Full {
        data: Arc<RwLock<Box<dyn Any + Sync + Send>>>,
        type_name: String,
        metatable: Option<LuaValue>,
    },
}

impl UserData {
    pub fn new_light<T: Any + Sync + Send>(data: T) -> Self {
        UserData::Light(Arc::new(RwLock::new(Box::new(data))))
    }

    pub fn new_full<T: Any + Sync + Send>(data: T, metatable: LuaTable) -> Self {
        let type_name = type_name_of_val(&data);
        let (_, type_name) = type_name.rsplit_once("::").unwrap_or(("", type_name));
        let type_name = type_name.to_lowercase();

        UserData::Full {
            data: Arc::new(RwLock::new(Box::new(data))),
            type_name,
            metatable: Some(metatable.into()),
        }
    }
}

impl PartialEq for UserData {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (UserData::Light(a), UserData::Light(b)) => Arc::ptr_eq(a, b),
            (UserData::Full { data: a, .. }, UserData::Full { data: b, .. }) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Display for UserData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UserData::Light(_) => write!(f, "light userdata"),
            UserData::Full {
                data, type_name, ..
            } => {
                write!(f, "{} (0x{:x})", type_name, Arc::as_ptr(data) as usize)
            }
        }
    }
}
