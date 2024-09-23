use std::sync::{Arc, RwLock};

use super::LuaValue;

#[derive(Debug, Clone)]
pub struct LuaClosure {
    pub name: Option<String>,
    pub chunk: usize,
    pub ip: u16,
    pub upvalues: Vec<Option<Arc<RwLock<LuaValue>>>>,
}
