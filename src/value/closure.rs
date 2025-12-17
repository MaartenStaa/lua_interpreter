use std::sync::{Arc, RwLock};

use super::UpValue;

#[derive(Debug, Clone)]
pub struct LuaClosure {
    pub name: Option<Vec<u8>>,
    pub chunk: usize,
    pub ip: u16,
    pub max_registers: u8,
    pub upvalues: Vec<Option<Arc<RwLock<UpValue>>>>,
    pub num_params: u8,
    pub has_varargs: bool,
}
