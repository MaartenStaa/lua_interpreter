use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

use crate::{value::UpValue, vm::ConstIndex};

#[derive(Debug, Clone)]
pub struct LuaClosure {
    pub chunk: ConstIndex,
    pub upvalues: HashMap<usize, Arc<RwLock<UpValue>>>,
}

impl LuaClosure {
    pub(crate) fn new(chunk_index: ConstIndex) -> Self {
        Self {
            chunk: chunk_index,
            upvalues: HashMap::new(),
        }
    }
}
