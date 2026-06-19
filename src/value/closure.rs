use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

use crate::{value::UpValue, vm::ChunkIndex};

#[derive(Debug, Clone)]
pub struct LuaClosure {
    pub chunk: ChunkIndex,
    pub upvalues: HashMap<usize, Arc<RwLock<UpValue>>>,
}

impl LuaClosure {
    pub(crate) fn new(chunk_index: ChunkIndex) -> Self {
        Self {
            chunk: chunk_index,
            upvalues: HashMap::new(),
        }
    }
}
