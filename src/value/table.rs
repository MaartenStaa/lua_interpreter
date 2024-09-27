use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use super::{number::LuaNumber, LuaValue};

#[derive(Debug, Clone, PartialEq)]
pub struct LuaTable {
    fields: HashMap<LuaValue, LuaValue>,
    last_number_key: i64,
    is_sequence: bool,
}

impl LuaTable {
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
            last_number_key: 0,
            is_sequence: true,
        }
    }

    pub fn len(&self) -> usize {
        if self.is_sequence {
            self.last_number_key as usize
        } else {
            // The length operator applied on a table returns a border in that table. A border in a
            // table t is any non-negative integer that satisfies the following condition:
            //
            // (border == 0 or t[border] ~= nil) and
            // (t[border + 1] == nil or border == math.maxinteger)

            // TODO: There must be a more efficient way to do this
            let mut border = 0;
            loop {
                let current = self
                    .fields
                    .get(&LuaValue::Number(LuaNumber::Integer(border)))
                    .unwrap_or(&LuaValue::Nil);
                let next = self
                    .fields
                    .get(&LuaValue::Number(LuaNumber::Integer(border + 1)))
                    .unwrap_or(&LuaValue::Nil);
                if (border == 0 || current != &LuaValue::Nil)
                    && (next == &LuaValue::Nil || border == i64::MAX)
                {
                    break;
                }

                border += 1;
            }

            border as usize
        }
    }

    pub fn insert(&mut self, key: LuaValue, value: LuaValue) {
        match &key {
            LuaValue::Number(LuaNumber::Integer(i)) => {
                if *i == self.last_number_key + 1 {
                    self.last_number_key = *i;
                } else if *i > self.last_number_key || *i < 1 {
                    self.is_sequence = false;
                }
            }
            _ => self.is_sequence = false,
        }

        if value == LuaValue::Nil {
            self.is_sequence = false;
        }

        self.fields.insert(key, value);
    }

    pub fn append(&mut self, value: LuaValue) {
        self.insert(
            LuaValue::Number(LuaNumber::Integer(self.last_number_key + 1)),
            value,
        );
    }

    pub fn mark_sequence_dangerous(&mut self, last_number_key: i64) {
        self.is_sequence = true;
        self.last_number_key = last_number_key;
    }
}

impl Deref for LuaTable {
    type Target = HashMap<LuaValue, LuaValue>;

    fn deref(&self) -> &Self::Target {
        &self.fields
    }
}

impl DerefMut for LuaTable {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.fields
    }
}

impl Default for LuaTable {
    fn default() -> Self {
        Self::new()
    }
}
