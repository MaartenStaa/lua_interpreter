use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use super::{number::LuaNumber, LuaValue};

#[derive(Debug, Clone, PartialEq)]
pub struct LuaTable {
    fields: HashMap<LuaValue, LuaValue>,
    metatable: Option<LuaValue>,
    last_number_key: i64,
    is_sequence: bool,
}

impl LuaTable {
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
            metatable: None,
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

    pub fn get(&self, key: &LuaValue) -> Option<&LuaValue> {
        if let LuaValue::Number(float @ LuaNumber::Float(_)) = key {
            if let Ok(i) = float.integer_repr() {
                return self.fields.get(&LuaValue::Number(LuaNumber::Integer(i)));
            }
        }

        self.fields.get(key)
    }

    pub fn is_empty(&self) -> bool {
        self.fields.is_empty() || self.fields.values().all(|v| v == &LuaValue::Nil)
    }

    pub fn insert(&mut self, mut key: LuaValue, value: LuaValue) {
        if let LuaValue::Number(float @ LuaNumber::Float(_)) = &key {
            if let Ok(i) = float.integer_repr() {
                key = LuaValue::Number(LuaNumber::Integer(i));
            }
        }

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

    pub fn remove(&mut self, key: &LuaValue) {
        if let LuaValue::Number(float @ LuaNumber::Float(_)) = key {
            if let Ok(i) = float.integer_repr() {
                if i == self.last_number_key {
                    self.last_number_key -= 1;
                }

                self.remove(&LuaValue::Number(LuaNumber::Integer(i)));
                return;
            }
        }

        if let LuaValue::Number(LuaNumber::Integer(i)) = key {
            if *i == self.last_number_key {
                self.last_number_key -= 1;
            }
        }

        self.fields.remove(key);
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

    pub fn set_metatable(&mut self, metatable: Option<LuaValue>) {
        self.metatable = metatable;
    }

    pub fn metatable(&self) -> Option<&LuaValue> {
        self.metatable.as_ref()
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
