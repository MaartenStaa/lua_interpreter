use std::{
    collections::HashMap,
    fmt::Debug,
    ops::{Deref, DerefMut},
};

use crate::value::IntegerRepr;

use super::{LuaObject, LuaValue};

#[derive(Clone, PartialEq)]
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
            let nil = LuaValue::NIL;
            let mut border = 0;
            loop {
                let key_current = LuaValue::from_integer(border);
                let key_next = LuaValue::from_integer(border + 1);
                let current = self.fields.get(&key_current).unwrap_or(&nil);
                let next = self.fields.get(&key_next).unwrap_or(&nil);
                if (border == 0 || !current.is_nil()) && (next.is_nil() || border == i64::MAX) {
                    break;
                }

                border += 1;
            }

            border as usize
        }
    }

    pub fn get(&self, key: &LuaValue) -> Option<&LuaValue> {
        if key.is_float()
            && let Ok(i) = unsafe { key.as_float() }.integer_repr()
        {
            return self.fields.get(&LuaValue::from_integer(i));
        }

        self.fields.get(key)
    }

    pub fn is_empty(&self) -> bool {
        self.fields.is_empty() || self.fields.values().all(LuaValue::is_nil)
    }

    pub fn insert(&mut self, mut key: LuaValue, value: LuaValue) {
        if key.is_float()
            && let Ok(i) = unsafe { key.as_float() }.integer_repr()
        {
            key = LuaValue::from_integer(i);
        }

        if key.is_integer() {
            let i = unsafe { key.as_integer() };
            if i == self.last_number_key + 1 {
                self.last_number_key = i;
            } else if i > self.last_number_key || i < 1 {
                self.is_sequence = false;
            }
        } else {
            self.is_sequence = false
        }

        if value.is_nil() {
            self.is_sequence = false;
            self.fields.remove(&key);
            return;
        }

        self.fields.insert(key, value);
    }

    pub fn remove(&mut self, key: &LuaValue) {
        if key.is_float()
            && let Ok(i) = unsafe { key.as_float() }.integer_repr()
        {
            if i == self.last_number_key {
                self.last_number_key -= 1;
            }

            self.remove(&LuaValue::from_integer(i));
            return;
        }

        if key.is_integer() && unsafe { key.as_integer() } == self.last_number_key {
            self.last_number_key -= 1;
        }

        self.fields.remove(key);
    }

    pub fn append(&mut self, value: LuaValue) {
        self.insert(LuaValue::from_integer(self.last_number_key + 1), value);
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

    pub fn size_bytes(&self) -> u64 {
        size_of::<Self>() as u64
            + self
                .fields
                .iter()
                .map(|(k, v)| k.size_bytes() + v.size_bytes())
                .sum::<u64>()
            + self
                .metatable
                .as_ref()
                .map(|mt| mt.size_bytes())
                .unwrap_or_default()
    }
}

impl Debug for LuaTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(metatable) = &self.metatable
            && metatable.is_object()
            && let o = unsafe { metatable.as_object() }
            && let LuaObject::Table(t) = &*o.read().unwrap()
            && let Some(debug_repr) = t.get(&"__debug_repr".into())
        {
            return f.debug_tuple("LuaTable").field(debug_repr).finish();
        }

        if self.metatable.is_none() {
            if self.is_sequence {
                f.debug_list()
                    .entries(
                        (1..=self.last_number_key)
                            .filter_map(|i| self.fields.get(&LuaValue::from_integer(i))),
                    )
                    .finish()
            } else {
                f.debug_map().entries(self.fields.iter()).finish()
            }
        } else {
            f.debug_struct("LuaTable")
                .field("fields", &self.fields)
                .field("metatable", &self.metatable)
                .field("last_number_key", &self.last_number_key)
                .field("is_sequence", &self.is_sequence)
                .finish()
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_table_len() {
        let mut table = LuaTable::new();
        assert_eq!(table.len(), 0);

        table.insert(LuaValue::from_integer(1), LuaValue::from_integer(1));
        assert_eq!(table.len(), 1);

        table.insert(LuaValue::from_integer(2), LuaValue::from_integer(2));
        assert_eq!(table.len(), 2);

        table.insert(LuaValue::from_integer(4), LuaValue::from_integer(4));
        assert_eq!(table.len(), 2);

        table.insert(LuaValue::from_integer(3), LuaValue::from_integer(3));
        assert_eq!(table.len(), 4);

        table.remove(&LuaValue::from_integer(4));
        assert_eq!(table.len(), 3);
    }

    #[test]
    fn test_table_insert_remove() {
        let mut table = LuaTable::new();
        table.insert(LuaValue::from_integer(1), LuaValue::from_integer(1));
        table.insert(LuaValue::from_integer(2), LuaValue::from_integer(2));
        table.insert(LuaValue::from_integer(3), LuaValue::from_integer(3));

        assert_eq!(
            table.get(&LuaValue::from_integer(1)),
            Some(&LuaValue::from_integer(1))
        );
        assert_eq!(
            table.get(&LuaValue::from_integer(2)),
            Some(&LuaValue::from_integer(2))
        );
        assert_eq!(
            table.get(&LuaValue::from_integer(3)),
            Some(&LuaValue::from_integer(3))
        );

        table.remove(&LuaValue::from_integer(2));
        assert_eq!(table.get(&LuaValue::from_integer(2)), None);
    }

    #[test]
    fn test_string_keys() {
        let mut table = LuaTable::new();
        table.insert(LuaValue::from_string(b"foo"), LuaValue::from_integer(42));
        assert_eq!(
            table.get(&LuaValue::from_string(b"foo")),
            Some(&LuaValue::from_integer(42))
        );

        table.remove(&LuaValue::from_string(b"foo"));
        assert_eq!(table.get(&LuaValue::from_string(b"foo")), None);
    }
}
