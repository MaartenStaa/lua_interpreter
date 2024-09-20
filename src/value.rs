use miette::miette;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
    sync::{Arc, RwLock},
};

use crate::vm::VM;

#[derive(Debug, Clone, PartialEq)]
pub struct LuaFunctionDefinition {
    pub name: Option<String>,
    pub chunk: usize,
    pub ip: u16,
    pub upvalues: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LuaConst {
    Marker,
    Nil,
    Boolean(bool),
    Number(LuaNumber),
    String(Vec<u8>),
    Function(LuaFunctionDefinition),
}

#[derive(Clone)]
pub enum LuaValue {
    Marker,
    Nil,
    Boolean(bool),
    Number(LuaNumber),
    String(Vec<u8>),
    Object(Arc<RwLock<LuaObject>>),
    UpValue(Arc<RwLock<LuaValue>>),
}

impl Debug for LuaValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaValue::Marker => write!(f, "<marker>"),
            LuaValue::Nil => write!(f, "nil"),
            LuaValue::Boolean(b) => write!(f, "{}", b),
            LuaValue::Number(n) => write!(f, "{}", n),
            LuaValue::String(s) => write!(f, "{:?}", String::from_utf8_lossy(s)),
            LuaValue::Object(o) => write!(f, "{:?}", o),
            LuaValue::UpValue(u) => {
                write!(
                    f,
                    "upvalue<0x{:x}>: {:?}",
                    u as *const _ as usize,
                    u.read().unwrap()
                )
            }
        }
    }
}

impl PartialEq for LuaValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LuaValue::Marker, LuaValue::Marker) => true,
            (LuaValue::Nil, LuaValue::Nil) => true,
            (LuaValue::Boolean(a), LuaValue::Boolean(b)) => a == b,
            (LuaValue::Number(a), LuaValue::Number(b)) => a == b,
            (LuaValue::String(a), LuaValue::String(b)) => a == b,
            (LuaValue::Object(a), LuaValue::Object(b)) => Arc::ptr_eq(a, b),
            (LuaValue::UpValue(a), LuaValue::UpValue(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Hash for LuaValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            LuaValue::Marker => 0.hash(state),
            LuaValue::Nil => 0.hash(state),
            LuaValue::Boolean(b) => b.hash(state),
            LuaValue::Number(n) => n.hash(state),
            LuaValue::String(s) => s.hash(state),
            LuaValue::Object(o) => Arc::as_ptr(o).hash(state),
            LuaValue::UpValue(u) => Arc::as_ptr(u).hash(state),
        }
    }
}

impl Eq for LuaValue {}

#[derive(Debug, Clone)]
pub struct LuaClosure {
    pub name: Option<String>,
    pub chunk: usize,
    pub ip: u16,
    pub upvalues: Vec<Option<Arc<RwLock<LuaValue>>>>,
}

#[derive(Debug, Clone)]
pub enum LuaObject {
    Table(LuaTable),
    Closure(LuaClosure),
    NativeFunction(fn(&mut VM, Vec<LuaValue>) -> miette::Result<Vec<LuaValue>>),

    // TODO: Implement these
    Thread,
    UserData,
}

impl PartialEq for LuaObject {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LuaObject::Table(a), LuaObject::Table(b)) => a == b,
            (LuaObject::Closure(LuaClosure { .. }), LuaObject::Closure(LuaClosure { .. })) => false,
            (LuaObject::NativeFunction(a), LuaObject::NativeFunction(b)) => a == b,

            // TODO: Implement these
            (LuaObject::Thread, LuaObject::Thread) => true,
            (LuaObject::UserData, LuaObject::UserData) => true,

            _ => false,
        }
    }
}

impl Hash for LuaObject {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Self).hash(state);
    }
}

impl From<bool> for LuaValue {
    fn from(b: bool) -> Self {
        LuaValue::Boolean(b)
    }
}

impl From<&str> for LuaValue {
    fn from(s: &str) -> Self {
        LuaValue::String(s.as_bytes().to_vec())
    }
}

impl From<LuaTable> for LuaValue {
    fn from(table: LuaTable) -> Self {
        LuaValue::Object(Arc::new(RwLock::new(LuaObject::Table(table))))
    }
}

impl From<LuaConst> for LuaValue {
    fn from(constant: LuaConst) -> Self {
        match constant {
            LuaConst::Marker => LuaValue::Marker,
            LuaConst::Nil => LuaValue::Nil,
            LuaConst::Boolean(b) => LuaValue::Boolean(b),
            LuaConst::Number(n) => LuaValue::Number(n),
            LuaConst::String(s) => LuaValue::String(s),
            LuaConst::Function(LuaFunctionDefinition {
                name,
                chunk,
                ip,
                upvalues,
            }) => LuaValue::Object(Arc::new(RwLock::new(LuaObject::Closure(LuaClosure {
                name,
                chunk,
                ip,
                upvalues: vec![None; upvalues],
            })))),
        }
    }
}

impl From<LuaObject> for LuaValue {
    fn from(object: LuaObject) -> Self {
        LuaValue::Object(Arc::new(RwLock::new(object)))
    }
}

impl LuaValue {
    pub fn type_name(&self) -> &'static str {
        match self {
            LuaValue::Marker => "<marker>",
            LuaValue::Nil => "nil",
            LuaValue::Boolean(_) => "boolean",
            LuaValue::Number(_) => "number",
            LuaValue::String(_) => "string",
            LuaValue::Object(o) => o.read().unwrap().type_name(),
            LuaValue::UpValue(u) => u.read().unwrap().type_name(),
        }
    }
}

impl LuaObject {
    pub fn type_name(&self) -> &'static str {
        match self {
            LuaObject::Table(_) => "table",
            LuaObject::Closure(LuaClosure { .. }) => "function",
            LuaObject::NativeFunction(_) => "function",
            LuaObject::UserData => "userdata",
            LuaObject::Thread => "thread",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LuaNumber {
    Integer(i64),
    Float(f64),
}

impl Hash for LuaNumber {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            LuaNumber::Integer(i) => i.hash(state),
            LuaNumber::Float(f) => {
                if f.is_nan() {
                    "#!LuaFloatNaN!#".hash(state);
                } else {
                    f.to_bits().hash(state);
                }
            }
        }
    }
}

impl LuaNumber {
    pub fn integer_repr(&self) -> miette::Result<i64> {
        match self {
            LuaNumber::Integer(i) => Ok(*i),
            LuaNumber::Float(f) => {
                if f.is_nan() {
                    return Err(miette!("NaN has no integer representation",));
                }

                if f.is_infinite() {
                    return Err(miette!("infinity has no integer representation",));
                }

                if f.fract() != 0.0 {
                    return Err(miette!("float has no integer representation",));
                }

                Ok(*f as i64)
            }
        }
    }
}

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

    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
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

    pub fn get(&self, key: &LuaValue) -> Option<&LuaValue> {
        self.fields.get(key)
    }

    pub fn mark_sequence_dangerous(&mut self, last_number_key: i64) {
        self.is_sequence = true;
        self.last_number_key = last_number_key;
    }
}

impl Default for LuaTable {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for LuaValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaValue::Marker => write!(f, "<marker>"),
            LuaValue::Nil => write!(f, "nil"),
            LuaValue::Boolean(b) => write!(f, "{}", b),
            LuaValue::Number(n) => write!(f, "{}", n),
            LuaValue::String(s) => write!(f, "{}", String::from_utf8_lossy(s)),
            LuaValue::Object(o) => write!(f, "{}", o.read().unwrap()),
            LuaValue::UpValue(u) => write!(f, "{}", u.read().unwrap()),
        }
    }
}

impl Display for LuaObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaObject::Table(_) => write!(f, "table: 0x{:x}", self as *const _ as usize),
            LuaObject::Closure(LuaClosure { name, .. }) => {
                write!(f, "function: 0x{:x}", self as *const _ as usize)?;
                if let Some(name) = name {
                    write!(f, " ({name})")
                } else {
                    Ok(())
                }
            }
            LuaObject::NativeFunction(func) => {
                write!(f, "function: 0x{:x}", func as *const _ as usize)
            }
            _ => todo!("formatting a {self:?}"),
        }
    }
}

impl Display for LuaNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaNumber::Integer(i) => write!(f, "{}", i),
            LuaNumber::Float(fl) => {
                // Ensure we always print the decimal point
                if fl.fract() == 0.0 {
                    write!(f, "{:.1}", fl)
                } else {
                    write!(f, "{}", fl)
                }
            }
        }
    }
}
