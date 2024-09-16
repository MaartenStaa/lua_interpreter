use miette::miette;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
    sync::{Arc, RwLock},
};

#[derive(Debug, Clone, PartialEq)]
pub enum LuaConst {
    Marker,
    Nil,
    Boolean(bool),
    Number(LuaNumber),
    String(Vec<u8>),
    Function(Option<String>, u16),
}

#[derive(Clone)]
pub enum LuaValue {
    Marker,
    Nil,
    Boolean(bool),
    Number(LuaNumber),
    String(Vec<u8>),
    Object(Arc<RwLock<LuaObject>>),
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
            LuaValue::Object(o) => o.read().unwrap().hash(state),
        }
    }
}

impl Eq for LuaValue {}

#[derive(Debug, Clone, PartialEq)]
pub enum LuaObject {
    Table(LuaTable),
    Function(Option<String>, u16),
    NativeFunction(fn(Vec<LuaValue>) -> miette::Result<LuaValue>),

    // TODO: Implement these
    Thread,
    UserData,
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

impl From<LuaConst> for LuaValue {
    fn from(constant: LuaConst) -> Self {
        match constant {
            LuaConst::Marker => LuaValue::Marker,
            LuaConst::Nil => LuaValue::Nil,
            LuaConst::Boolean(b) => LuaValue::Boolean(b),
            LuaConst::Number(n) => LuaValue::Number(n),
            LuaConst::String(s) => LuaValue::String(s),
            LuaConst::Function(name, f) => {
                LuaValue::Object(Arc::new(RwLock::new(LuaObject::Function(name, f))))
            }
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
        }
    }
}

impl LuaObject {
    pub fn type_name(&self) -> &'static str {
        match self {
            LuaObject::Table(_) => "table",
            LuaObject::Function(_, _) => "function",
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
        todo!("implement len for LuaTable")
    }

    pub fn insert(&mut self, key: LuaValue, value: LuaValue) {
        match &key {
            LuaValue::Number(LuaNumber::Integer(i)) if *i == self.last_number_key + 1 => {
                self.last_number_key = *i;
            }
            _ => self.is_sequence = false,
        }

        self.fields.insert(key, value);
    }

    pub fn get(&self, key: &LuaValue) -> Option<&LuaValue> {
        self.fields.get(key)
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
        }
    }
}

impl Display for LuaObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaObject::Table(_) => write!(f, "table: 0x{:x}", self as *const _ as usize),
            LuaObject::Function(name, a) => {
                if let Some(name) = name {
                    write!(f, "function<{name}:{a:04}>")
                } else {
                    write!(f, "function<{a:04}>")
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
