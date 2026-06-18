use std::{
    fmt::{Debug, Display},
    iter::Copied,
    ops::{Deref, DerefMut},
};

use crate::{error::lua_error, stdlib, vm::VM};

use super::{LuaNumber, LuaObject, LuaTable, LuaValue, metatables};

#[derive(Clone, PartialEq, Eq, Hash, Default, PartialOrd)]
pub struct LuaString(Vec<u8>);

impl LuaString {
    pub const fn new() -> Self {
        LuaString(Vec::new())
    }
}

impl Debug for LuaString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match String::from_utf8(self.0.clone()) {
            Ok(s) => write!(f, "{:?}", s),
            Err(_) => write!(f, "{:?} (lossy)", String::from_utf8_lossy(&self.0)),
        }
    }
}

impl Display for LuaString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match String::from_utf8(self.0.clone()) {
            Ok(s) => write!(f, "{}", s),
            Err(_) => write!(f, "{}", String::from_utf8_lossy(&self.0)),
        }
    }
}

impl From<Vec<u8>> for LuaString {
    fn from(value: Vec<u8>) -> Self {
        LuaString(value)
    }
}

impl From<LuaString> for Vec<u8> {
    fn from(value: LuaString) -> Self {
        value.0
    }
}

impl From<&[u8]> for LuaString {
    fn from(value: &[u8]) -> Self {
        LuaString(value.to_vec())
    }
}

impl<const T: usize> From<&[u8; T]> for LuaString {
    fn from(value: &[u8; T]) -> Self {
        LuaString(value.to_vec())
    }
}

impl Deref for LuaString {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for LuaString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl PartialEq<&[u8]> for LuaString {
    fn eq(&self, other: &&[u8]) -> bool {
        &self.0 == other
    }
}

impl<const T: usize> PartialEq<[u8; T]> for LuaString {
    fn eq(&self, other: &[u8; T]) -> bool {
        self.0 == other
    }
}

impl<'a> IntoIterator for &'a LuaString {
    type Item = u8;
    type IntoIter = Copied<std::slice::Iter<'a, u8>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter().copied()
    }
}

impl IntoIterator for LuaString {
    type Item = u8;
    type IntoIter = std::vec::IntoIter<u8>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

pub(crate) fn get_string_metatable() -> LuaTable {
    let mut metatable = LuaTable::new();

    metatable.insert(metatables::INDEX_KEY.clone(), stdlib::string());

    metatable.insert(
        metatables::ADD_KEY.clone(),
        LuaObject::NativeFunction("add", add).into(),
    );
    metatable.insert(
        metatables::SUB_KEY.clone(),
        LuaObject::NativeFunction("sub", sub).into(),
    );
    metatable.insert(
        metatables::MUL_KEY.clone(),
        LuaObject::NativeFunction("mul", mul).into(),
    );
    metatable.insert(
        metatables::DIV_KEY.clone(),
        LuaObject::NativeFunction("div", div).into(),
    );
    metatable.insert(
        metatables::MOD_KEY.clone(),
        LuaObject::NativeFunction("mod", mod_).into(),
    );
    metatable.insert(
        metatables::POW_KEY.clone(),
        LuaObject::NativeFunction("pow", pow).into(),
    );
    metatable.insert(
        metatables::UNM_KEY.clone(),
        LuaObject::NativeFunction("unm", unm).into(),
    );
    metatable.insert(
        metatables::IDIV_KEY.clone(),
        LuaObject::NativeFunction("idiv", idiv).into(),
    );
    metatable.insert(
        metatables::BAND_KEY.clone(),
        LuaObject::NativeFunction("band", band).into(),
    );
    metatable.insert(
        metatables::BOR_KEY.clone(),
        LuaObject::NativeFunction("bor", bor).into(),
    );
    metatable.insert(
        metatables::BXOR_KEY.clone(),
        LuaObject::NativeFunction("bxor", bxor).into(),
    );

    metatable
}

pub(crate) fn coerce_number(value: LuaValue) -> crate::Result<LuaNumber> {
    // TODO: This should follow the same string parsing used in the lexer, with the addition of
    // trimming whitespace, and allowing a mark for the current locale (which is not part of the
    // grammar for lexing).
    match value {
        LuaValue::Number(n) => Ok(n),
        LuaValue::String(s) => {
            let s = String::from_utf8_lossy(&s);
            match s.trim().parse::<i64>() {
                Ok(n) => Ok(LuaNumber::Integer(n)),
                Err(_) => match s.trim().parse::<f64>() {
                    Ok(n) => Ok(LuaNumber::Float(n)),
                    Err(_) => Err(lua_error!("expected number")),
                },
            }
        }
        _ => Err(lua_error!("expected number")),
    }
}

fn value_numbers(mut values: Vec<LuaValue>) -> crate::Result<(LuaNumber, LuaNumber)> {
    if values.len() != 2 {
        return Err(lua_error!("metamethod expects 2 arguments"));
    }

    let b = values.remove(1);
    let a = values.remove(0);

    let a = coerce_number(a)?;
    let b = coerce_number(b)?;

    Ok((a, b))
}

fn add(_: &mut VM, values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let (a, b) = value_numbers(values)?;

    Ok(vec![LuaValue::Number(a + b)])
}

fn sub(_: &mut VM, values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let (a, b) = value_numbers(values)?;

    Ok(vec![LuaValue::Number(a - b)])
}

fn mul(_: &mut VM, values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let (a, b) = value_numbers(values)?;

    Ok(vec![LuaValue::Number(a * b)])
}

fn div(_: &mut VM, values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let (a, b) = value_numbers(values)?;

    Ok(vec![LuaValue::Number(a / b)])
}

fn mod_(_: &mut VM, values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let (a, b) = value_numbers(values)?;

    Ok(vec![LuaValue::Number(a % b)])
}

fn pow(_: &mut VM, values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let (a, b) = value_numbers(values)?;

    Ok(vec![LuaValue::Number(a.pow(&b))])
}

fn unm(_: &mut VM, mut values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    if values.len() != 1 {
        return Err(lua_error!("__unm expects 1 argument"));
    }

    let a = coerce_number(values.remove(0))?;

    Ok(vec![LuaValue::Number(-a)])
}

fn idiv(_: &mut VM, values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let (a, b) = value_numbers(values)?;

    Ok(vec![LuaValue::Number(a.idiv(&b))])
}

fn band(_: &mut VM, values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let (a, b) = value_numbers(values)?;

    Ok(vec![LuaValue::Number((a & b)?)])
}

fn bor(_: &mut VM, values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let (a, b) = value_numbers(values)?;

    Ok(vec![LuaValue::Number((a | b)?)])
}

fn bxor(_: &mut VM, values: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let (a, b) = value_numbers(values)?;

    Ok(vec![LuaValue::Number((a ^ b)?)])
}
