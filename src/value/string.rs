use std::{
    borrow::Cow,
    fmt::{Debug, Display},
    iter::Copied,
    ops::{Deref, DerefMut},
};

use crate::{
    LuaState,
    error::lua_error,
    macros::{lua_return, native_function},
    stdlib,
};

use super::{LuaNumber, LuaTable, LuaValue, metatables};

#[derive(Clone, PartialEq, Eq, Hash, Default, PartialOrd)]
pub struct LuaString(Vec<u8>);

impl LuaString {
    #[must_use]
    pub const fn new() -> Self {
        LuaString(Vec::new())
    }

    #[must_use]
    pub fn from_bytes(bytes: Vec<u8>) -> Self {
        LuaString(bytes)
    }

    #[must_use]
    pub fn as_str(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(&self.0)
    }

    pub(crate) fn into_inner(self) -> Vec<u8> {
        self.0
    }
}

impl Debug for LuaString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match String::from_utf8(self.0.clone()) {
            Ok(s) => write!(f, "{s:?}"),
            Err(_) => write!(f, "{:?} (lossy)", String::from_utf8_lossy(&self.0)),
        }
    }
}

impl Display for LuaString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match String::from_utf8(self.0.clone()) {
            Ok(s) => write!(f, "{s}"),
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

impl From<Cow<'_, [u8]>> for LuaString {
    fn from(value: Cow<'_, [u8]>) -> Self {
        LuaString(value.into_owned())
    }
}

impl From<Cow<'_, str>> for LuaString {
    fn from(value: Cow<'_, str>) -> Self {
        LuaString(value.into_owned().into_bytes())
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

    metatable.insert(metatables::ADD_KEY.clone(), native_function!(add));
    metatable.insert(metatables::SUB_KEY.clone(), native_function!(sub));
    metatable.insert(metatables::MUL_KEY.clone(), native_function!(mul));
    metatable.insert(metatables::DIV_KEY.clone(), native_function!(div));
    metatable.insert(metatables::MOD_KEY.clone(), native_function!("mod", mod_));
    metatable.insert(metatables::POW_KEY.clone(), native_function!(pow));
    metatable.insert(metatables::UNM_KEY.clone(), native_function!(unm));
    metatable.insert(metatables::IDIV_KEY.clone(), native_function!(idiv));
    metatable.insert(metatables::BAND_KEY.clone(), native_function!(band));
    metatable.insert(metatables::BOR_KEY.clone(), native_function!(bor));
    metatable.insert(metatables::BXOR_KEY.clone(), native_function!(bxor));

    metatable
}

pub(crate) fn coerce_number(value: LuaValue) -> crate::Result<LuaNumber> {
    // TODO: This should follow the same string parsing used in the lexer, with the addition of
    // trimming whitespace, and allowing a mark for the current locale (which is not part of the
    // grammar for lexing).
    if value.is_number() {
        Ok(unsafe { value.as_number() })
    } else if value.is_string() {
        let bytes = unsafe { value.into_string() };
        let s = String::from_utf8_lossy(&bytes);
        match s.trim().parse::<i64>() {
            Ok(n) => Ok(LuaNumber::Integer(n)),
            Err(_) => match s.trim().parse::<f64>() {
                Ok(n) => Ok(LuaNumber::Float(n)),
                Err(_) => Err(lua_error!("expected number")),
            },
        }
    } else {
        Err(lua_error!("expected number"))
    }
}

fn value_numbers(state: &mut LuaState) -> crate::Result<(LuaNumber, LuaNumber)> {
    if state.num_args() != 2 {
        return Err(lua_error!("metamethod expects 2 arguments"));
    }

    let a = state.arg(0);
    let b = state.arg(1);

    let a = coerce_number(a)?;
    let b = coerce_number(b)?;

    Ok((a, b))
}

fn add(state: &mut LuaState) -> crate::Result<usize> {
    let (a, b) = value_numbers(state)?;

    lua_return!(state; LuaValue::from_number(a + b))
}

fn sub(state: &mut LuaState) -> crate::Result<usize> {
    let (a, b) = value_numbers(state)?;

    lua_return!(state; LuaValue::from_number(a - b))
}

fn mul(state: &mut LuaState) -> crate::Result<usize> {
    let (a, b) = value_numbers(state)?;

    lua_return!(state; LuaValue::from_number(a * b))
}

fn div(state: &mut LuaState) -> crate::Result<usize> {
    let (a, b) = value_numbers(state)?;

    lua_return!(state; LuaValue::from_number(a / b))
}

fn mod_(state: &mut LuaState) -> crate::Result<usize> {
    let (a, b) = value_numbers(state)?;

    lua_return!(state; LuaValue::from_number(a % b))
}

fn pow(state: &mut LuaState) -> crate::Result<usize> {
    let (a, b) = value_numbers(state)?;

    lua_return!(state; LuaValue::from_number(a.pow(&b)))
}

fn unm(state: &mut LuaState) -> crate::Result<usize> {
    if state.num_args() != 1 {
        return Err(lua_error!("__unm expects 1 argument"));
    }

    let a = coerce_number(state.arg(0))?;

    lua_return!(state; LuaValue::from_number(-a))
}

fn idiv(state: &mut LuaState) -> crate::Result<usize> {
    let (a, b) = value_numbers(state)?;

    lua_return!(state; LuaValue::from_number(a.idiv(&b)))
}

fn band(state: &mut LuaState) -> crate::Result<usize> {
    let (a, b) = value_numbers(state)?;

    lua_return!(state; LuaValue::from_number((a & b)?))
}

fn bor(state: &mut LuaState) -> crate::Result<usize> {
    let (a, b) = value_numbers(state)?;

    lua_return!(state; LuaValue::from_number((a | b)?))
}

fn bxor(state: &mut LuaState) -> crate::Result<usize> {
    let (a, b) = value_numbers(state)?;

    lua_return!(state; LuaValue::from_number((a ^ b)?))
}
