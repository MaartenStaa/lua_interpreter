use crate::{error::lua_error, stdlib, vm::VM};

use super::{metatables, LuaNumber, LuaObject, LuaTable, LuaValue};

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

    Ok(vec![LuaValue::Number(a.idiv(b))])
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
