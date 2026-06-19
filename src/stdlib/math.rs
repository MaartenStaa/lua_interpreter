use std::sync::{LazyLock, RwLock};

use crate::{
    error::lua_error,
    state::LuaState,
    value::{LuaNumber, LuaObject, LuaTable, LuaValue},
};
use rand::{prelude::*, rng};

pub static MATH: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut math = LuaTable::new();

    // Functions
    math.insert(
        "abs".into(),
        LuaObject::NativeFunction("math.abs", abs).into(),
    );
    math.insert(
        "acos".into(),
        LuaObject::NativeFunction("math.acos", acos).into(),
    );
    math.insert(
        "asin".into(),
        LuaObject::NativeFunction("math.asin", asin).into(),
    );
    math.insert(
        "atan".into(),
        LuaObject::NativeFunction("math.atan", atan).into(),
    );
    math.insert(
        "ceil".into(),
        LuaObject::NativeFunction("math.ceil", ceil).into(),
    );
    math.insert(
        "cos".into(),
        LuaObject::NativeFunction("math.cos", cos).into(),
    );
    math.insert(
        "deg".into(),
        LuaObject::NativeFunction("math.deg", deg).into(),
    );
    math.insert(
        "exp".into(),
        LuaObject::NativeFunction("math.exp", exp).into(),
    );
    math.insert(
        "floor".into(),
        LuaObject::NativeFunction("math.floor", floor).into(),
    );
    math.insert(
        "fmod".into(),
        LuaObject::NativeFunction("math.fmod", fmod).into(),
    );
    math.insert(
        "log".into(),
        LuaObject::NativeFunction("math.log", log).into(),
    );
    math.insert(
        "max".into(),
        LuaObject::NativeFunction("math.max", max).into(),
    );
    math.insert(
        "min".into(),
        LuaObject::NativeFunction("math.min", min).into(),
    );
    math.insert(
        "modf".into(),
        LuaObject::NativeFunction("math.modf", modf).into(),
    );
    math.insert(
        "rad".into(),
        LuaObject::NativeFunction("math.rad", rad).into(),
    );
    math.insert(
        "random".into(),
        LuaObject::NativeFunction("math.random", random).into(),
    );
    math.insert(
        "randomseed".into(),
        LuaObject::NativeFunction("math.randomseed", randomseed).into(),
    );
    math.insert(
        "sin".into(),
        LuaObject::NativeFunction("math.sin", sin).into(),
    );
    math.insert(
        "sqrt".into(),
        LuaObject::NativeFunction("math.sqrt", sqrt).into(),
    );
    math.insert(
        "tan".into(),
        LuaObject::NativeFunction("math.tan", tan).into(),
    );
    math.insert(
        "tointeger".into(),
        LuaObject::NativeFunction("math.tointeger", tointeger).into(),
    );
    math.insert(
        "type".into(),
        LuaObject::NativeFunction("math.r", r#type).into(),
    );
    math.insert(
        "ult".into(),
        LuaObject::NativeFunction("math.ult", ult).into(),
    );

    // Constants
    math.insert("huge".into(), LuaValue::from_float(f64::INFINITY));
    math.insert("maxinteger".into(), LuaValue::from_integer(i64::MAX));
    math.insert("mininteger".into(), LuaValue::from_integer(i64::MIN));
    math.insert("pi".into(), LuaValue::from_float(std::f64::consts::PI));

    math.into()
});

fn abs(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let value = state.arg_as_number(0)?;

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::from_integer(i.abs()),
        LuaNumber::Float(f) => LuaValue::from_float(f.abs()),
    }])
}

fn acos(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let value = state.arg_as_number(0)?;

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).acos()),
        LuaNumber::Float(f) => LuaValue::from_float(f.acos()),
    }])
}

fn asin(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let value = state.arg_as_number(0)?;

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).asin()),
        LuaNumber::Float(f) => LuaValue::from_float(f.asin()),
    }])
}

fn atan(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let _ = state.arg_as_number(0)?;
    todo!("implement atan")
}

fn ceil(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let value = state.arg_as_number(0)?;

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::from_integer(i),
        LuaNumber::Float(f) => LuaValue::from_integer(f.ceil() as i64),
    }])
}

fn cos(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let value = state.arg_as_number(0)?;

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).cos()),
        LuaNumber::Float(f) => LuaValue::from_float(f.cos()),
    }])
}

fn deg(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let value = state.arg_as_number(0)?;

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).to_degrees()),
        LuaNumber::Float(f) => LuaValue::from_float(f.to_degrees()),
    }])
}

fn exp(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let value = state.arg_as_number(0)?;

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).exp()),
        LuaNumber::Float(f) => LuaValue::from_float(f.exp()),
    }])
}

fn floor(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let value = state.arg_as_number(0)?;

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::from_integer(i),
        LuaNumber::Float(f) => LuaValue::from_integer(f.floor() as i64),
    }])
}

fn fmod(_: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    todo!("implement fmod")
}

fn log(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let x = state.arg_as_number(0)?;
    let base = state
        .try_arg_as_number(1)?
        .unwrap_or(LuaNumber::Float(std::f64::consts::E));

    Ok(vec![match (x, base) {
        (LuaNumber::Integer(x), LuaNumber::Integer(base)) => {
            LuaValue::from_float((x as f64).log(base as f64))
        }
        (LuaNumber::Float(x), LuaNumber::Integer(base)) => LuaValue::from_float(x.log(base as f64)),
        (LuaNumber::Integer(x), LuaNumber::Float(base)) => {
            LuaValue::from_float((x as f64).log(base))
        }
        (LuaNumber::Float(x), LuaNumber::Float(base)) => LuaValue::from_float(x.log(base)),
    }])
}

fn max(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let mut max = None;

    for i in 0..state.num_args() {
        let number = state.arg_as_number(i)?;

        match (&max, number) {
            (None, n) => max = Some(n),
            (Some(LuaNumber::Integer(v)), LuaNumber::Integer(n)) => {
                if n > *v {
                    max = Some(LuaNumber::Integer(n));
                }
            }
            (Some(LuaNumber::Float(v)), LuaNumber::Integer(n)) => {
                if n as f64 > *v {
                    max = Some(LuaNumber::Float(n as f64));
                }
            }
            (Some(LuaNumber::Integer(v)), LuaNumber::Float(n)) => {
                if n > *v as f64 {
                    max = Some(LuaNumber::Float(n));
                }
            }
            (Some(LuaNumber::Float(v)), LuaNumber::Float(n)) => {
                if n > *v {
                    max = Some(LuaNumber::Float(n));
                }
            }
        }
    }

    Ok(vec![LuaValue::from_number(max.expect(
        "there is at least one argument per require_number!",
    ))])
}

fn min(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let mut min = None;

    for i in 0..state.num_args() {
        let number = state.arg_as_number(i)?;

        match (&min, number) {
            (None, n) => min = Some(n),
            (Some(LuaNumber::Integer(v)), LuaNumber::Integer(n)) => {
                if n < *v {
                    min = Some(LuaNumber::Integer(n));
                }
            }
            (Some(LuaNumber::Float(v)), LuaNumber::Integer(n)) => {
                if (n as f64) < *v {
                    min = Some(LuaNumber::Float(n as f64));
                }
            }
            (Some(LuaNumber::Integer(v)), LuaNumber::Float(n)) => {
                if n < *v as f64 {
                    min = Some(LuaNumber::Float(n));
                }
            }
            (Some(LuaNumber::Float(v)), LuaNumber::Float(n)) => {
                if n < *v {
                    min = Some(LuaNumber::Float(n));
                }
            }
        }
    }

    Ok(vec![LuaValue::from_number(min.expect(
        "there is at least one argument per require_number!",
    ))])
}

fn modf(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let value = state.arg_as_number(0)?;

    Ok(match value {
        LuaNumber::Integer(i) => vec![LuaValue::from_integer(i), LuaValue::from_float(0.0)],
        LuaNumber::Float(f) => {
            let i = f.trunc();
            let f = f.fract();

            vec![
                // return an integer when the result fits in the range of an integer, or a float otherwise
                if i >= i64::MIN as f64 && i <= i64::MAX as f64 {
                    LuaValue::from_integer(i as i64)
                } else {
                    LuaValue::from_float(i)
                },
                LuaValue::from_float(if f.is_nan() && !i.is_nan() { 0.0 } else { f }),
            ]
        }
    })
}

fn rad(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let value = state.arg_as_number(0)?;

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).to_radians()),
        LuaNumber::Float(f) => LuaValue::from_float(f.to_radians()),
    }])
}

static RANDOM: LazyLock<RwLock<StdRng>> =
    LazyLock::new(|| RwLock::new(StdRng::from_rng(&mut rng())));

fn random(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    if state.num_args() == 0 {
        return Ok(vec![LuaValue::from_float(
            RANDOM.write().unwrap().random_range(0f64..1f64),
        )]);
    }

    let m = state.arg_as_integer(0)?;
    if state.num_args() == 1 {
        return match m.partial_cmp(&0) {
            Some(std::cmp::Ordering::Less) => Err(lua_error!(
                "bad argument #1 to 'math.random' (interval is empty)"
            )),
            Some(std::cmp::Ordering::Equal) => Ok(vec![LuaValue::from_integer(
                RANDOM.write().unwrap().random(),
            )]),
            _ => Ok(vec![LuaValue::from_integer(
                RANDOM.write().unwrap().random_range(1..m),
            )]),
        };
    }

    let n = state.arg_as_integer(1)?;
    if n < m {
        return Err(lua_error!(
            "bad argument #2 to 'math.random' (interval is empty)"
        ));
    }
    if m == n {
        return Ok(vec![LuaValue::from_integer(m)]);
    }

    Ok(vec![LuaValue::from_integer(
        RANDOM.write().unwrap().random_range(m..n),
    )])
}

fn randomseed(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let x = state.try_arg_as_number(0)?.unwrap_or_else(|| {
        LuaNumber::Integer(i64::from_le_bytes(
            RANDOM.write().unwrap().next_u64().to_le_bytes(),
        ))
    });
    let y = state.try_arg_as_number(1)?.unwrap_or(LuaNumber::Integer(0));

    macro_rules! bytes {
        ($x:expr) => {
            match $x {
                LuaNumber::Integer(i) => i.to_le_bytes(),
                LuaNumber::Float(f) => f.to_le_bytes(),
            }
        };
    }

    let mut seed = [0u8; size_of::<i64>() * 4];
    let size_of = std::mem::size_of::<i64>();
    seed[..size_of].copy_from_slice(&bytes!(x));
    seed[size_of..size_of * 2].copy_from_slice(&bytes!(y));
    seed[size_of * 2..size_of * 3].copy_from_slice(&bytes!(x));
    seed[size_of * 3..].copy_from_slice(&bytes!(y));

    *RANDOM.write().unwrap() = StdRng::from_seed(seed);

    Ok(vec![LuaValue::from_number(x), LuaValue::from_number(y)])
}

fn sin(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let value = state.arg_as_number(0)?;

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).sin()),
        LuaNumber::Float(f) => LuaValue::from_float(f.sin()),
    }])
}

fn sqrt(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let value = state.arg_as_number(0)?;

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).sqrt()),
        LuaNumber::Float(f) => LuaValue::from_float(f.sqrt()),
    }])
}

fn tan(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let value = state.arg_as_number(0)?;

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).tan()),
        LuaNumber::Float(f) => LuaValue::from_float(f.tan()),
    }])
}

fn tointeger(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    match state.try_arg(0) {
        Some(v) if v.is_integer() => Ok(vec![v.clone()]),
        Some(v) if v.is_float() && unsafe { v.as_float() }.fract() == 0.0 => {
            Ok(vec![LuaValue::from_integer(unsafe { v.as_float() } as i64)])
        }
        Some(v) if v.is_string() => {
            let s = String::from_utf8_lossy(unsafe { v.as_string() });
            match s.parse::<i64>() {
                Ok(i) => Ok(vec![LuaValue::from_integer(i)]),
                Err(_) => Ok(vec![LuaValue::NIL]),
            }
        }
        _ => Ok(vec![LuaValue::NIL]),
    }
}

fn r#type(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    match state.arg(0) {
        v if v.is_integer() => Ok(vec!["integer".into()]),
        v if v.is_float() => Ok(vec!["float".into()]),
        _ => Ok(vec![LuaValue::NIL]),
    }
}

// Returns a boolean, true if and only if integer m is below integer n when they are compared as unsigned integers.
fn ult(state: &mut LuaState) -> crate::Result<Vec<LuaValue>> {
    let a = state.arg_as_number(0)?;
    let b = state.arg_as_number(1)?;

    match (a, b) {
        (LuaNumber::Integer(a), LuaNumber::Integer(b)) if a >= 0 && b >= 0 => {
            Ok(vec![(a < b).into()])
        }
        _ => todo!("implement ult for non-trivial inputs"),
    }
}
