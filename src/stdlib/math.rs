use std::sync::{LazyLock, RwLock};

use crate::{
    error::lua_error,
    macros::{lua_return, native_function},
    state::LuaState,
    value::{LuaNumber, LuaTable, LuaValue},
};
use rand::{prelude::*, rng};

pub static MATH: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut math = LuaTable::new();

    // Functions
    math.insert("abs".into(), native_function!("math"; abs));
    math.insert("acos".into(), native_function!("math"; acos));
    math.insert("asin".into(), native_function!("math"; asin));
    math.insert("atan".into(), native_function!("math"; atan));
    math.insert("ceil".into(), native_function!("math"; ceil));
    math.insert("cos".into(), native_function!("math"; cos));
    math.insert("deg".into(), native_function!("math"; deg));
    math.insert("exp".into(), native_function!("math"; exp));
    math.insert("floor".into(), native_function!("math"; floor));
    math.insert("fmod".into(), native_function!("math"; fmod));
    math.insert("log".into(), native_function!("math"; log));
    math.insert("max".into(), native_function!("math"; max));
    math.insert("min".into(), native_function!("math"; min));
    math.insert("modf".into(), native_function!("math"; modf));
    math.insert("rad".into(), native_function!("math"; rad));
    math.insert("random".into(), native_function!("math"; random));
    math.insert("randomseed".into(), native_function!("math"; randomseed));
    math.insert("sin".into(), native_function!("math"; sin));
    math.insert("sqrt".into(), native_function!("math"; sqrt));
    math.insert("tan".into(), native_function!("math"; tan));
    math.insert("tointeger".into(), native_function!("math"; tointeger));
    math.insert("type".into(), native_function!("math"; r#type));
    math.insert("ult".into(), native_function!("math"; ult));

    // Constants
    math.insert("huge".into(), LuaValue::from_float(f64::INFINITY));
    math.insert("maxinteger".into(), LuaValue::from_integer(i64::MAX));
    math.insert("mininteger".into(), LuaValue::from_integer(i64::MIN));
    math.insert("pi".into(), LuaValue::from_float(std::f64::consts::PI));

    math.into()
});

fn abs(state: &mut LuaState) -> crate::Result<u16> {
    let value = state.arg_as_number(0)?;

    lua_return!(state; match value {
        LuaNumber::Integer(i) => LuaValue::from_integer(i.abs()),
        LuaNumber::Float(f) => LuaValue::from_float(f.abs()),
    })
}

fn acos(state: &mut LuaState) -> crate::Result<u16> {
    let value = state.arg_as_number(0)?;

    lua_return!(state; match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).acos()),
        LuaNumber::Float(f) => LuaValue::from_float(f.acos()),
    })
}

fn asin(state: &mut LuaState) -> crate::Result<u16> {
    let value = state.arg_as_number(0)?;

    lua_return!(state; match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).asin()),
        LuaNumber::Float(f) => LuaValue::from_float(f.asin()),
    })
}

fn atan(state: &mut LuaState) -> crate::Result<u16> {
    let _ = state.arg_as_number(0)?;
    todo!("implement atan")
}

fn ceil(state: &mut LuaState) -> crate::Result<u16> {
    let value = state.arg_as_number(0)?;

    lua_return!(state; match value {
        LuaNumber::Integer(i) => LuaValue::from_integer(i),
        LuaNumber::Float(f) => LuaValue::from_integer(f.ceil() as i64),
    })
}

fn cos(state: &mut LuaState) -> crate::Result<u16> {
    let value = state.arg_as_number(0)?;

    lua_return!(state; match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).cos()),
        LuaNumber::Float(f) => LuaValue::from_float(f.cos()),
    })
}

fn deg(state: &mut LuaState) -> crate::Result<u16> {
    let value = state.arg_as_number(0)?;

    lua_return!(state; match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).to_degrees()),
        LuaNumber::Float(f) => LuaValue::from_float(f.to_degrees()),
    })
}

fn exp(state: &mut LuaState) -> crate::Result<u16> {
    let value = state.arg_as_number(0)?;

    lua_return!(state; match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).exp()),
        LuaNumber::Float(f) => LuaValue::from_float(f.exp()),
    })
}

fn floor(state: &mut LuaState) -> crate::Result<u16> {
    let value = state.arg_as_number(0)?;

    lua_return!(state; match value {
        LuaNumber::Integer(i) => LuaValue::from_integer(i),
        LuaNumber::Float(f) => LuaValue::from_integer(f.floor() as i64),
    })
}

fn fmod(_: &mut LuaState) -> crate::Result<u16> {
    todo!("implement fmod")
}

fn log(state: &mut LuaState) -> crate::Result<u16> {
    let x = state.arg_as_number(0)?;
    let base = state
        .try_arg_as_number(1)?
        .unwrap_or(LuaNumber::Float(std::f64::consts::E));

    lua_return!(state; match (x, base) {
        (LuaNumber::Integer(x), LuaNumber::Integer(base)) => {
            LuaValue::from_float((x as f64).log(base as f64))
        }
        (LuaNumber::Float(x), LuaNumber::Integer(base)) => LuaValue::from_float(x.log(base as f64)),
        (LuaNumber::Integer(x), LuaNumber::Float(base)) => {
            LuaValue::from_float((x as f64).log(base))
        }
        (LuaNumber::Float(x), LuaNumber::Float(base)) => LuaValue::from_float(x.log(base)),
    })
}

fn max(state: &mut LuaState) -> crate::Result<u16> {
    let mut max = state.arg_as_number(0)?;

    for i in 1..state.num_args() {
        let number = state.arg_as_number(i as usize)?;
        if number > max {
            max = number;
        }
    }

    lua_return!(state; LuaValue::from_number(max))
}

fn min(state: &mut LuaState) -> crate::Result<u16> {
    let mut min = state.arg_as_number(0)?;

    for i in 1..state.num_args() {
        let number = state.arg_as_number(i as usize)?;
        if number < min {
            min = number;
        }
    }

    lua_return!(state; LuaValue::from_number(min))
}

fn modf(state: &mut LuaState) -> crate::Result<u16> {
    let value = state.arg_as_number(0)?;

    match value {
        LuaNumber::Integer(i) => {
            lua_return!(state; LuaValue::from_integer(i), LuaValue::from_float(0.0))
        }
        LuaNumber::Float(f) => {
            let i = f.trunc();
            let f = f.fract();

            lua_return!(state;
                // return an integer when the result fits in the range of an integer, or a float otherwise
                if i >= i64::MIN as f64 && i <= i64::MAX as f64 {
                    LuaValue::from_integer(i as i64)
                } else {
                    LuaValue::from_float(i)
                },
                LuaValue::from_float(if f.is_nan() && !i.is_nan() { 0.0 } else { f }),
            )
        }
    }
}

fn rad(state: &mut LuaState) -> crate::Result<u16> {
    let value = state.arg_as_number(0)?;

    lua_return!(state; match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).to_radians()),
        LuaNumber::Float(f) => LuaValue::from_float(f.to_radians()),
    })
}

static RANDOM: LazyLock<RwLock<StdRng>> =
    LazyLock::new(|| RwLock::new(StdRng::from_rng(&mut rng())));

fn random(state: &mut LuaState) -> crate::Result<u16> {
    if state.num_args() == 0 {
        lua_return!(state; LuaValue::from_float(
            RANDOM.write().unwrap().random_range(0f64..1f64),
        ));
    }

    let m = state.arg_as_integer(0)?;
    if state.num_args() == 1 {
        return match m.partial_cmp(&0) {
            Some(std::cmp::Ordering::Less) => Err(lua_error!(
                "bad argument #1 to 'math.random' (interval is empty)"
            )),
            Some(std::cmp::Ordering::Equal) => lua_return!(state; LuaValue::from_integer(
                RANDOM.write().unwrap().random(),
            )),
            _ => lua_return!(state; LuaValue::from_integer(
                RANDOM.write().unwrap().random_range(1..m),
            )),
        };
    }

    let n = state.arg_as_integer(1)?;
    if n < m {
        return Err(lua_error!(
            "bad argument #2 to 'math.random' (interval is empty)"
        ));
    }
    if m == n {
        lua_return!(state; LuaValue::from_integer(m))
    }

    lua_return!(state; LuaValue::from_integer(
        RANDOM.write().unwrap().random_range(m..n),
    ))
}

fn randomseed(state: &mut LuaState) -> crate::Result<u16> {
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

    lua_return!(state; LuaValue::from_number(x), LuaValue::from_number(y))
}

fn sin(state: &mut LuaState) -> crate::Result<u16> {
    let value = state.arg_as_number(0)?;

    lua_return!(state; match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).sin()),
        LuaNumber::Float(f) => LuaValue::from_float(f.sin()),
    })
}

fn sqrt(state: &mut LuaState) -> crate::Result<u16> {
    let value = state.arg_as_number(0)?;

    lua_return!(state; match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).sqrt()),
        LuaNumber::Float(f) => LuaValue::from_float(f.sqrt()),
    })
}

fn tan(state: &mut LuaState) -> crate::Result<u16> {
    let value = state.arg_as_number(0)?;

    lua_return!(state; match value {
        LuaNumber::Integer(i) => LuaValue::from_float((i as f64).tan()),
        LuaNumber::Float(f) => LuaValue::from_float(f.tan()),
    })
}

fn tointeger(state: &mut LuaState) -> crate::Result<u16> {
    match state.try_arg(0) {
        Some(v) if v.is_integer() => lua_return!(state; v.clone()),
        Some(v) if v.is_float() && unsafe { v.as_float() }.fract() == 0.0 => {
            lua_return!(state; LuaValue::from_integer(unsafe { v.as_float() } as i64))
        }
        Some(v) if v.is_string() => {
            let s = String::from_utf8_lossy(unsafe { v.as_string() });
            if let Ok(i) = s.parse::<i64>() {
                lua_return!(state; LuaValue::from_integer(i))
            }
            lua_return!(state)
        }
        _ => lua_return!(state),
    }
}

fn r#type(state: &mut LuaState) -> crate::Result<u16> {
    match state.arg(0) {
        v if v.is_integer() => lua_return!(state; "integer".into()),
        v if v.is_float() => lua_return!(state; "float".into()),
        _ => lua_return!(state),
    }
}

// Returns a boolean, true if and only if integer m is below integer n when they are compared as unsigned integers.
fn ult(state: &mut LuaState) -> crate::Result<u16> {
    let a = state.arg_as_number(0)?;
    let b = state.arg_as_number(1)?;

    match (a, b) {
        (LuaNumber::Integer(a), LuaNumber::Integer(b)) if a >= 0 && b >= 0 => {
            lua_return!(state; (a < b).into())
        }
        _ => todo!("implement ult for non-trivial inputs"),
    }
}
