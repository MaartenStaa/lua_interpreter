use core::f64;
use std::sync::LazyLock;

use crate::value::{LuaNumber, LuaObject, LuaTable, LuaValue};
use miette::miette;

pub static MATH: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut math = LuaTable::new();

    // Functions
    math.insert("abs".into(), LuaObject::NativeFunction(abs).into());
    math.insert("acos".into(), LuaObject::NativeFunction(acos).into());
    math.insert("asin".into(), LuaObject::NativeFunction(asin).into());
    math.insert("atan".into(), LuaObject::NativeFunction(atan).into());
    math.insert("ceil".into(), LuaObject::NativeFunction(ceil).into());
    math.insert("cos".into(), LuaObject::NativeFunction(cos).into());
    math.insert("deg".into(), LuaObject::NativeFunction(deg).into());
    math.insert("exp".into(), LuaObject::NativeFunction(exp).into());
    math.insert("floor".into(), LuaObject::NativeFunction(floor).into());
    math.insert("fmod".into(), LuaObject::NativeFunction(fmod).into());
    math.insert("log".into(), LuaObject::NativeFunction(log).into());
    math.insert("max".into(), LuaObject::NativeFunction(max).into());
    math.insert("min".into(), LuaObject::NativeFunction(min).into());
    math.insert("modf".into(), LuaObject::NativeFunction(modf).into());
    math.insert("rad".into(), LuaObject::NativeFunction(rad).into());
    math.insert("random".into(), LuaObject::NativeFunction(random).into());
    math.insert(
        "randomseed".into(),
        LuaObject::NativeFunction(randomseed).into(),
    );
    math.insert("sin".into(), LuaObject::NativeFunction(sin).into());
    math.insert("sqrt".into(), LuaObject::NativeFunction(sqrt).into());
    math.insert("tan".into(), LuaObject::NativeFunction(tan).into());
    math.insert(
        "tointeger".into(),
        LuaObject::NativeFunction(tointeger).into(),
    );
    math.insert("type".into(), LuaObject::NativeFunction(r#type).into());
    math.insert("ult".into(), LuaObject::NativeFunction(ult).into());

    // Constants
    math.insert(
        "huge".into(),
        LuaValue::Number(LuaNumber::Float(f64::INFINITY)),
    );
    math.insert(
        "maxinteger".into(),
        LuaValue::Number(LuaNumber::Integer(i64::MAX)),
    );
    math.insert(
        "mininteger".into(),
        LuaValue::Number(LuaNumber::Integer(i64::MIN)),
    );
    math.insert(
        "pi".into(),
        LuaValue::Number(LuaNumber::Float(f64::consts::PI)),
    );

    math.into()
});

macro_rules! require_number {
    ($values:expr, $index:expr) => {
        match $values.get($index) {
            Some(LuaValue::Number(n)) => n,
            Some(v) => {
                return Err(miette!(
                    "bad argument #{}, expected number, got {}",
                    $index + 1,
                    v.type_name()
                ))
            }
            _ => {
                return Err(miette::miette!(
                    "bad argument #{}, expected number, got no value",
                    $index + 1,
                ))
            }
        }
    };
    ($values:expr) => {
        require_number!($values, 0)
    };
}

macro_rules! get_number {
    ($values:expr, $index:expr) => {
        match $values.get($index) {
            Some(LuaValue::Number(n)) => Some(n),
            Some(v) => {
                return Err(miette!(
                    "bad argument #{}, expected number, got {}",
                    $index + 1,
                    v.type_name()
                ))
            }
            _ => None,
        }
    };
    ($values:expr) => {
        get_number!($values, 0)
    };
}

fn abs(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let value = require_number!(values);

    Ok(match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Integer(i.abs())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.abs())),
    })
}

fn acos(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let value = require_number!(values);

    Ok(match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).acos())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.acos())),
    })
}

fn asin(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let value = require_number!(values);

    Ok(match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).asin())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.asin())),
    })
}

fn atan(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let _y = require_number!(values);
    todo!("implement atan")
}

fn ceil(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let value = require_number!(values);

    Ok(match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Integer(*i)),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Integer(f.ceil() as i64)),
    })
}

fn cos(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let value = require_number!(values);

    Ok(match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).cos())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.cos())),
    })
}

fn deg(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let value = require_number!(values);

    Ok(match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).to_degrees())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.to_degrees())),
    })
}

fn exp(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let value = require_number!(values);

    Ok(match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).exp())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.exp())),
    })
}

fn floor(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let value = require_number!(values);

    Ok(match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Integer(*i)),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Integer(f.floor() as i64)),
    })
}

fn fmod(_: Vec<LuaValue>) -> miette::Result<LuaValue> {
    todo!("implement fmod")
}

fn log(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let x = require_number!(values);
    let base = get_number!(values, 1).unwrap_or(&LuaNumber::Float(f64::consts::E));

    Ok(match (x, base) {
        (LuaNumber::Integer(x), LuaNumber::Integer(base)) => {
            LuaValue::Number(LuaNumber::Float((*x as f64).log(*base as f64)))
        }
        (LuaNumber::Float(x), LuaNumber::Integer(base)) => {
            LuaValue::Number(LuaNumber::Float(x.log(*base as f64)))
        }
        (LuaNumber::Integer(x), LuaNumber::Float(base)) => {
            LuaValue::Number(LuaNumber::Float((*x as f64).log(*base)))
        }
        (LuaNumber::Float(x), LuaNumber::Float(base)) => {
            LuaValue::Number(LuaNumber::Float(x.log(*base)))
        }
    })
}

fn max(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    require_number!(values);

    let mut max = None;

    for (i, value) in values.into_iter().enumerate() {
        let number = match value {
            LuaValue::Number(n) => n,
            v => {
                return Err(miette!(
                    "bad argument #{}, expected number, got {}",
                    i + 1,
                    v.type_name()
                ))
            }
        };

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

    Ok(LuaValue::Number(max.expect(
        "there is at least one argument per require_number!",
    )))
}

fn min(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    require_number!(values);

    let mut min = None;

    for (i, value) in values.into_iter().enumerate() {
        let number = match value {
            LuaValue::Number(n) => n,
            v => {
                return Err(miette!(
                    "bad argument #{}, expected number, got {}",
                    i + 1,
                    v.type_name()
                ))
            }
        };

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

    Ok(LuaValue::Number(min.expect(
        "there is at least one argument per require_number!",
    )))
}

fn modf(_: Vec<LuaValue>) -> miette::Result<LuaValue> {
    todo!("implement modf; has two return values")
}

fn rad(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let value = require_number!(values);

    Ok(match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).to_radians())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.to_radians())),
    })
}

fn random(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let _start = get_number!(values, 0).unwrap_or(&LuaNumber::Float(0.0));
    let _end = get_number!(values, 1).unwrap_or(&LuaNumber::Float(1.0));

    todo!("implement random")
}

fn randomseed(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let _seed = require_number!(values);

    todo!("implement randomseed")
}

fn sin(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let value = require_number!(values);

    Ok(match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).sin())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.sin())),
    })
}

fn sqrt(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let value = require_number!(values);

    Ok(match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).sqrt())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.sqrt())),
    })
}

fn tan(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let value = require_number!(values);

    Ok(match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).tan())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.tan())),
    })
}

fn tointeger(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    match values.first() {
        Some(LuaValue::Number(LuaNumber::Integer(i))) => {
            Ok(LuaValue::Number(LuaNumber::Integer(*i)))
        }
        Some(LuaValue::Number(LuaNumber::Float(f))) if f.fract() == 0.0 => {
            Ok(LuaValue::Number(LuaNumber::Integer(*f as i64)))
        }
        Some(LuaValue::String(s)) => {
            let s = String::from_utf8_lossy(s);
            match s.parse::<i64>() {
                Ok(i) => Ok(LuaValue::Number(LuaNumber::Integer(i))),
                Err(_) => Ok(LuaValue::Nil),
            }
        }
        _ => Ok(LuaValue::Nil),
    }
}

fn r#type(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    match values.first() {
        Some(LuaValue::Number(LuaNumber::Integer(_))) => Ok("integer".into()),
        Some(LuaValue::Number(LuaNumber::Float(_))) => Ok("float".into()),
        _ => Ok(LuaValue::Nil),
    }
}

// Returns a boolean, true if and only if integer m is below integer n when they are compared as unsigned integers.
fn ult(values: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let a = require_number!(values, 0);
    let b = require_number!(values, 1);

    match (a, b) {
        (LuaNumber::Integer(a), LuaNumber::Integer(b)) if *a >= 0 && *b >= 0 => {
            Ok((*a < *b).into())
        }
        _ => todo!("implement ult for non-trivial inputs"),
    }
}
