use core::f64;
use std::sync::LazyLock;

use crate::{
    macros::{get_number, require_number},
    value::{LuaNumber, LuaObject, LuaTable, LuaValue},
    vm::VM,
};
use miette::miette;

pub static MATH: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut math = LuaTable::new();

    // Functions
    math.insert("abs".into(), LuaObject::NativeFunction("abs", abs).into());
    math.insert(
        "acos".into(),
        LuaObject::NativeFunction("acos", acos).into(),
    );
    math.insert(
        "asin".into(),
        LuaObject::NativeFunction("asin", asin).into(),
    );
    math.insert(
        "atan".into(),
        LuaObject::NativeFunction("atan", atan).into(),
    );
    math.insert(
        "ceil".into(),
        LuaObject::NativeFunction("ceil", ceil).into(),
    );
    math.insert("cos".into(), LuaObject::NativeFunction("cos", cos).into());
    math.insert("deg".into(), LuaObject::NativeFunction("deg", deg).into());
    math.insert("exp".into(), LuaObject::NativeFunction("exp", exp).into());
    math.insert(
        "floor".into(),
        LuaObject::NativeFunction("floor", floor).into(),
    );
    math.insert(
        "fmod".into(),
        LuaObject::NativeFunction("fmod", fmod).into(),
    );
    math.insert("log".into(), LuaObject::NativeFunction("log", log).into());
    math.insert("max".into(), LuaObject::NativeFunction("max", max).into());
    math.insert("min".into(), LuaObject::NativeFunction("min", min).into());
    math.insert(
        "modf".into(),
        LuaObject::NativeFunction("modf", modf).into(),
    );
    math.insert("rad".into(), LuaObject::NativeFunction("rad", rad).into());
    math.insert(
        "random".into(),
        LuaObject::NativeFunction("random", random).into(),
    );
    math.insert(
        "randomseed".into(),
        LuaObject::NativeFunction("randomseed", randomseed).into(),
    );
    math.insert("sin".into(), LuaObject::NativeFunction("sin", sin).into());
    math.insert(
        "sqrt".into(),
        LuaObject::NativeFunction("sqrt", sqrt).into(),
    );
    math.insert("tan".into(), LuaObject::NativeFunction("tan", tan).into());
    math.insert(
        "tointeger".into(),
        LuaObject::NativeFunction("tointeger", tointeger).into(),
    );
    math.insert("type".into(), LuaObject::NativeFunction("r", r#type).into());
    math.insert("ult".into(), LuaObject::NativeFunction("ult", ult).into());

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

fn abs(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = require_number!(values, "abs");

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Integer(i.abs())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.abs())),
    }])
}

fn acos(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = require_number!(values, "acos");

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).acos())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.acos())),
    }])
}

fn asin(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = require_number!(values, "asin");

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).asin())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.asin())),
    }])
}

fn atan(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let _y = require_number!(values, "atan");
    todo!("implement atan")
}

fn ceil(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = require_number!(values, "ceil");

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Integer(*i)),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Integer(f.ceil() as i64)),
    }])
}

fn cos(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = require_number!(values, "cos");

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).cos())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.cos())),
    }])
}

fn deg(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = require_number!(values, "deg");

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).to_degrees())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.to_degrees())),
    }])
}

fn exp(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = require_number!(values, "exp");

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).exp())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.exp())),
    }])
}

fn floor(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = require_number!(values, "floor");

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Integer(*i)),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Integer(f.floor() as i64)),
    }])
}

fn fmod(_: &mut VM, _: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    todo!("implement fmod")
}

fn log(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let x = require_number!(values, "log");
    let base = get_number!(values, "log", 1).unwrap_or(&LuaNumber::Float(f64::consts::E));

    Ok(vec![match (x, base) {
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
    }])
}

fn max(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    require_number!(values, "max");

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

    Ok(vec![LuaValue::Number(max.expect(
        "there is at least one argument per require_number!",
    ))])
}

fn min(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    require_number!(values, "min");

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

    Ok(vec![LuaValue::Number(min.expect(
        "there is at least one argument per require_number!",
    ))])
}

fn modf(_: &mut VM, _: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    todo!("implement modf; has two return values")
}

fn rad(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = require_number!(values, "rad");

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).to_radians())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.to_radians())),
    }])
}

fn random(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let _start = get_number!(values, 0).unwrap_or(&LuaNumber::Float(0.0));
    let _end = get_number!(values, 1).unwrap_or(&LuaNumber::Float(1.0));

    todo!("implement random")
}

fn randomseed(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let _seed = require_number!(values, "randomseed");

    todo!("implement randomseed")
}

fn sin(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = require_number!(values, "sin");

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).sin())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.sin())),
    }])
}

fn sqrt(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = require_number!(values, "sqrt");

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).sqrt())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.sqrt())),
    }])
}

fn tan(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let value = require_number!(values, "tan");

    Ok(vec![match value {
        LuaNumber::Integer(i) => LuaValue::Number(LuaNumber::Float((*i as f64).tan())),
        LuaNumber::Float(f) => LuaValue::Number(LuaNumber::Float(f.tan())),
    }])
}

fn tointeger(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    match values.first() {
        Some(LuaValue::Number(LuaNumber::Integer(i))) => {
            Ok(vec![LuaValue::Number(LuaNumber::Integer(*i))])
        }
        Some(LuaValue::Number(LuaNumber::Float(f))) if f.fract() == 0.0 => {
            Ok(vec![LuaValue::Number(LuaNumber::Integer(*f as i64))])
        }
        Some(LuaValue::String(s)) => {
            let s = String::from_utf8_lossy(s);
            match s.parse::<i64>() {
                Ok(i) => Ok(vec![LuaValue::Number(LuaNumber::Integer(i))]),
                Err(_) => Ok(vec![LuaValue::Nil]),
            }
        }
        _ => Ok(vec![LuaValue::Nil]),
    }
}

fn r#type(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    match values.first() {
        Some(LuaValue::Number(LuaNumber::Integer(_))) => Ok(vec!["integer".into()]),
        Some(LuaValue::Number(LuaNumber::Float(_))) => Ok(vec!["float".into()]),
        _ => Ok(vec![LuaValue::Nil]),
    }
}

// Returns a boolean, true if and only if integer m is below integer n when they are compared as unsigned integers.
fn ult(_: &mut VM, values: Vec<LuaValue>) -> miette::Result<Vec<LuaValue>> {
    let a = require_number!(values, 0);
    let b = require_number!(values, 1);

    match (a, b) {
        (LuaNumber::Integer(a), LuaNumber::Integer(b)) if *a >= 0 && *b >= 0 => {
            Ok(vec![(*a < *b).into()])
        }
        _ => todo!("implement ult for non-trivial inputs"),
    }
}
