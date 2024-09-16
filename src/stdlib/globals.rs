use crate::value::LuaValue;

use miette::miette;

pub(crate) fn assert(input: Vec<LuaValue>) -> miette::Result<LuaValue> {
    let Some(value) = input.first() else {
        return Err(miette::miette!(
            "bad argument #1 to 'assert' (value expected)"
        ));
    };

    if !value {
        match input.get(1) {
            Some(LuaValue::String(s)) => {
                return Err(miette!("{}", String::from_utf8_lossy(s)));
            }
            Some(LuaValue::Number(n)) => {
                return Err(miette!("{}", n));
            }
            Some(value) => {
                return Err(miette!("(error object is a {} value)", value.type_name()));
            }
            _ => return Err(miette!("assertion failed!")),
        }
    }

    Ok(LuaValue::Nil)
}

pub(crate) fn print(input: Vec<LuaValue>) -> miette::Result<LuaValue> {
    for (i, value) in input.into_iter().enumerate() {
        if i != 0 {
            print!("\t");
        }
        print!("{}", value);
    }
    println!();

    Ok(LuaValue::Nil)
}
