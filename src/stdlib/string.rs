use regex::bytes::Regex;
use std::{cmp::Ordering, sync::LazyLock};
use subslice::SubsliceExt;

use crate::{
    error::lua_error,
    macros::{get_number, get_string, require_number, require_string},
    value::{LuaNumber, LuaObject, LuaTable, LuaValue},
    vm::VM,
};

pub static STRING: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut string = LuaTable::new();

    string.insert(
        "byte".into(),
        LuaObject::NativeFunction("byte", byte).into(),
    );
    string.insert(
        "char".into(),
        LuaObject::NativeFunction("char", char).into(),
    );
    string.insert(
        "find".into(),
        LuaObject::NativeFunction("find", find).into(),
    );
    string.insert(
        "format".into(),
        LuaObject::NativeFunction("format", format).into(),
    );
    string.insert("len".into(), LuaObject::NativeFunction("len", len).into());
    string.insert(
        "match".into(),
        LuaObject::NativeFunction("match", r#match).into(),
    );
    string.insert("rep".into(), LuaObject::NativeFunction("rep", rep).into());
    string.insert(
        "reverse".into(),
        LuaObject::NativeFunction("reverse", reverse).into(),
    );
    string.insert("sub".into(), LuaObject::NativeFunction("sub", sub).into());

    string.into()
});

#[derive(Debug)]
enum LuaPattern {
    Plain(Vec<u8>),
    Regex(Regex),
}

fn pattern_to_regex(pattern: &[u8]) -> crate::Result<LuaPattern> {
    let mut regex = String::with_capacity(pattern.len());
    let len = pattern.len();
    let mut chars = pattern.iter().enumerate().peekable();
    let mut any_special = false;
    let mut inside_character_class = false;
    while let Some((i, char)) = chars.next() {
        match char {
            b'%' => {
                any_special = true;

                if i == len - 1 {
                    return Err(lua_error!("malformed pattern (ends with '%')"));
                }

                match chars.next().unwrap().1 {
                    // Character classes
                    b'a' => regex.push_str("[a-zA-Z]"),
                    b'A' => regex.push_str("[^a-zA-Z]"),
                    b'c' => regex.push_str("[\x00-\x1F]"),
                    b'C' => regex.push_str("[^\x00-\x1F]"),
                    b'd' => regex.push_str("\\d"),
                    b'D' => regex.push_str("\\D"),
                    b'g' => regex.push_str("[\x21-\x7E]"),
                    b'G' => regex.push_str("[^\x21-\x7E]"),
                    b'l' => regex.push_str("[a-z]"),
                    b'L' => regex.push_str("[^a-z]"),
                    b'p' => regex.push_str("[[:punct:]]"),
                    b'P' => regex.push_str("[^[:punct:]]"),
                    b's' => regex.push_str("\\s"),
                    b'S' => regex.push_str("\\S"),
                    b'u' => regex.push_str("[A-Z]"),
                    b'U' => regex.push_str("[^A-Z]"),
                    b'w' => regex.push_str("\\w"),
                    b'W' => regex.push_str("\\W"),
                    b'x' => regex.push_str("[0-9a-fA-F]"),
                    b'X' => regex.push_str("[^0-9a-fA-F]"),

                    // Backreferences
                    num @ b'0'..=b'9' => {
                        regex.push('\\');
                        regex.push(*num as char);
                    }

                    // Others
                    b'b' => return Err(lua_error!("unsupported pattern character: '%b'")),
                    b'f' => return Err(lua_error!("unsupported pattern character: '%f'")),

                    // Things we have to escape for regex
                    other @ b'%'
                    | other @ b'.'
                    | other @ b'['
                    | other @ b']'
                    | other @ b'*'
                    | other @ b'+'
                    | other @ b'-'
                    | other @ b'('
                    | other @ b')'
                    | other @ b'?'
                    | other @ b'^'
                    | other @ b'$' => {
                        regex.push('\\');
                        regex.push(*other as char)
                    }

                    other => regex.push(*other as char),
                }
            }

            // Anchors
            b'^' if i == 0 => {
                any_special = true;
                regex.push('^')
            }
            b'$' if i == len - 1 => {
                any_special = true;
                regex.push('$')
            }
            b'^' if !inside_character_class => {
                any_special = true;
                regex.push_str("\\^")
            }
            b'$' => {
                any_special = true;
                regex.push_str("\\$")
            }

            // Translate special characters
            b'-' if !inside_character_class => {
                any_special = true;
                regex.push_str("*?")
            }

            // Special to regex, but not special in Lua
            b'\\' | b'{' | b'}' => {
                regex.push('\\');
                regex.push(*char as char)
            }

            // Some don't need translating, but we just mark them as special
            b'^' | b'(' | b')' | b'.' | b'*' | b'+' | b'?' => {
                any_special = true;
                regex.push(*char as char)
            }
            b'[' => {
                any_special = true;
                regex.push(*char as char);
                inside_character_class = true;
            }
            b']' => {
                any_special = true;
                regex.push(*char as char);
                inside_character_class = false;
            }

            other => regex.push(*other as char),
        }
    }

    if any_special {
        Regex::new(&regex)
            .map(LuaPattern::Regex)
            .map_err(|e| lua_error!("{e}"))
    } else {
        Ok(LuaPattern::Plain(pattern.to_vec()))
    }
}

fn byte(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let s = require_string!(input, "string.byte");
    let i = get_number!(input, "string.byte", 1)
        .unwrap_or(&LuaNumber::Integer(1))
        .integer_repr()?;
    let j = get_number!(input, "string.byte", 2)
        .unwrap_or(&LuaNumber::Integer(i))
        .integer_repr()?;

    let i = match i.cmp(&0) {
        Ordering::Less => s.len() as i64 + i + 1,
        Ordering::Greater => i,
        Ordering::Equal => 1,
    };
    let i = if i < 1 { 1 } else { i };
    let j = match j.cmp(&0) {
        Ordering::Less => s.len() as i64 + j + 1,
        Ordering::Greater if j >= s.len() as i64 => s.len() as i64,
        Ordering::Greater => j,
        Ordering::Equal => return Ok(vec![]),
    };
    let j = if j > s.len() as i64 {
        s.len() as i64
    } else {
        j
    };

    let result: Vec<_> = (i..=j)
        .map(|i| s[i as usize - 1])
        .map(|v| (v as i64).into())
        .collect();
    Ok(if result.is_empty() {
        vec![LuaValue::Nil]
    } else {
        result
    })
}

fn char(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let mut str = Vec::with_capacity(input.len());
    for (i, _) in input.iter().enumerate() {
        let i = require_number!(input, "string.char", i).integer_repr()?;
        if i < u8::MIN as i64 || i > u8::MAX as i64 {
            return Err(lua_error!(
                "bad argument #{i} to 'string.char' (value out of range)",
            ));
        }

        str.push(i as u8);
    }

    Ok(vec![LuaValue::String(str)])
}

fn find(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let s = require_string!(input, "string.find");
    let pattern = require_string!(input, "string.find", 1);
    let init = match input.get(2) {
        Some(LuaValue::Number(LuaNumber::Integer(i))) => *i,
        Some(LuaValue::Number(f @ LuaNumber::Float(_))) => f.integer_repr()?,
        Some(v) => {
            return Err(lua_error!(
                "bad argument #3 to 'find' (number expected, got {type_name})",
                type_name = v.type_name()
            ));
        }
        None => 1,
    };

    let init = match init.cmp(&0) {
        Ordering::Less => s.len() as i64 + init + 1,
        Ordering::Greater => init,
        Ordering::Equal => 1,
    };

    if init > s.len() as i64 + 1 {
        return Ok(vec![LuaValue::Nil]);
    }

    if pattern.is_empty() {
        return Ok(vec![
            LuaValue::Number(LuaNumber::Integer(init)),
            LuaValue::Number(LuaNumber::Integer(init - 1)),
        ]);
    }

    let s = &s[(init - 1) as usize..];

    let plain = input.get(3).is_some_and(|v| v.as_boolean());
    let pattern = if plain {
        LuaPattern::Plain(pattern.clone())
    } else {
        pattern_to_regex(pattern)?
    };
    let start_end = match &pattern {
        LuaPattern::Plain(pattern) => s
            .find(pattern)
            .map(|start| (start as i64, start as i64 + pattern.len() as i64)),
        LuaPattern::Regex(regex) => regex.find(s).map(|m| (m.start() as i64, m.end() as i64)),
    };

    Ok(match start_end {
        Some((start, end)) => {
            vec![
                LuaValue::Number(LuaNumber::Integer(start + init)),
                LuaValue::Number(LuaNumber::Integer(end + init - 1)),
            ]
        }
        None => vec![LuaValue::Nil],
    })
}

fn format(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let mut format_string = require_string!(input, "string.format").clone();

    let mut format_from = 0;
    for v in input.iter().skip(1) {
        let Some((index, format)) =
            format_string
                .iter()
                .enumerate()
                .skip(format_from)
                .find_map(|(i, c)| {
                    if *c == b'%' {
                        match format_string.get(i + 1) {
                            Some(b'%') | None => None,
                            Some(f) => Some((i, *f as char)),
                        }
                    } else {
                        None
                    }
                })
        else {
            break;
        };

        let formatted_value = match format {
            's' => v.to_string(),
            'd' => match v {
                LuaValue::Number(n) => n.to_string(),
                _ => {
                    return Err(lua_error!(
                        "bad argument #{} to 'format' (number expected, got {type_name})",
                        input.iter().position(|x| x == v).unwrap() + 1,
                        type_name = v.type_name()
                    ));
                }
            },
            other => return Err(lua_error!("unsupported format specifier: {:?}", other)),
        };

        format_string.splice(index..index + 2, formatted_value.bytes());
        format_from = index + formatted_value.len();
    }

    Ok(vec![LuaValue::String(format_string)])
}

fn len(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let input = require_string!(input, "string.len");

    Ok(vec![LuaValue::Number(LuaNumber::Integer(
        input.len() as i64
    ))])
}

fn r#match(_: &mut VM, mut input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let s = require_string!(input, "string.match");
    let pattern = require_string!(input, "string.match", 1);
    // TODO: init parameter

    let pattern = pattern_to_regex(pattern)?;
    match pattern {
        LuaPattern::Plain(pattern) => match s.find(&pattern) {
            Some(start) => Ok(vec![LuaValue::String(
                s[start..start + pattern.len()].to_vec(),
            )]),
            None => Ok(vec![LuaValue::Nil]),
        },
        LuaPattern::Regex(regex) => {
            let captures = regex.captures(s);

            match captures {
                Some(captures) => {
                    if let Some(capture) = captures.get(0) {
                        Ok(vec![LuaValue::String(capture.as_bytes().to_vec())])
                    } else {
                        Ok(vec![input.swap_remove(0)])
                    }
                }
                None => Ok(vec![LuaValue::Nil]),
            }
        }
    }
}

fn rep(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let s = require_string!(input, "string.rep");
    let n = require_number!(input, "string.rep", 1).integer_repr()?;
    let sep = get_string!(input, "string.rep", 2)
        .map(|s| s.as_slice())
        .unwrap_or(b"");
    if n < 1 {
        return Ok(vec![LuaValue::String(vec![])]);
    }

    let mut result = s.clone();
    for _ in 1..n {
        result.extend_from_slice(sep);
        result.extend_from_slice(s);
    }

    Ok(vec![LuaValue::String(result)])
}

fn reverse(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let input = require_string!(input, "string.reverse");

    let mut reversed = input.clone();
    reversed.reverse();
    Ok(vec![LuaValue::String(reversed)])
}

fn sub(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let s = require_string!(input, "string.sub");
    let i = require_number!(input, "string.sub", 1).integer_repr()?;
    let j = match input.get(2) {
        Some(LuaValue::Number(LuaNumber::Integer(i))) => *i,
        Some(LuaValue::Number(f @ LuaNumber::Float(_))) => f.integer_repr()?,
        Some(v) => {
            return Err(lua_error!(
                "bad argument #3 to 'string.sub' (number expected, got {type_name})",
                type_name = v.type_name()
            ));
        }
        None => -1,
    };

    let i = match i.cmp(&0) {
        Ordering::Less => s.len() as i64 + i + 1,
        Ordering::Greater => i,
        Ordering::Equal => 1,
    };
    let i = if i < 1 { 1 } else { i };
    let j = match j.cmp(&0) {
        Ordering::Less => s.len() as i64 + j + 1,
        Ordering::Greater if j >= s.len() as i64 => s.len() as i64,
        Ordering::Greater => j,
        Ordering::Equal => return Ok(vec![LuaValue::String(vec![])]),
    };
    let j = if j > s.len() as i64 {
        s.len() as i64
    } else {
        j
    };

    if i > j {
        return Ok(vec![LuaValue::String(vec![])]);
    }

    Ok(vec![LuaValue::String(
        s[(i - 1) as usize..=(j - 1) as usize].to_vec(),
    )])
}
