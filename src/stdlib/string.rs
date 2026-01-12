use regex::bytes::Regex;
use std::{cmp::Ordering, sync::LazyLock};
use subslice::SubsliceExt;

use crate::{
    error::lua_error,
    macros::{get_number, get_string, require_number, require_string},
    stdlib::utils::pack::{
        PackOption, PackOptionItem, copy_bytes_fix_endian, get_pack_option, pack_int, unpack_int,
    },
    value::{LuaNumber, LuaObject, LuaString, LuaTable, LuaValue},
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
        "lower".into(),
        LuaObject::NativeFunction("lower", lower).into(),
    );
    string.insert(
        "match".into(),
        LuaObject::NativeFunction("match", r#match).into(),
    );
    string.insert("rep".into(), LuaObject::NativeFunction("rep", rep).into());
    string.insert(
        "reverse".into(),
        LuaObject::NativeFunction("reverse", reverse).into(),
    );
    string.insert(
        "pack".into(),
        LuaObject::NativeFunction("pack", pack).into(),
    );
    string.insert(
        "packsize".into(),
        LuaObject::NativeFunction("packsize", packsize).into(),
    );
    string.insert("sub".into(), LuaObject::NativeFunction("sub", sub).into());
    string.insert(
        "unpack".into(),
        LuaObject::NativeFunction("unpack", unpack).into(),
    );
    string.insert(
        "upper".into(),
        LuaObject::NativeFunction("upper", upper).into(),
    );

    string.into()
});

#[derive(Debug)]
enum LuaPattern {
    Plain(LuaString),
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
        Ok(LuaPattern::Plain(pattern.into()))
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

    Ok(vec![LuaValue::String(str.into())])
}

fn find(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let s = require_string!(input, "string.find");
    let pattern = require_string!(input, "string.find", 1);
    let init = match input.get(2) {
        Some(LuaValue::Number(LuaNumber::Integer(i))) => *i,
        Some(LuaValue::Number(f @ LuaNumber::Float(_))) => f.integer_repr()?,
        Some(LuaValue::Nil) | None => 1,
        Some(v) => {
            return Err(lua_error!(
                "bad argument #3 to 'find' (number expected, got {type_name})",
                type_name = v.type_name()
            ));
        }
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

fn lower(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let input = require_string!(input, "string.lower");
    let string = String::from_utf8_lossy(input);
    let lower = string.to_lowercase();

    Ok(vec![LuaValue::String(lower.into_bytes().into())])
}

fn r#match(_: &mut VM, mut input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let s = require_string!(input, "string.match");
    let pattern = require_string!(input, "string.match", 1);
    // TODO: init parameter

    let pattern = pattern_to_regex(pattern)?;
    match pattern {
        LuaPattern::Plain(pattern) => match s.find(&pattern) {
            Some(start) => Ok(vec![LuaValue::String(
                s[start..start + pattern.len()].into(),
            )]),
            None => Ok(vec![LuaValue::Nil]),
        },
        LuaPattern::Regex(regex) => {
            let captures = regex.captures(s);

            match captures {
                Some(captures) => {
                    if let Some(capture) = captures.get(0) {
                        Ok(vec![LuaValue::String(capture.as_bytes().into())])
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
        return Ok(vec![LuaValue::String(LuaString::new())]);
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

fn pack(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let fmt = require_string!(input, "string.pack");
    let mut result = LuaString::new();
    let mut max_alignment = 1;
    let mut is_little_endian = cfg!(target_endian = "little");

    let mut chars = fmt.iter().peekable();
    let mut param = 1;

    loop {
        let PackOptionItem {
            option,
            size,
            align,
        } = get_pack_option(&mut chars, result.len(), max_alignment)?;

        if align > 0 {
            result.extend(std::iter::repeat_n(0, align));
        }

        match option {
            PackOption::SignedInt => {
                let number = require_number!(input, "string.pack", param);
                param += 1;
                let value = number.integer_repr()?;

                if size < size_of_val(&value) {
                    // need to check for overflow
                    let limit = 1i64 << (size * 8 - 1);
                    if value < -limit || value >= limit {
                        return Err(lua_error!(
                            "bad argument #{param} to 'string.pack' (integer overflow)",
                        ));
                    }
                }

                result.extend(pack_int(
                    value.cast_unsigned(),
                    is_little_endian,
                    size,
                    value < 0,
                ));
            }
            PackOption::UnsignedInt => {
                let number = require_number!(input, "string.pack", param);
                param += 1;
                let value = number.integer_repr()?;

                if size < size_of_val(&value) {
                    // need to check for overflow
                    let limit = 1i64 << (size * 8);
                    if value < 0 || value >= limit {
                        return Err(lua_error!(
                            "bad argument #{param} to 'string.pack' (unsigned overflow)",
                        ));
                    }
                }

                result.extend(pack_int(value as u64, is_little_endian, size, false));
            }
            PackOption::Float => {
                let number = require_number!(input, "string.pack", param);
                param += 1;
                let float = f32::from(number.clone());
                let mut buf = [0; size_of::<f32>()];
                copy_bytes_fix_endian(&mut buf, &float.to_ne_bytes(), is_little_endian);
                result.extend(buf);
            }
            PackOption::LuaNumber | PackOption::Double => {
                let number = require_number!(input, "string.pack", param);
                param += 1;
                let double = f64::from(number.clone());
                let mut buf = [0; size_of::<f64>()];
                copy_bytes_fix_endian(&mut buf, &double.to_ne_bytes(), is_little_endian);
                result.extend(buf);
            }
            PackOption::FixedString => {
                let string = require_string!(input, "string.pack", param);
                param += 1;
                if string.len() > size {
                    return Err(lua_error!(
                        "bad argument #{param} to 'string.pack' (string longer than given size)"
                    ));
                }
                // Option 'c' is not aligned
                let padding = size - string.len();
                result.extend_from_slice(string);
                result.extend(std::iter::repeat_n(0, padding));
            }
            PackOption::String => {
                let string = require_string!(input, "string.pack", param);
                param += 1;
                let len = string.len();
                if size < size_of_val(&len) && len >= 1 << (size * 8) {
                    return Err(lua_error!(
                        "bad argument #{param} to 'string.pack' (string length does not fit in given size)",
                    ));
                }
                result.extend(pack_int(len as u64, is_little_endian, size, false));
                result.extend_from_slice(string);
            }
            PackOption::Zstr => {
                let string = require_string!(input, "string.pack", param);
                if string.contains(&0) {
                    return Err(lua_error!(
                        "bad argument #{param} to 'string.pack' (string contains zeros)",
                    ));
                }
                param += 1;
                // Option 'z' is not aligned
                result.extend_from_slice(string);
                result.push(0);
            }
            PackOption::Padding => {
                result.push(0);
            }
            PackOption::PaddingAlign => {}
            PackOption::LittleEndian => {
                is_little_endian = true;
            }
            PackOption::BigEndian => {
                is_little_endian = false;
            }
            PackOption::NativeEndian => {
                is_little_endian = cfg!(target_endian = "little");
            }
            PackOption::SetAlignment { value } => {
                max_alignment = value;
            }
            PackOption::Nop => {}
            PackOption::Eof => break,
        }
    }

    Ok(vec![LuaValue::String(result)])
}

fn packsize(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let fmt = require_string!(input, "string.packsize");
    let mut result = 0;
    let mut max_alignment = 1;

    let mut chars = fmt.iter().peekable();
    let mut item = 0;
    loop {
        let PackOptionItem {
            option,
            size,
            align,
        } = get_pack_option(&mut chars, result, max_alignment)?;

        result += align;

        match option {
            PackOption::Zstr => {
                return Err(lua_error!(
                    "'z' option not supported in 'string.packsize': variable-length format"
                ));
            }
            PackOption::String => {
                return Err(lua_error!(
                    "'s' option not supported in 'string.packsize': variable-length format"
                ));
            }
            PackOption::Eof => break,
            PackOption::SetAlignment { value } => {
                max_alignment = value;
            }
            _ => {
                if result > LuaValue::MAX_STRING_LENGTH - size {
                    return Err(lua_error!(
                        "format result too large (item={item}, result={result}, max={}, adding {size}",
                        LuaValue::MAX_STRING_LENGTH
                    ));
                }

                result += size;
            }
        }

        item += 1;
    }

    Ok(vec![LuaValue::Number(LuaNumber::Integer(result as i64))])
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
        Ordering::Equal => return Ok(vec![LuaValue::String(LuaString::new())]),
    };
    let j = if j > s.len() as i64 {
        s.len() as i64
    } else {
        j
    };

    if i > j {
        return Ok(vec![LuaValue::String(LuaString::new())]);
    }

    Ok(vec![LuaValue::String(
        s[(i - 1) as usize..=(j - 1) as usize].into(),
    )])
}

fn unpack(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let fmt = require_string!(input, "string.unpack");
    let s = require_string!(input, "string.unpack", 1);
    let mut pos = get_number!(input, "string.unpack", 2)
        .map(|v| v.integer_repr())
        .transpose()?
        .map(|n| match n.cmp(&0) {
            Ordering::Less => s.len() as i64 + n + 1,
            Ordering::Greater => n,
            Ordering::Equal => 1,
        } as usize)
        .unwrap_or(1) as usize
        - 1;

    if pos > s.len() {
        return Err(lua_error!(
            "bad argument #2 to 'string.unpack' (initial position out of string)",
        ));
    }

    let mut result = vec![];
    let mut is_little_endian = cfg!(target_endian = "little");
    let mut max_alignment = 1;

    macro_rules! align {
        ($size:expr) => {
            if $size > 1 {
                let mut align = $size;
                if align > max_alignment {
                    align = max_alignment;
                }
                if align & (align - 1) != 0 {
                    return Err(lua_error!("format asks for alignment not power of 2"));
                }

                align = (align - (pos & (align - 1))) & (align - 1);
                if pos + align > s.len() {
                    return Err(lua_error!(
                        "bad argument #2 to 'string.unpack' (data string too short)",
                    ));
                }

                pos += align;
            }
        };
    }

    let mut chars = fmt.iter().peekable();
    loop {
        let PackOptionItem {
            option,
            size,
            align,
        } = get_pack_option(&mut chars, pos, max_alignment)?;

        pos += align;

        match option {
            PackOption::SignedInt => {
                align!(size);
                if pos + size > s.len() {
                    return Err(lua_error!(
                        "bad argument #2 to 'string.unpack' (data string too short)",
                    ));
                }

                result.push(unpack_int(&s[pos..pos + size], is_little_endian, size, true)?.into());
                pos += size;
            }
            PackOption::UnsignedInt => {
                align!(size);
                if pos + size > s.len() {
                    return Err(lua_error!(
                        "bad argument #2 to 'string.unpack' (data string too short)",
                    ));
                }

                result.push(unpack_int(&s[pos..pos + size], is_little_endian, size, false)?.into());
                pos += size;
            }
            PackOption::Float => {
                align!(size);
                if pos + size > s.len() {
                    return Err(lua_error!(
                        "bad argument #2 to 'string.unpack' (data string too short)",
                    ));
                }

                let mut buff = [0u8; size_of::<f32>()];
                copy_bytes_fix_endian(&mut buff, &s[pos..], is_little_endian);
                let float = f32::from_ne_bytes(buff);
                result.push(LuaNumber::Float(float as f64).into());
                pos += size;
            }
            PackOption::Double | PackOption::LuaNumber => {
                align!(size);
                if pos + size > s.len() {
                    return Err(lua_error!(
                        "bad argument #2 to 'string.unpack' (data string too short)",
                    ));
                }

                let mut buff = [0u8; size_of::<f64>()];
                copy_bytes_fix_endian(&mut buff, &s[pos..], is_little_endian);
                let float = f64::from_ne_bytes(buff);
                result.push(LuaNumber::Float(float).into());
                pos += size;
            }
            PackOption::FixedString => {
                // 'c' is not aligned
                if pos + size > s.len() {
                    return Err(lua_error!(
                        "bad argument #2 to 'string.unpack' (data string too short)",
                    ));
                }

                result.push(LuaValue::String(s[pos..pos + size].into()));
                pos += size;
            }
            PackOption::String => {
                align!(size);
                if pos + size > s.len() {
                    return Err(lua_error!(
                        "bad argument #2 to 'string.unpack' (data string too short)",
                    ));
                }

                let len = unpack_int(&s[pos..pos + size], is_little_endian, size, false)? as usize;
                pos += size;
                if pos + len > s.len() {
                    return Err(lua_error!(
                        "bad argument #2 to 'string.unpack' (data string too short)",
                    ));
                }

                let string = Vec::from(&s[pos..pos + len]);
                result.push(LuaValue::String(string.into()));
                pos += len;
            }
            PackOption::Zstr => {
                // 'z' is not aligned
                let Some(len) = s[pos..].iter().position(|&c| c == 0) else {
                    return Err(lua_error!(
                        "bad argument #2 to 'string.unpack' (unfinished string for format 'z')",
                    ));
                };

                let string = Vec::from(&s[pos..pos + len]);
                result.push(LuaValue::String(string.into()));
                // skip past string and the null terminator
                pos += len + 1;
            }
            PackOption::LittleEndian => {
                is_little_endian = true;
            }
            PackOption::BigEndian => {
                is_little_endian = false;
            }
            PackOption::NativeEndian => {
                is_little_endian = cfg!(target_endian = "little");
            }
            PackOption::SetAlignment { value } => {
                max_alignment = value;
            }
            PackOption::Padding | PackOption::PaddingAlign => {
                pos += size;
            }
            PackOption::Nop => {}
            PackOption::Eof => break,
        }
    }

    result.push((pos as i64 + 1).into());
    Ok(result)
}

fn upper(_: &mut VM, input: Vec<LuaValue>) -> crate::Result<Vec<LuaValue>> {
    let input = require_string!(input, "string.upper");
    let string = String::from_utf8_lossy(input);
    let upper = string.to_uppercase();

    Ok(vec![LuaValue::String(upper.into_bytes().into())])
}
