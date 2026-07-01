use regex::bytes::Regex;
use std::{cmp::Ordering, sync::LazyLock};
use subslice::SubsliceExt;

use crate::{
    error::lua_error,
    macros::{lua_return, native_function, require_userdata},
    state::LuaState,
    stdlib::utils::pack::{
        PackOption, PackOptionItem, copy_bytes_fix_endian, get_pack_option, pack_int, unpack_int,
    },
    value::{IntegerRepr, LuaNumber, LuaObject, LuaString, LuaTable, LuaValue, UserData},
};

pub static STRING: LazyLock<LuaValue> = LazyLock::new(|| {
    let mut string = LuaTable::new();

    string.insert("byte".into(), native_function!("string"; byte));
    string.insert("char".into(), native_function!("string"; char));
    string.insert("find".into(), native_function!("string"; find));
    string.insert("format".into(), native_function!("string"; format));
    string.insert("gmatch".into(), native_function!("string"; gmatch));
    string.insert("len".into(), native_function!("string"; len));
    string.insert("lower".into(), native_function!("string"; lower));
    string.insert("match".into(), native_function!("string"; "match", r#match));
    string.insert("rep".into(), native_function!("string"; rep));
    string.insert("reverse".into(), native_function!("string"; reverse));
    string.insert("pack".into(), native_function!("string"; pack));
    string.insert("packsize".into(), native_function!("string"; packsize));
    string.insert("sub".into(), native_function!("string"; sub));
    string.insert("unpack".into(), native_function!("string"; unpack));
    string.insert("upper".into(), native_function!("string"; upper));

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
                    other @ (b'%' | b'.' | b'[' | b']' | b'*' | b'+' | b'-' | b'(' | b')'
                    | b'?' | b'^' | b'$') => {
                        regex.push('\\');
                        regex.push(*other as char);
                    }

                    other => regex.push(*other as char),
                }
            }

            // Anchors
            b'^' if i == 0 => {
                any_special = true;
                regex.push('^');
            }
            b'$' if i == len - 1 => {
                any_special = true;
                regex.push('$');
            }
            b'^' if !inside_character_class => {
                any_special = true;
                regex.push_str("\\^");
            }
            b'$' => {
                any_special = true;
                regex.push_str("\\$");
            }

            // Translate special characters
            b'-' if !inside_character_class => {
                any_special = true;
                regex.push_str("*?");
            }

            // Special to regex, but not special in Lua
            b'\\' | b'{' | b'}' => {
                regex.push('\\');
                regex.push(*char as char);
            }

            // Some don't need translating, but we just mark them as special
            b'^' | b'(' | b')' | b'.' | b'*' | b'+' | b'?' => {
                any_special = true;
                regex.push(*char as char);
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

fn byte(state: &mut LuaState) -> crate::Result<usize> {
    let s = state.arg_as_string(0)?;
    let i = state.try_arg_as_integer(1)?.unwrap_or(1);
    let j = state.try_arg_as_integer(2)?.unwrap_or(i);

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
        Ordering::Equal => lua_return!(state),
    };
    let j = if j > s.len() as i64 {
        s.len() as i64
    } else {
        j
    };

    if j < i {
        lua_return!(state)
    }

    for (r, i) in (i..=j).enumerate() {
        state.set(r, (s[i as usize - 1] as i64).into());
    }
    Ok((j - i + 1) as usize)
}

fn char(state: &mut LuaState) -> crate::Result<usize> {
    let mut str = Vec::with_capacity(state.num_args());
    for i in 0..state.num_args() {
        let i = state.arg_as_integer(i)?;
        if i < i64::from(u8::MIN) || i > i64::from(u8::MAX) {
            return Err(lua_error!(
                "bad argument #{i} to 'string.char' (value out of range)",
            ));
        }

        #[allow(clippy::cast_possible_truncation)]
        str.push(i as u8);
    }

    lua_return!(state; LuaValue::from_string(str))
}

fn find(state: &mut LuaState) -> crate::Result<usize> {
    let s = state.arg_as_string(0)?;
    let pattern = state.arg_as_string(1)?;
    let init = match state.arg(2) {
        v if v.is_integer() => unsafe { v.as_integer() },
        v if v.is_float() => unsafe { v.as_number() }.integer_repr()?,
        v if v.is_nil() => 1,
        v => {
            return state.arg_err(3, format!("number expected, got {}", v.type_name()));
        }
    };

    let init = match init.cmp(&0) {
        Ordering::Less => s.len() as i64 + init + 1,
        Ordering::Greater => init,
        Ordering::Equal => 1,
    };

    if init > s.len() as i64 + 1 {
        lua_return!(state; LuaValue::NIL)
    }

    if pattern.is_empty() {
        lua_return!(state;
            LuaValue::from_integer(init),
            LuaValue::from_integer(init - 1),
        );
    }

    let s = &s[(init - 1) as usize..];

    let plain = state.try_arg(3).is_some_and(|v| v.cast_to_bool());
    let pattern = if plain {
        LuaPattern::Plain(pattern.clone())
    } else {
        pattern_to_regex(&pattern)?
    };
    let start_end = match &pattern {
        LuaPattern::Plain(pattern) => s
            .find(pattern)
            .map(|start| (start as i64, start as i64 + pattern.len() as i64)),
        LuaPattern::Regex(regex) => regex.find(s).map(|m| (m.start() as i64, m.end() as i64)),
    };

    if let Some((start, end)) = start_end {
        lua_return!(state; LuaValue::from_integer(start + init), LuaValue::from_integer(end + init - 1));
    }
    lua_return!(state)
}

#[derive(Debug)]
struct GmatchState {
    s: LuaString,
    pattern: LuaPattern,
    pos: usize,
    last_match_empty: bool,
}

fn gmatch(state: &mut LuaState) -> crate::Result<usize> {
    let s = state.arg_as_string(0)?;
    let pattern_str = state.arg_as_string(1)?;
    let init = match state.arg(2) {
        v if v.is_integer() => unsafe { v.as_integer() },
        v if v.is_float() => unsafe { v.as_number() }.integer_repr()?,
        v if v.is_nil() => 1,
        v => {
            return Err(lua_error!(
                "bad argument #3 to 'gmatch' (number expected, got {type_name})",
                type_name = v.type_name()
            ));
        }
    };

    let init = match init.cmp(&0) {
        Ordering::Less => (s.len() as i64 + init + 1).max(1) as usize,
        Ordering::Greater => init as usize,
        Ordering::Equal => 1,
    };

    let pattern_str = if pattern_str.first() == Some(&b'^') {
        &pattern_str[1..]
    } else {
        &pattern_str
    };
    let pattern = pattern_to_regex(pattern_str)?;

    let gmatch_state = GmatchState {
        pos: (init - 1).min(s.len()),
        s,
        pattern,
        last_match_empty: false,
    };

    lua_return!(state;
        native_function!(gmatch_iter),
        UserData::new_light(gmatch_state).into(),
        LuaValue::NIL,
    )
}

fn gmatch_iter(state: &mut LuaState) -> crate::Result<usize> {
    require_userdata!(write, state.try_arg(0), "gmatch_iter", 0, userdata, {
        let UserData::Light(data) = userdata else {
            lua_return!(state);
        };
        let gmatch_state = if let Some(s) = data.write().unwrap().downcast_mut::<GmatchState>() {
            std::ptr::from_mut(s)
        } else {
            lua_return!(state)
        };

        // SAFETY: we hold the write lock on the Arc<RwLock<..>> for the duration of this
        // block (the require_userdata macro holds it), so exclusive access is guaranteed.
        let gmatch_state = unsafe { &mut *gmatch_state };

        loop {
            if gmatch_state.pos > gmatch_state.s.len() {
                lua_return!(state)
            }

            let haystack = &gmatch_state.s[gmatch_state.pos..];

            match &gmatch_state.pattern {
                LuaPattern::Plain(pattern) => {
                    if let Some(offset) = haystack.find(pattern) {
                        let abs_start = gmatch_state.pos + offset;
                        let abs_end = abs_start + pattern.len();
                        if pattern.is_empty() {
                            if gmatch_state.last_match_empty {
                                gmatch_state.pos = abs_end + 1;
                                gmatch_state.last_match_empty = false;
                                continue;
                            }
                            gmatch_state.last_match_empty = true;
                        } else {
                            gmatch_state.last_match_empty = false;
                        }
                        gmatch_state.pos = abs_end;
                        lua_return!(state; LuaValue::from_string(&gmatch_state.s[abs_start..abs_end]))
                    }
                    lua_return!(state)
                }
                LuaPattern::Regex(regex) => {
                    let captures = regex.captures(haystack);

                    if let Some(captures) = captures {
                        let full_match = captures.get(0).unwrap();
                        let abs_end = gmatch_state.pos + full_match.end();

                        if full_match.as_bytes().is_empty() {
                            if gmatch_state.last_match_empty {
                                gmatch_state.pos += 1;
                                gmatch_state.last_match_empty = false;
                                continue;
                            }
                            gmatch_state.last_match_empty = true;
                        } else {
                            gmatch_state.last_match_empty = false;
                        }

                        gmatch_state.pos = abs_end;

                        let has_groups = regex.captures_len() > 1;
                        if has_groups {
                            for i in 1..regex.captures_len() {
                                match captures.get(i) {
                                    Some(m) => {
                                        state.set(i - 1, LuaValue::from_string(m.as_bytes()));
                                    }
                                    None => state.set(i - 1, LuaValue::NIL),
                                }
                            }
                            return Ok(
                                regex.captures_len() - 1
                            );
                        }
                        lua_return!(state; LuaValue::from_string(full_match.as_bytes()))
                    }
                    lua_return!(state)
                }
            }
        }
    })
}

fn format(state: &mut LuaState) -> crate::Result<usize> {
    let mut format_string = state.arg_as_string(0)?;

    let mut format_from = 0;
    for i in 1..state.num_args() {
        let v = state.arg(i);
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
            'd' => {
                if v.is_number() {
                    unsafe { v.as_number() }.to_string()
                } else {
                    return Err(lua_error!(
                        "bad argument #{} to 'format' (number expected, got {type_name})",
                        i + 1,
                        type_name = v.type_name()
                    ));
                }
            }
            'p' => format!("{v:012p}", v = &v),
            other => return Err(lua_error!("unsupported format specifier: {:?}", other)),
        };

        format_string.splice(index..index + 2, formatted_value.bytes());
        format_from = index + formatted_value.len();
    }

    lua_return!(state; LuaValue::from_string(format_string))
}

fn len(state: &mut LuaState) -> crate::Result<usize> {
    let input = state.arg_as_string(0)?;

    lua_return!(state; LuaValue::from_integer(input.len() as i64))
}

fn lower(state: &mut LuaState) -> crate::Result<usize> {
    let input = state.arg_as_string(0)?;
    let string = String::from_utf8_lossy(&input);
    let lower = string.to_lowercase();

    lua_return!(state; LuaValue::from_string(lower.into_bytes()))
}

fn r#match(state: &mut LuaState) -> crate::Result<usize> {
    let s = state.arg_as_string(0)?;
    let pattern = state.arg_as_string(1)?;
    // TODO: init parameter

    let pattern = pattern_to_regex(&pattern)?;
    match pattern {
        LuaPattern::Plain(pattern) => {
            if let Some(start) = s.find(&pattern) {
                lua_return!(state; LuaValue::from_string(&s[start..start + pattern.len()]))
            }
            lua_return!(state)
        }
        LuaPattern::Regex(regex) => {
            let captures = regex.captures(&s);

            if let Some(captures) = captures {
                if let Some(capture) = captures.get(0) {
                    lua_return!(state; LuaValue::from_string(capture.as_bytes()))
                }
                lua_return!(state; s.into())
            }
            lua_return!(state)
        }
    }
}

fn rep(state: &mut LuaState) -> crate::Result<usize> {
    let s = state.arg_as_string(0)?;
    let n = state.arg_as_integer(1)?;
    let sep = state.try_arg_as_string(2)?;
    let sep = sep.as_ref().map_or(b"".as_slice(), |s| s.as_slice());
    if n < 1 {
        lua_return!(state; LuaValue::from_string(LuaString::new()))
    }

    let mut result = s.clone();
    for _ in 1..n {
        result.extend_from_slice(sep);
        result.extend_from_slice(&s);
    }

    lua_return!(state; LuaValue::from_string(result))
}

fn reverse(state: &mut LuaState) -> crate::Result<usize> {
    let input = state.arg_as_string(0)?;

    let mut reversed = input.clone();
    reversed.reverse();
    lua_return!(state; LuaValue::from_string(reversed))
}

fn pack(state: &mut LuaState) -> crate::Result<usize> {
    let fmt = state.arg_as_string(0)?;
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
                let value = state.arg_as_integer(param)?;
                param += 1;

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
                let value = state.arg_as_integer(param)?;
                param += 1;

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
                let number = state.arg_as_number(param)?;
                param += 1;
                let float = f32::from(number.clone());
                let mut buf = [0; size_of::<f32>()];
                copy_bytes_fix_endian(&mut buf, &float.to_ne_bytes(), is_little_endian);
                result.extend(buf);
            }
            PackOption::LuaNumber | PackOption::Double => {
                let number = state.arg_as_number(param)?;
                param += 1;
                let double = f64::from(number.clone());
                let mut buf = [0; size_of::<f64>()];
                copy_bytes_fix_endian(&mut buf, &double.to_ne_bytes(), is_little_endian);
                result.extend(buf);
            }
            PackOption::FixedString => {
                let string = state.arg_as_string(param)?;
                param += 1;
                if string.len() > size {
                    return Err(lua_error!(
                        "bad argument #{param} to 'string.pack' (string longer than given size)"
                    ));
                }
                // Option 'c' is not aligned
                let padding = size - string.len();
                result.extend_from_slice(&string);
                result.extend(std::iter::repeat_n(0, padding));
            }
            PackOption::String => {
                let string = state.arg_as_string(param)?;
                param += 1;
                let len = string.len();
                if size < size_of_val(&len) && len >= 1 << (size * 8) {
                    return Err(lua_error!(
                        "bad argument #{param} to 'string.pack' (string length does not fit in given size)",
                    ));
                }
                result.extend(pack_int(len as u64, is_little_endian, size, false));
                result.extend_from_slice(&string);
            }
            PackOption::Zstr => {
                let string = state.arg_as_string(param)?;
                if string.contains(&0) {
                    return Err(lua_error!(
                        "bad argument #{param} to 'string.pack' (string contains zeros)",
                    ));
                }
                param += 1;
                // Option 'z' is not aligned
                result.extend_from_slice(&string);
                result.push(0);
            }
            PackOption::Padding => {
                result.push(0);
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
            PackOption::PaddingAlign | PackOption::Nop => {}
            PackOption::Eof => break,
        }
    }

    lua_return!(state; LuaValue::from_string(result))
}

fn packsize(state: &mut LuaState) -> crate::Result<usize> {
    let fmt = state.arg_as_string(0)?;
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

    lua_return!(state; LuaValue::from_integer(result as i64))
}

fn sub(state: &mut LuaState) -> crate::Result<usize> {
    let s = state.arg_as_string(0)?;
    let i = state.arg_as_integer(1)?;
    let j = match state.try_arg(2) {
        Some(v) if v.is_integer() => unsafe { v.as_integer() },
        Some(v) if v.is_float() => unsafe { v.as_number() }.integer_repr()?,
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
        Ordering::Equal => lua_return!(state; LuaValue::from_string(LuaString::new())),
    };
    let j = if j > s.len() as i64 {
        s.len() as i64
    } else {
        j
    };

    if i > j {
        lua_return!(state; LuaValue::from_string(LuaString::new()))
    }

    lua_return!(state; LuaValue::from_string(
        &s[(i - 1) as usize..=(j - 1) as usize],
    ))
}

fn unpack(state: &mut LuaState) -> crate::Result<usize> {
    let fmt = state.arg_as_string(0)?;
    let s = state.arg_as_string(1)?;
    let mut pos = state.try_arg_as_integer(2)?.map_or(1, |n| match n.cmp(&0) {
        Ordering::Less => s.len() as i64 + n + 1,
        Ordering::Greater => n,
        Ordering::Equal => 1,
    } as usize)
        - 1;

    if pos > s.len() {
        return Err(lua_error!(
            "bad argument #2 to 'string.unpack' (initial position out of string)",
        ));
    }

    let mut result_index: usize = 0;
    macro_rules! result(
        ($value:expr) => {{
            state.set(result_index, $value);
            result_index += 1;
        }};
    );
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

                result!(unpack_int(&s[pos..pos + size], is_little_endian, size, true)?.into());
                pos += size;
            }
            PackOption::UnsignedInt => {
                align!(size);
                if pos + size > s.len() {
                    return Err(lua_error!(
                        "bad argument #2 to 'string.unpack' (data string too short)",
                    ));
                }

                result!(unpack_int(&s[pos..pos + size], is_little_endian, size, false)?.into());
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
                result!(LuaNumber::Float(float as f64).into());
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
                result!(LuaNumber::Float(float).into());
                pos += size;
            }
            PackOption::FixedString => {
                // 'c' is not aligned
                if pos + size > s.len() {
                    return Err(lua_error!(
                        "bad argument #2 to 'string.unpack' (data string too short)",
                    ));
                }

                result!(LuaValue::from_string(&s[pos..pos + size]));
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
                result!(LuaValue::from_string(string));
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
                result!(LuaValue::from_string(string));
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

    result!((pos as i64 + 1).into());
    Ok(result_index)
}

fn upper(state: &mut LuaState) -> crate::Result<usize> {
    let input = state.arg_as_string(0)?;
    let string = String::from_utf8_lossy(&input);
    let upper = string.to_uppercase();

    lua_return!(state; LuaValue::from_string(upper.into_bytes()))
}
