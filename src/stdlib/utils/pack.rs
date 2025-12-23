use std::{iter::Peekable, slice::Iter};

use crate::{error::lua_error, value::LuaValue};

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) enum PackOption {
    /// a signed integer with a given amount of bytes
    SignedInt,
    /// an unsigned integer with a given amount of bytes
    UnsignedInt,
    /// a float (f32; native size)
    Float,
    /// a double (f64; native size)
    Double,
    /// a lua number
    LuaNumber,
    /// a fixed-size string (of "size" bytes)
    FixedString,
    /// a string preceded by its length coded as an unsigned integer with n bytes (default is a
    /// usize)
    String,
    /// a zero-terminated string
    Zstr,
    /// one byte of padding
    Padding,
    /// align according to the following option (which is otherwise ignored)
    PaddingAlign,
    /// set to little endian
    LittleEndian,
    /// set to big endian
    BigEndian,
    /// set to native endian
    NativeEndian,
    /// set maximum alignment
    SetAlignment { value: usize },
    /// no-op, don't do anything
    Nop,
    /// reached the end of the format string
    Eof,
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct PackOptionItem {
    pub(crate) option: PackOption,
    pub(crate) size: usize,
    /// Number of bytes needed to align.
    pub(crate) align: usize,
}

impl PackOptionItem {
    fn new(
        option: PackOption,
        size: usize,
        position: usize,
        max_alignment: usize,
        fmt: &mut Peekable<Iter<'_, u8>>,
    ) -> crate::Result<Self> {
        // Usually, alignment follows size.
        let mut align = size;
        if option == PackOption::PaddingAlign {
            let mut valid = true;
            if fmt.peek() == Some(&&0) {
                valid = false;
            }
            if valid {
                let next_option = get_pack_option(fmt, position, max_alignment)?;
                align = next_option.size;
                if next_option.option == PackOption::FixedString || align == 0 {
                    valid = false;
                }
            }
            if !valid {
                return Err(lua_error!("invalid next option for option 'X'"));
            }
        }

        // fixed string needs no alignment
        if align <= 1 || option == PackOption::FixedString {
            align = 0;
        } else {
            align = align.min(max_alignment);

            // is it a power of two?
            if align & (align - 1) != 0 {
                return Err(lua_error!(
                    "format asks for alignment not power of 2 (found {align})"
                ));
            }

            align = (align - (position & (align - 1))) & (align - 1);
        }

        Ok(Self {
            option,
            size,
            align,
        })
    }
}

pub(crate) fn get_pack_option(
    fmt: &mut Peekable<Iter<'_, u8>>,
    position: usize,
    max_alignment: usize,
) -> crate::Result<PackOptionItem> {
    let Some(&c) = fmt.next() else {
        return PackOptionItem::new(PackOption::Eof, 0, position, max_alignment, fmt);
    };

    Ok(match c {
        b'<' => PackOptionItem::new(PackOption::LittleEndian, 0, position, max_alignment, fmt)?,
        b'>' => PackOptionItem::new(PackOption::BigEndian, 0, position, max_alignment, fmt)?,
        b'=' => PackOptionItem::new(PackOption::NativeEndian, 0, position, max_alignment, fmt)?,
        b'!' => {
            const NATIVE_ALIGNMENT: usize = align_of::<usize>();

            PackOptionItem::new(
                PackOption::SetAlignment {
                    value: get_pack_num_limit(fmt, NATIVE_ALIGNMENT)?,
                },
                0,
                position,
                max_alignment,
                fmt,
            )?
        }
        b'b' => PackOptionItem::new(
            PackOption::SignedInt,
            size_of::<i8>(),
            position,
            max_alignment,
            fmt,
        )?,
        b'B' => PackOptionItem::new(
            PackOption::UnsignedInt,
            size_of::<u8>(),
            position,
            max_alignment,
            fmt,
        )?,
        b'h' => PackOptionItem::new(
            PackOption::SignedInt,
            size_of::<i16>(),
            position,
            max_alignment,
            fmt,
        )?,
        b'H' => PackOptionItem::new(
            PackOption::UnsignedInt,
            size_of::<u16>(),
            position,
            max_alignment,
            fmt,
        )?,
        b'l' => PackOptionItem::new(
            PackOption::SignedInt,
            size_of::<i64>(),
            position,
            max_alignment,
            fmt,
        )?,
        b'L' => PackOptionItem::new(
            PackOption::UnsignedInt,
            size_of::<u64>(),
            position,
            max_alignment,
            fmt,
        )?,
        b'j' => PackOptionItem::new(
            PackOption::SignedInt,
            size_of::<i64>(),
            position,
            max_alignment,
            fmt,
        )?,
        b'J' => PackOptionItem::new(
            PackOption::UnsignedInt,
            size_of::<i64>(),
            position,
            max_alignment,
            fmt,
        )?,
        b'T' => PackOptionItem::new(
            PackOption::UnsignedInt,
            size_of::<usize>(),
            position,
            max_alignment,
            fmt,
        )?,
        b'f' => PackOptionItem::new(
            PackOption::Float,
            size_of::<f32>(),
            position,
            max_alignment,
            fmt,
        )?,
        b'n' => PackOptionItem::new(
            PackOption::LuaNumber,
            size_of::<f64>(),
            position,
            max_alignment,
            fmt,
        )?,
        b'd' => PackOptionItem::new(
            PackOption::Double,
            size_of::<f64>(),
            position,
            max_alignment,
            fmt,
        )?,
        b'i' => PackOptionItem::new(
            PackOption::SignedInt,
            get_pack_num_limit(fmt, size_of::<i32>())?,
            position,
            max_alignment,
            fmt,
        )?,
        b'I' => PackOptionItem::new(
            PackOption::UnsignedInt,
            get_pack_num_limit(fmt, size_of::<u32>())?,
            position,
            max_alignment,
            fmt,
        )?,
        b's' => PackOptionItem::new(
            PackOption::String,
            get_pack_num_limit(fmt, size_of::<usize>())?,
            position,
            max_alignment,
            fmt,
        )?,
        b'c' => {
            let Some(size) = get_pack_num(fmt) else {
                return Err(lua_error!("missing size for format option 'c'"));
            };
            PackOptionItem::new(PackOption::FixedString, size, position, max_alignment, fmt)?
        }
        b'z' => PackOptionItem::new(PackOption::Zstr, 0, position, max_alignment, fmt)?,
        b'x' => PackOptionItem::new(PackOption::Padding, 1, position, max_alignment, fmt)?,
        b'X' => PackOptionItem::new(PackOption::PaddingAlign, 0, position, max_alignment, fmt)?,
        b' ' => PackOptionItem::new(PackOption::Nop, 0, position, max_alignment, fmt)?,
        _ => {
            return Err(lua_error!("invalid format option '{}'", c as char));
        }
    })
}

pub(crate) fn pack_int(
    mut value: u64,
    is_little_endian: bool,
    size: usize,
    negative: bool,
) -> Vec<u8> {
    const MASK_CHARACTER: u64 = (1 << 8) - 1;

    let mut result = vec![0; size];
    result[if is_little_endian { 0 } else { size - 1 }] = (value & MASK_CHARACTER) as u8;
    for i in 1..size {
        value >>= 8;
        result[if is_little_endian { i } else { size - 1 - i }] = (value & MASK_CHARACTER) as u8;
    }
    if negative && size > size_of_val(&value) {
        for i in size_of_val(&value)..size {
            result[if is_little_endian { i } else { size - 1 - i }] = MASK_CHARACTER as u8;
        }
    }

    result
}

pub(crate) fn unpack_int(
    str: &[u8],
    is_little_endian: bool,
    size: usize,
    is_signed: bool,
) -> crate::Result<i64> {
    const MASK_CHARACTER: u64 = (1 << 8) - 1;
    const SIZE_INT: usize = size_of::<i64>();

    let mut result = 0;
    let limit = if size <= SIZE_INT { size } else { SIZE_INT };
    for i in (0..limit).rev() {
        result <<= 8;
        result |= str[if is_little_endian { i } else { size - 1 - i }] as i64;
    }
    if size < SIZE_INT {
        if is_signed {
            let mask = 1 << (size * 8 - 1);
            // sign extension
            result = (result ^ mask) - mask;
        }
    } else if size > SIZE_INT {
        let mask = if !is_signed || result >= 0 {
            0
        } else {
            MASK_CHARACTER
        };
        for i in limit..size {
            if str[if is_little_endian { i } else { size - 1 - i }] as u64 != mask {
                return Err(lua_error!(
                    "{size}-byte integer does not fit into Lua integer"
                ));
            }
        }
    }

    Ok(result)
}

pub(crate) fn copy_bytes_fix_endian(dest: &mut [u8], src: &[u8], is_little_endian: bool) {
    debug_assert!(dest.len() <= src.len());

    if cfg!(target_endian = "little") == is_little_endian {
        dest.copy_from_slice(&src[..dest.len()]);
    } else {
        for (i, byte) in src.iter().enumerate() {
            dest[dest.len() - 1 - i] = *byte;
        }
    }
}

fn get_pack_num(fmt: &mut Peekable<Iter<'_, u8>>) -> Option<usize> {
    if !fmt.peek().is_some_and(|c| c.is_ascii_digit()) {
        return None;
    }

    let mut result = 0;
    loop {
        result = result * 10 + (fmt.next().unwrap() - b'0') as usize;
        if fmt.peek().is_none_or(|c| !c.is_ascii_digit())
            || result >= (LuaValue::MAX_STRING_LENGTH - 9) / 10
        {
            break;
        }
    }

    Some(result)
}

fn get_pack_num_limit(fmt: &mut Peekable<Iter<'_, u8>>, default: usize) -> crate::Result<usize> {
    const MAX_INT_SIZE: usize = 16;

    let size = get_pack_num(fmt).unwrap_or(default);
    if size == 0 || size > MAX_INT_SIZE {
        return Err(lua_error!(
            "integral size ({size}) out of limits [1,{MAX_INT_SIZE}]"
        ));
    }

    Ok(size)
}
