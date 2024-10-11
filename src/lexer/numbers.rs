use std::fmt::Display;

use crate::token::TokenKind;

use super::compat::{ByteCharExt, ByteSliceExt};
use subslice::SubsliceExt;
use thiserror::Error;

pub(crate) struct NumberParser;

pub(crate) enum ParsedNumberValue {
    Integer(i64),
    Float(f64),
}

pub(crate) struct ParsedNumber<T> {
    pub(crate) value: T,
    pub(crate) literal_len: usize,
}

#[derive(Error, Debug)]
pub(crate) struct NumberParseError {
    pub(crate) message: String,
    pub(crate) span: std::ops::Range<usize>,
}

impl Display for NumberParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl NumberParser {
    pub(crate) fn parse_number<T>(
        chars: std::slice::Iter<u8>,
    ) -> Result<ParsedNumber<T>, NumberParseError>
    where
        T: From<ParsedNumberValue>,
    {
        let slice = chars.as_slice();
        let mut chars = chars.enumerate().peekable();
        let mut has_fraction = false;
        let mut is_hex = false;
        let mut has_exponent = false;
        let mut just_started_exponent = false;
        let mut starting_character = None;

        // Find the end of the number
        let end = chars.position(|(i, &c)| {
            let min_fraction_index = if is_hex { 1 } else { 0 };

            let is_end = if c == b'.' && !has_fraction && i >= min_fraction_index {
                has_fraction = true;
                false
            } else if (c == b'x' || c == b'X') && i == 1 && matches!(starting_character, Some(b'0'))
            {
                is_hex = true;
                false
            } else if (is_hex && !has_exponent && i > 2 && (c == b'p' || c == b'P'))
                || (!is_hex && !has_exponent && (c == b'e' || c == b'E'))
            {
                has_exponent = true;
                just_started_exponent = true;
                false
            } else if just_started_exponent && (c == b'+' || c == b'-') {
                just_started_exponent = false;
                false
            } else if is_hex && !has_exponent {
                just_started_exponent = false;
                !c.is_ascii_hexdigit()
            } else {
                just_started_exponent = false;
                !c.is_ascii_digit()
            };

            if starting_character.is_none() {
                starting_character = Some(c);
            }

            is_end
        });

        // Only floats have exponents
        let is_float = has_fraction || has_exponent;
        let end = end.unwrap_or(slice.len());

        // Let's check that the end here makes sense (e.g. no "0xfi" or "123a")
        let mut rest_chars = slice[end..].iter().peekable();
        let next = rest_chars.peek();
        match next {
            Some(c) if c.is_ascii_alphabetic() => {
                return Err(NumberParseError {
                    message: format!(
                        "invalid number literal near '{}'",
                        String::from_utf8_lossy(&slice[..end + 1])
                    ),
                    span: 0..end + 1,
                });
            }
            _ => {}
        }

        let literal = &slice[..end];

        let number = match (is_float, is_hex) {
            // Integer
            (false, false) => {
                ParsedNumberValue::Integer(parse_decimal_number(literal.iter().copied()))
            }
            // Hex integer
            (false, true) => {
                ParsedNumberValue::Integer(parse_hex_number(literal[2..].iter().copied()))
            }
            // Float
            (true, false) => ParsedNumberValue::Float(
                std::str::from_utf8(literal)
                    .map_err(|e| NumberParseError {
                        message: format!("failed to parse float literal: {e}"),
                        span: 0..end,
                    })?
                    .parse()
                    .map_err(|e| NumberParseError {
                        message: format!("failed to parse float literal: {e}"),
                        span: 0..end,
                    })?,
            ),
            // Hex float
            (true, true) => {
                let exponent_start = if has_exponent {
                    literal
                        .iter()
                        .position(|&c| c == b'p' || c == b'P')
                        .expect("has exponent")
                } else {
                    literal.len()
                };
                let fraction_start = if has_fraction {
                    literal.find(b".").expect("has fraction")
                } else {
                    exponent_start
                };

                let base_str = &literal[2..fraction_start];
                let base = if base_str.is_empty() {
                    0f64
                } else {
                    i64::from_bytes_radix(base_str, 16).map_err(|e| NumberParseError {
                        message: format!("failed to parse hex float literal: {e}"),
                        span: 0..end,
                    })? as f64
                };
                let fraction = if has_fraction {
                    let fraction_str = &literal[fraction_start + 1..exponent_start];
                    let fraction =
                        i64::from_bytes_radix(fraction_str, 16).map_err(|e| NumberParseError {
                            message: format!("failed to parse hex float literal fraction: {e}"),
                            span: 0..end,
                        })? as f64;
                    let fraction_digits = fraction_str.len() as f64;
                    fraction / 16f64.powf(fraction_digits)
                } else {
                    0.0
                };

                let value = base + fraction;
                ParsedNumberValue::Float(if has_exponent {
                    let exponent_str = std::str::from_utf8(&literal[exponent_start + 1..])
                        .map_err(|e| NumberParseError {
                            message: format!("failed to parse hex float literal exponent: {e}"),
                            span: 0..end,
                        })?;
                    let exponent = exponent_str.parse().map_err(|e| NumberParseError {
                        message: format!("failed to parse hex float literal exponent: {e}"),
                        span: 0..end,
                    })?;

                    value * 2f64.powf(exponent)
                } else {
                    value
                })
            }
        };

        Ok(ParsedNumber {
            value: number.into(),
            literal_len: end,
        })
    }
}

pub(crate) fn parse_decimal_number(literal: impl Iterator<Item = u8>) -> i64 {
    literal.fold(0i64, |acc, c| {
        acc.wrapping_mul(10).wrapping_add(
            c.to_digit(10)
                .expect("expected only decimal numbers as input to parse_decimal_number")
                as i64,
        )
    })
}

pub(crate) fn parse_hex_number(literal: impl Iterator<Item = u8>) -> i64 {
    literal.fold(0i64, |acc, c| {
        acc.wrapping_mul(16).wrapping_add(
            c.to_digit(16)
                .expect("expected only hex numbers as input to parse_hex_number")
                as i64,
        )
    })
}

impl From<ParsedNumberValue> for TokenKind<'_> {
    fn from(number: ParsedNumberValue) -> Self {
        match number {
            ParsedNumberValue::Integer(i) => TokenKind::Integer(i),
            ParsedNumberValue::Float(f) => TokenKind::Float(f),
        }
    }
}
