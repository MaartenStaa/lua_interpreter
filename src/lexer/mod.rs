mod compat;
pub(crate) mod numbers;

use std::path::Path;

use miette::LabeledSpan;
use numbers::{NumberParser, ParsedNumber};
use subslice::SubsliceExt;

use crate::error::{Context, LuaError, lua_error};
use crate::token::{Span, Token, TokenKind};
use compat::ByteCharExt;

pub struct Lexer<'path, 'source> {
    filename: Option<&'path Path>,
    source: &'source [u8],
    rest: &'source [u8],
    position: usize,
    peeked: Option<Token<'source>>,
}

impl<'path, 'source> Lexer<'path, 'source> {
    pub fn new(filename: Option<&'path Path>, source: &'source [u8]) -> Self {
        Self {
            filename,
            source,
            rest: source,
            position: 0,
            peeked: None,
        }
    }

    pub fn label_at_current_position(&self, message: &str) -> LabeledSpan {
        let mut position = self.position;
        if let Some(peeked) = &self.peeked {
            position = peeked.span.start;
        }

        LabeledSpan::at(position..position, message)
    }

    pub fn with_source_code(&self, report: LuaError) -> LuaError {
        if let Some(filename) = self.filename {
            report.with_source_code(
                miette::NamedSource::new(filename.to_string_lossy(), self.source.to_vec())
                    .with_language("lua"),
            )
        } else {
            report.with_source_code(self.source.to_vec())
        }
    }
}

enum State<'source> {
    String,
    Number,
    Ident,
    Dot,
    MaybeMultiCharacterOperator {
        option_a: MultiCharacterOperatorOption<'source>,
        option_b: Option<MultiCharacterOperatorOption<'source>>,
        next_does_not_match: TokenKind<'source>,
    },
}

struct MultiCharacterOperatorOption<'source> {
    next: u8,
    next_matches: TokenKind<'source>,
}

macro_rules! token {
    ($kind:ident, $start:ident, $self:ident) => {
        return Some(Ok(Token {
            kind: TokenKind::$kind,
            span: Span {
                start: $start,
                end: $self.position,
            },
        }))
    };
}

impl<'path, 'source> Iterator for Lexer<'path, 'source> {
    type Item = crate::Result<Token<'source>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(peeked) = self.peeked.take() {
            return Some(Ok(peeked));
        }

        if self.rest.is_empty() {
            return None;
        }

        loop {
            let mut chars = self.rest.iter();
            let c = *chars.next()?;
            let c_start = self.position;

            if c == b'#' && c_start == 0 {
                let line_break = self.rest.find(b"\n").unwrap_or(self.rest.len());
                self.position += line_break;
                self.rest = &self.rest[line_break..];
                continue;
            }

            self.rest = chars.as_slice();
            self.position += 1;

            let state = match c {
                // Punctuation
                // All single-character punctuation tokens
                b'+' => token!(Plus, c_start, self),
                b'-' => {
                    // Handle the special case of "--" comments
                    if let Some(b'-') = self.rest.iter().next() {
                        self.position += 1;
                        self.rest = &self.rest[1..];

                        self.eat_comment();
                        continue;
                    }

                    token!(Minus, c_start, self)
                }
                b'*' => token!(Star, c_start, self),
                b'%' => token!(Percent, c_start, self),
                b'^' => token!(Caret, c_start, self),
                b'#' => token!(Hash, c_start, self),
                b'&' => token!(Ampersand, c_start, self),
                b'|' => token!(Pipe, c_start, self),
                b'(' => token!(OpenParen, c_start, self),
                b')' => token!(CloseParen, c_start, self),
                b'{' => token!(OpenBrace, c_start, self),
                b'}' => token!(CloseBrace, c_start, self),
                b'[' => {
                    // This might be a long form string
                    let mut chars = self.rest.iter().peekable();
                    match chars.peek() {
                        Some(b'=') => {
                            let mut equals = 0u8;
                            while let Some(b'=') = chars.peek() {
                                chars.next();
                                equals += 1;
                            }

                            if let Some(b'[') = chars.next() {
                                let advanced = equals as usize + 1;
                                self.position += advanced;
                                self.rest = &self.rest[advanced..];

                                return Some(self.parse_string(c, c_start, Some(equals)));
                            }
                        }
                        Some(b'[') => {
                            self.position += 1;
                            self.rest = &self.rest[1..];

                            return Some(self.parse_string(c, c_start, Some(0)));
                        }
                        _ => {}
                    }

                    token!(OpenBracket, c_start, self)
                }
                b']' => token!(CloseBracket, c_start, self),
                b';' => token!(Semicolon, c_start, self),
                b',' => token!(Comma, c_start, self),

                // Ones that might be two characters
                b'/' => State::MaybeMultiCharacterOperator {
                    option_a: MultiCharacterOperatorOption {
                        next: b'/',
                        next_matches: TokenKind::SlashSlash,
                    },
                    option_b: None,
                    next_does_not_match: TokenKind::Slash,
                },
                b'~' => State::MaybeMultiCharacterOperator {
                    option_a: MultiCharacterOperatorOption {
                        next: b'=',
                        next_matches: TokenKind::TildeEquals,
                    },
                    option_b: None,
                    next_does_not_match: TokenKind::Tilde,
                },
                b'<' => State::MaybeMultiCharacterOperator {
                    option_a: MultiCharacterOperatorOption {
                        next: b'=',
                        next_matches: TokenKind::LessEquals,
                    },
                    option_b: Some(MultiCharacterOperatorOption {
                        next: b'<',
                        next_matches: TokenKind::ShiftLeft,
                    }),
                    next_does_not_match: TokenKind::Less,
                },
                b'>' => State::MaybeMultiCharacterOperator {
                    option_a: MultiCharacterOperatorOption {
                        next: b'=',
                        next_matches: TokenKind::GreaterEquals,
                    },
                    option_b: Some(MultiCharacterOperatorOption {
                        next: b'>',
                        next_matches: TokenKind::ShiftRight,
                    }),
                    next_does_not_match: TokenKind::Greater,
                },
                b'=' => State::MaybeMultiCharacterOperator {
                    option_a: MultiCharacterOperatorOption {
                        next: b'=',
                        next_matches: TokenKind::EqualsEquals,
                    },
                    option_b: None,
                    next_does_not_match: TokenKind::Equals,
                },
                b':' => State::MaybeMultiCharacterOperator {
                    option_a: MultiCharacterOperatorOption {
                        next: b':',
                        next_matches: TokenKind::DoubleColon,
                    },
                    option_b: None,
                    next_does_not_match: TokenKind::Colon,
                },

                // Special cases (dot operator or number literal? maybe even "...")
                b'.' => State::Dot,

                // Idents and keywords
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => State::Ident,

                // Literals
                b'"' | b'\'' => State::String,
                b'0'..=b'9' => State::Number,

                c if c.is_ascii_whitespace() => continue,

                _ => {
                    return Some(Err(self.with_source_code(lua_error!(
                        labels = vec![LabeledSpan::at(
                            c_start..self.position,
                            "unexpected character"
                        )],
                        "unexpected token {c}"
                    ))));
                }
            };

            break Some(match state {
                State::String => self
                    .parse_string(c, c_start, None)
                    .wrap_err("when parsing string literal"),
                State::Number => self
                    .parse_number(c_start)
                    .wrap_err("when parsing number literal"),
                State::Ident => self
                    .parse_ident(c_start)
                    .wrap_err("when parsing identifier"),
                State::Dot => self
                    .parse_dot(c_start)
                    .wrap_err("when parsing dot operator or number literal"),
                State::MaybeMultiCharacterOperator {
                    option_a,
                    option_b,
                    next_does_not_match,
                } => self.parse_maybe_multi_character_operator(
                    c_start,
                    option_a,
                    option_b,
                    next_does_not_match,
                ),
            });
        }
    }
}

impl<'path, 'source> Lexer<'path, 'source> {
    pub fn peek(&mut self) -> crate::Result<Option<&Token<'source>>> {
        if self.peeked.is_none() {
            self.peeked = self.next().transpose()?;
        }

        Ok(self.peeked.as_ref())
    }

    pub fn expect<F>(&mut self, matcher: F, expected: &str) -> crate::Result<Token<'source>>
    where
        F: FnOnce(&'_ TokenKind) -> bool,
    {
        match self.next().transpose()? {
            Some(token) if matcher(&token.kind) => Ok(token),
            Some(token) => Err(lua_error!(
                labels = vec![LabeledSpan::at(
                    token.span.start..token.span.end,
                    format!("expected '{expected}', found {:?}", token.kind)
                )],
                "unexpected token"
            )),
            None => Err(lua_error!(
                labels = vec![LabeledSpan::at(
                    self.position..self.position + 1,
                    format!("expected '{expected}'")
                )],
                "unexpected end of input"
            )),
        }
    }

    fn eat_comment(&mut self) {
        // First need to check if this is a long comment
        let mut chars = self.rest.iter().peekable();
        if let Some(b'[') = chars.peek() {
            chars.next();

            let mut equals = 0;
            while let Some(b'=') = chars.peek() {
                chars.next();
                equals += 1;
            }

            if let Some(b'[') = chars.next() {
                // This is a long form comment, so we need to find the end
                self.rest = &self.rest[equals + 2..];
                self.position += equals + 2;

                let end_pattern = format!("]{}]", "=".repeat(equals)).into_bytes();
                let end = self.rest.find(&end_pattern).unwrap_or(self.rest.len());
                self.position += end + end_pattern.len();
                self.rest = &self.rest[end + end_pattern.len()..];
                return;
            }
        }

        // Otherwise, this is a short comment
        let line_break = self.rest.find(b"\n").unwrap_or(self.rest.len());
        self.position += line_break;
        self.rest = &self.rest[line_break..];
    }

    fn parse_ident(&mut self, start: usize) -> crate::Result<Token<'source>> {
        let mut chars = self.rest.iter().peekable();
        while let Some(c) = chars.peek() {
            if matches!(c, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_') {
                chars.next();
                self.position += 1;
            } else {
                break;
            }
        }

        let ident = &self.source[start..self.position];
        self.rest = &self.rest[self.position - start - 1..];

        let kind = match ident {
            // Keywords
            b"and" => TokenKind::And,
            b"break" => TokenKind::Break,
            b"do" => TokenKind::Do,
            b"else" => TokenKind::Else,
            b"elseif" => TokenKind::ElseIf,
            b"end" => TokenKind::End,
            b"for" => TokenKind::For,
            b"function" => TokenKind::Function,
            b"goto" => TokenKind::Goto,
            b"if" => TokenKind::If,
            b"in" => TokenKind::In,
            b"local" => TokenKind::Local,
            b"not" => TokenKind::Not,
            b"or" => TokenKind::Or,
            b"repeat" => TokenKind::Repeat,
            b"return" => TokenKind::Return,
            b"then" => TokenKind::Then,
            b"until" => TokenKind::Until,
            b"while" => TokenKind::While,

            // Literals
            b"true" => TokenKind::True,
            b"false" => TokenKind::False,
            b"nil" => TokenKind::Nil,

            // Identifiers
            ident => TokenKind::Identifier(ident),
        };

        Ok(Token {
            kind,
            span: Span {
                start,
                end: self.position,
            },
        })
    }

    fn parse_string(
        &mut self,
        starting_character: u8,
        start: usize,
        long_form: Option<u8>,
    ) -> crate::Result<Token<'source>> {
        // Try to guess roughly how big our vector will be
        let ending_character = if long_form.is_some() {
            b']'
        } else {
            starting_character
        };
        let possible_length = self
            .rest
            .iter()
            .position(|&c| c == ending_character)
            .unwrap_or(self.rest.len());
        let mut string = Vec::with_capacity(possible_length);

        let mut chars = self.rest.iter().peekable();
        // Long-form string: newline immediately after the starting character is ignored
        if long_form.is_some()
            && let Some(b'\n') = chars.peek()
        {
            chars.next();
            self.position += 1;
        }

        let mut escape = false;
        while let Some(c) = chars.next() {
            if escape {
                let escaped = match c {
                    b'a' => b'\x07',
                    b'b' => b'\x08',
                    b'f' => b'\x0C',
                    b'n' => b'\n',
                    b'r' => b'\r',
                    b't' => b'\t',
                    b'v' => b'\x0B',
                    b'\\' => b'\\',
                    b'"' => b'"',
                    b'\'' => b'\'',
                    b'\n' => b'\n',
                    b'z' => {
                        // Skip whitespace
                        let mut skipped = 0;
                        while let Some(c) = chars.peek() {
                            if c.is_ascii_whitespace() {
                                skipped += 1;
                                chars.next();
                            } else {
                                break;
                            }
                        }

                        // NOTE: Need to add 1 here as we're `continue`ing, not hitting the regular
                        // `position` increment.
                        self.position += skipped + 1;
                        escape = false;
                        continue;
                    }
                    b'x' => {
                        let mut hex = [b'0'; 2];
                        (0..2).try_for_each(|i| -> crate::Result<()> {
                            hex[i] = chars.next().copied().ok_or_else(|| {
                                self.with_source_code(lua_error!(
                                    labels = vec![LabeledSpan::at(
                                        self.position..self.position + 2,
                                        "expected two hex digits"
                                    )],
                                    "unexpected end of string"
                                ))
                            })?;
                            Ok(())
                        })?;

                        self.position += 2;

                        // Let's avoid allocating a String
                        hex.iter().try_fold(0u8, |acc, c| {
                            c.to_digit(16)
                                .ok_or_else(|| {
                                    self.with_source_code(lua_error!(
                                        labels = vec![LabeledSpan::at(
                                            self.position - 2..self.position + 1,
                                            "invalid hex escape"
                                        )],
                                        "invalid hex escape"
                                    ))
                                })
                                .map(|d| acc * 16 + d)
                        })?
                    }
                    b'u' => {
                        let escape_start = self.position - 1;

                        if !matches!(chars.next(), Some(b'{')) {
                            return Err(self.with_source_code(lua_error!(
                                labels = vec![LabeledSpan::at(
                                    self.position - 1..self.position + 1,
                                    "expected { after \\u"
                                )],
                                "expected {{ after \\u"
                            )));
                        }

                        // Lua allows any value up to 2^31 - 1, so account for up to 8 hex digits
                        let mut hex = [b'0'; 8];
                        let mut n = 0;
                        let mut seen_close_brace = false;
                        for c in chars.by_ref().copied() {
                            if c == b'}' {
                                seen_close_brace = true;
                                break;
                            }

                            if !c.is_ascii_hexdigit() {
                                return Err(self.with_source_code(lua_error!(
                                    labels = vec![LabeledSpan::at(
                                        self.position - 1..self.position + 1,
                                        "expected hex digit"
                                    )],
                                    "expected hex digit"
                                )));
                            }

                            if n == 8 {
                                return Err(self.with_source_code(lua_error!(
                                    labels = vec![LabeledSpan::at(
                                        escape_start..self.position + 1,
                                        "too many hex digits"
                                    )],
                                    "too many hex digits"
                                )));
                            }

                            hex[n] = c;
                            n += 1;
                        }

                        if n == 0 {
                            return Err(self.with_source_code(lua_error!(
                                labels = vec![LabeledSpan::at(
                                    escape_start..self.position + 1,
                                    "expected at least one hex digit"
                                )],
                                "expected at least one hex digit"
                            )));
                        } else if !seen_close_brace {
                            return Err(self.with_source_code(lua_error!(
                                labels = vec![LabeledSpan::at(
                                    escape_start..self.position + 1,
                                    "expected } to close escape"
                                )],
                                "expected }} to close escape"
                            )));
                        }

                        self.position += n + 3;

                        let value = numbers::parse_hex_number(hex[..n].iter().copied());
                        string.extend(value.to_be_bytes().iter());

                        escape = false;
                        continue;
                    }
                    _ if c.is_ascii_digit() => {
                        let mut digits = [c; 3];
                        let mut n = 1;
                        for digit in digits.iter_mut().skip(1) {
                            match chars.peek() {
                                Some(c) if c.is_ascii_digit() => {
                                    *digit = chars.next().unwrap();
                                    n += 1;
                                }
                                _ => break,
                            }
                        }

                        // We'll advance for `c` below, so subtract 1 here
                        self.position += n - 1;

                        digits[..n].iter().try_fold(0u8, |acc, c| {
                            c.to_digit(10)
                                .ok_or_else(|| {
                                    self.with_source_code(lua_error!(
                                        labels = vec![LabeledSpan::at(
                                            self.position - n..self.position + 1,
                                            "invalid decimal escape"
                                        )],
                                        "invalid decimal escape"
                                    ))
                                })
                                .map(|d| acc * 10u8 + d)
                        })?
                    }
                    _ => {
                        return Err(self.with_source_code(lua_error!(
                            labels = vec![LabeledSpan::at(
                                self.position - 1..self.position + 1,
                                "invalid escape sequence"
                            )],
                            "invalid escape sequence"
                        )));
                    }
                };

                self.position += 1;
                string.push(escaped);
                escape = false;
                continue;
            }

            match c {
                b'\\' if long_form.is_none() => escape = true,
                c if *c == starting_character && long_form.is_none() => {
                    self.position += 1;
                    self.rest = &self.rest[self.position - start - 1..];

                    return Ok(Token {
                        kind: TokenKind::String(string),
                        span: Span {
                            start,
                            end: self.position,
                        },
                    });
                }
                b']' if long_form.is_some() => {
                    let expected_equals = long_form.unwrap();
                    let mut equals = 0u8;
                    while let Some(b'=') = chars.peek() {
                        equals += 1;
                        chars.next();
                    }

                    if equals == expected_equals {
                        if let Some(b']') = chars.peek() {
                            self.position += 2 + equals as usize;
                            self.rest =
                                &self.rest[self.position - start - 2 - expected_equals as usize..];

                            return Ok(Token {
                                kind: TokenKind::String(string),
                                span: Span {
                                    start,
                                    end: self.position,
                                },
                            });
                        }
                    } else {
                        string.push(*c);
                        string.extend(std::iter::repeat_n(b'=', equals as usize));

                        self.position += equals as usize;
                    }
                }
                b'\r' | b'\n' if long_form.is_none() => {
                    return Err(self.with_source_code(lua_error!(
                        labels = vec![LabeledSpan::at(
                            self.position - 1..self.position,
                            "unexpected newline in string"
                        )],
                        "unexpected newline in string"
                    )));
                }
                b'\r' if long_form.is_some() => {
                    if let Some(b'\n') = chars.peek() {
                        chars.next();
                        self.position += 1;
                    }
                    string.push(b'\n');
                }
                b'\n' if long_form.is_some() => {
                    if let Some(b'\r') = chars.peek() {
                        chars.next();
                        self.position += 1;
                    }
                    string.push(b'\n');
                }
                _ => string.push(*c),
            }

            self.position += 1;
        }

        Err(self.with_source_code(lua_error!(
            labels = vec![LabeledSpan::at(
                self.position..self.position + 1,
                if let Some(equals) = long_form {
                    format!("expected ]{}]", "=".repeat(equals as usize))
                } else {
                    format!("expected {}", starting_character)
                }
            )],
            "unexpected end of string"
        )))
    }

    fn parse_number(&mut self, start: usize) -> crate::Result<Token<'source>> {
        // next() already consumed the first character, correct for that using the whole source and
        // the position
        self.rest = &self.source[start..];
        self.position = start;

        let ParsedNumber { value, literal_len } = NumberParser::parse_number(self.rest.iter())
            .map_err(|e| {
                lua_error!(
                    labels = vec![LabeledSpan::at(
                        start + e.span.start..start + e.span.end,
                        "this literal"
                    )],
                    "{e}",
                )
            })?;

        let end = self.position + literal_len;

        self.rest = &self.rest[literal_len..];
        self.position += literal_len;

        Ok(Token {
            kind: value,
            span: Span { start, end },
        })
    }

    fn parse_dot(&mut self, start: usize) -> crate::Result<Token<'source>> {
        let mut chars = self.rest.iter().peekable();
        match chars.peek() {
            Some(b'.') => {
                chars.next();
                let width = 1;
                self.position += width;
                self.rest = &self.rest[width..];

                match chars.peek() {
                    Some(b'.') => {
                        chars.next();
                        let width = 1;
                        self.position += width;
                        self.rest = &self.rest[width..];

                        Ok(Token {
                            kind: TokenKind::DotDotDot,
                            span: Span {
                                start,
                                end: self.position,
                            },
                        })
                    }
                    _ => Ok(Token {
                        kind: TokenKind::DotDot,
                        span: Span {
                            start,
                            end: self.position,
                        },
                    }),
                }
            }
            Some(b'0'..=b'9') => self
                .parse_number(start)
                .wrap_err("when parsing number literal"),
            _ => Ok(Token {
                kind: TokenKind::Dot,
                span: Span {
                    start,
                    end: self.position,
                },
            }),
        }
    }

    fn parse_maybe_multi_character_operator(
        &mut self,
        start: usize,
        option_a: MultiCharacterOperatorOption<'source>,
        option_b: Option<MultiCharacterOperatorOption<'source>>,
        next_does_not_match: TokenKind<'source>,
    ) -> crate::Result<Token<'source>> {
        let mut chars = self.rest.iter().peekable();
        match (chars.peek(), option_b) {
            (Some(&next), Some(option_b)) if *next == option_b.next => {
                chars.next();
                let width = 1;
                self.position += width;
                self.rest = &self.rest[width..];

                Ok(Token {
                    kind: option_b.next_matches,
                    span: Span {
                        start,
                        end: self.position,
                    },
                })
            }
            (Some(&next), _) if *next == option_a.next => {
                chars.next();
                let width = 1;
                self.position += width;
                self.rest = &self.rest[width..];

                Ok(Token {
                    kind: option_a.next_matches,
                    span: Span {
                        start,
                        end: self.position,
                    },
                })
            }
            _ => Ok(Token {
                kind: next_does_not_match,
                span: Span {
                    start,
                    end: self.position,
                },
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_numbers() {
        // https://www.lua.org/manual/5.4/manual.html#3.3
        // Examples of valid integer constants are
        // 3   345   0xff   0xBEBADA
        //
        // Examples of valid float constants are
        // 3.0     3.1416     314.16e-2     0.31416E1     34e1
        // 0x0.1E  0xA23p-4   0X1.921FB54442D18P+1

        for (input, expected) in [
            ("3", TokenKind::Integer(3)),
            ("345", TokenKind::Integer(345)),
            ("5e3", TokenKind::Float(5000.0)),
            ("0xff", TokenKind::Integer(255)),
            ("0xBEBADA", TokenKind::Integer(0xBEBADA)),
            ("3.0", TokenKind::Float(3.0)),
            #[allow(clippy::approx_constant)]
            ("3.1416", TokenKind::Float(3.1416)),
            #[allow(clippy::approx_constant)]
            ("314.16e-2", TokenKind::Float(3.1416)),
            #[allow(clippy::approx_constant)]
            ("0.31416E1", TokenKind::Float(3.1416)),
            ("34e1", TokenKind::Float(340.0)),
            ("0x0.1E", TokenKind::Float(0.1171875)),
            ("0xA23p-4", TokenKind::Float(162.1875)),
            #[allow(clippy::approx_constant)]
            ("0X1.921FB54442D18P+1", TokenKind::Float(3.141592653589793)),
            ("0x.0p-3", TokenKind::Float(0.0)),
        ] {
            let mut lexer = Lexer::new(Some(Path::new("test.lua")), input.as_bytes());
            let token = lexer.next().unwrap().unwrap();
            assert_eq!(token.kind, expected, "when parsing '{input}'");
        }

        // Invalid numbers
        for input in &["0x5pf", "0xi"] {
            let mut lexer = Lexer::new(Some(Path::new("test.lua")), input.as_bytes());
            let token = lexer.next().unwrap();
            assert!(token.is_err(), "when parsing '{input}'");
        }
    }

    #[test]
    fn test_parse_strings() {
        // Examples from the Lua manual
        for example in &[
            r#"'alo\n123"'"#,
            r#""alo\n123\"""#,
            r#"'\97lo\10\04923"'"#,
            "[[alo\n123\"]]",
            "[==[alo\n123\"]==]",
        ] {
            let mut lexer = Lexer::new(Some(Path::new("test.lua")), example.as_bytes());
            let token = lexer.next().unwrap().unwrap();
            assert_eq!(
                token.kind,
                TokenKind::String(vec![b'a', b'l', b'o', b'\n', b'1', b'2', b'3', b'"']),
                "when parsing '{example}'"
            );
        }

        // Escape codes
        for (input, expected) in [
            ("'\\n'", vec![10u8]),
            (
                "'  foo \\z \n  \n  bar'",
                vec![b' ', b' ', b'f', b'o', b'o', b' ', b'b', b'a', b'r'],
            ),
        ] {
            let mut lexer = Lexer::new(Some(Path::new("test.lua")), input.as_bytes());
            let token = lexer.next().unwrap().unwrap();
            assert_eq!(token.kind, TokenKind::String(expected));
        }
    }

    #[test]
    fn comments() {
        for input in [
            "-- this is a comment\n",
            "--[=[\n  example of a long [comment],\n  [[spanning several [lines]]]\n\n]=]",
        ] {
            let mut lexer = Lexer::new(Some(Path::new("test.lua")), input.as_bytes());
            let token = lexer.next();
            assert!(token.is_none(), "when parsing '{input}'");

            // Also check that we can find tokens after the comment
            let input = format!("{} 3", input);
            let mut lexer = Lexer::new(Some(Path::new("test.lua")), input.as_bytes());
            let token = lexer.next().unwrap().unwrap();
            assert_eq!(token.kind, TokenKind::Integer(3));
        }
    }

    #[test]
    fn longform_strings_newlines() {
        let expected = b"foo\nbar";

        for input in [
            // carriage return is converted to newline
            "[[foo\rbar]]",
            // newline is preserved
            "[[foo\nbar]]",
            // carriage return + newline is converted to newline
            "[[foo\r\nbar]]",
            // newline + carriage return is converted to newline
            "[[foo\n\rbar]]",
            // newline immediately after opening delimiter is ignored
            "[[\nfoo\nbar]]",
        ] {
            let mut lexer = Lexer::new(Some(Path::new("test.lua")), input.as_bytes());
            let token = lexer.next().unwrap().unwrap();
            assert_eq!(token.kind, TokenKind::String(expected.to_vec()));
        }
    }
}
