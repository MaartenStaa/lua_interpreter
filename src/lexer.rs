use std::path::PathBuf;

use crate::token::{Span, Token, TokenKind};
use miette::{miette, Context, LabeledSpan, NamedSource};

pub struct Lexer<'source> {
    filename: PathBuf,
    source: &'source str,
    rest: &'source str,
    pub position: usize,
    peeked: Option<Token<'source>>,
}

impl<'source> Lexer<'source> {
    pub fn new(filename: PathBuf, source: &'source str) -> Self {
        Self {
            filename,
            source,
            rest: source,
            position: 0,
            peeked: None,
        }
    }

    pub fn get_source_code(&self) -> NamedSource<String> {
        NamedSource::new(self.filename.to_string_lossy(), self.source.to_string())
            .with_language("lua")
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
    next: char,
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

impl<'source> Iterator for Lexer<'source> {
    type Item = miette::Result<Token<'source>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(peeked) = self.peeked.take() {
            return Some(Ok(peeked));
        }

        if self.rest.is_empty() {
            return None;
        }

        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let c_start = self.position;

            if c == '#' && c_start == 0 {
                let line_break = self.rest.find('\n').unwrap_or(self.rest.len());
                self.position += line_break;
                self.rest = &self.rest[line_break..];
                continue;
            }

            self.rest = chars.as_str();
            self.position += c.len_utf8();

            let state = match c {
                // Punctuation
                // All single-character punctuation tokens
                '+' => token!(Plus, c_start, self),
                '-' => {
                    // Handle the special case of "--" comments
                    if let Some('-') = self.rest.chars().next() {
                        self.position += 1;
                        self.rest = &self.rest[1..];

                        self.eat_comment();
                        continue;
                    }

                    token!(Minus, c_start, self)
                }
                '*' => token!(Star, c_start, self),
                '%' => token!(Percent, c_start, self),
                '^' => token!(Caret, c_start, self),
                '#' => token!(Hash, c_start, self),
                '&' => token!(Ampersand, c_start, self),
                '|' => token!(Pipe, c_start, self),
                '(' => token!(OpenParen, c_start, self),
                ')' => token!(CloseParen, c_start, self),
                '{' => token!(OpenBrace, c_start, self),
                '}' => token!(CloseBrace, c_start, self),
                '[' => {
                    // This might be a long form string
                    let mut chars = self.rest.chars().peekable();
                    match chars.peek() {
                        Some('=') => {
                            let mut equals = 0u8;
                            while let Some('=') = chars.peek() {
                                chars.next();
                                equals += 1;
                            }

                            if let Some('[') = chars.next() {
                                let advanced = equals as usize + 1;
                                self.position += advanced;
                                self.rest = &self.rest[advanced..];

                                return Some(self.parse_string(c, c_start, Some(equals)));
                            }
                        }
                        Some('[') => {
                            self.position += 1;
                            self.rest = &self.rest[1..];

                            return Some(self.parse_string(c, c_start, Some(0)));
                        }
                        _ => {}
                    }

                    token!(OpenBracket, c_start, self)
                }
                ']' => token!(CloseBracket, c_start, self),
                ';' => token!(Semicolon, c_start, self),
                ',' => token!(Comma, c_start, self),

                // Ones that might be two characters
                '/' => State::MaybeMultiCharacterOperator {
                    option_a: MultiCharacterOperatorOption {
                        next: '/',
                        next_matches: TokenKind::SlashSlash,
                    },
                    option_b: None,
                    next_does_not_match: TokenKind::Slash,
                },
                '~' => State::MaybeMultiCharacterOperator {
                    option_a: MultiCharacterOperatorOption {
                        next: '=',
                        next_matches: TokenKind::TildeEquals,
                    },
                    option_b: None,
                    next_does_not_match: TokenKind::Tilde,
                },
                '<' => State::MaybeMultiCharacterOperator {
                    option_a: MultiCharacterOperatorOption {
                        next: '=',
                        next_matches: TokenKind::LessEquals,
                    },
                    option_b: Some(MultiCharacterOperatorOption {
                        next: '<',
                        next_matches: TokenKind::ShiftLeft,
                    }),
                    next_does_not_match: TokenKind::Less,
                },
                '>' => State::MaybeMultiCharacterOperator {
                    option_a: MultiCharacterOperatorOption {
                        next: '=',
                        next_matches: TokenKind::GreaterEquals,
                    },
                    option_b: Some(MultiCharacterOperatorOption {
                        next: '>',
                        next_matches: TokenKind::ShiftRight,
                    }),
                    next_does_not_match: TokenKind::Greater,
                },
                '=' => State::MaybeMultiCharacterOperator {
                    option_a: MultiCharacterOperatorOption {
                        next: '=',
                        next_matches: TokenKind::EqualsEquals,
                    },
                    option_b: None,
                    next_does_not_match: TokenKind::Equals,
                },
                ':' => State::MaybeMultiCharacterOperator {
                    option_a: MultiCharacterOperatorOption {
                        next: ':',
                        next_matches: TokenKind::DoubleColon,
                    },
                    option_b: None,
                    next_does_not_match: TokenKind::Colon,
                },

                // Special cases (dot operator or number literal? maybe even "...")
                '.' => State::Dot,

                // Idents and keywords
                'a'..='z' | 'A'..='Z' | '_' => State::Ident,

                // Literals
                '"' | '\'' => State::String,
                '0'..='9' => State::Number,

                c if c.is_whitespace() => continue,

                _ => {
                    return Some(Err(miette!(
                        labels = vec![LabeledSpan::at(
                            c_start..self.position,
                            "unexpected character"
                        )],
                        "unexpected token {c}"
                    )
                    .with_source_code(self.get_source_code())))
                }
            };

            break Some(match state {
                State::String => self
                    .parse_string(c, c_start, None)
                    .wrap_err("when parsing string literal"),
                State::Number => self
                    .parse_number(c, c_start)
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

impl<'source> Lexer<'source> {
    pub fn peek(&mut self) -> miette::Result<Option<&Token<'source>>> {
        if self.peeked.is_none() {
            self.peeked = self.next().transpose()?;
        }

        Ok(self.peeked.as_ref())
    }

    pub fn expect<F>(&mut self, matcher: F, expected: &str) -> miette::Result<Token<'source>>
    where
        F: FnOnce(&'_ TokenKind) -> bool,
    {
        match self.next().transpose()? {
            Some(token) if matcher(&token.kind) => Ok(token),
            Some(token) => Err(miette!(
                labels = vec![LabeledSpan::at(
                    token.span.start..token.span.end,
                    format!("expected '{expected}', found {:?}", token.kind)
                )],
                "unexpected token"
            )),
            None => Err(miette!(
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
        let mut chars = self.rest.chars().peekable();
        if let Some('[') = chars.peek() {
            chars.next();

            let mut equals = 0;
            while let Some('=') = chars.peek() {
                chars.next();
                equals += 1;
            }

            if let Some('[') = chars.next() {
                // This is a long form comment, so we need to find the end
                self.rest = &self.rest[equals + 2..];
                self.position += equals + 2;

                let end_pattern = format!("]{}]", "=".repeat(equals));
                let end = self.rest.find(&end_pattern).unwrap_or(self.rest.len());
                self.position += end + end_pattern.len();
                self.rest = &self.rest[end + end_pattern.len()..];
                return;
            }
        }

        // Otherwise, this is a short comment
        let line_break = self.rest.find('\n').unwrap_or(self.rest.len());
        self.position += line_break;
        self.rest = &self.rest[line_break..];
    }

    fn parse_ident(&mut self, start: usize) -> miette::Result<Token<'source>> {
        let mut chars = self.rest.chars().peekable();
        while let Some(c) = chars.peek() {
            if matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_') {
                let c = chars.next();
                let width = c.expect("already matched on it").len_utf8();
                self.position += width;
            } else {
                break;
            }
        }

        let ident = &self.source[start..self.position];
        self.rest = &self.rest[self.position - start - 1..];

        let kind = match ident {
            // Keywords
            "and" => TokenKind::And,
            "break" => TokenKind::Break,
            "do" => TokenKind::Do,
            "else" => TokenKind::Else,
            "elseif" => TokenKind::ElseIf,
            "end" => TokenKind::End,
            "for" => TokenKind::For,
            "function" => TokenKind::Function,
            "goto" => TokenKind::Goto,
            "if" => TokenKind::If,
            "in" => TokenKind::In,
            "local" => TokenKind::Local,
            "not" => TokenKind::Not,
            "or" => TokenKind::Or,
            "repeat" => TokenKind::Repeat,
            "return" => TokenKind::Return,
            "then" => TokenKind::Then,
            "until" => TokenKind::Until,
            "while" => TokenKind::While,

            // Literals
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "nil" => TokenKind::Nil,

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
        starting_character: char,
        start: usize,
        long_form: Option<u8>,
    ) -> miette::Result<Token<'source>> {
        // Try to guess roughly how big our vector will be
        let ending_character = if long_form.is_some() {
            ']'
        } else {
            starting_character
        };
        let possible_length = self.rest.find(ending_character).unwrap_or(self.rest.len());
        let mut string = Vec::with_capacity(possible_length);

        let mut chars = self.rest.chars().peekable();
        let mut escape = false;
        while let Some(c) = chars.next() {
            if escape {
                let escaped = match c {
                    'a' => b'\x07',
                    'b' => b'\x08',
                    'f' => b'\x0C',
                    'n' => b'\n',
                    'r' => b'\r',
                    't' => b'\t',
                    'v' => b'\x0B',
                    '\\' => b'\\',
                    '"' => b'"',
                    '\'' => b'\'',
                    '\n' => b'\n',
                    'z' => {
                        // Skip whitespace
                        let mut skipped = 0;
                        while let Some(c) = chars.peek() {
                            if c.is_whitespace() {
                                skipped += c.len_utf8();
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
                    'x' => {
                        let mut hex = ['0'; 2];
                        (0..2).try_for_each(|i| -> miette::Result<()> {
                            hex[i] = chars.next().ok_or_else(|| {
                                miette!(
                                    labels = vec![LabeledSpan::at(
                                        self.position..self.position + 2,
                                        "expected two hex digits"
                                    )],
                                    "unexpected end of string"
                                )
                                .with_source_code(self.get_source_code())
                            })?;
                            Ok(())
                        })?;

                        self.position += 2;

                        // Let's avoid allocating a String
                        hex.iter().try_fold(0u8, |acc, c| {
                            c.to_digit(16)
                                .ok_or_else(|| {
                                    miette!(
                                        labels = vec![LabeledSpan::at(
                                            self.position - 2..self.position + 1,
                                            "invalid hex escape"
                                        )],
                                        "invalid hex escape"
                                    )
                                    .with_source_code(self.get_source_code())
                                })
                                .map(|d| acc * 16 + d as u8)
                        })?
                    }
                    'u' => {
                        let escape_start = self.position - 1;

                        if !matches!(chars.next(), Some('{')) {
                            return Err(miette!(
                                labels = vec![LabeledSpan::at(
                                    self.position - 1..self.position + 1,
                                    "expected { after \\u"
                                )],
                                "expected {{ after \\u"
                            )
                            .with_source_code(self.get_source_code()));
                        }

                        // Lua allows any value up to 2^31 - 1, so account for up to 8 hex digits
                        let mut hex = ['0'; 8];
                        let mut n = 0;
                        let mut seen_close_brace = false;
                        for c in chars.by_ref() {
                            if c == '}' {
                                seen_close_brace = true;
                                break;
                            }

                            if !c.is_ascii_hexdigit() {
                                return Err(miette!(
                                    labels = vec![LabeledSpan::at(
                                        self.position - 1..self.position + 1,
                                        "expected hex digit"
                                    )],
                                    "expected hex digit"
                                )
                                .with_source_code(self.get_source_code()));
                            }

                            if n == 8 {
                                return Err(miette!(
                                    labels = vec![LabeledSpan::at(
                                        escape_start..self.position + 1,
                                        "too many hex digits"
                                    )],
                                    "too many hex digits"
                                )
                                .with_source_code(self.get_source_code()));
                            }

                            hex[n] = c;
                            n += 1;
                        }

                        if n == 0 {
                            return Err(miette!(
                                labels = vec![LabeledSpan::at(
                                    escape_start..self.position + 1,
                                    "expected at least one hex digit"
                                )],
                                "expected at least one hex digit"
                            )
                            .with_source_code(self.get_source_code()));
                        } else if !seen_close_brace {
                            return Err(miette!(
                                labels = vec![LabeledSpan::at(
                                    escape_start..self.position + 1,
                                    "expected } to close escape"
                                )],
                                "expected }} to close escape"
                            )
                            .with_source_code(self.get_source_code()));
                        }

                        self.position += n + 3;

                        let value = parse_hex_number(hex[..n].iter().copied());
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
                                    miette!(
                                        labels = vec![LabeledSpan::at(
                                            self.position - n..self.position + 1,
                                            "invalid decimal escape"
                                        )],
                                        "invalid decimal escape"
                                    )
                                    .with_source_code(self.get_source_code())
                                })
                                .map(|d| acc * 10u8 + d as u8)
                        })?
                    }
                    _ => {
                        return Err(miette!(
                            labels = vec![LabeledSpan::at(
                                self.position - 1..self.position + 1,
                                "invalid escape sequence"
                            )],
                            "invalid escape sequence"
                        )
                        .with_source_code(self.get_source_code()));
                    }
                };

                self.position += c.len_utf8();
                string.push(escaped);
                escape = false;
                continue;
            }

            match c {
                '\\' if long_form.is_none() => escape = true,
                c if c == starting_character && long_form.is_none() => {
                    self.position += c.len_utf8();
                    self.rest = &self.rest[self.position - start - 1..];

                    return Ok(Token {
                        kind: TokenKind::String(string),
                        span: Span {
                            start,
                            end: self.position,
                        },
                    });
                }
                ']' if long_form.is_some() => {
                    let expected_equals = long_form.unwrap();
                    let mut equals = 0u8;
                    while let Some('=') = chars.peek() {
                        equals += 1;
                        chars.next();
                    }

                    if equals == expected_equals {
                        if let Some(']') = chars.peek() {
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
                        string.push(c as u8);
                        string.extend(std::iter::repeat(b'=').take(equals as usize));

                        self.position += equals as usize;
                    }
                }
                '\n' if long_form.is_none() => {
                    return Err(miette!(
                        labels = vec![LabeledSpan::at(
                            self.position - 1..self.position,
                            "unexpected newline in string"
                        )],
                        "unexpected newline in string"
                    )
                    .with_source_code(self.get_source_code()));
                }
                _ => string.push(c as u8),
            }

            self.position += c.len_utf8();
        }

        Err(miette!(
            labels = vec![LabeledSpan::at(
                self.position..self.position + 1,
                if let Some(equals) = long_form {
                    format!("expected ]{}]", "=".repeat(equals as usize))
                } else {
                    format!("expected {}", starting_character)
                }
            )],
            "unexpected end of string"
        )
        .with_source_code(self.get_source_code()))
    }

    fn parse_number(
        &mut self,
        starting_character: char,
        start: usize,
    ) -> miette::Result<Token<'source>> {
        let mut has_fraction = starting_character == '.';
        let mut is_hex = false;
        let mut has_exponent = false;
        let mut just_started_exponent = false;
        let mut chars = self.rest.chars().enumerate();

        // Find the end of the number
        let end = chars.position(|(i, c)| {
            let min_fraction_index = if is_hex { 1 } else { 0 };

            if c == '.' && !has_fraction && i >= min_fraction_index {
                has_fraction = true;
                false
            } else if (c == 'x' || c == 'X') && i == 0 && starting_character == '0' {
                is_hex = true;
                false
            } else if (is_hex && !has_exponent && i > 1 && (c == 'p' || c == 'P'))
                || (!is_hex && !has_exponent && (c == 'e' || c == 'E'))
            {
                has_exponent = true;
                just_started_exponent = true;
                false
            } else if just_started_exponent && (c == '+' || c == '-') {
                just_started_exponent = false;
                false
            } else if is_hex && !has_exponent {
                just_started_exponent = false;
                !c.is_ascii_hexdigit()
            } else {
                just_started_exponent = false;
                !c.is_ascii_digit()
            }
        });

        // Only floats have exponents
        let is_float = has_fraction || has_exponent;

        let end = end.unwrap_or(self.rest.len());
        self.position += end;
        self.rest = &self.rest[end..];

        // Let's check that the end here makes sense (e.g. no "0xfi" or "123a")
        let next = self.rest.chars().next();
        match next {
            Some(c) if c.is_ascii_alphabetic() => {
                return Err(miette!(
                    labels = vec![LabeledSpan::at(start..self.position + 1, "this literal")],
                    "invalid number literal"
                )
                .with_source_code(self.get_source_code()));
            }
            _ => {}
        }

        let literal = &self.source[start..self.position];

        Ok(Token {
            kind: match (is_float, is_hex) {
                // Integer
                (false, false) => TokenKind::Integer(parse_decimal_number(literal.chars())),
                // Hex integer
                (false, true) => TokenKind::Integer(parse_hex_number(literal[2..].chars())),
                // Float
                (true, false) => TokenKind::Float(literal.parse().map_err(|e| {
                    miette!(
                        labels = vec![LabeledSpan::at(start..self.position, "this literal")],
                        "failed to parse float literal: {e}"
                    )
                    .with_source_code(self.get_source_code())
                })?),
                // Hex float
                (true, true) => {
                    let exponent_start = if has_exponent {
                        literal
                            .find(|c: char| c == 'p' || c == 'P')
                            .expect("has exponent")
                    } else {
                        literal.len()
                    };
                    let fraction_start = if has_fraction {
                        literal.find('.').expect("has fraction")
                    } else {
                        exponent_start
                    };

                    let base_str = &literal[2..fraction_start];
                    let base = if base_str.is_empty() {
                        0f64
                    } else {
                        i64::from_str_radix(base_str, 16).map_err(|e| {
                            miette!(
                                labels =
                                    vec![LabeledSpan::at(start..self.position, "this literal")],
                                "failed to parse hex float literal: {e}"
                            )
                            .with_source_code(self.get_source_code())
                        })? as f64
                    };
                    let fraction = if has_fraction {
                        let fraction_str = &literal[fraction_start + 1..exponent_start];
                        let fraction = i64::from_str_radix(fraction_str, 16).map_err(|e| {
                            miette!(
                                labels =
                                    vec![LabeledSpan::at(start..self.position, "this literal")],
                                "failed to parse hex float literal fraction: {e}"
                            )
                            .with_source_code(self.get_source_code())
                        })? as f64;
                        let fraction_digits = fraction_str.len() as f64;
                        fraction / 16f64.powf(fraction_digits)
                    } else {
                        0.0
                    };

                    let value = base + fraction;
                    TokenKind::Float(if has_exponent {
                        let exponent_str = &literal[exponent_start + 1..];
                        let exponent = exponent_str.parse::<f64>().map_err(|e| {
                            miette!(
                                labels =
                                    vec![LabeledSpan::at(start..self.position, "this literal")],
                                "failed to parse hex float literal exponent: {e}"
                            )
                            .with_source_code(self.get_source_code())
                        })?;

                        value * 2f64.powf(exponent)
                    } else {
                        value
                    })
                }
            },
            span: Span {
                start,
                end: self.position,
            },
        })
    }

    fn parse_dot(&mut self, start: usize) -> miette::Result<Token<'source>> {
        let mut chars = self.rest.chars().peekable();
        match chars.peek() {
            Some('.') => {
                let next = chars.next().expect("already peeked on it");
                let width = next.len_utf8();
                self.position += width;
                self.rest = &self.rest[width..];

                match chars.peek() {
                    Some('.') => {
                        let next = chars.next().expect("already peeked on it");
                        let width = next.len_utf8();
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
            Some('0'..='9') => self
                .parse_number('.', start)
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
    ) -> miette::Result<Token<'source>> {
        let mut chars = self.rest.chars().peekable();
        match (chars.peek(), option_b) {
            (Some(&next), Some(option_b)) if next == option_b.next => {
                let next = chars.next().expect("already peeked on it");
                let width = next.len_utf8();
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
            (Some(&next), _) if next == option_a.next => {
                let next = chars.next().expect("already peeked on it");
                let width = next.len_utf8();
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

fn parse_decimal_number(literal: impl Iterator<Item = char>) -> i64 {
    literal.fold(0i64, |acc, c| {
        acc.wrapping_mul(10).wrapping_add(
            c.to_digit(10)
                .expect("expected only decimal numbers as input to parse_decimal_number")
                as i64,
        )
    })
}

fn parse_hex_number(literal: impl Iterator<Item = char>) -> i64 {
    literal.fold(0i64, |acc, c| {
        acc.wrapping_mul(16).wrapping_add(
            c.to_digit(16)
                .expect("expected only hex numbers as input to parse_hex_number")
                as i64,
        )
    })
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
            let mut lexer = Lexer::new(PathBuf::from("test.lua"), input);
            let token = lexer.next().unwrap().unwrap();
            assert_eq!(token.kind, expected, "when parsing '{input}'");
        }

        // Invalid numbers
        for input in &["0x5pf", "0xi"] {
            let mut lexer = Lexer::new(PathBuf::from("test.lua"), input);
            let token = lexer.next().unwrap();
            assert!(token.is_err());
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
            let mut lexer = Lexer::new(PathBuf::from("test.lua"), example);
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
            let mut lexer = Lexer::new(PathBuf::from("test.lua"), input);
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
            let mut lexer = Lexer::new(PathBuf::from("test.lua"), input);
            let token = lexer.next();
            assert!(token.is_none(), "when parsing '{input}'");

            // Also check that we can find tokens after the comment
            let input = format!("{} 3", input);
            let mut lexer = Lexer::new(PathBuf::from("test.lua"), &input);
            let token = lexer.next().unwrap().unwrap();
            assert_eq!(token.kind, TokenKind::Integer(3));
        }
    }
}
