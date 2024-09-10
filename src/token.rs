use miette::{LabeledSpan, SourceSpan};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn labeled(&self, label: impl Into<String>) -> LabeledSpan {
        LabeledSpan::at(SourceSpan::from(*self), label)
    }
}

impl From<Span> for SourceSpan {
    fn from(span: Span) -> SourceSpan {
        SourceSpan::new(span.start.into(), span.end - span.start)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'source> {
    pub kind: TokenKind<'source>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind<'source> {
    // Keywords
    And,
    Break,
    Do,
    Else,
    ElseIf,
    End,
    False,
    For,
    Function,
    Goto,
    If,
    In,
    Local,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,

    // Punctuation
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Hash,
    Ampersand,
    Tilde,
    Pipe,
    ShiftLeft,
    ShiftRight,
    SlashSlash,
    EqualsEquals,
    TildeEquals,
    LessEquals,
    GreaterEquals,
    Less,
    Greater,
    Equals,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    DoubleColon,
    Semicolon,
    Colon,
    Comma,
    Dot,
    DotDot,
    DotDotDot,

    // Identifiers
    Identifier(&'source str),

    // Literals
    Nil,
    Integer(i64),
    Float(f64),
    String(Vec<u8>), // TODO: Extract to dedicated type.
}
