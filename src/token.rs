#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
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
    Identifier, // NOTE: The value of the identifier is stored in the token's span.

    // Literals
    Nil,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(Vec<u8>), // TODO: Extract to dedicated type.
}
