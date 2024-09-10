use crate::{ast::BinaryOperator, token::TokenKind};

impl BinaryOperator {
    pub(crate) fn from(op: &TokenKind) -> Self {
        match op {
            TokenKind::Or => Self::Or,
            TokenKind::And => Self::And,
            TokenKind::Less => Self::LessThan,
            TokenKind::Greater => Self::GreaterThan,
            TokenKind::LessEquals => Self::LessThanOrEqual,
            TokenKind::GreaterEquals => Self::GreaterThanOrEqual,
            TokenKind::TildeEquals => Self::NotEqual,
            TokenKind::EqualsEquals => Self::Equal,
            TokenKind::Pipe => Self::BitwiseOr,
            TokenKind::Tilde => Self::BitwiseXor,
            TokenKind::Ampersand => Self::BitwiseAnd,
            TokenKind::ShiftLeft => Self::ShiftLeft,
            TokenKind::ShiftRight => Self::ShiftRight,
            TokenKind::DotDot => Self::Concat,
            TokenKind::Plus => Self::Add,
            TokenKind::Minus => Self::Sub,
            TokenKind::Star => Self::Mul,
            TokenKind::Slash => Self::Div,
            TokenKind::SlashSlash => Self::FloorDiv,
            TokenKind::Percent => Self::Mod,
            TokenKind::Caret => Self::Pow,

            _ => unreachable!(),
        }
    }
}

impl<'source> TokenKind<'source> {
    // For easier matching
    pub(crate) fn is_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Or
                | TokenKind::And
                | TokenKind::Less
                | TokenKind::Greater
                | TokenKind::LessEquals
                | TokenKind::GreaterEquals
                | TokenKind::TildeEquals
                | TokenKind::EqualsEquals
                | TokenKind::Pipe
                | TokenKind::Tilde
                | TokenKind::Ampersand
                | TokenKind::ShiftLeft
                | TokenKind::ShiftRight
                | TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::SlashSlash
                | TokenKind::Percent
                | TokenKind::Caret
                | TokenKind::DotDot
                | TokenKind::Not
                | TokenKind::Hash
        )
    }
}
