//! The compiler's input: lexical bindings are explicit and globals are `_ENV` fields.
use crate::{ast, value::LuaString};
pub use ast::{BinaryOperator, Literal, LocalAttribute, Number, TokenTree, UnaryOperator};

pub type BindingId = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub id: BindingId,
    pub name: LuaString,
    pub attributes: u8,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Name {
    Local(Binding),
    Upvalue(Binding),
}

impl Name {
    pub fn binding(&self) -> &Binding {
        match self {
            Self::Local(binding) | Self::Upvalue(binding) => binding,
        }
    }
}

/// A resolved main chunk. Its captures include the externally supplied `_ENV`.
#[derive(Debug, Clone, PartialEq)]
pub struct Chunk {
    pub block: TokenTree<Block>,
    pub parameters: Vec<TokenTree<Name>>,
    pub captures: Vec<Name>,
}

pub type Block = ast::Block<Name>;
pub type ReturnStatement = ast::ReturnStatement<Name>;
pub type Statement = ast::Statement<Name>;
pub type AttributedName = ast::AttributedName<Name>;
pub type Variable = ast::Variable<Name>;
pub type ElseIf = ast::ElseIf<Name>;
pub type ForCondition = ast::ForCondition<Name>;
pub type NumericFor = ast::NumericFor<Name>;
pub type Expression = ast::Expression<Name>;
pub type PrefixExpression = ast::PrefixExpression<Name>;
pub type FunctionCall = ast::FunctionCall<Name>;
pub type TableConstructor = ast::TableConstructor<Name>;
pub type Field = ast::Field<Name>;
pub type FunctionDef = ast::FunctionDef<Name>;
