//! Lua syntax trees, parameterized by the representation of lexical names.
//!
//! The parser produces the default `Name` nodes. Name resolution produces the
//! same tree structure with `resolved_ast::Name` nodes carrying binding data.
//! Field keys, method names, and labels remain syntactic names in both trees.
// Reference: https://www.lua.org/manual/5.4/manual.html#9

use std::fmt::Debug;

use crate::{token::Span, value::LuaString};

#[derive(PartialEq, Clone)]
pub struct TokenTree<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Debug for TokenTree<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} - {:#?}", self.span, self.node)
    }
}

impl<T> TokenTree<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block<N = Name> {
    pub statements: Vec<TokenTree<Statement<N>>>,
    pub return_statement: Option<ReturnStatement<N>>,
    pub end_span: Option<Span>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement<N = Name> {
    pub return_span: Span,
    pub values: Vec<TokenTree<Expression<N>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<N = Name> {
    Assignment {
        varlist: Vec<TokenTree<Variable<N>>>,
        explist: Vec<TokenTree<Expression<N>>>,
    },
    Block(TokenTree<Block<N>>),
    While {
        condition: TokenTree<Expression<N>>,
        block: TokenTree<Block<N>>,
    },
    Repeat {
        block: TokenTree<Block<N>>,
        condition: TokenTree<Expression<N>>,
    },
    If {
        condition: TokenTree<Expression<N>>,
        block: TokenTree<Block<N>>,
        else_ifs: Vec<TokenTree<ElseIf<N>>>,
        else_block: Option<TokenTree<Block<N>>>,
    },
    For {
        condition: TokenTree<ForCondition<N>>,
        block: TokenTree<Block<N>>,
    },
    Goto(TokenTree<Name>),
    Label(TokenTree<Name>),
    Break,
    FunctionCall(TokenTree<FunctionCall<N>>),
    LocalDeclaraction(
        Vec<TokenTree<AttributedName<N>>>,
        Vec<TokenTree<Expression<N>>>,
    ),
    LocalFunctionDeclaration(TokenTree<N>, TokenTree<FunctionDef<N>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Name(pub LuaString);

#[derive(Debug, PartialEq, Clone)]
pub struct AttributedName<N = Name> {
    pub name: TokenTree<N>,
    pub attribute: Option<TokenTree<LocalAttribute>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LocalAttribute {
    Const,
    Close,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Variable<N = Name> {
    Name(TokenTree<N>),
    Indexed(
        Box<TokenTree<PrefixExpression<N>>>,
        Box<TokenTree<Expression<N>>>,
    ),
    Field(Box<TokenTree<PrefixExpression<N>>>, TokenTree<Name>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ElseIf<N = Name> {
    pub condition: TokenTree<Expression<N>>,
    pub block: TokenTree<Block<N>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForCondition<N = Name> {
    NumericFor(Box<NumericFor<N>>),
    GenericFor {
        names: Vec<TokenTree<N>>,
        expressions: Vec<TokenTree<Expression<N>>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct NumericFor<N = Name> {
    pub name: TokenTree<N>,
    pub initial: TokenTree<Expression<N>>,
    pub limit: TokenTree<Expression<N>>,
    pub step: Option<TokenTree<Expression<N>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<N = Name> {
    PrefixExpression(TokenTree<PrefixExpression<N>>),
    Literal(TokenTree<Literal>),
    FunctionDef(TokenTree<FunctionDef<N>>),
    TableConstructor(TokenTree<TableConstructor<N>>),
    Ellipsis,
    BinaryOp {
        op: TokenTree<BinaryOperator>,
        lhs: Box<TokenTree<Expression<N>>>,
        rhs: Box<TokenTree<Expression<N>>>,
    },
    UnaryOp {
        op: TokenTree<UnaryOperator>,
        rhs: Box<TokenTree<Expression<N>>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrefixExpression<N = Name> {
    Variable(TokenTree<Variable<N>>),
    FunctionCall(TokenTree<FunctionCall<N>>),
    Parenthesized(Box<TokenTree<Expression<N>>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall<N = Name> {
    pub function: Box<TokenTree<PrefixExpression<N>>>,
    pub method_name: Option<TokenTree<Name>>,
    pub args: TokenTree<Vec<TokenTree<Expression<N>>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Nil,
    Boolean(bool),
    Number(Number),
    String(LuaString),
}

#[derive(Debug, Copy, Clone)]
pub enum Number {
    Integer(i64),
    Float(f64),
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Number::Integer(a), Number::Integer(b)) => a == b,
            (Number::Float(a), Number::Float(b)) => a == b,
            (Number::Integer(a), Number::Float(b)) => (*a as f64) == *b,
            (Number::Float(a), Number::Integer(b)) => *a == (*b as f64),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOperator {
    // Numeric
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Mod,
    Pow,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,

    // Comparison
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,

    // Logical
    And,
    Or,

    // String
    Concat,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    // Numeric
    Neg,
    BitwiseNot,

    // Logical
    Not,

    // Length
    Length,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TableConstructor<N = Name> {
    pub fields: Vec<TokenTree<Field<N>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Field<N = Name> {
    Named(TokenTree<Name>, TokenTree<Expression<N>>),
    Indexed(TokenTree<Expression<N>>, TokenTree<Expression<N>>),
    Value(TokenTree<Expression<N>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDef<N = Name> {
    pub name: Option<LuaString>,
    pub method_name: Option<LuaString>,
    pub parameters: Vec<TokenTree<N>>,
    /// Captured bindings, populated by name resolution.
    pub captures: Vec<N>,
    pub varargs: Option<Span>,
    pub block: TokenTree<Block<N>>,
}
