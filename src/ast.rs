// Reference: https://www.lua.org/manual/5.4/manual.html#9

use crate::token::Span;

#[derive(Debug, PartialEq, Clone)]
pub struct TokenTree<T> {
    pub node: T,
    pub span: Span,
}

impl<T> TokenTree<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<TokenTree<Statement>>,
    pub return_statement: Option<Vec<TokenTree<Expression>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Assignment {
        varlist: Vec<TokenTree<Variable>>,
        explist: Vec<TokenTree<Expression>>,
    },
    Block(TokenTree<Block>),
    While {
        condition: TokenTree<Expression>,
        block: TokenTree<Block>,
    },
    Repeat {
        block: TokenTree<Block>,
        condition: TokenTree<Expression>,
    },
    If {
        condition: TokenTree<Expression>,
        block: TokenTree<Block>,
        else_ifs: Vec<TokenTree<ElseIf>>,
        else_block: Option<TokenTree<Block>>,
    },
    For {
        condition: TokenTree<ForCondition>,
        block: TokenTree<Block>,
    },
    Goto(TokenTree<Name>),
    Label(TokenTree<Name>),
    Break,
    FunctionCall(TokenTree<FunctionCall>),
    LocalDeclaraction(Vec<TokenTree<AttributedName>>, Vec<TokenTree<Expression>>),
    LocalFunctionDeclaration(TokenTree<Name>, TokenTree<FunctionDef>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Name(pub Vec<u8>);

#[derive(Debug, PartialEq, Clone)]
pub struct AttributedName {
    pub name: TokenTree<Name>,
    pub attribute: Option<TokenTree<LocalAttribute>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LocalAttribute {
    Const,
    Close,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Variable {
    Name(TokenTree<Name>),
    Indexed(Box<TokenTree<PrefixExpression>>, Box<TokenTree<Expression>>),
    Field(Box<TokenTree<PrefixExpression>>, TokenTree<Name>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ElseIf {
    pub condition: TokenTree<Expression>,
    pub block: TokenTree<Block>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForCondition {
    NumericFor {
        name: TokenTree<Name>,
        initial: TokenTree<Expression>,
        limit: TokenTree<Expression>,
        step: Option<TokenTree<Expression>>,
    },
    GenericFor {
        names: Vec<TokenTree<Name>>,
        expressions: Vec<TokenTree<Expression>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    PrefixExpression(TokenTree<PrefixExpression>),
    Literal(TokenTree<Literal>),
    FunctionDef(TokenTree<FunctionDef>),
    TableConstructor(TokenTree<TableConstructor>),
    Ellipsis,
    BinaryOp {
        op: TokenTree<BinaryOperator>,
        lhs: Box<TokenTree<Expression>>,
        rhs: Box<TokenTree<Expression>>,
    },
    UnaryOp {
        op: TokenTree<UnaryOperator>,
        rhs: Box<TokenTree<Expression>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrefixExpression {
    Variable(TokenTree<Variable>),
    FunctionCall(TokenTree<FunctionCall>),
    Parenthesized(Box<TokenTree<Expression>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall {
    pub function: Box<TokenTree<PrefixExpression>>,
    pub as_method: bool,
    pub name: Option<TokenTree<Name>>,
    pub args: TokenTree<Vec<TokenTree<Expression>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Nil,
    Boolean(bool),
    Number(Number),
    String(Vec<u8>),
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
pub struct TableConstructor {
    pub fields: Vec<TokenTree<Field>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Field {
    Named(TokenTree<Name>, TokenTree<Expression>),
    Indexed(TokenTree<Expression>, TokenTree<Expression>),
    Value(TokenTree<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDef {
    pub name: Option<Vec<u8>>,
    pub parameters: Vec<TokenTree<Name>>,
    pub has_varargs: bool,
    pub block: TokenTree<Block>,
}
