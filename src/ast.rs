// Reference: https://www.lua.org/manual/5.4/manual.html#9

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub return_statement: Option<Vec<Expression>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Assignment(Assignment),
    Block(Block),
    While(While),
    Repeat(Repeat),
    If(If),
    For(For),
    Goto(Name),
    Label(Name),
    Break,
    FunctionCall(FunctionCall),
    LocalDeclaraction(Vec<AttributedName>, Vec<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Name(pub String);

#[derive(Debug, PartialEq, Clone)]
pub struct AttributedName {
    pub name: Name,
    pub attribute: Option<LocalAttribute>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LocalAttribute {
    Const,
    Close,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Variable {
    Name(Name),
    Indexed(Box<PrefixExpression>, Box<Expression>),
    Field(Box<PrefixExpression>, Name),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assignment {
    pub varlist: Vec<Variable>,
    pub explist: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct While {
    pub condition: Expression,
    pub block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Repeat {
    pub block: Block,
    pub condition: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct If {
    pub condition: Expression,
    pub block: Block,
    pub else_ifs: Vec<ElseIf>,
    pub else_block: Option<Block>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ElseIf {
    pub condition: Expression,
    pub block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub struct For {
    pub condition: ForCondition,
    pub block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForCondition {
    NumericFor {
        name: Name,
        initial: Expression,
        limit: Expression,
        step: Option<Expression>,
    },
    GenericFor {
        names: Vec<Name>,
        expressions: Vec<Expression>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    PrefixExpression(PrefixExpression),
    Literal(Literal),
    FunctionDef(FunctionDef),
    TableConstructor(TableConstructor),
    Ellipsis,
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrefixExpression {
    Variable(Variable),
    FunctionCall(FunctionCall),
    Parenthesized(Box<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall {
    pub function: Box<PrefixExpression>,
    pub as_method: bool,
    pub name: Option<Name>,
    pub args: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Nil,
    Boolean(bool),
    Number(Number),
    String(Vec<u8>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    Integer(i64),
    Float(f64),
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryOp {
    pub op: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryOp {
    pub op: UnaryOperator,
    pub operand: Box<Expression>,
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
    pub fields: Vec<Field>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Field {
    Named(Name, Expression),
    Indexed(Expression, Expression),
    Value(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDef {
    pub parameters: Vec<Name>,
    pub has_varargs: bool,
    pub block: Block,
}
