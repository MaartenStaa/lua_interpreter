use crate::ast::*;

/// Apply optimizations to the AST
/// At the moment, we only do constant folding
pub fn optimize(program: Block) -> Block {
    Block {
        statements: program
            .statements
            .into_iter()
            .map(optimize_statement)
            .collect(),
        return_statement: program
            .return_statement
            .map(|exprs| exprs.into_iter().map(optimize_expression).collect()),
    }
}

fn optimize_statement(statement: Statement) -> Statement {
    match statement {
        Statement::Assignment { varlist, explist } => Statement::Assignment {
            varlist: varlist.into_iter().map(optimize_variable).collect(),
            explist: explist.into_iter().map(optimize_expression).collect(),
        },
        Statement::Block(block) => Statement::Block(optimize(block)),
        Statement::While { condition, block } => Statement::While {
            condition: optimize_expression(condition),
            block: optimize(block),
        },
        Statement::Repeat { block, condition } => Statement::Repeat {
            block: optimize(block),
            condition: optimize_expression(condition),
        },
        Statement::If {
            condition,
            block,
            else_ifs,
            else_block,
        } => Statement::If {
            condition: optimize_expression(condition),
            block: optimize(block),
            else_ifs: else_ifs.into_iter().map(optimize_else_if).collect(),
            else_block: else_block.map(optimize),
        },
        Statement::For { condition, block } => Statement::For {
            condition: optimize_for_condition(condition),
            block: optimize(block),
        },
        Statement::FunctionCall(function_call) => {
            Statement::FunctionCall(optimize_function_call(function_call))
        }
        Statement::LocalDeclaraction(names, expressions) => Statement::LocalDeclaraction(
            names,
            expressions.into_iter().map(optimize_expression).collect(),
        ),

        // No optimization to be done
        Statement::Goto(label) => Statement::Goto(label),
        Statement::Label(label) => Statement::Label(label),
        Statement::Break => Statement::Break,
    }
}

fn optimize_else_if(else_if: ElseIf) -> ElseIf {
    ElseIf {
        condition: optimize_expression(else_if.condition),
        block: optimize(else_if.block),
    }
}

fn optimize_for_condition(condition: ForCondition) -> ForCondition {
    match condition {
        ForCondition::NumericFor {
            name,
            initial,
            step,
            limit,
        } => ForCondition::NumericFor {
            name,
            initial: optimize_expression(initial),
            step: step.map(optimize_expression),
            limit: optimize_expression(limit),
        },
        ForCondition::GenericFor { names, expressions } => ForCondition::GenericFor {
            names,
            expressions: expressions.into_iter().map(optimize_expression).collect(),
        },
    }
}

fn optimize_function_call(function_call: FunctionCall) -> FunctionCall {
    FunctionCall {
        function: Box::new(optimize_prefix_expression(*function_call.function)),
        as_method: function_call.as_method,
        name: function_call.name,
        args: function_call
            .args
            .into_iter()
            .map(optimize_expression)
            .collect(),
    }
}

fn optimize_expression(expression: Expression) -> Expression {
    match expression {
        Expression::PrefixExpression(prefix) => {
            let optimized_prefix = optimize_prefix_expression(prefix);
            match optimized_prefix {
                PrefixExpression::Parenthesized(parenthesized) => {
                    if matches!(&*parenthesized, Expression::Literal(_)) {
                        *parenthesized
                    } else {
                        Expression::PrefixExpression(PrefixExpression::Parenthesized(parenthesized))
                    }
                }
                other => Expression::PrefixExpression(other),
            }
        }
        Expression::FunctionDef(func) => Expression::FunctionDef(optimize_function_def(func)),
        Expression::TableConstructor(table) => {
            Expression::TableConstructor(optimize_table_constructor(table))
        }
        Expression::BinaryOp { op, lhs, rhs } => optimize_binary_op(op, *lhs, *rhs),
        Expression::UnaryOp { op, rhs } => optimize_unary_op(op, *rhs),

        // No optimization to be done
        Expression::Literal(literal) => Expression::Literal(literal),
        Expression::Ellipsis => Expression::Ellipsis,
    }
}

fn optimize_prefix_expression(prefix: PrefixExpression) -> PrefixExpression {
    match prefix {
        PrefixExpression::Variable(var) => PrefixExpression::Variable(optimize_variable(var)),
        PrefixExpression::Parenthesized(expr) => {
            PrefixExpression::Parenthesized(Box::new(optimize_expression(*expr)))
        }
        PrefixExpression::FunctionCall(func) => {
            PrefixExpression::FunctionCall(optimize_function_call(func))
        }
    }
}

fn optimize_variable<T>(var: Variable<T>) -> Variable<T> {
    match var {
        Variable::Name(name) => Variable::Name(name),
        Variable::Field(prefix, name) => {
            Variable::Field(Box::new(optimize_prefix_expression(*prefix)), name)
        }
        Variable::Indexed(prefix, index_expression) => Variable::Indexed(
            Box::new(optimize_prefix_expression(*prefix)),
            Box::new(optimize_expression(*index_expression)),
        ),
    }
}

fn optimize_function_def(func: FunctionDef) -> FunctionDef {
    FunctionDef {
        parameters: func.parameters,
        has_varargs: func.has_varargs,
        block: optimize(func.block),
    }
}

fn optimize_table_constructor(table: TableConstructor) -> TableConstructor {
    TableConstructor {
        fields: table.fields.into_iter().map(optimize_table_field).collect(),
    }
}

fn optimize_table_field(field: Field) -> Field {
    match field {
        Field::Value(expression) => Field::Value(optimize_expression(expression)),
        Field::Named(name, expression) => Field::Named(name, optimize_expression(expression)),
        Field::Indexed(index, value) => {
            Field::Indexed(optimize_expression(index), optimize_expression(value))
        }
    }
}

fn optimize_binary_op(op: BinaryOperator, lhs: Expression, rhs: Expression) -> Expression {
    let lhs = optimize_expression(lhs);
    let rhs = optimize_expression(rhs);

    match (op, lhs, rhs) {
        (
            BinaryOperator::Add,
            Expression::Literal(Literal::Number(lhs)),
            Expression::Literal(Literal::Number(rhs)),
        ) => Expression::Literal(Literal::Number(lhs + rhs)),
        (
            BinaryOperator::Sub,
            Expression::Literal(Literal::Number(lhs)),
            Expression::Literal(Literal::Number(rhs)),
        ) => Expression::Literal(Literal::Number(lhs - rhs)),
        (
            BinaryOperator::Mul,
            Expression::Literal(Literal::Number(lhs)),
            Expression::Literal(Literal::Number(rhs)),
        ) => Expression::Literal(Literal::Number(lhs * rhs)),
        (
            BinaryOperator::Div,
            Expression::Literal(Literal::Number(lhs)),
            Expression::Literal(Literal::Number(rhs)),
        ) => Expression::Literal(Literal::Number(lhs / rhs)),
        (
            BinaryOperator::Mod,
            Expression::Literal(Literal::Number(lhs)),
            Expression::Literal(Literal::Number(rhs)),
        ) => Expression::Literal(Literal::Number(lhs % rhs)),
        (
            BinaryOperator::Pow,
            Expression::Literal(Literal::Number(lhs)),
            Expression::Literal(Literal::Number(rhs)),
        ) => Expression::Literal(Literal::Number(lhs.pow(rhs))),
        (
            BinaryOperator::Concat,
            Expression::Literal(Literal::String(mut lhs)),
            Expression::Literal(Literal::String(rhs)),
        ) => {
            lhs.extend(rhs);
            Expression::Literal(Literal::String(lhs))
        }
        (
            BinaryOperator::Concat,
            Expression::Literal(Literal::String(mut lhs)),
            Expression::Literal(Literal::Number(rhs)),
        ) => {
            lhs.extend(rhs.to_string().into_bytes());
            Expression::Literal(Literal::String(lhs))
        }
        (
            BinaryOperator::Concat,
            Expression::Literal(Literal::Number(lhs)),
            Expression::Literal(Literal::String(rhs)),
        ) => Expression::Literal(Literal::String(Vec::from_iter(
            lhs.to_string().into_bytes().into_iter().chain(rhs),
        ))),
        (
            BinaryOperator::Concat,
            Expression::Literal(Literal::Number(lhs)),
            Expression::Literal(Literal::Number(rhs)),
        ) => {
            let mut lhs = lhs.to_string();
            lhs.push_str(&rhs.to_string());
            Expression::Literal(Literal::String(lhs.into_bytes()))
        }
        (
            BinaryOperator::FloorDiv,
            Expression::Literal(Literal::Number(lhs)),
            Expression::Literal(Literal::Number(rhs)),
        ) => Expression::Literal(Literal::Number((lhs / rhs).floor())),
        (
            BinaryOperator::And,
            Expression::Literal(Literal::Boolean(lhs)),
            Expression::Literal(Literal::Boolean(rhs)),
        ) => Expression::Literal(Literal::Boolean(lhs && rhs)),
        (
            BinaryOperator::Or,
            Expression::Literal(Literal::Boolean(lhs)),
            Expression::Literal(Literal::Boolean(rhs)),
        ) => Expression::Literal(Literal::Boolean(lhs || rhs)),
        (
            BinaryOperator::LessThan,
            Expression::Literal(Literal::Number(lhs)),
            Expression::Literal(Literal::Number(rhs)),
        ) => Expression::Literal(Literal::Boolean(lhs < rhs)),
        (
            BinaryOperator::GreaterThan,
            Expression::Literal(Literal::Number(lhs)),
            Expression::Literal(Literal::Number(rhs)),
        ) => Expression::Literal(Literal::Boolean(lhs > rhs)),
        (
            BinaryOperator::LessThanOrEqual,
            Expression::Literal(Literal::Number(lhs)),
            Expression::Literal(Literal::Number(rhs)),
        ) => Expression::Literal(Literal::Boolean(lhs <= rhs)),
        (
            BinaryOperator::GreaterThanOrEqual,
            Expression::Literal(Literal::Number(lhs)),
            Expression::Literal(Literal::Number(rhs)),
        ) => Expression::Literal(Literal::Boolean(lhs >= rhs)),
        (BinaryOperator::Equal, Expression::Literal(lhs), Expression::Literal(rhs)) => {
            Expression::Literal(Literal::Boolean(lhs == rhs))
        }
        (BinaryOperator::NotEqual, Expression::Literal(lhs), Expression::Literal(rhs)) => {
            Expression::Literal(Literal::Boolean(lhs != rhs))
        }
        (
            BinaryOperator::BitwiseOr,
            Expression::Literal(Literal::Number(Number::Integer(lhs))),
            Expression::Literal(Literal::Number(Number::Integer(rhs))),
        ) => Expression::Literal(Literal::Number(Number::Integer(lhs | rhs))),
        (
            BinaryOperator::BitwiseXor,
            Expression::Literal(Literal::Number(Number::Integer(lhs))),
            Expression::Literal(Literal::Number(Number::Integer(rhs))),
        ) => Expression::Literal(Literal::Number(Number::Integer(lhs ^ rhs))),
        (
            BinaryOperator::BitwiseAnd,
            Expression::Literal(Literal::Number(Number::Integer(lhs))),
            Expression::Literal(Literal::Number(Number::Integer(rhs))),
        ) => Expression::Literal(Literal::Number(Number::Integer(lhs & rhs))),
        (
            BinaryOperator::ShiftLeft,
            Expression::Literal(Literal::Number(Number::Integer(lhs))),
            Expression::Literal(Literal::Number(Number::Integer(rhs))),
        ) => Expression::Literal(Literal::Number(Number::Integer(lhs << rhs))),
        (
            BinaryOperator::ShiftRight,
            Expression::Literal(Literal::Number(Number::Integer(lhs))),
            Expression::Literal(Literal::Number(Number::Integer(rhs))),
        ) => Expression::Literal(Literal::Number(Number::Integer(lhs >> rhs))),
        (op, lhs, rhs) => Expression::BinaryOp {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        },
    }
}

fn optimize_unary_op(op: UnaryOperator, rhs: Expression) -> Expression {
    let rhs = optimize_expression(rhs);

    match (op, rhs) {
        (UnaryOperator::Neg, Expression::Literal(Literal::Number(rhs))) => {
            Expression::Literal(Literal::Number(-rhs))
        }
        (UnaryOperator::BitwiseNot, Expression::Literal(Literal::Number(Number::Integer(rhs)))) => {
            Expression::Literal(Literal::Number(Number::Integer(!rhs)))
        }
        (UnaryOperator::Not, Expression::Literal(Literal::Boolean(rhs))) => {
            Expression::Literal(Literal::Boolean(!rhs))
        }
        (UnaryOperator::Not, Expression::Literal(Literal::Nil)) => {
            Expression::Literal(Literal::Boolean(true))
        }
        (UnaryOperator::Not, Expression::Literal(_)) => {
            Expression::Literal(Literal::Boolean(false))
        }
        (op, rhs) => Expression::UnaryOp {
            op,
            rhs: Box::new(rhs),
        },
    }
}
