use crate::ast;

/// Apply optimizations to the AST
/// At the moment, we only do constant folding
pub fn optimize(program: ast::Block) -> ast::Block {
    ast::Block {
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

fn optimize_statement(statement: ast::Statement) -> ast::Statement {
    match statement {
        ast::Statement::Assignment { varlist, explist } => ast::Statement::Assignment {
            varlist: varlist.into_iter().map(optimize_variable).collect(),
            explist: explist.into_iter().map(optimize_expression).collect(),
        },
        ast::Statement::Block(block) => ast::Statement::Block(optimize(block)),
        ast::Statement::While { condition, block } => ast::Statement::While {
            condition: optimize_expression(condition),
            block: optimize(block),
        },
        ast::Statement::Repeat { block, condition } => ast::Statement::Repeat {
            block: optimize(block),
            condition: optimize_expression(condition),
        },
        ast::Statement::If {
            condition,
            block,
            else_ifs,
            else_block,
        } => ast::Statement::If {
            condition: optimize_expression(condition),
            block: optimize(block),
            else_ifs: else_ifs.into_iter().map(optimize_else_if).collect(),
            else_block: else_block.map(optimize),
        },
        ast::Statement::For { condition, block } => ast::Statement::For {
            condition: optimize_for_condition(condition),
            block: optimize(block),
        },
        ast::Statement::FunctionCall(function_call) => {
            ast::Statement::FunctionCall(optimize_function_call(function_call))
        }
        ast::Statement::LocalDeclaraction(names, expressions) => ast::Statement::LocalDeclaraction(
            names,
            expressions.into_iter().map(optimize_expression).collect(),
        ),

        // No optimization to be done
        ast::Statement::Goto(label) => ast::Statement::Goto(label),
        ast::Statement::Label(label) => ast::Statement::Label(label),
        ast::Statement::Break => ast::Statement::Break,
    }
}

fn optimize_else_if(else_if: ast::ElseIf) -> ast::ElseIf {
    ast::ElseIf {
        condition: optimize_expression(else_if.condition),
        block: optimize(else_if.block),
    }
}

fn optimize_for_condition(condition: ast::ForCondition) -> ast::ForCondition {
    match condition {
        ast::ForCondition::NumericFor {
            name,
            initial,
            step,
            limit,
        } => ast::ForCondition::NumericFor {
            name,
            initial: optimize_expression(initial),
            step: step.map(optimize_expression),
            limit: optimize_expression(limit),
        },
        ast::ForCondition::GenericFor { names, expressions } => ast::ForCondition::GenericFor {
            names,
            expressions: expressions.into_iter().map(optimize_expression).collect(),
        },
    }
}

fn optimize_function_call(function_call: ast::FunctionCall) -> ast::FunctionCall {
    ast::FunctionCall {
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

fn optimize_expression(expression: ast::Expression) -> ast::Expression {
    match expression {
        ast::Expression::PrefixExpression(prefix) => {
            let optimized_prefix = optimize_prefix_expression(prefix);
            match optimized_prefix {
                ast::PrefixExpression::Parenthesized(parenthesized) => {
                    if matches!(&*parenthesized, ast::Expression::Literal(_)) {
                        *parenthesized
                    } else {
                        ast::Expression::PrefixExpression(ast::PrefixExpression::Parenthesized(
                            parenthesized,
                        ))
                    }
                }
                other => ast::Expression::PrefixExpression(other),
            }
        }
        ast::Expression::FunctionDef(func) => {
            ast::Expression::FunctionDef(optimize_function_def(func))
        }
        ast::Expression::TableConstructor(table) => {
            ast::Expression::TableConstructor(optimize_table_constructor(table))
        }
        ast::Expression::BinaryOp { op, lhs, rhs } => optimize_binary_op(op, *lhs, *rhs),
        ast::Expression::UnaryOp { op, rhs } => optimize_unary_op(op, *rhs),

        // No optimization to be done
        ast::Expression::Literal(literal) => ast::Expression::Literal(literal),
        ast::Expression::Ellipsis => ast::Expression::Ellipsis,
    }
}

fn optimize_prefix_expression(prefix: ast::PrefixExpression) -> ast::PrefixExpression {
    match prefix {
        ast::PrefixExpression::Variable(var) => {
            ast::PrefixExpression::Variable(optimize_variable(var))
        }
        ast::PrefixExpression::Parenthesized(expr) => {
            ast::PrefixExpression::Parenthesized(Box::new(optimize_expression(*expr)))
        }
        ast::PrefixExpression::FunctionCall(func) => {
            ast::PrefixExpression::FunctionCall(optimize_function_call(func))
        }
    }
}

fn optimize_variable<T>(var: ast::Variable<T>) -> ast::Variable<T> {
    match var {
        ast::Variable::Name(name) => ast::Variable::Name(name),
        ast::Variable::Field(prefix, name) => {
            ast::Variable::Field(Box::new(optimize_prefix_expression(*prefix)), name)
        }
        ast::Variable::Indexed(prefix, index_expression) => ast::Variable::Indexed(
            Box::new(optimize_prefix_expression(*prefix)),
            Box::new(optimize_expression(*index_expression)),
        ),
    }
}

fn optimize_function_def(func: ast::FunctionDef) -> ast::FunctionDef {
    ast::FunctionDef {
        parameters: func.parameters,
        has_varargs: func.has_varargs,
        block: optimize(func.block),
    }
}

fn optimize_table_constructor(table: ast::TableConstructor) -> ast::TableConstructor {
    ast::TableConstructor {
        fields: table.fields.into_iter().map(optimize_table_field).collect(),
    }
}

fn optimize_table_field(field: ast::Field) -> ast::Field {
    match field {
        ast::Field::Value(expression) => ast::Field::Value(optimize_expression(expression)),
        ast::Field::Named(name, expression) => {
            ast::Field::Named(name, optimize_expression(expression))
        }
        ast::Field::Indexed(index, value) => {
            ast::Field::Indexed(optimize_expression(index), optimize_expression(value))
        }
    }
}

fn optimize_binary_op(
    op: ast::BinaryOperator,
    lhs: ast::Expression,
    rhs: ast::Expression,
) -> ast::Expression {
    let lhs = optimize_expression(lhs);
    let rhs = optimize_expression(rhs);

    match (op, lhs, rhs) {
        (
            ast::BinaryOperator::Add,
            ast::Expression::Literal(ast::Literal::Number(lhs)),
            ast::Expression::Literal(ast::Literal::Number(rhs)),
        ) => ast::Expression::Literal(ast::Literal::Number(lhs + rhs)),
        (
            ast::BinaryOperator::Sub,
            ast::Expression::Literal(ast::Literal::Number(lhs)),
            ast::Expression::Literal(ast::Literal::Number(rhs)),
        ) => ast::Expression::Literal(ast::Literal::Number(lhs - rhs)),
        (
            ast::BinaryOperator::Mul,
            ast::Expression::Literal(ast::Literal::Number(lhs)),
            ast::Expression::Literal(ast::Literal::Number(rhs)),
        ) => ast::Expression::Literal(ast::Literal::Number(lhs * rhs)),
        (
            ast::BinaryOperator::Div,
            ast::Expression::Literal(ast::Literal::Number(lhs)),
            ast::Expression::Literal(ast::Literal::Number(rhs)),
        ) => ast::Expression::Literal(ast::Literal::Number(lhs / rhs)),
        (
            ast::BinaryOperator::Mod,
            ast::Expression::Literal(ast::Literal::Number(lhs)),
            ast::Expression::Literal(ast::Literal::Number(rhs)),
        ) => ast::Expression::Literal(ast::Literal::Number(lhs % rhs)),
        (
            ast::BinaryOperator::Pow,
            ast::Expression::Literal(ast::Literal::Number(lhs)),
            ast::Expression::Literal(ast::Literal::Number(rhs)),
        ) => ast::Expression::Literal(ast::Literal::Number(lhs.pow(rhs))),
        (
            ast::BinaryOperator::Concat,
            ast::Expression::Literal(ast::Literal::String(mut lhs)),
            ast::Expression::Literal(ast::Literal::String(rhs)),
        ) => {
            lhs.extend(rhs);
            ast::Expression::Literal(ast::Literal::String(lhs))
        }
        (
            ast::BinaryOperator::FloorDiv,
            ast::Expression::Literal(ast::Literal::Number(lhs)),
            ast::Expression::Literal(ast::Literal::Number(rhs)),
        ) => ast::Expression::Literal(ast::Literal::Number((lhs / rhs).floor())),
        (
            ast::BinaryOperator::And,
            ast::Expression::Literal(ast::Literal::Boolean(lhs)),
            ast::Expression::Literal(ast::Literal::Boolean(rhs)),
        ) => ast::Expression::Literal(ast::Literal::Boolean(lhs && rhs)),
        (
            ast::BinaryOperator::Or,
            ast::Expression::Literal(ast::Literal::Boolean(lhs)),
            ast::Expression::Literal(ast::Literal::Boolean(rhs)),
        ) => ast::Expression::Literal(ast::Literal::Boolean(lhs || rhs)),
        (
            ast::BinaryOperator::LessThan,
            ast::Expression::Literal(ast::Literal::Number(lhs)),
            ast::Expression::Literal(ast::Literal::Number(rhs)),
        ) => ast::Expression::Literal(ast::Literal::Boolean(lhs < rhs)),
        (
            ast::BinaryOperator::GreaterThan,
            ast::Expression::Literal(ast::Literal::Number(lhs)),
            ast::Expression::Literal(ast::Literal::Number(rhs)),
        ) => ast::Expression::Literal(ast::Literal::Boolean(lhs > rhs)),
        (
            ast::BinaryOperator::LessThanOrEqual,
            ast::Expression::Literal(ast::Literal::Number(lhs)),
            ast::Expression::Literal(ast::Literal::Number(rhs)),
        ) => ast::Expression::Literal(ast::Literal::Boolean(lhs <= rhs)),
        (
            ast::BinaryOperator::GreaterThanOrEqual,
            ast::Expression::Literal(ast::Literal::Number(lhs)),
            ast::Expression::Literal(ast::Literal::Number(rhs)),
        ) => ast::Expression::Literal(ast::Literal::Boolean(lhs >= rhs)),
        (
            ast::BinaryOperator::Equal,
            ast::Expression::Literal(lhs),
            ast::Expression::Literal(rhs),
        ) => ast::Expression::Literal(ast::Literal::Boolean(lhs == rhs)),
        (
            ast::BinaryOperator::NotEqual,
            ast::Expression::Literal(lhs),
            ast::Expression::Literal(rhs),
        ) => ast::Expression::Literal(ast::Literal::Boolean(lhs != rhs)),
        (
            ast::BinaryOperator::BitwiseOr,
            ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(lhs))),
            ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(rhs))),
        ) => ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(lhs | rhs))),
        (
            ast::BinaryOperator::BitwiseXor,
            ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(lhs))),
            ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(rhs))),
        ) => ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(lhs ^ rhs))),
        (
            ast::BinaryOperator::BitwiseAnd,
            ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(lhs))),
            ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(rhs))),
        ) => ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(lhs & rhs))),
        (
            ast::BinaryOperator::ShiftLeft,
            ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(lhs))),
            ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(rhs))),
        ) => ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(lhs << rhs))),
        (
            ast::BinaryOperator::ShiftRight,
            ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(lhs))),
            ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(rhs))),
        ) => ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(lhs >> rhs))),
        (op, lhs, rhs) => ast::Expression::BinaryOp {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        },
    }
}

fn optimize_unary_op(op: ast::UnaryOperator, rhs: ast::Expression) -> ast::Expression {
    let rhs = optimize_expression(rhs);

    match (op, rhs) {
        (ast::UnaryOperator::Neg, ast::Expression::Literal(ast::Literal::Number(rhs))) => {
            ast::Expression::Literal(ast::Literal::Number(-rhs))
        }
        (
            ast::UnaryOperator::BitwiseNot,
            ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(rhs))),
        ) => ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(!rhs))),
        (ast::UnaryOperator::Not, ast::Expression::Literal(ast::Literal::Boolean(rhs))) => {
            ast::Expression::Literal(ast::Literal::Boolean(!rhs))
        }
        (ast::UnaryOperator::Not, ast::Expression::Literal(ast::Literal::Nil)) => {
            ast::Expression::Literal(ast::Literal::Boolean(true))
        }
        (op, rhs) => ast::Expression::UnaryOp {
            op,
            rhs: Box::new(rhs),
        },
    }
}
