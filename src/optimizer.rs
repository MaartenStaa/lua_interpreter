use crate::{ast::*, token::Span};

/// Apply optimizations to the AST
/// At the moment, we only do constant folding
pub fn optimize(program: TokenTree<Block>) -> TokenTree<Block> {
    let span = program.span;
    TokenTree::new(
        Block {
            statements: program
                .node
                .statements
                .into_iter()
                .map(optimize_statement)
                .collect(),
            return_statement: program
                .node
                .return_statement
                .map(|exprs| exprs.into_iter().map(optimize_expression).collect()),
        },
        span,
    )
}

fn optimize_statement(statement: TokenTree<Statement>) -> TokenTree<Statement> {
    let span = statement.span;
    TokenTree::new(
        match statement.node {
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
            Statement::LocalFunctionDeclaration(name, func) => {
                Statement::LocalFunctionDeclaration(name, optimize_function_def(func))
            }

            // No optimization to be done
            Statement::Goto(label) => Statement::Goto(label),
            Statement::Label(label) => Statement::Label(label),
            Statement::Break => Statement::Break,
        },
        span,
    )
}

fn optimize_else_if(else_if: TokenTree<ElseIf>) -> TokenTree<ElseIf> {
    let span = else_if.span;
    TokenTree::new(
        ElseIf {
            condition: optimize_expression(else_if.node.condition),
            block: optimize(else_if.node.block),
        },
        span,
    )
}

fn optimize_for_condition(condition: TokenTree<ForCondition>) -> TokenTree<ForCondition> {
    let span = condition.span;
    TokenTree::new(
        match condition.node {
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
        },
        span,
    )
}

fn optimize_function_call(function_call: TokenTree<FunctionCall>) -> TokenTree<FunctionCall> {
    let span = function_call.span;
    let args_span = function_call.node.args.span;
    TokenTree::new(
        FunctionCall {
            function: Box::new(optimize_prefix_expression(*function_call.node.function)),
            method_name: function_call.node.method_name,
            args: TokenTree::new(
                function_call
                    .node
                    .args
                    .node
                    .into_iter()
                    .map(optimize_expression)
                    .collect(),
                args_span,
            ),
        },
        span,
    )
}

fn optimize_expression(expression: TokenTree<Expression>) -> TokenTree<Expression> {
    let span = expression.span;
    TokenTree::new(
        match expression.node {
            Expression::PrefixExpression(prefix) => {
                let optimized_prefix = optimize_prefix_expression(prefix);
                match optimized_prefix {
                    TokenTree {
                        node: PrefixExpression::Parenthesized(parenthesized),
                        ..
                    } => {
                        if matches!(
                            &*parenthesized,
                            TokenTree {
                                node: Expression::Literal(_),
                                ..
                            }
                        ) {
                            return *parenthesized;
                        } else {
                            let span = parenthesized.span;
                            Expression::PrefixExpression(TokenTree::new(
                                PrefixExpression::Parenthesized(parenthesized),
                                span,
                            ))
                        }
                    }
                    other => Expression::PrefixExpression(other),
                }
            }
            Expression::FunctionDef(func) => Expression::FunctionDef(optimize_function_def(func)),
            Expression::TableConstructor(table) => {
                Expression::TableConstructor(optimize_table_constructor(table))
            }
            Expression::BinaryOp { op, lhs, rhs } => return optimize_binary_op(op, *lhs, *rhs),
            Expression::UnaryOp { op, rhs } => return optimize_unary_op(op, *rhs),

            // No optimization to be done
            Expression::Literal(literal) => Expression::Literal(literal),
            Expression::Ellipsis => Expression::Ellipsis,
        },
        span,
    )
}

fn optimize_prefix_expression(prefix: TokenTree<PrefixExpression>) -> TokenTree<PrefixExpression> {
    let span = prefix.span;
    TokenTree::new(
        match prefix.node {
            PrefixExpression::Variable(var) => PrefixExpression::Variable(optimize_variable(var)),
            PrefixExpression::Parenthesized(expr) => {
                if let Expression::PrefixExpression(expr) = expr.node {
                    return optimize_prefix_expression(expr);
                }

                PrefixExpression::Parenthesized(Box::new(optimize_expression(*expr)))
            }
            PrefixExpression::FunctionCall(func) => {
                PrefixExpression::FunctionCall(optimize_function_call(func))
            }
        },
        span,
    )
}

fn optimize_variable(var: TokenTree<Variable>) -> TokenTree<Variable> {
    let span = var.span;
    TokenTree::new(
        match var.node {
            Variable::Name(name) => Variable::Name(name),
            Variable::Field(prefix, name) => {
                Variable::Field(Box::new(optimize_prefix_expression(*prefix)), name)
            }
            Variable::Indexed(prefix, index_expression) => Variable::Indexed(
                Box::new(optimize_prefix_expression(*prefix)),
                Box::new(optimize_expression(*index_expression)),
            ),
        },
        span,
    )
}

fn optimize_function_def(func: TokenTree<FunctionDef>) -> TokenTree<FunctionDef> {
    let span = func.span;
    TokenTree::new(
        FunctionDef {
            name: func.node.name,
            method_name: func.node.method_name,
            parameters: func.node.parameters,
            varargs: func.node.varargs,
            block: optimize(func.node.block),
        },
        span,
    )
}

fn optimize_table_constructor(table: TokenTree<TableConstructor>) -> TokenTree<TableConstructor> {
    let span = table.span;
    TokenTree::new(
        TableConstructor {
            fields: table
                .node
                .fields
                .into_iter()
                .map(optimize_table_field)
                .collect(),
        },
        span,
    )
}

fn optimize_table_field(field: TokenTree<Field>) -> TokenTree<Field> {
    let span = field.span;
    TokenTree::new(
        match field.node {
            Field::Value(expression) => Field::Value(optimize_expression(expression)),
            Field::Named(name, expression) => Field::Named(name, optimize_expression(expression)),
            Field::Indexed(index, value) => {
                Field::Indexed(optimize_expression(index), optimize_expression(value))
            }
        },
        span,
    )
}

fn optimize_binary_op(
    op: TokenTree<BinaryOperator>,
    lhs: TokenTree<Expression>,
    rhs: TokenTree<Expression>,
) -> TokenTree<Expression> {
    let span = Span::new(lhs.span.start, rhs.span.end);
    let op_span = op.span;
    let lhs_span = lhs.span;
    let rhs_span = rhs.span;

    let lhs = optimize_expression(lhs);
    let rhs = optimize_expression(rhs);

    TokenTree::new(
        match (op.node, lhs.node, rhs.node) {
            (
                BinaryOperator::Add,
                Expression::Literal(TokenTree {
                    node: Literal::Number(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Number(lhs + rhs),
                span,
            }),
            (
                BinaryOperator::Sub,
                Expression::Literal(TokenTree {
                    node: Literal::Number(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Number(lhs - rhs),
                span,
            }),
            (
                BinaryOperator::Mul,
                Expression::Literal(TokenTree {
                    node: Literal::Number(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Number(lhs * rhs),
                span,
            }),
            (
                BinaryOperator::Div,
                Expression::Literal(TokenTree {
                    node: Literal::Number(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Number(lhs / rhs),
                span,
            }),
            (
                BinaryOperator::Mod,
                Expression::Literal(TokenTree {
                    node: Literal::Number(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(rhs),
                    ..
                }),
            ) if !rhs.is_zero() => Expression::Literal(TokenTree {
                node: Literal::Number(lhs % rhs),
                span,
            }),
            (
                BinaryOperator::Pow,
                Expression::Literal(TokenTree {
                    node: Literal::Number(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Number(lhs.pow(&rhs)),
                span,
            }),
            (
                BinaryOperator::Concat,
                Expression::Literal(TokenTree {
                    node: Literal::String(mut lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::String(rhs),
                    ..
                }),
            ) => {
                lhs.extend(rhs);
                Expression::Literal(TokenTree {
                    node: Literal::String(lhs),
                    span,
                })
            }
            (
                BinaryOperator::Concat,
                Expression::Literal(TokenTree {
                    node: Literal::String(mut lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(rhs),
                    ..
                }),
            ) => {
                lhs.extend(rhs.to_string().into_bytes());
                Expression::Literal(TokenTree {
                    node: Literal::String(lhs),
                    span,
                })
            }
            (
                BinaryOperator::Concat,
                Expression::Literal(TokenTree {
                    node: Literal::Number(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::String(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree::new(
                Literal::String(Vec::from_iter(
                    lhs.to_string().into_bytes().into_iter().chain(rhs),
                )),
                span,
            )),
            (
                BinaryOperator::Concat,
                Expression::Literal(TokenTree {
                    node: Literal::Number(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(rhs),
                    ..
                }),
            ) => {
                let mut lhs = lhs.to_string();
                lhs.push_str(&rhs.to_string());
                Expression::Literal(TokenTree {
                    node: Literal::String(lhs.into_bytes()),
                    span,
                })
            }
            (
                BinaryOperator::FloorDiv,
                Expression::Literal(TokenTree {
                    node: Literal::Number(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(rhs),
                    ..
                }),
            ) if !rhs.is_zero() => Expression::Literal(TokenTree {
                node: Literal::Number((lhs / rhs).floor()),
                span,
            }),
            (
                BinaryOperator::And,
                Expression::Literal(TokenTree {
                    node: Literal::Boolean(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Boolean(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Boolean(lhs && rhs),
                span,
            }),
            (
                BinaryOperator::Or,
                Expression::Literal(TokenTree {
                    node: Literal::Boolean(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Boolean(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Boolean(lhs || rhs),
                span,
            }),
            (
                BinaryOperator::LessThan,
                Expression::Literal(TokenTree {
                    node: Literal::Number(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Boolean(lhs < rhs),
                span,
            }),
            (
                BinaryOperator::LessThan,
                Expression::Literal(TokenTree {
                    node: Literal::String(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::String(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Boolean(lhs < rhs),
                span,
            }),
            (
                BinaryOperator::GreaterThan,
                Expression::Literal(TokenTree {
                    node: Literal::Number(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Boolean(lhs > rhs),
                span,
            }),
            (
                BinaryOperator::GreaterThan,
                Expression::Literal(TokenTree {
                    node: Literal::String(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::String(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Boolean(lhs > rhs),
                span,
            }),
            (
                BinaryOperator::LessThanOrEqual,
                Expression::Literal(TokenTree {
                    node: Literal::Number(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Boolean(lhs <= rhs),
                span,
            }),
            (
                BinaryOperator::LessThanOrEqual,
                Expression::Literal(TokenTree {
                    node: Literal::String(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::String(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Boolean(lhs <= rhs),
                span,
            }),
            (
                BinaryOperator::GreaterThanOrEqual,
                Expression::Literal(TokenTree {
                    node: Literal::Number(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Boolean(lhs >= rhs),
                span,
            }),
            (
                BinaryOperator::GreaterThanOrEqual,
                Expression::Literal(TokenTree {
                    node: Literal::String(lhs),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::String(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Boolean(lhs >= rhs),
                span,
            }),
            (
                BinaryOperator::Equal,
                Expression::Literal(TokenTree { node: lhs, .. }),
                Expression::Literal(TokenTree { node: rhs, .. }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Boolean(lhs == rhs),
                span,
            }),
            (
                BinaryOperator::NotEqual,
                Expression::Literal(TokenTree { node: lhs, .. }),
                Expression::Literal(TokenTree { node: rhs, .. }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Boolean(lhs != rhs),
                span,
            }),
            (
                BinaryOperator::BitwiseOr,
                Expression::Literal(TokenTree {
                    node: Literal::Number(Number::Integer(lhs)),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(Number::Integer(rhs)),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Number(Number::Integer(lhs | rhs)),
                span,
            }),
            (
                BinaryOperator::BitwiseXor,
                Expression::Literal(TokenTree {
                    node: Literal::Number(Number::Integer(lhs)),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(Number::Integer(rhs)),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Number(Number::Integer(lhs ^ rhs)),
                span,
            }),
            (
                BinaryOperator::BitwiseAnd,
                Expression::Literal(TokenTree {
                    node: Literal::Number(Number::Integer(lhs)),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(Number::Integer(rhs)),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Number(Number::Integer(lhs & rhs)),
                span,
            }),
            (
                BinaryOperator::ShiftLeft,
                Expression::Literal(TokenTree {
                    node: Literal::Number(Number::Integer(lhs)),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(Number::Integer(rhs)),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Number(Number::Integer(if rhs >= 0 {
                    lhs.wrapping_shl(rhs as u32)
                } else {
                    lhs.wrapping_shr(-rhs as u32)
                })),
                span,
            }),
            (
                BinaryOperator::ShiftRight,
                Expression::Literal(TokenTree {
                    node: Literal::Number(Number::Integer(lhs)),
                    ..
                }),
                Expression::Literal(TokenTree {
                    node: Literal::Number(Number::Integer(rhs)),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Number(Number::Integer(if rhs >= 0 {
                    lhs.wrapping_shr(rhs as u32)
                } else {
                    lhs.wrapping_shl(-rhs as u32)
                })),
                span,
            }),
            (op, lhs, rhs) => Expression::BinaryOp {
                lhs: Box::new(TokenTree::new(lhs, lhs_span)),
                op: TokenTree::new(op, op_span),
                rhs: Box::new(TokenTree::new(rhs, rhs_span)),
            },
        },
        span,
    )
}

fn optimize_unary_op(
    op: TokenTree<UnaryOperator>,
    rhs: TokenTree<Expression>,
) -> TokenTree<Expression> {
    let rhs = optimize_expression(rhs);
    let span = Span::new(op.span.start, rhs.span.end);

    TokenTree::new(
        match (&op.node, &rhs.node) {
            (
                UnaryOperator::Neg,
                Expression::Literal(TokenTree {
                    node: Literal::Number(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree::new(Literal::Number(-*rhs), span)),
            (
                UnaryOperator::BitwiseNot,
                Expression::Literal(TokenTree {
                    node: Literal::Number(Number::Integer(rhs)),
                    ..
                }),
            ) => Expression::Literal(TokenTree::new(Literal::Number(Number::Integer(!rhs)), span)),
            (
                UnaryOperator::Not,
                Expression::Literal(TokenTree {
                    node: Literal::Boolean(rhs),
                    ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Boolean(!rhs),
                span,
            }),
            (
                UnaryOperator::Not,
                Expression::Literal(TokenTree {
                    node: Literal::Nil, ..
                }),
            ) => Expression::Literal(TokenTree {
                node: Literal::Boolean(true),
                span,
            }),
            (UnaryOperator::Not, Expression::Literal(_)) => Expression::Literal(TokenTree {
                node: Literal::Boolean(false),
                span,
            }),
            (
                UnaryOperator::Length,
                Expression::Literal(TokenTree {
                    node: Literal::String(s),
                    ..
                }),
            ) => Expression::Literal(TokenTree::new(
                Literal::Number(Number::Integer(s.len() as i64)),
                span,
            )),
            (_, _) => Expression::UnaryOp {
                op,
                rhs: Box::new(rhs),
            },
        },
        span,
    )
}
