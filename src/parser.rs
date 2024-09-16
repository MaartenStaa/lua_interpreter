use std::path::Path;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::scope::{DraftScope, NameLocation};
use crate::token::{Span, Token, TokenKind};
use miette::{miette, Context, LabeledSpan};

pub struct Parser<'path, 'source> {
    pub filename: &'path Path,
    source: &'source str,
    lexer: Lexer<'path, 'source>,
    scopes: Vec<DraftScope>,
}

impl<'path, 'source> Parser<'path, 'source> {
    pub fn new(filename: &'path Path, source: &'source str) -> Self {
        Self {
            filename,
            source,
            lexer: Lexer::new(filename, source),
            scopes: vec![DraftScope::new()],
        }
    }

    pub fn parse(&mut self) -> miette::Result<TokenTree<Block>> {
        let result = self
            // NOTE: Don't start a new scope right away at the top-level
            .parse_block_inner()
            .map_err(|r| self.with_source_code(r))?;

        // Ensure we've consumed all tokens
        if let Some(token) = self.lexer.next().transpose()? {
            return Err(self.with_source_code(miette!(
                labels = vec![LabeledSpan::at(
                    token.span.start..token.span.end,
                    "unexpected token"
                )],
                "unexpected token"
            )));
        }

        Ok(result)
    }

    fn with_source_code(&self, report: miette::Report) -> miette::Report {
        report.with_source_code(
            miette::NamedSource::new(self.filename.to_string_lossy(), self.source.to_string())
                .with_language("lua"),
        )
    }

    fn parse_block(&mut self) -> miette::Result<TokenTree<Block>> {
        self.begin_scope();
        let block = self.parse_block_inner()?;
        self.end_scope();

        Ok(block)
    }

    fn parse_block_inner(&mut self) -> miette::Result<TokenTree<Block>> {
        let start = self.lexer.position;

        let mut statements = Vec::new();
        while let Some(statement) = self.parse_statement()? {
            statements.push(statement);
        }

        let return_statement = match self.lexer.peek()? {
            Some(token) if token.kind == TokenKind::Return => {
                self.lexer.next();
                let mut return_statement = Vec::new();
                while let Some(expression) = self
                    .parse_expression()
                    .wrap_err("in block return statement")?
                {
                    return_statement.push(expression);

                    match self.lexer.peek()? {
                        Some(token) if token.kind == TokenKind::Comma => {
                            self.lexer.next();
                            continue;
                        }
                        _ => break,
                    }
                }

                match self.lexer.peek()? {
                    Some(token) if token.kind == TokenKind::Semicolon => {
                        self.lexer.next();
                    }
                    _ => {}
                }

                Some(return_statement)
            }
            _ => None,
        };

        Ok(TokenTree {
            node: Block {
                statements,
                return_statement,
            },
            span: Span::new(start, self.lexer.position),
        })
    }

    fn parse_statement(&mut self) -> miette::Result<Option<TokenTree<Statement>>> {
        loop {
            let next_token = self.lexer.peek()?;
            break match next_token {
                Some(Token {
                    kind: TokenKind::Semicolon,
                    ..
                }) => {
                    self.lexer.next();
                    continue;
                }
                Some(Token {
                    kind: TokenKind::Break,
                    span,
                }) => {
                    let span = *span;
                    self.lexer.next();
                    Ok(Some(TokenTree::new(Statement::Break, span)))
                }
                Some(Token {
                    kind: TokenKind::Goto,
                    span,
                }) => {
                    let span = *span;
                    self.lexer.next();
                    let name = self.parse_name().wrap_err("in goto statement")?;
                    Ok(Some(TokenTree::new(Statement::Goto(name), span)))
                }
                Some(Token {
                    kind: TokenKind::DoubleColon,
                    span,
                }) => {
                    let span = *span;
                    self.lexer.next();
                    let name = self.parse_name().wrap_err("in label")?;
                    self.lexer.expect(|k| k == &TokenKind::DoubleColon, "::")?;
                    let end = self.lexer.position;
                    self.declare_label(&name.node, Span::new(span.start, end))?;
                    Ok(Some(TokenTree::new(Statement::Label(name), span)))
                }
                Some(Token {
                    kind: TokenKind::Local,
                    span,
                }) => {
                    let Span { start, .. } = *span;
                    self.lexer.next();
                    self.parse_local_declaration(start)
                        .wrap_err("in local declaration")
                        .map(Some)
                }
                Some(Token {
                    kind: TokenKind::Do,
                    span,
                }) => {
                    let Span { start, .. } = *span;
                    self.lexer.next();
                    let block = self.parse_block().wrap_err("in do statement")?;
                    self.lexer.expect(|k| k == &TokenKind::End, "end")?;
                    Ok(Some(TokenTree::new(
                        Statement::Block(block),
                        Span::new(start, self.lexer.position),
                    )))
                }
                Some(Token {
                    kind: TokenKind::While,
                    span,
                }) => {
                    let Span { start, .. } = *span;
                    self.lexer.next();
                    let condition = self.expect_expression().wrap_err("in while statement")?;
                    self.lexer.expect(|k| k == &TokenKind::Do, "do")?;
                    let block = self.parse_block().wrap_err("in while statement")?;
                    self.lexer.expect(|k| k == &TokenKind::End, "end")?;
                    Ok(Some(TokenTree::new(
                        Statement::While { condition, block },
                        Span::new(start, self.lexer.position),
                    )))
                }
                Some(Token {
                    kind: TokenKind::Repeat,
                    span,
                }) => {
                    let Span { start, .. } = *span;
                    self.lexer.next();
                    let block = self.parse_block().wrap_err("in repeat statement")?;
                    self.lexer.expect(|k| k == &TokenKind::Until, "until")?;
                    let condition = self.expect_expression().wrap_err("in repeat statement")?;
                    Ok(Some(TokenTree::new(
                        Statement::Repeat { block, condition },
                        Span::new(start, self.lexer.position),
                    )))
                }
                Some(Token {
                    kind: TokenKind::If,
                    span,
                }) => {
                    let Span { start, .. } = *span;
                    self.lexer.next();
                    let condition = self.expect_expression().wrap_err("in if statement")?;
                    self.lexer.expect(|k| k == &TokenKind::Then, "then")?;
                    let block = self.parse_block().wrap_err("in if statement")?;
                    let mut else_ifs = Vec::new();
                    loop {
                        match self.lexer.peek()? {
                            Some(token) if token.kind == TokenKind::ElseIf => {
                                let else_if_start = self.lexer.position;
                                self.lexer.next();
                                let condition = self.expect_expression().wrap_err("in elseif")?;
                                self.lexer.expect(|k| k == &TokenKind::Then, "then")?;
                                let block = self.parse_block().wrap_err("in elseif")?;
                                else_ifs.push(TokenTree::new(
                                    ElseIf { condition, block },
                                    Span::new(else_if_start, self.lexer.position),
                                ));
                            }
                            _ => break,
                        }
                    }
                    let else_block =
                        if self.lexer.peek()?.map(|t| &t.kind) == Some(&TokenKind::Else) {
                            self.lexer.next();
                            Some(self.parse_block().wrap_err("in else")?)
                        } else {
                            None
                        };

                    self.lexer.expect(|k| k == &TokenKind::End, "end")?;

                    Ok(Some(TokenTree::new(
                        Statement::If {
                            condition,
                            block,
                            else_ifs,
                            else_block,
                        },
                        Span::new(start, self.lexer.position),
                    )))
                }
                Some(Token {
                    kind: TokenKind::Function,
                    ..
                }) => {
                    let start = self.lexer.position;
                    self.lexer.next();
                    let (name, implicit_self_parameter) = {
                        let name = self.parse_name().wrap_err("in function declaration")?;
                        let mut name_span = name.span;
                        let mut name =
                            TokenTree::new(Variable::Name(self.declare_variable(name)), name_span);
                        let mut implicit_self_parameter = None;

                        loop {
                            match self.lexer.peek()? {
                                Some(Token {
                                    kind: TokenKind::Dot,
                                    ..
                                }) => {
                                    self.lexer.next();
                                    let field =
                                        self.parse_name().wrap_err("in function declaration")?;
                                    let field_span = field.span;
                                    name = TokenTree::new(
                                        Variable::Field(
                                            Box::new(TokenTree::new(
                                                PrefixExpression::Variable(name),
                                                name_span,
                                            )),
                                            field,
                                        ),
                                        Span::new(name_span.start, field_span.end),
                                    );
                                    name_span = name.span;
                                    continue;
                                }
                                Some(Token {
                                    kind: TokenKind::Colon,
                                    ..
                                }) => {
                                    self.lexer.next();
                                    let method_name =
                                        self.parse_name().wrap_err("in function declaration")?;
                                    let method_name_span = method_name.span;
                                    name = TokenTree::new(
                                        Variable::Field(
                                            Box::new(TokenTree::new(
                                                PrefixExpression::Variable(name),
                                                name_span,
                                            )),
                                            method_name,
                                        ),
                                        Span::new(name_span.start, method_name_span.end),
                                    );
                                    implicit_self_parameter =
                                        Some(Name::unresolved("self".to_string()));
                                    break;
                                }
                                _ => break,
                            }
                        }

                        (name, implicit_self_parameter)
                    };

                    let function_def = self
                        .parse_function_body(implicit_self_parameter)
                        .wrap_err("in function declaration")?;
                    let function_def_span = function_def.span;

                    Ok(Some(TokenTree::new(
                        Statement::Assignment {
                            varlist: vec![name],
                            explist: vec![TokenTree::new(
                                Expression::FunctionDef(function_def),
                                function_def_span,
                            )],
                        },
                        Span::new(start, self.lexer.position),
                    )))
                }
                Some(Token {
                    kind: TokenKind::For,
                    ..
                }) => {
                    let start = self.lexer.position;
                    self.lexer.next();
                    self.parse_for_statement(start)
                        .wrap_err("in for statement")
                        .map(Some)
                }
                Some(Token {
                    kind:
                        TokenKind::Return
                        | TokenKind::End
                        | TokenKind::ElseIf
                        | TokenKind::Else
                        | TokenKind::Until,
                    ..
                }) => {
                    // Handled by the block parser or outside of it
                    Ok(None)
                }
                Some(Token {
                    kind: TokenKind::Identifier(_) | TokenKind::OpenParen,
                    ..
                }) => self
                    .parse_varlist_or_functioncall()
                    .wrap_err("in variable assignment or function call")
                    .map(Some),
                Some(_) => {
                    return Err(miette!(
                        labels = vec![next_token.unwrap().span.labeled("this is not a statement")],
                        "expected a statement"
                    ))
                }

                // Reached the end of the input
                None => Ok(None),
            };
        }
    }

    fn parse_local_declaration(&mut self, start: usize) -> miette::Result<TokenTree<Statement>> {
        if matches!(
            self.lexer.peek()?.map(|t| &t.kind),
            Some(TokenKind::Function)
        ) {
            self.lexer.next();
            let name = self
                .parse_name()
                .wrap_err("in local function declaration")?;
            let name = self.declare_variable(name);
            let function_def = self.parse_function_body(None).wrap_err_with(|| {
                format!("in local {} function declaration", name.node.identifier)
            })?;

            let name_span = name.span;
            let function_def_span = function_def.span;
            Ok(TokenTree::new(
                Statement::LocalDeclaraction(
                    vec![TokenTree::new(
                        AttributedName {
                            name,
                            attribute: None,
                        },
                        name_span,
                    )],
                    vec![TokenTree::new(
                        Expression::FunctionDef(function_def),
                        function_def_span,
                    )],
                ),
                Span::new(start, self.lexer.position),
            ))
        } else {
            let mut attributed_names = Vec::new();
            loop {
                let name = self.parse_name()?;
                match self.lexer.peek()? {
                    Some(token) if token.kind == TokenKind::Comma => {
                        self.lexer.next();
                        attributed_names.push(AttributedName {
                            name,
                            attribute: None,
                        });
                        continue;
                    }
                    Some(Token {
                        kind: TokenKind::Less,
                        ..
                    }) => {
                        self.lexer.next();
                        let attr_token = self
                            .lexer
                            .expect(|k| matches!(k, TokenKind::Identifier(_)), "identifier")?;
                        let attr_str = match attr_token {
                            Token {
                                kind: TokenKind::Identifier(s),
                                ..
                            } => s,
                            _ => unreachable!(),
                        };

                        let attr = TokenTree::new(
                            match attr_str {
                                "const" => LocalAttribute::Const,
                                "close" => LocalAttribute::Close,
                                _ => {
                                    return Err(miette!(
                                        labels = vec![attr_token
                                            .span
                                            .labeled("expected 'const' or 'close'")],
                                        "unexpected token"
                                    ))
                                }
                            },
                            attr_token.span,
                        );

                        self.lexer.expect(|k| k == &TokenKind::Greater, ">")?;

                        attributed_names.push(AttributedName {
                            name,
                            attribute: Some(attr),
                        });

                        match self.lexer.peek()? {
                            Some(token) if token.kind == TokenKind::Comma => {
                                self.lexer.next();
                                continue;
                            }
                            _ => break,
                        }
                    }
                    _ => {
                        attributed_names.push(AttributedName {
                            name,
                            attribute: None,
                        });
                        break;
                    }
                }
            }

            let expressions = if self.lexer.peek()?.map(|t| &t.kind) == Some(&TokenKind::Equals) {
                self.lexer.next();
                let mut expressions = Vec::new();
                loop {
                    expressions.push(
                        self.expect_expression()
                            .wrap_err("in local declaration initializer")?,
                    );

                    match self.lexer.peek()? {
                        Some(token) if token.kind == TokenKind::Comma => {
                            self.lexer.next();
                            continue;
                        }
                        _ => break,
                    }
                }

                expressions
            } else {
                vec![]
            };

            let attributed_names = attributed_names
                .into_iter()
                .map(|n| {
                    let start = n.name.span.start;
                    let end = n.attribute.as_ref().map_or(n.name.span.end, |a| a.span.end);
                    TokenTree::new(
                        AttributedName {
                            name: self.declare_variable(n.name),
                            attribute: n.attribute,
                        },
                        Span::new(start, end),
                    )
                })
                .collect();

            Ok(TokenTree::new(
                Statement::LocalDeclaraction(attributed_names, expressions),
                Span::new(start, self.lexer.position),
            ))
        }
    }

    fn parse_for_statement(
        &mut self,
        start_position: usize,
    ) -> miette::Result<TokenTree<Statement>> {
        self.begin_scope();

        let name = self.parse_name().wrap_err("name in for statement")?;
        let condition = match self.lexer.peek()? {
            Some(token) if token.kind == TokenKind::Equals => {
                self.lexer.next();
                let initial = self
                    .expect_expression()
                    .wrap_err("initializer expression in for statement")?;

                let name = self.declare_variable(name);

                self.lexer.expect(|k| k == &TokenKind::Comma, ",")?;
                let limit = self
                    .expect_expression()
                    .wrap_err("limit expression in for statement")?;
                let step = match self.lexer.peek()? {
                    Some(token) if token.kind == TokenKind::Comma => {
                        self.lexer.next();
                        Some(
                            self.expect_expression()
                                .wrap_err("step expression in for statement")?,
                        )
                    }
                    _ => None,
                };

                TokenTree::new(
                    ForCondition::NumericFor {
                        name,
                        initial,
                        limit,
                        step,
                    },
                    Span::new(start_position, self.lexer.position),
                )
            }
            Some(Token {
                kind: TokenKind::Comma | TokenKind::In,
                ..
            }) => {
                let mut names = vec![name];
                loop {
                    match self.lexer.peek()? {
                        Some(token) if token.kind == TokenKind::Comma => {
                            self.lexer.next();
                            names.push(self.parse_name().wrap_err("names in for statement")?);
                            continue;
                        }
                        _ => break,
                    }
                }

                self.lexer.expect(|k| k == &TokenKind::In, "in")?;
                let mut expressions = Vec::new();
                loop {
                    expressions.push(
                        self.expect_expression()
                            .wrap_err("iterator expressions in for statement")?,
                    );

                    match self.lexer.peek()? {
                        Some(token) if token.kind == TokenKind::Comma => {
                            self.lexer.next();
                            continue;
                        }
                        _ => break,
                    }
                }

                let names = names
                    .into_iter()
                    .map(|n| self.declare_variable(n))
                    .collect();

                TokenTree::new(
                    ForCondition::GenericFor { names, expressions },
                    Span::new(start_position, self.lexer.position),
                )
            }
            Some(token) => {
                return Err(miette!(
                    labels = vec![token.span.labeled("expected '=', ',', or 'in'")],
                    "unexpected token"
                ))
            }
            _ => {
                return Err(miette!(
                    labels = vec![LabeledSpan::at(
                        self.source.len()..self.source.len(),
                        "expected '=', ',', or 'in'"
                    )],
                    "unexpected end of input"
                ))
            }
        };

        self.lexer.expect(|k| k == &TokenKind::Do, "do")?;
        let block = self
            .parse_block_inner()
            .wrap_err("in block of for statement")?;
        self.lexer.expect(|k| k == &TokenKind::End, "end")?;

        self.end_scope();

        Ok(TokenTree::new(
            Statement::For { condition, block },
            Span::new(start_position, self.lexer.position),
        ))
    }

    fn parse_varlist_or_functioncall(&mut self) -> miette::Result<TokenTree<Statement>> {
        // This can be one of a few things:
        // - An assignment (ident1, ident2 = expr1, expr2)
        // - A function call (ident(args) or ident arg or ident:method(args))
        //
        // For both the assignment and the function call case, the ident can actually be a prefix
        // expression, so we may need to parse that first.
        //
        // stat ::=  varlist ‘=’ explist |
        //           functioncall |
        //           <snip>
        //
        // varlist ::= var {‘,’ var}
        // var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name
        //
        // functioncall ::=  prefixexp args | prefixexp ‘:’ Name args
        //
        // prefixexp ::= var | functioncall | ‘(’ exp ‘)’

        let prefix_expression = self
            .parse_prefix_expression()
            .wrap_err("in varlist or functioncall")?;
        let start = prefix_expression.span.start;

        match self.lexer.peek()? {
            Some(Token {
                kind: TokenKind::Comma,
                ..
            })
            | Some(Token {
                kind: TokenKind::Equals,
                ..
            }) => {
                let mut prefix_expressions = vec![prefix_expression];
                while let Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) = self.lexer.peek()?
                {
                    self.lexer.next();
                    prefix_expressions.push(self.parse_prefix_expression().wrap_err("in varlist")?);
                }

                let mut varlist = Vec::with_capacity(prefix_expressions.len());
                for var in prefix_expressions {
                    varlist.push(match var {
                        TokenTree {
                            node: PrefixExpression::Variable(v),
                            ..
                        } => v,
                        _ => {
                            return Err(miette!(
                                labels = vec![LabeledSpan::at(
                                    // TODO: This location is wrong
                                    self.lexer.position..self.lexer.position,
                                    "expected variable"
                                )],
                                "unexpected token"
                            ));
                        }
                    });
                }

                self.lexer.expect(|k| k == &TokenKind::Equals, "=")?;
                let mut explist = Vec::new();
                loop {
                    explist.push(self.expect_expression().wrap_err("in varlist")?);

                    match self.lexer.peek()? {
                        Some(Token {
                            kind: TokenKind::Comma,
                            ..
                        }) => {
                            self.lexer.next();
                            continue;
                        }
                        _ => break,
                    }
                }

                return Ok(TokenTree::new(
                    Statement::Assignment { varlist, explist },
                    Span::new(start, self.lexer.position),
                ));
            }
            _ => {}
        }

        match prefix_expression {
            TokenTree {
                node: PrefixExpression::FunctionCall(f),
                span,
            } => Ok(TokenTree::new(Statement::FunctionCall(f), span)),
            _ => Err(miette!(
                labels = vec![LabeledSpan::at(
                    self.lexer.position..self.lexer.position,
                    "expected function call or variable assignment"
                )],
                "unexpected token"
            )),
        }
    }

    fn parse_prefix_expression(&mut self) -> miette::Result<TokenTree<PrefixExpression>> {
        let mut prefix = match self.lexer.next().transpose()? {
            Some(Token {
                kind: TokenKind::OpenParen,
                ..
            }) => {
                let start = self.lexer.position;
                let expression = self.expect_expression()?;
                self.lexer.expect(|k| k == &TokenKind::CloseParen, ")")?;
                TokenTree::new(
                    PrefixExpression::Parenthesized(Box::new(expression)),
                    Span::new(start, self.lexer.position),
                )
            }
            Some(Token {
                kind: TokenKind::Identifier(ident),
                span,
            }) => {
                let name = self.lookup_variable(Name::unresolved(ident.to_string()));
                TokenTree::new(
                    PrefixExpression::Variable(TokenTree::new(
                        Variable::Name(TokenTree::new(name, span)),
                        span,
                    )),
                    span,
                )
            }
            Some(t) => {
                return Err(miette!(
                    labels = vec![LabeledSpan::at(
                        t.span.start..t.span.end,
                        "expected prefix expression"
                    )],
                    "unexpected token"
                ))
            }
            None => {
                return Err(miette!(
                    labels = vec![LabeledSpan::at(
                        self.source.len()..self.source.len(),
                        "expected prefix expression"
                    )],
                    "unexpected end of input"
                ))
            }
        };

        loop {
            match self.lexer.peek()? {
                Some(Token {
                    kind: TokenKind::OpenBracket,
                    ..
                }) => {
                    self.lexer.next();
                    let expression = self.expect_expression()?;
                    self.lexer.expect(|k| k == &TokenKind::CloseBracket, "]")?;
                    let span = Span::new(prefix.span.start, self.lexer.position);
                    prefix = TokenTree::new(
                        PrefixExpression::Variable(TokenTree::new(
                            Variable::Indexed(Box::new(prefix), Box::new(expression)),
                            span,
                        )),
                        span,
                    );
                }
                Some(Token {
                    kind: TokenKind::Dot,
                    ..
                }) => {
                    self.lexer.next();
                    let name = self.parse_name().wrap_err("in prefix expression")?;
                    let span = Span::new(prefix.span.start, name.span.end);
                    prefix = TokenTree::new(
                        PrefixExpression::Variable(TokenTree::new(
                            Variable::Field(Box::new(prefix), name),
                            span,
                        )),
                        span,
                    );
                }
                Some(Token {
                    kind: TokenKind::Colon,
                    ..
                }) => {
                    self.lexer.next();
                    let name = self.parse_name().wrap_err("in prefix expression")?;
                    let args = self.parse_args().wrap_err("in prefix expression")?;
                    let span = Span::new(prefix.span.start, self.lexer.position);
                    prefix = TokenTree::new(
                        PrefixExpression::FunctionCall(TokenTree::new(
                            FunctionCall {
                                function: Box::new(prefix),
                                as_method: true,
                                name: Some(name),
                                args,
                            },
                            span,
                        )),
                        span,
                    );
                }
                Some(Token {
                    kind: TokenKind::OpenParen | TokenKind::String(_),
                    ..
                }) => {
                    let args = self.parse_args().wrap_err("in prefix expression")?;
                    let span = Span::new(prefix.span.start, self.lexer.position);
                    prefix = TokenTree::new(
                        PrefixExpression::FunctionCall(TokenTree::new(
                            FunctionCall {
                                function: Box::new(prefix),
                                as_method: false,
                                name: None,
                                args,
                            },
                            span,
                        )),
                        span,
                    );
                }
                Some(Token {
                    kind: TokenKind::OpenBrace,
                    ..
                }) => {
                    let table = self
                        .parse_table_constructor()
                        .wrap_err("in prefix expression")?;
                    let table_span = table.span;
                    let span = Span::new(prefix.span.start, table_span.end);
                    prefix = TokenTree::new(
                        PrefixExpression::FunctionCall(TokenTree::new(
                            FunctionCall {
                                function: Box::new(prefix),
                                as_method: false,
                                name: None,
                                args: vec![TokenTree::new(
                                    Expression::TableConstructor(table),
                                    table_span,
                                )],
                            },
                            span,
                        )),
                        span,
                    );
                }
                _ => break,
            }
        }

        Ok(prefix)
    }

    fn parse_args(&mut self) -> miette::Result<Vec<TokenTree<Expression>>> {
        let mut args = Vec::new();
        if matches!(
            self.lexer.peek()?.map(|t| &t.kind),
            Some(TokenKind::String(_))
        ) {
            args.push(self.expect_expression().wrap_err("in function call args")?);
            return Ok(args);
        }

        self.lexer.expect(|k| k == &TokenKind::OpenParen, "(")?;
        loop {
            if self.lexer.peek()?.map(|t| &t.kind) == Some(&TokenKind::CloseParen) {
                self.lexer.next();
                break;
            }

            if !args.is_empty() {
                self.lexer.expect(|k| k == &TokenKind::Comma, ",")?;
            }

            let expression = self.expect_expression().wrap_err("in function call args")?;
            args.push(expression);
        }

        Ok(args)
    }

    fn parse_table_constructor(&mut self) -> miette::Result<TokenTree<TableConstructor>> {
        macro_rules! expect_end_of_field {
            () => {
                match self.lexer.peek()? {
                    Some(Token {
                        kind: TokenKind::Comma | TokenKind::Semicolon,
                        ..
                    }) => {
                        self.lexer.next();
                        continue;
                    }
                    Some(Token {
                        kind: TokenKind::CloseBrace,
                        ..
                    }) => {
                        self.lexer.next();
                        break;
                    }
                    Some(t) => {
                        return Err(miette!(
                            labels = vec![LabeledSpan::at(
                                t.span.start..t.span.end,
                                "expected table field separator"
                            )],
                            "unexpected token"
                        ))
                    }
                    None => {
                        return Err(miette!(
                            labels = vec![LabeledSpan::at(
                                self.source.len()..self.source.len(),
                                "expected table field separator"
                            )],
                            "unexpected end of input"
                        ))
                    }
                }
            };
        }

        let start = self.lexer.position;

        self.lexer.expect(|k| k == &TokenKind::OpenBrace, "{")?;
        let mut fields = vec![];
        loop {
            match self.lexer.peek()? {
                Some(Token {
                    kind: TokenKind::CloseBrace,
                    ..
                }) => {
                    self.lexer.next();
                    break;
                }
                Some(Token {
                    kind: TokenKind::OpenBracket,
                    ..
                }) => {
                    let field_start = self.lexer.position;
                    self.lexer.next();
                    let key = self
                        .expect_expression()
                        .wrap_err("field index expression in table constructor")?;
                    self.lexer.expect(|k| k == &TokenKind::CloseBracket, "]")?;
                    self.lexer.expect(|k| k == &TokenKind::Equals, "=")?;
                    let value = self
                        .expect_expression()
                        .wrap_err("field value in table constructor")?;
                    fields.push(TokenTree::new(
                        Field::Indexed(key, value),
                        Span::new(field_start, self.lexer.position),
                    ));

                    expect_end_of_field!();
                }
                Some(Token {
                    kind: TokenKind::Identifier(_),
                    ..
                }) => {
                    // Try to parse it as a sequence expression first (without an index or name)
                    let (name, value) = match self.parse_expression()? {
                        // What a horrendous pattern this is
                        Some(TokenTree {
                            node:
                                Expression::PrefixExpression(TokenTree {
                                    node:
                                        PrefixExpression::Variable(TokenTree {
                                            node: Variable::Name(name),
                                            ..
                                        }),
                                    ..
                                }),
                            ..
                        }) if self.lexer.peek()?.map(|t| &t.kind) == Some(&TokenKind::Equals) => {
                            // This is an indexed field
                            self.lexer.next();
                            let value = self
                                .expect_expression()
                                .wrap_err("field value in table constructor")?;

                            (Some(name), value)
                        }
                        Some(expression) => (None, expression),
                        None => {
                            return Err(miette!(
                                labels = vec![LabeledSpan::at(
                                    self.lexer.position..self.lexer.position,
                                    "expected table field"
                                )],
                                "unexpected token"
                            ))
                        }
                    };

                    if let Some(name) = name {
                        let value_span = value.span;
                        fields.push(TokenTree::new(
                            // TODO: We resolve and then unresolve? That's annoying.
                            Field::Named(TokenTree::new(name.node.unresolve(), name.span), value),
                            Span::new(name.span.start, value_span.end),
                        ));
                    } else {
                        let value_span = value.span;
                        fields.push(TokenTree::new(Field::Value(value), value_span));
                    }

                    expect_end_of_field!();
                }
                Some(t) => {
                    // Avoid borrow checker issue
                    let expected_span = t.span.labeled("expected table field");

                    // Maybe it's just an expression of some sort?
                    let expression = self.parse_expression().wrap_err("in table constructor")?;
                    match expression {
                        Some(expression) => {
                            let span = expression.span;
                            fields.push(TokenTree::new(Field::Value(expression), span));
                            expect_end_of_field!();
                        }
                        None => {
                            return Err(miette!(labels = vec![expected_span], "unexpected token"));
                        }
                    }
                }
                None => {
                    return Err(miette!(
                        labels = vec![LabeledSpan::at(
                            self.source.len()..self.source.len(),
                            "expected table field"
                        )],
                        "unexpected end of input"
                    ))
                }
            }
        }

        Ok(TokenTree {
            node: TableConstructor { fields },
            span: Span::new(start, self.lexer.position),
        })
    }

    fn expect_expression(&mut self) -> miette::Result<TokenTree<Expression>> {
        self.expect_expression_within(0)
    }

    fn expect_expression_within(&mut self, min_bp: u8) -> miette::Result<TokenTree<Expression>> {
        match self.parse_expression_within(min_bp)? {
            Some(expression) => Ok(expression),
            None => Err(miette!(
                labels = vec![LabeledSpan::at(
                    self.lexer.position..self.lexer.position,
                    "here"
                )],
                "expected an expression"
            )),
        }
    }

    fn parse_expression(&mut self) -> miette::Result<Option<TokenTree<Expression>>> {
        self.parse_expression_within(0)
    }

    fn parse_expression_within(
        &mut self,
        min_bp: u8,
    ) -> miette::Result<Option<TokenTree<Expression>>> {
        let mut lhs = match self.lexer.peek()? {
            Some(Token {
                kind: TokenKind::Identifier(_) | TokenKind::OpenParen,
                ..
            }) => {
                let start = self.lexer.position;
                TokenTree::new(
                    Expression::PrefixExpression(
                        self.parse_prefix_expression().wrap_err("in expression")?,
                    ),
                    Span::new(start, self.lexer.position),
                )
            }
            Some(Token {
                kind: TokenKind::Nil,
                span,
            }) => {
                let span = *span;
                self.lexer.next();
                TokenTree::new(
                    Expression::Literal(TokenTree::new(Literal::Nil, span)),
                    span,
                )
            }
            Some(Token {
                kind: TokenKind::True,
                span,
            }) => {
                let span = *span;
                self.lexer.next();
                TokenTree::new(
                    Expression::Literal(TokenTree::new(Literal::Boolean(true), span)),
                    span,
                )
            }
            Some(Token {
                kind: TokenKind::False,
                span,
            }) => {
                let span = *span;
                self.lexer.next();
                TokenTree::new(
                    Expression::Literal(TokenTree::new(Literal::Boolean(false), span)),
                    span,
                )
            }
            Some(Token {
                kind: TokenKind::Integer(i),
                span,
            }) => {
                let span = *span;
                let i = *i;
                self.lexer.next();
                TokenTree::new(
                    Expression::Literal(TokenTree::new(Literal::Number(Number::Integer(i)), span)),
                    span,
                )
            }
            Some(Token {
                kind: TokenKind::Float(f),
                span,
            }) => {
                let span = *span;
                let f = *f;
                self.lexer.next();
                TokenTree::new(
                    Expression::Literal(TokenTree::new(Literal::Number(Number::Float(f)), span)),
                    span,
                )
            }
            Some(Token {
                kind: TokenKind::String(_),
                span,
            }) => {
                let span = *span;
                let s = match self.lexer.next() {
                    Some(Ok(Token {
                        kind: TokenKind::String(s),
                        ..
                    })) => s,
                    _ => unreachable!(),
                };
                TokenTree::new(
                    Expression::Literal(TokenTree::new(Literal::String(s), span)),
                    span,
                )
            }
            Some(Token {
                kind: TokenKind::DotDotDot,
                span,
            }) => {
                let span = *span;
                self.lexer.next();
                TokenTree::new(Expression::Ellipsis, span)
            }
            Some(Token {
                kind: TokenKind::Minus,
                span,
            }) => {
                let span = *span;
                self.lexer.next();
                let ((), r_bp) = prefix_binding_power(&TokenKind::Minus);
                let rhs = self.expect_expression_within(r_bp)?;
                TokenTree::new(
                    Expression::UnaryOp {
                        op: TokenTree::new(UnaryOperator::Neg, span),
                        rhs: Box::new(rhs),
                    },
                    span,
                )
            }
            Some(Token {
                kind: TokenKind::Not,
                span,
            }) => {
                let span = *span;
                let start = self.lexer.position;
                self.lexer.next();
                let ((), r_bp) = prefix_binding_power(&TokenKind::Not);
                let rhs = self.expect_expression_within(r_bp)?;
                TokenTree::new(
                    Expression::UnaryOp {
                        op: TokenTree::new(UnaryOperator::Not, span),
                        rhs: Box::new(rhs),
                    },
                    Span::new(start, self.lexer.position),
                )
            }
            Some(Token {
                kind: TokenKind::Hash,
                span,
            }) => {
                let span = *span;
                let start = self.lexer.position;
                self.lexer.next();
                let ((), r_bp) = prefix_binding_power(&TokenKind::Hash);
                let rhs = self.expect_expression_within(r_bp)?;
                TokenTree::new(
                    Expression::UnaryOp {
                        op: TokenTree::new(UnaryOperator::Length, span),
                        rhs: Box::new(rhs),
                    },
                    Span::new(start, self.lexer.position),
                )
            }
            Some(Token {
                kind: TokenKind::Tilde,
                span,
            }) => {
                let span = *span;
                let start = self.lexer.position;
                self.lexer.next();
                let ((), r_bp) = prefix_binding_power(&TokenKind::Tilde);
                let rhs = self.expect_expression_within(r_bp)?;
                TokenTree::new(
                    Expression::UnaryOp {
                        op: TokenTree::new(UnaryOperator::BitwiseNot, span),
                        rhs: Box::new(rhs),
                    },
                    Span::new(start, self.lexer.position),
                )
            }
            Some(Token {
                kind: TokenKind::Function,
                ..
            }) => {
                let start = self.lexer.position;
                self.lexer.next();
                TokenTree::new(
                    Expression::FunctionDef(self.parse_function_body(None)?),
                    Span::new(start, self.lexer.position),
                )
            }
            Some(Token {
                kind: TokenKind::OpenBrace,
                ..
            }) => {
                let table = self.parse_table_constructor().wrap_err("in expression")?;
                let span = table.span;
                TokenTree::new(Expression::TableConstructor(table), span)
            }

            _ => {
                return Ok(None);
            }
        };

        loop {
            let op = match self.lexer.peek()? {
                Some(token) if token.kind.is_operator() => token,
                Some(Token {
                    kind: TokenKind::Colon,
                    ..
                }) => {
                    let prefix = match lhs {
                        TokenTree {
                            node: Expression::PrefixExpression(p),
                            ..
                        } => p,
                        _ => {
                            return Ok(Some(lhs));
                        }
                    };

                    self.lexer.next();
                    let name = self.parse_name().wrap_err("in expression method call")?;
                    let args = self.parse_args().wrap_err("in expression method call")?;

                    let span = Span::new(prefix.span.start, self.lexer.position);

                    lhs = TokenTree::new(
                        Expression::PrefixExpression(TokenTree::new(
                            PrefixExpression::FunctionCall(TokenTree::new(
                                FunctionCall {
                                    function: Box::new(prefix),
                                    as_method: true,
                                    name: Some(name),
                                    args,
                                },
                                span,
                            )),
                            span,
                        )),
                        span,
                    );
                    continue;
                }
                _ => break,
            };

            if let Some((l_bp, r_bp)) = infix_binding_power(&op.kind) {
                if l_bp < min_bp {
                    break;
                }

                let op = TokenTree::new(BinaryOperator::from(&op.kind), op.span);
                self.lexer.next();

                let rhs = self.expect_expression_within(r_bp)?;
                let span = Span::new(lhs.span.start, rhs.span.end);
                lhs = TokenTree::new(
                    Expression::BinaryOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    span,
                );
            }
        }

        Ok(Some(lhs))
    }

    fn parse_function_body(
        &mut self,
        implicit_self_parameter: Option<Name<()>>,
    ) -> miette::Result<TokenTree<FunctionDef>> {
        self.begin_scope();

        let start = self.lexer.position;

        self.lexer.expect(|k| k == &TokenKind::OpenParen, "(")?;
        let mut parameters = Vec::new();
        if let Some(name) = implicit_self_parameter {
            parameters.push(TokenTree::new(
                name,
                Span::new(self.lexer.position, self.lexer.position),
            ));
        }
        let mut has_varargs = false;

        loop {
            match self.lexer.peek()? {
                Some(Token {
                    kind: TokenKind::CloseParen,
                    ..
                }) => {
                    break;
                }
                Some(Token {
                    kind: TokenKind::DotDotDot,
                    ..
                }) => {
                    self.lexer.next();
                    has_varargs = true;
                    break;
                }
                Some(Token {
                    kind: TokenKind::Identifier(_),
                    ..
                }) => {
                    parameters.push(self.parse_name().wrap_err("in function definition")?);
                    match self.lexer.peek()? {
                        Some(Token {
                            kind: TokenKind::Comma,
                            ..
                        }) => {
                            self.lexer.next();
                            continue;
                        }
                        _ => break,
                    }
                }
                Some(token) => {
                    return Err(miette!(
                        labels = vec![token.span.labeled("expected parameter name")],
                        "unexpected token"
                    ))
                }
                None => {
                    return Err(miette!(
                        labels = vec![LabeledSpan::at(
                            self.source.len()..self.source.len(),
                            "expected function signature"
                        )],
                        "unexpected end of input"
                    ))
                }
            }
        }

        let parameters = parameters
            .into_iter()
            .map(|n| self.declare_variable(n))
            .collect();

        self.lexer.expect(|k| k == &TokenKind::CloseParen, ")")?;
        let block = self
            .parse_block_inner()
            .wrap_err("in function definition")?;
        self.lexer.expect(|k| k == &TokenKind::End, "end")?;

        self.end_scope();

        Ok(TokenTree::new(
            FunctionDef {
                parameters,
                has_varargs,
                block,
            },
            Span::new(start, self.lexer.position),
        ))
    }

    fn parse_name(&mut self) -> miette::Result<TokenTree<Name<()>>> {
        let token = self
            .lexer
            .expect(|k| matches!(k, &TokenKind::Identifier(_)), "identifier")?;
        let name = match token.kind {
            TokenKind::Identifier(ident) => ident,
            _ => unreachable!(),
        };

        Ok(TokenTree::new(
            Name::unresolved(name.to_string()),
            token.span,
        ))
    }
}

// Precendence reference: https://www.lua.org/manual/5.4/manual.html#3.4.8
fn prefix_binding_power(kind: &TokenKind) -> ((), u8) {
    match kind {
        TokenKind::Not | TokenKind::Hash | TokenKind::Minus | TokenKind::Tilde => ((), 21),
        _ => unimplemented!("prefix_binding_power with {kind:?}"),
    }
}

fn infix_binding_power(kind: &TokenKind) -> Option<(u8, u8)> {
    Some(match kind {
        TokenKind::Or => (1, 2),
        TokenKind::And => (3, 4),
        TokenKind::Less
        | TokenKind::Greater
        | TokenKind::LessEquals
        | TokenKind::GreaterEquals
        | TokenKind::TildeEquals
        | TokenKind::EqualsEquals => (5, 6),
        TokenKind::Pipe => (7, 8),
        TokenKind::Tilde => (9, 10),
        TokenKind::Ampersand => (11, 12),
        TokenKind::ShiftLeft | TokenKind::ShiftRight => (13, 14),
        TokenKind::DotDot => (16, 15), // Right associative
        TokenKind::Plus | TokenKind::Minus => (17, 18),
        TokenKind::Star | TokenKind::Slash | TokenKind::SlashSlash | TokenKind::Percent => (19, 20),
        TokenKind::Caret => (22, 21), // Right associative

        _ => return None,
    })
}

/// Scoping methods
impl<'path, 'source> Parser<'path, 'source> {
    fn begin_scope(&mut self) {
        self.scopes.push(DraftScope::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare_variable<T>(&mut self, name: TokenTree<Name<T>>) -> TokenTree<Name<NameLocation>> {
        let current_scope = self.scopes.len() - 1;
        let current_scope = &mut self.scopes[current_scope];
        current_scope.register_variable(&name.node.identifier);

        TokenTree::new(
            Name {
                location: NameLocation { scope_offset: 0 },
                identifier: name.node.identifier,
            },
            name.span,
        )
    }

    fn declare_label<T>(&mut self, name: &Name<T>, span: Span) -> miette::Result<()> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.has_label(&name.identifier) {
                return Err(miette!(
                    labels = vec![span.labeled("label already defined")],
                    "label already defined in this scope"
                ));
            }
        }

        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.register_label(&name.identifier);

        Ok(())
    }

    fn lookup_variable<T>(&mut self, name: Name<T>) -> Name<NameLocation> {
        for (scope_offset, scope) in self.scopes.iter().rev().enumerate() {
            if scope.has_variable(&name.identifier) {
                return Name {
                    location: NameLocation { scope_offset },
                    identifier: name.identifier,
                };
            }
        }

        // We've reached the global scope. Undefined variables are
        // implicitly declared as global (though without an initial value)
        self.scopes[0].register_variable(&name.identifier);
        Name {
            location: NameLocation {
                scope_offset: self.scopes.len() - 1,
            },
            identifier: name.identifier,
        }
    }
}
