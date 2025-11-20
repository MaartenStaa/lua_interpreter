use std::path::Path;

use crate::ast::*;
use crate::error::{lua_error, Context, LuaError};
use crate::lexer::Lexer;
use crate::token::{Span, Token, TokenKind};
use miette::LabeledSpan;

pub struct Parser<'path, 'source> {
    pub filename: Option<&'path Path>,
    source: &'source [u8],
    lexer: Lexer<'path, 'source>,
}

impl<'path, 'source> Parser<'path, 'source> {
    pub fn new(filename: Option<&'path Path>, source: &'source [u8]) -> Self {
        Self {
            filename,
            source,
            lexer: Lexer::new(filename, source),
        }
    }

    pub fn parse(&mut self) -> crate::Result<TokenTree<Block>> {
        let result = self
            // NOTE: Don't start a new scope right away at the top-level
            .parse_block_inner(0, false)
            .map_err(|r| self.with_source_code(r))?;

        // Ensure we've consumed all tokens
        if let Some(token) = self.lexer.next().transpose()? {
            return Err(self.with_source_code(lua_error!(
                labels = vec![LabeledSpan::at(
                    token.span.start..token.span.end,
                    "unexpected token"
                )],
                "unexpected token"
            )));
        }

        Ok(result)
    }

    fn with_source_code(&self, report: LuaError) -> LuaError {
        let source = String::from_utf8_lossy(self.source).into_owned();
        if let Some(filename) = self.filename {
            report.with_source_code(
                miette::NamedSource::new(filename.to_string_lossy(), source).with_language("lua"),
            )
        } else {
            report.with_source_code(source)
        }
    }

    fn parse_block(
        &mut self,
        start: usize,
        inside_vararg_function: bool,
    ) -> crate::Result<TokenTree<Block>> {
        self.parse_block_inner(start, inside_vararg_function)
    }

    fn parse_block_inner(
        &mut self,
        start: usize,
        inside_vararg_function: bool,
    ) -> crate::Result<TokenTree<Block>> {
        let mut statements = Vec::new();
        while let Some(statement) = self.parse_statement(inside_vararg_function)? {
            statements.push(statement);
        }

        let return_statement = match self.lexer.peek()? {
            Some(token) if token.kind == TokenKind::Return => {
                self.lexer.next();
                let mut return_statement = Vec::new();
                while let Some(expression) = self
                    .parse_expression(inside_vararg_function)
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

        let end = return_statement
            .as_ref()
            .and_then(|r| r.last().map(|e| e.span.end))
            .or_else(|| statements.last().map(|s| s.span.end))
            .unwrap_or(start);

        Ok(TokenTree {
            node: Block {
                statements,
                return_statement,
            },
            span: Span::new(start, end),
        })
    }

    fn parse_statement(
        &mut self,
        inside_vararg_function: bool,
    ) -> crate::Result<Option<TokenTree<Statement>>> {
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
                    let name_span = name.span;
                    Ok(Some(TokenTree::new(
                        Statement::Goto(name),
                        Span::new(span.start, name_span.end),
                    )))
                }
                Some(Token {
                    kind: TokenKind::DoubleColon,
                    span,
                }) => {
                    let span = *span;
                    self.lexer.next();
                    let name = self.parse_name().wrap_err("in label")?;
                    let double_colon_token =
                        self.lexer.expect(|k| k == &TokenKind::DoubleColon, "::")?;
                    let end = double_colon_token.span.end;
                    Ok(Some(TokenTree::new(
                        Statement::Label(name),
                        Span::new(span.start, end),
                    )))
                }
                Some(Token {
                    kind: TokenKind::Local,
                    span,
                }) => {
                    let Span { start, .. } = *span;
                    self.lexer.next();
                    self.parse_local_declaration(start, inside_vararg_function)
                        .wrap_err("in local declaration")
                        .map(Some)
                }
                Some(Token {
                    kind: TokenKind::Do,
                    span,
                }) => {
                    let Span { start, end: do_end } = *span;
                    self.lexer.next();
                    let block = self
                        .parse_block(do_end, inside_vararg_function)
                        .wrap_err("in do statement")?;
                    let end_token = self.lexer.expect(|k| k == &TokenKind::End, "end")?;
                    Ok(Some(TokenTree::new(
                        Statement::Block(block),
                        Span::new(start, end_token.span.end),
                    )))
                }
                Some(Token {
                    kind: TokenKind::While,
                    span,
                }) => {
                    let Span { start, .. } = *span;
                    self.lexer.next();
                    let condition = self
                        .expect_expression(inside_vararg_function)
                        .wrap_err("in while statement")?;
                    let do_token = self.lexer.expect(|k| k == &TokenKind::Do, "do")?;
                    let block = self
                        .parse_block(do_token.span.start, inside_vararg_function)
                        .wrap_err("in while statement")?;
                    let end_token = self.lexer.expect(|k| k == &TokenKind::End, "end")?;
                    Ok(Some(TokenTree::new(
                        Statement::While { condition, block },
                        Span::new(start, end_token.span.end),
                    )))
                }
                Some(Token {
                    kind: TokenKind::Repeat,
                    span,
                }) => {
                    let Span {
                        start,
                        end: repeat_end,
                    } = *span;
                    self.lexer.next();
                    let block = self
                        .parse_block(repeat_end, inside_vararg_function)
                        .wrap_err("in repeat statement")?;
                    self.lexer.expect(|k| k == &TokenKind::Until, "until")?;
                    let condition = self
                        .expect_expression(inside_vararg_function)
                        .wrap_err("in repeat statement")?;
                    let end = condition.span.end;
                    Ok(Some(TokenTree::new(
                        Statement::Repeat { block, condition },
                        Span::new(start, end),
                    )))
                }
                Some(Token {
                    kind: TokenKind::If,
                    span,
                }) => {
                    let Span { start, .. } = *span;
                    self.lexer.next();
                    let condition = self
                        .expect_expression(inside_vararg_function)
                        .wrap_err("in if statement")?;
                    let then_token = self.lexer.expect(|k| k == &TokenKind::Then, "then")?;
                    let block = self
                        .parse_block(then_token.span.end, inside_vararg_function)
                        .wrap_err("in if statement")?;
                    let mut else_ifs = Vec::new();
                    loop {
                        match self.lexer.peek()? {
                            Some(token) if token.kind == TokenKind::ElseIf => {
                                let else_if_start = token.span.start;
                                self.lexer.next();
                                let condition = self
                                    .expect_expression(inside_vararg_function)
                                    .wrap_err("in elseif")?;
                                let then_token =
                                    self.lexer.expect(|k| k == &TokenKind::Then, "then")?;
                                let block = self
                                    .parse_block(then_token.span.end, inside_vararg_function)
                                    .wrap_err("in elseif")?;
                                let block_span = block.span;
                                else_ifs.push(TokenTree::new(
                                    ElseIf { condition, block },
                                    Span::new(else_if_start, block_span.end),
                                ));
                            }
                            _ => break,
                        }
                    }
                    let else_block =
                        if self.lexer.peek()?.map(|t| &t.kind) == Some(&TokenKind::Else) {
                            let else_token = self.lexer.next().expect("else").expect("else");
                            Some(
                                self.parse_block(else_token.span.end, inside_vararg_function)
                                    .wrap_err("in else")?,
                            )
                        } else {
                            None
                        };

                    let end_token = self.lexer.expect(|k| k == &TokenKind::End, "end")?;

                    Ok(Some(TokenTree::new(
                        Statement::If {
                            condition,
                            block,
                            else_ifs,
                            else_block,
                        },
                        Span::new(start, end_token.span.end),
                    )))
                }
                Some(Token {
                    kind: TokenKind::Function,
                    span,
                }) => {
                    let start = span.start;
                    self.lexer.next();
                    let (name, method_name) = {
                        let name = self.parse_name().wrap_err("in function declaration")?;
                        let mut name_span = name.span;
                        let mut name = TokenTree::new(Variable::Name(name), name_span);
                        let mut method_name = None;

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
                                    let method_name_ =
                                        self.parse_name().wrap_err("in function declaration")?;
                                    method_name = Some(method_name_.node.0.clone());
                                    let method_name_span = method_name_.span;
                                    name = TokenTree::new(
                                        Variable::Field(
                                            Box::new(TokenTree::new(
                                                PrefixExpression::Variable(name),
                                                name_span,
                                            )),
                                            method_name_,
                                        ),
                                        Span::new(name_span.start, method_name_span.end),
                                    );
                                    break;
                                }
                                _ => break,
                            }
                        }

                        (name, method_name)
                    };

                    let function_def = self
                        .parse_function_body(
                            start,
                            method_name,
                            match &name.node {
                                Variable::Name(name) => Some(name.node.0.clone()),
                                _ => None,
                            },
                        )
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
                        Span::new(start, function_def_span.end),
                    )))
                }
                Some(Token {
                    kind: TokenKind::For,
                    span,
                }) => {
                    let start = span.start;
                    self.lexer.next();
                    self.parse_for_statement(start, inside_vararg_function)
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
                    .parse_varlist_or_functioncall(inside_vararg_function)
                    .wrap_err("in variable assignment or function call")
                    .map(Some),
                Some(_) => {
                    return Err(lua_error!(
                        labels = vec![next_token.unwrap().span.labeled("this is not a statement")],
                        "expected a statement"
                    ))
                }

                // Reached the end of the input
                None => Ok(None),
            };
        }
    }

    fn parse_local_declaration(
        &mut self,
        start: usize,
        inside_vararg_function: bool,
    ) -> crate::Result<TokenTree<Statement>> {
        if matches!(
            self.lexer.peek()?.map(|t| &t.kind),
            Some(TokenKind::Function)
        ) {
            self.lexer.next();
            let name = self
                .parse_name()
                .wrap_err("in local function declaration")?;
            let function_def = self
                .parse_function_body(start, None, Some(name.node.0.clone()))
                .wrap_err_with(|| {
                    format!(
                        "in local {} function declaration",
                        String::from_utf8_lossy(&name.node.0)
                    )
                })?;

            let function_def_span = function_def.span;
            Ok(TokenTree::new(
                Statement::LocalFunctionDeclaration(name, function_def),
                Span::new(start, function_def_span.end),
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
                                b"const" => LocalAttribute::Const,
                                b"close" => LocalAttribute::Close,
                                _ => {
                                    return Err(lua_error!(
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
                        self.expect_expression(inside_vararg_function)
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

            let attributed_names: Vec<_> = attributed_names
                .into_iter()
                .map(|n| {
                    let start = n.name.span.start;
                    let end = n.attribute.as_ref().map_or(n.name.span.end, |a| a.span.end);
                    TokenTree::new(
                        AttributedName {
                            name: n.name,
                            attribute: n.attribute,
                        },
                        Span::new(start, end),
                    )
                })
                .collect();

            let end_position = expressions
                .last()
                .map_or(attributed_names.last().unwrap().span.end, |e| e.span.end);
            Ok(TokenTree::new(
                Statement::LocalDeclaraction(attributed_names, expressions),
                Span::new(start, end_position),
            ))
        }
    }

    fn parse_for_statement(
        &mut self,
        start_position: usize,
        inside_vararg_function: bool,
    ) -> crate::Result<TokenTree<Statement>> {
        let name = self.parse_name().wrap_err("name in for statement")?;
        let condition = match self.lexer.peek()? {
            Some(token) if token.kind == TokenKind::Equals => {
                self.lexer.next();
                let initial = self
                    .expect_expression(inside_vararg_function)
                    .wrap_err("initializer expression in for statement")?;

                self.lexer.expect(|k| k == &TokenKind::Comma, ",")?;
                let limit = self
                    .expect_expression(inside_vararg_function)
                    .wrap_err("limit expression in for statement")?;
                let step = match self.lexer.peek()? {
                    Some(token) if token.kind == TokenKind::Comma => {
                        self.lexer.next();
                        Some(
                            self.expect_expression(inside_vararg_function)
                                .wrap_err("step expression in for statement")?,
                        )
                    }
                    _ => None,
                };
                let end = step.as_ref().map_or(limit.span.end, |s| s.span.end);

                TokenTree::new(
                    ForCondition::NumericFor {
                        name,
                        initial,
                        limit,
                        step,
                    },
                    Span::new(start_position, end),
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
                let end_position = loop {
                    expressions.push(
                        self.expect_expression(inside_vararg_function)
                            .wrap_err("iterator expressions in for statement")?,
                    );

                    match self.lexer.peek()? {
                        Some(token) if token.kind == TokenKind::Comma => {
                            self.lexer.next();
                            continue;
                        }
                        _ => break expressions.last().unwrap().span.end,
                    }
                };

                TokenTree::new(
                    ForCondition::GenericFor { names, expressions },
                    Span::new(start_position, end_position),
                )
            }
            Some(token) => {
                return Err(lua_error!(
                    labels = vec![token.span.labeled("expected '=', ',', or 'in'")],
                    "unexpected token"
                ))
            }
            _ => {
                return Err(lua_error!(
                    labels = vec![LabeledSpan::at(
                        self.source.len()..self.source.len(),
                        "expected '=', ',', or 'in'"
                    )],
                    "unexpected end of input"
                ))
            }
        };

        let do_token = self.lexer.expect(|k| k == &TokenKind::Do, "do")?;
        let block = self
            .parse_block_inner(do_token.span.end, inside_vararg_function)
            .wrap_err("in block of for statement")?;
        let end_token = self.lexer.expect(|k| k == &TokenKind::End, "end")?;

        Ok(TokenTree::new(
            Statement::For { condition, block },
            Span::new(start_position, end_token.span.end),
        ))
    }

    fn parse_varlist_or_functioncall(
        &mut self,
        inside_vararg_function: bool,
    ) -> crate::Result<TokenTree<Statement>> {
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
            .parse_prefix_expression(inside_vararg_function)
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
                    prefix_expressions.push(
                        self.parse_prefix_expression(inside_vararg_function)
                            .wrap_err("in varlist")?,
                    );
                }

                let mut varlist = Vec::with_capacity(prefix_expressions.len());
                for var in prefix_expressions {
                    varlist.push(match var {
                        TokenTree {
                            node: PrefixExpression::Variable(v),
                            ..
                        } => v,
                        TokenTree { span, .. } => {
                            return Err(lua_error!(
                                labels = vec![span.labeled("expected variable")],
                                "unexpected token"
                            ));
                        }
                    });
                }

                self.lexer.expect(|k| k == &TokenKind::Equals, "=")?;
                let mut explist = Vec::new();
                let end_position = loop {
                    explist.push(
                        self.expect_expression(inside_vararg_function)
                            .wrap_err("in varlist")?,
                    );

                    match self.lexer.peek()? {
                        Some(Token {
                            kind: TokenKind::Comma,
                            ..
                        }) => {
                            self.lexer.next();
                            continue;
                        }
                        _ => break explist.last().unwrap().span.end,
                    }
                };

                return Ok(TokenTree::new(
                    Statement::Assignment { varlist, explist },
                    Span::new(start, end_position),
                ));
            }
            _ => {}
        }

        match prefix_expression {
            TokenTree {
                node: PrefixExpression::FunctionCall(f),
                span,
            } => Ok(TokenTree::new(Statement::FunctionCall(f), span)),
            TokenTree { span, .. } => Err(lua_error!(
                labels = vec![span.labeled("expected function call or variable assignment")],
                "unexpected token"
            )),
        }
    }

    fn parse_prefix_expression(
        &mut self,
        inside_vararg_function: bool,
    ) -> crate::Result<TokenTree<PrefixExpression>> {
        let mut prefix = match self.lexer.next().transpose()? {
            Some(Token {
                kind: TokenKind::OpenParen,
                span,
            }) => {
                let expression = self.expect_expression(inside_vararg_function)?;
                let closing_paren = self.lexer.expect(|k| k == &TokenKind::CloseParen, ")")?;
                TokenTree::new(
                    PrefixExpression::Parenthesized(Box::new(expression)),
                    Span::new(span.start, closing_paren.span.end),
                )
            }
            Some(Token {
                kind: TokenKind::Identifier(ident),
                span,
            }) => {
                let name = Name(ident.to_vec());
                TokenTree::new(
                    PrefixExpression::Variable(TokenTree::new(
                        Variable::Name(TokenTree::new(name, span)),
                        span,
                    )),
                    span,
                )
            }
            Some(t) => {
                return Err(lua_error!(
                    labels = vec![LabeledSpan::at(
                        t.span.start..t.span.end,
                        "expected prefix expression"
                    )],
                    "unexpected token"
                ))
            }
            None => {
                return Err(lua_error!(
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
                    let expression = self.expect_expression(inside_vararg_function)?;
                    let closing_bracket =
                        self.lexer.expect(|k| k == &TokenKind::CloseBracket, "]")?;
                    let span = Span::new(prefix.span.start, closing_bracket.span.end);
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
                    let args = self
                        .parse_args(inside_vararg_function)
                        .wrap_err("in prefix expression")?;
                    let span = Span::new(prefix.span.start, args.span.end);
                    prefix = TokenTree::new(
                        PrefixExpression::FunctionCall(TokenTree::new(
                            FunctionCall {
                                function: Box::new(prefix),
                                method_name: Some(name),
                                args,
                            },
                            span,
                        )),
                        span,
                    );
                }
                Some(Token {
                    kind: TokenKind::OpenParen | TokenKind::String(_) | TokenKind::OpenBrace,
                    ..
                }) => {
                    let args = self
                        .parse_args(inside_vararg_function)
                        .wrap_err("in prefix expression")?;
                    let span = Span::new(prefix.span.start, args.span.end);
                    prefix = TokenTree::new(
                        PrefixExpression::FunctionCall(TokenTree::new(
                            FunctionCall {
                                function: Box::new(prefix),
                                method_name: None,
                                args,
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

    fn parse_args(
        &mut self,
        inside_vararg_function: bool,
    ) -> crate::Result<TokenTree<Vec<TokenTree<Expression>>>> {
        let mut args = Vec::new();
        if matches!(
            self.lexer.peek()?.map(|t| &t.kind),
            Some(TokenKind::String(_))
        ) {
            let (string, span) = match self
                .lexer
                .expect(|k| matches!(k, &TokenKind::String(_)), "string")?
            {
                Token {
                    kind: TokenKind::String(s),
                    span,
                } => (s, span),
                _ => unreachable!(),
            };
            args.push(TokenTree::new(
                Expression::Literal(TokenTree::new(Literal::String(string), span)),
                span,
            ));
            return Ok(TokenTree::new(args, span));
        }

        if self.lexer.peek()?.map(|t| &t.kind) == Some(&TokenKind::OpenBrace) {
            let table = self
                .parse_table_constructor(inside_vararg_function)
                .wrap_err("in function call args")?;
            let table_span = table.span;

            return Ok(TokenTree::new(
                vec![TokenTree::new(
                    Expression::TableConstructor(table),
                    table_span,
                )],
                table_span,
            ));
        }

        let start = self
            .lexer
            .expect(|k| k == &TokenKind::OpenParen, "(")?
            .span
            .start;
        let end = loop {
            if self.lexer.peek()?.map(|t| &t.kind) == Some(&TokenKind::CloseParen) {
                let close_paren = self
                    .lexer
                    .next()
                    .expect("close paren")
                    .expect("close paren");
                break close_paren.span.end;
            }

            if !args.is_empty() {
                self.lexer.expect(|k| k == &TokenKind::Comma, ",")?;
            }

            let expression = self
                .expect_expression(inside_vararg_function)
                .wrap_err("in function call args")?;
            args.push(expression);
        };

        Ok(TokenTree::new(args, Span::new(start, end)))
    }

    fn parse_table_constructor(
        &mut self,
        inside_vararg_function: bool,
    ) -> crate::Result<TokenTree<TableConstructor>> {
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
                        span,
                    }) => {
                        let span = *span;
                        self.lexer.next();
                        break span.end;
                    }
                    Some(t) => {
                        return Err(lua_error!(
                            labels = vec![LabeledSpan::at(
                                t.span.start..t.span.end,
                                "expected table field separator"
                            )],
                            "unexpected token"
                        ))
                    }
                    None => {
                        return Err(lua_error!(
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

        let open_brace_start = self
            .lexer
            .expect(|k| k == &TokenKind::OpenBrace, "{")?
            .span
            .start;
        let mut fields = vec![];
        let closing_brace_end = loop {
            match self.lexer.peek()? {
                Some(Token {
                    kind: TokenKind::CloseBrace,
                    span,
                }) => {
                    let span = *span;
                    self.lexer.next();
                    break span.end;
                }
                Some(Token {
                    kind: TokenKind::OpenBracket,
                    span,
                }) => {
                    let field_start = span.start;
                    self.lexer.next();
                    let key = self
                        .expect_expression(inside_vararg_function)
                        .wrap_err("field index expression in table constructor")?;
                    self.lexer.expect(|k| k == &TokenKind::CloseBracket, "]")?;
                    self.lexer.expect(|k| k == &TokenKind::Equals, "=")?;
                    let value = self
                        .expect_expression(inside_vararg_function)
                        .wrap_err("field value in table constructor")?;
                    let field_end = value.span.end;
                    fields.push(TokenTree::new(
                        Field::Indexed(key, value),
                        Span::new(field_start, field_end),
                    ));

                    expect_end_of_field!();
                }
                Some(Token {
                    kind: TokenKind::Identifier(_),
                    ..
                }) => {
                    // Try to parse it as a sequence expression first (without an index or name)
                    let (name, value) = match self.parse_expression(inside_vararg_function)? {
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
                                .expect_expression(inside_vararg_function)
                                .wrap_err("field value in table constructor")?;

                            (Some(name), value)
                        }
                        Some(expression) => (None, expression),
                        None => {
                            return Err(lua_error!(
                                labels = vec![self
                                    .lexer
                                    .label_at_current_position("expected table field")],
                                "unexpected token"
                            ))
                        }
                    };

                    if let Some(name) = name {
                        let name_span = name.span;
                        let value_span = value.span;

                        fields.push(TokenTree::new(
                            Field::Named(name, value),
                            Span::new(name_span.start, value_span.end),
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
                    let expression = self
                        .parse_expression(inside_vararg_function)
                        .wrap_err("in table constructor")?;
                    match expression {
                        Some(expression) => {
                            let span = expression.span;
                            fields.push(TokenTree::new(Field::Value(expression), span));
                            expect_end_of_field!();
                        }
                        None => {
                            return Err(lua_error!(
                                labels = vec![expected_span],
                                "unexpected token"
                            ));
                        }
                    }
                }
                None => {
                    return Err(lua_error!(
                        labels = vec![LabeledSpan::at(
                            self.source.len()..self.source.len(),
                            "expected table field"
                        )],
                        "unexpected end of input"
                    ))
                }
            }
        };

        Ok(TokenTree {
            node: TableConstructor { fields },
            span: Span::new(open_brace_start, closing_brace_end),
        })
    }

    fn expect_expression(
        &mut self,
        inside_vararg_function: bool,
    ) -> crate::Result<TokenTree<Expression>> {
        self.expect_expression_within(0, inside_vararg_function)
    }

    fn expect_expression_within(
        &mut self,
        min_bp: u8,
        inside_vararg_function: bool,
    ) -> crate::Result<TokenTree<Expression>> {
        match self.parse_expression_within(min_bp, inside_vararg_function)? {
            Some(expression) => Ok(expression),
            None => Err(lua_error!(
                labels = vec![self.lexer.label_at_current_position("here")],
                "expected an expression"
            )),
        }
    }

    fn parse_expression(
        &mut self,
        inside_vararg_function: bool,
    ) -> crate::Result<Option<TokenTree<Expression>>> {
        self.parse_expression_within(0, inside_vararg_function)
    }

    fn parse_expression_within(
        &mut self,
        min_bp: u8,
        inside_vararg_function: bool,
    ) -> crate::Result<Option<TokenTree<Expression>>> {
        let mut lhs = match self.lexer.peek()? {
            Some(Token {
                kind: TokenKind::Identifier(_) | TokenKind::OpenParen,
                span,
            }) => {
                let start = span.start;
                let prefix_expression = self
                    .parse_prefix_expression(inside_vararg_function)
                    .wrap_err("in expression")?;
                let end = prefix_expression.span.end;
                TokenTree::new(
                    Expression::PrefixExpression(prefix_expression),
                    Span::new(start, end),
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
                if !inside_vararg_function {
                    return Err(lua_error!(
                        labels = vec![span.labeled("cannot use ... outside a vararg function")],
                        "unexpected token"
                    ));
                }

                TokenTree::new(Expression::Ellipsis, span)
            }
            Some(Token {
                kind: TokenKind::Minus,
                span,
            }) => {
                let span = *span;
                self.lexer.next();
                let ((), r_bp) = prefix_binding_power(&TokenKind::Minus);
                let rhs = self.expect_expression_within(r_bp, inside_vararg_function)?;
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
                self.lexer.next();
                let ((), r_bp) = prefix_binding_power(&TokenKind::Not);
                let rhs = self.expect_expression_within(r_bp, inside_vararg_function)?;
                let end = rhs.span.end;
                TokenTree::new(
                    Expression::UnaryOp {
                        op: TokenTree::new(UnaryOperator::Not, span),
                        rhs: Box::new(rhs),
                    },
                    Span::new(span.start, end),
                )
            }
            Some(Token {
                kind: TokenKind::Hash,
                span,
            }) => {
                let span = *span;
                self.lexer.next();
                let ((), r_bp) = prefix_binding_power(&TokenKind::Hash);
                let rhs = self.expect_expression_within(r_bp, inside_vararg_function)?;
                let end = rhs.span.end;
                TokenTree::new(
                    Expression::UnaryOp {
                        op: TokenTree::new(UnaryOperator::Length, span),
                        rhs: Box::new(rhs),
                    },
                    Span::new(span.start, end),
                )
            }
            Some(Token {
                kind: TokenKind::Tilde,
                span,
            }) => {
                let span = *span;
                self.lexer.next();
                let ((), r_bp) = prefix_binding_power(&TokenKind::Tilde);
                let rhs = self.expect_expression_within(r_bp, inside_vararg_function)?;
                let end = rhs.span.end;
                TokenTree::new(
                    Expression::UnaryOp {
                        op: TokenTree::new(UnaryOperator::BitwiseNot, span),
                        rhs: Box::new(rhs),
                    },
                    Span::new(span.start, end),
                )
            }
            Some(Token {
                kind: TokenKind::Function,
                span: Span { start, .. },
            }) => {
                let start = *start;
                self.lexer.next();
                let function_def = self.parse_function_body(start, None, None)?;
                let end = function_def.span.end;

                TokenTree::new(Expression::FunctionDef(function_def), Span::new(start, end))
            }
            Some(Token {
                kind: TokenKind::OpenBrace,
                ..
            }) => {
                let table = self
                    .parse_table_constructor(inside_vararg_function)
                    .wrap_err("in expression")?;
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
                    let args = self
                        .parse_args(inside_vararg_function)
                        .wrap_err("in expression method call")?;

                    let span = Span::new(prefix.span.start, args.span.end);

                    lhs = TokenTree::new(
                        Expression::PrefixExpression(TokenTree::new(
                            PrefixExpression::FunctionCall(TokenTree::new(
                                FunctionCall {
                                    function: Box::new(prefix),
                                    method_name: Some(name),
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

                let rhs = self.expect_expression_within(r_bp, inside_vararg_function)?;
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
        start_position: usize,
        method_name: Option<Vec<u8>>,
        name: Option<Vec<u8>>,
    ) -> crate::Result<TokenTree<FunctionDef>> {
        let open_paren = self.lexer.expect(|k| k == &TokenKind::OpenParen, "(")?;

        let mut parameters = Vec::new();
        if method_name.is_some() {
            parameters.push(TokenTree::new(
                Name(b"self".to_vec()),
                Span::new(open_paren.span.end, open_paren.span.end),
            ));
        }
        let mut varargs = None;

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
                    span,
                }) => {
                    varargs = Some(*span);
                    self.lexer.next();
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
                    return Err(lua_error!(
                        labels = vec![token.span.labeled("expected parameter name")],
                        "unexpected token"
                    ))
                }
                None => {
                    return Err(lua_error!(
                        labels = vec![LabeledSpan::at(
                            self.source.len()..self.source.len(),
                            "expected function signature"
                        )],
                        "unexpected end of input"
                    ))
                }
            }
        }

        let close_paren = self.lexer.expect(|k| k == &TokenKind::CloseParen, ")")?;
        let block = self
            .parse_block_inner(close_paren.span.end, varargs.is_some())
            .wrap_err("in function definition")?;
        let end_token = self.lexer.expect(|k| k == &TokenKind::End, "end")?;

        Ok(TokenTree::new(
            FunctionDef {
                name,
                method_name,
                parameters,
                varargs,
                block,
            },
            Span::new(start_position, end_token.span.end),
        ))
    }

    fn parse_name(&mut self) -> crate::Result<TokenTree<Name>> {
        let token = self
            .lexer
            .expect(|k| matches!(k, &TokenKind::Identifier(_)), "identifier")?;
        let name = match token.kind {
            TokenKind::Identifier(ident) => ident,
            _ => unreachable!(),
        };

        Ok(TokenTree::new(Name(name.to_vec()), token.span))
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
