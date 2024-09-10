use std::path::PathBuf;

use crate::ast::{self, AttributedName, LocalAttribute};
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};
use miette::{miette, Context, LabeledSpan};

pub struct Parser<'source> {
    pub filename: PathBuf,
    source: &'source str,
    lexer: Lexer<'source>,
}

impl<'source> Parser<'source> {
    pub fn new(filename: PathBuf, source: &'source str) -> Self {
        Self {
            filename: filename.clone(),
            source,
            lexer: Lexer::new(filename, source),
        }
    }

    pub fn parse(&mut self) -> miette::Result<ast::Block> {
        let result = self.parse_block().map_err(|r| self.with_source_code(r))?;

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

    fn parse_block(&mut self) -> miette::Result<ast::Block> {
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

        Ok(ast::Block {
            statements,
            return_statement,
        })
    }

    fn parse_statement(&mut self) -> miette::Result<Option<ast::Statement>> {
        loop {
            let next_token = self.lexer.peek()?;
            break match next_token.map(|t| &t.kind) {
                Some(TokenKind::Semicolon) => {
                    self.lexer.next();
                    continue;
                }
                Some(TokenKind::Break) => {
                    self.lexer.next();
                    Ok(Some(ast::Statement::Break))
                }
                Some(TokenKind::Goto) => {
                    self.lexer.next();
                    let name = self.parse_name().wrap_err("in goto statement")?;
                    Ok(Some(ast::Statement::Goto(name)))
                }
                Some(TokenKind::DoubleColon) => {
                    self.lexer.next();
                    let name = self.parse_name().wrap_err("in label")?;
                    self.lexer.expect(|k| k == &TokenKind::DoubleColon, "::")?;
                    Ok(Some(ast::Statement::Label(name)))
                }
                Some(TokenKind::Local) => {
                    self.lexer.next();
                    self.parse_local_declaration()
                        .wrap_err("in local declaration")
                        .map(Some)
                }
                Some(TokenKind::Do) => {
                    self.lexer.next();
                    let block = self.parse_block().wrap_err("in do statement")?;
                    self.lexer.expect(|k| k == &TokenKind::End, "end")?;
                    Ok(Some(ast::Statement::Block(block)))
                }
                Some(TokenKind::While) => {
                    self.lexer.next();
                    let condition = self.expect_expression().wrap_err("in while statement")?;
                    self.lexer.expect(|k| k == &TokenKind::Do, "do")?;
                    let block = self.parse_block().wrap_err("in while statement")?;
                    self.lexer.expect(|k| k == &TokenKind::End, "end")?;
                    Ok(Some(ast::Statement::While(ast::While { condition, block })))
                }
                Some(TokenKind::Repeat) => {
                    self.lexer.next();
                    let block = self.parse_block().wrap_err("in repeat statement")?;
                    self.lexer.expect(|k| k == &TokenKind::Until, "until")?;
                    let condition = self.expect_expression().wrap_err("in repeat statement")?;
                    Ok(Some(ast::Statement::Repeat(ast::Repeat {
                        block,
                        condition,
                    })))
                }
                Some(TokenKind::If) => {
                    self.lexer.next();
                    let condition = self.expect_expression().wrap_err("in if statement")?;
                    self.lexer.expect(|k| k == &TokenKind::Then, "then")?;
                    let block = self.parse_block().wrap_err("in if statement")?;
                    let mut else_ifs = Vec::new();
                    loop {
                        match self.lexer.peek()? {
                            Some(token) if token.kind == TokenKind::ElseIf => {
                                self.lexer.next();
                                let condition = self.expect_expression().wrap_err("in elseif")?;
                                self.lexer.expect(|k| k == &TokenKind::Then, "then")?;
                                let block = self.parse_block().wrap_err("in elseif")?;
                                else_ifs.push(ast::ElseIf { condition, block });
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

                    Ok(Some(ast::Statement::If(ast::If {
                        condition,
                        block,
                        else_ifs,
                        else_block,
                    })))
                }
                Some(TokenKind::Function) => {
                    self.lexer.next();
                    let (name, implicit_self_parameter) = {
                        let mut name = ast::Variable::Name(
                            self.parse_name().wrap_err("in function declaration")?,
                        );
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
                                    name = ast::Variable::Field(
                                        Box::new(ast::PrefixExpression::Variable(name)),
                                        field,
                                    );
                                    continue;
                                }
                                Some(Token {
                                    kind: TokenKind::Colon,
                                    ..
                                }) => {
                                    self.lexer.next();
                                    let method_name =
                                        self.parse_name().wrap_err("in function declaration")?;
                                    name = ast::Variable::Field(
                                        Box::new(ast::PrefixExpression::Variable(name)),
                                        method_name,
                                    );
                                    implicit_self_parameter = Some(ast::Name("self".to_string()));
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

                    Ok(Some(ast::Statement::Assignment(ast::Assignment {
                        varlist: vec![name],
                        explist: vec![ast::Expression::FunctionDef(function_def)],
                    })))
                }
                Some(TokenKind::For) => {
                    self.lexer.next();
                    self.parse_for_statement()
                        .wrap_err("in for statement")
                        .map(Some)
                }
                Some(
                    TokenKind::Return
                    | TokenKind::End
                    | TokenKind::ElseIf
                    | TokenKind::Else
                    | TokenKind::Until,
                ) => {
                    // Handled by the block parser or outside of it
                    Ok(None)
                }
                Some(TokenKind::Identifier(_) | TokenKind::OpenParen) => self
                    .parse_varlist_or_functioncall()
                    .wrap_err("in variable assignment or function call")
                    .map(Some),
                Some(t) => {
                    return Err(miette!(
                        labels = vec![LabeledSpan::at(
                            self.lexer.position..self.lexer.position,
                            "here"
                        )],
                        "unimplemented token in parse_statement: {t:?}"
                    ))
                }

                // Reached the end of the input
                None => Ok(None),
            };
        }
    }

    fn parse_local_declaration(&mut self) -> miette::Result<ast::Statement> {
        if matches!(
            self.lexer.peek()?.map(|t| &t.kind),
            Some(TokenKind::Function)
        ) {
            self.lexer.next();
            let name = self
                .parse_name()
                .wrap_err("in local function declaration")?;
            let function_def = self
                .parse_function_body(None)
                .wrap_err_with(|| format!("in local {} function declaration", name.0))?;

            Ok(ast::Statement::LocalDeclaraction(
                vec![AttributedName {
                    name,
                    attribute: None,
                }],
                vec![ast::Expression::FunctionDef(function_def)],
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

                        let attr = match attr_str {
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
                        };

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
                    // Some(Token {
                    //     kind: TokenKind::Identifier(attr_str),
                    //     ..
                    // }) => {
                    //     let attr = match *attr_str {
                    //         "const" => LocalAttribute::Const,
                    //         "close" => LocalAttribute::Close,
                    //         _ => {
                    //             // This probably means we reached the end of the `local`
                    //             // declaration. For example:
                    //             // local a, b, c
                    //             // a = 1
                    //             attributed_names.push(AttributedName {
                    //                 name,
                    //                 attribute: None,
                    //             });
                    //             break;
                    //         }
                    //     };
                    //     self.lexer.next();

                    //     attributed_names.push(AttributedName {
                    //         name,
                    //         attribute: Some(attr),
                    //     });

                    //                       }
                    _ => {
                        attributed_names.push(AttributedName {
                            name,
                            attribute: None,
                        });
                        break;
                    }
                }
            }

            if self.lexer.peek()?.map(|t| &t.kind) == Some(&TokenKind::Equals) {
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

                Ok(ast::Statement::LocalDeclaraction(
                    attributed_names,
                    expressions,
                ))
            } else {
                Ok(ast::Statement::LocalDeclaraction(
                    attributed_names,
                    Vec::new(),
                ))
            }
        }
    }

    fn parse_for_statement(&mut self) -> miette::Result<ast::Statement> {
        let name = self.parse_name().wrap_err("name in for statement")?;
        let condition = match self.lexer.peek()? {
            Some(token) if token.kind == TokenKind::Equals => {
                self.lexer.next();
                let initial = self
                    .expect_expression()
                    .wrap_err("initializer expression in for statement")?;
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

                ast::ForCondition::NumericFor {
                    name,
                    initial,
                    limit,
                    step,
                }
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

                ast::ForCondition::GenericFor { names, expressions }
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
        let block = self.parse_block().wrap_err("in block of for statement")?;
        self.lexer.expect(|k| k == &TokenKind::End, "end")?;

        Ok(ast::Statement::For(ast::For { condition, block }))
    }

    fn parse_varlist_or_functioncall(&mut self) -> miette::Result<ast::Statement> {
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
        // functioncall ::=  prefixexp args | prefixexp ‘:’ Name argso
        //
        // prefixexp ::= var | functioncall | ‘(’ exp ‘)’

        let prefix_expression = self
            .parse_prefix_expression()
            .wrap_err("in varlist or functioncall")?;

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
                        ast::PrefixExpression::Variable(v) => v,
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

                return Ok(ast::Statement::Assignment(ast::Assignment {
                    varlist,
                    explist,
                }));
            }
            _ => {}
        }

        match prefix_expression {
            ast::PrefixExpression::FunctionCall(f) => Ok(ast::Statement::FunctionCall(f)),
            _ => Err(miette!(
                labels = vec![LabeledSpan::at(
                    self.lexer.position..self.lexer.position,
                    "expected function call or variable assignment"
                )],
                "unexpected token"
            )),
        }
    }

    fn parse_prefix_expression(&mut self) -> miette::Result<ast::PrefixExpression> {
        let mut prefix = match self.lexer.next().transpose()? {
            Some(Token {
                kind: TokenKind::OpenParen,
                ..
            }) => {
                let expression = self.expect_expression()?;
                self.lexer.expect(|k| k == &TokenKind::CloseParen, ")")?;
                ast::PrefixExpression::Parenthesized(Box::new(expression))
            }
            Some(Token {
                kind: TokenKind::Identifier(ident),
                ..
            }) => {
                let name = ast::Name(ident.to_string());
                ast::PrefixExpression::Variable(ast::Variable::Name(name))
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
                    prefix = ast::PrefixExpression::Variable(ast::Variable::Indexed(
                        Box::new(prefix),
                        Box::new(expression),
                    ));
                }
                Some(Token {
                    kind: TokenKind::Dot,
                    ..
                }) => {
                    self.lexer.next();
                    let name = self.parse_name().wrap_err("in prefix expression")?;
                    prefix = ast::PrefixExpression::Variable(ast::Variable::Field(
                        Box::new(prefix),
                        name,
                    ));
                }
                Some(Token {
                    kind: TokenKind::Colon,
                    ..
                }) => {
                    self.lexer.next();
                    let name = self.parse_name().wrap_err("in prefix expression")?;
                    let args = self.parse_args().wrap_err("in prefix expression")?;
                    prefix = ast::PrefixExpression::FunctionCall(ast::FunctionCall {
                        function: Box::new(prefix),
                        as_method: true,
                        name: Some(name),
                        args,
                    });
                }
                Some(Token {
                    kind: TokenKind::OpenParen | TokenKind::String(_),
                    ..
                }) => {
                    let args = self.parse_args().wrap_err("in prefix expression")?;
                    prefix = ast::PrefixExpression::FunctionCall(ast::FunctionCall {
                        function: Box::new(prefix),
                        as_method: false,
                        name: None,
                        args,
                    });
                }
                Some(Token {
                    kind: TokenKind::OpenBrace,
                    ..
                }) => {
                    let table = self
                        .parse_table_constructor()
                        .wrap_err("in prefix expression")?;
                    prefix = ast::PrefixExpression::FunctionCall(ast::FunctionCall {
                        function: Box::new(prefix),
                        as_method: false,
                        name: None,
                        args: vec![ast::Expression::TableConstructor(table)],
                    });
                }
                _ => break,
            }
        }

        Ok(prefix)
    }

    fn parse_args(&mut self) -> miette::Result<Vec<ast::Expression>> {
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

    fn parse_table_constructor(&mut self) -> miette::Result<ast::TableConstructor> {
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
                    self.lexer.next();
                    let key = self
                        .expect_expression()
                        .wrap_err("field index expression in table constructor")?;
                    self.lexer.expect(|k| k == &TokenKind::CloseBracket, "]")?;
                    self.lexer.expect(|k| k == &TokenKind::Equals, "=")?;
                    let value = self
                        .expect_expression()
                        .wrap_err("field value in table constructor")?;
                    fields.push(ast::Field::Indexed(key, value));

                    expect_end_of_field!();
                }
                Some(Token {
                    kind: TokenKind::Identifier(_),
                    ..
                }) => {
                    // Try to parse it as a sequence expression first (without an index or name)
                    let (name, value) = match self.parse_expression()? {
                        Some(ast::Expression::PrefixExpression(
                            ast::PrefixExpression::Variable(ast::Variable::Name(name)),
                        )) if self.lexer.peek()?.map(|t| &t.kind) == Some(&TokenKind::Equals) => {
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
                        fields.push(ast::Field::Named(name, value));
                    } else {
                        fields.push(ast::Field::Value(value));
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
                            fields.push(ast::Field::Value(expression));
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

        Ok(ast::TableConstructor { fields })
    }

    fn expect_expression(&mut self) -> miette::Result<ast::Expression> {
        self.expect_expression_within(0)
    }

    fn expect_expression_within(&mut self, min_bp: u8) -> miette::Result<ast::Expression> {
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

    fn parse_expression(&mut self) -> miette::Result<Option<ast::Expression>> {
        self.parse_expression_within(0)
    }

    fn parse_expression_within(&mut self, min_bp: u8) -> miette::Result<Option<ast::Expression>> {
        let mut lhs = match self.lexer.peek()? {
            Some(Token {
                kind: TokenKind::Identifier(_) | TokenKind::OpenParen,
                ..
            }) => ast::Expression::PrefixExpression(
                self.parse_prefix_expression().wrap_err("in expression")?,
            ),
            Some(Token {
                kind: TokenKind::Nil,
                ..
            }) => {
                self.lexer.next();
                ast::Expression::Literal(ast::Literal::Nil)
            }
            Some(Token {
                kind: TokenKind::True,
                ..
            }) => {
                self.lexer.next();
                ast::Expression::Literal(ast::Literal::Boolean(true))
            }
            Some(Token {
                kind: TokenKind::False,
                ..
            }) => {
                self.lexer.next();
                ast::Expression::Literal(ast::Literal::Boolean(false))
            }
            Some(Token {
                kind: TokenKind::Integer(i),
                ..
            }) => {
                let i = *i;
                self.lexer.next();
                ast::Expression::Literal(ast::Literal::Number(ast::Number::Integer(i)))
            }
            Some(Token {
                kind: TokenKind::Float(f),
                ..
            }) => {
                let f = *f;
                self.lexer.next();
                ast::Expression::Literal(ast::Literal::Number(ast::Number::Float(f)))
            }
            Some(Token {
                kind: TokenKind::String(_),
                ..
            }) => {
                let s = match self.lexer.next() {
                    Some(Ok(Token {
                        kind: TokenKind::String(s),
                        ..
                    })) => s,
                    _ => unreachable!(),
                };
                ast::Expression::Literal(ast::Literal::String(s))
            }
            Some(Token {
                kind: TokenKind::DotDotDot,
                ..
            }) => {
                self.lexer.next();
                ast::Expression::Ellipsis
            }
            Some(Token {
                kind: TokenKind::Minus,
                ..
            }) => {
                self.lexer.next();
                let ((), r_bp) = prefix_binding_power(&TokenKind::Minus);
                let rhs = self.expect_expression_within(r_bp)?;
                ast::Expression::UnaryOp(ast::UnaryOp {
                    op: ast::UnaryOperator::Neg,
                    operand: Box::new(rhs),
                })
            }
            Some(Token {
                kind: TokenKind::Not,
                ..
            }) => {
                self.lexer.next();
                let ((), r_bp) = prefix_binding_power(&TokenKind::Not);
                let rhs = self.expect_expression_within(r_bp)?;
                ast::Expression::UnaryOp(ast::UnaryOp {
                    op: ast::UnaryOperator::Not,
                    operand: Box::new(rhs),
                })
            }
            Some(Token {
                kind: TokenKind::Hash,
                ..
            }) => {
                self.lexer.next();
                let ((), r_bp) = prefix_binding_power(&TokenKind::Hash);
                let rhs = self.expect_expression_within(r_bp)?;
                ast::Expression::UnaryOp(ast::UnaryOp {
                    op: ast::UnaryOperator::Length,
                    operand: Box::new(rhs),
                })
            }
            Some(Token {
                kind: TokenKind::Tilde,
                ..
            }) => {
                self.lexer.next();
                let ((), r_bp) = prefix_binding_power(&TokenKind::Tilde);
                let rhs = self.expect_expression_within(r_bp)?;
                ast::Expression::UnaryOp(ast::UnaryOp {
                    op: ast::UnaryOperator::BitwiseNot,
                    operand: Box::new(rhs),
                })
            }
            Some(Token {
                kind: TokenKind::Function,
                ..
            }) => {
                self.lexer.next();
                ast::Expression::FunctionDef(self.parse_function_body(None)?)
            }
            Some(Token {
                kind: TokenKind::OpenBrace,
                ..
            }) => {
                let table = self.parse_table_constructor().wrap_err("in expression")?;
                ast::Expression::TableConstructor(table)
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
                        ast::Expression::PrefixExpression(p) => p,
                        _ => {
                            return Ok(Some(lhs));
                        }
                    };

                    self.lexer.next();
                    let name = self.parse_name().wrap_err("in expression method call")?;
                    let args = self.parse_args().wrap_err("in expression method call")?;
                    lhs = ast::Expression::PrefixExpression(ast::PrefixExpression::FunctionCall(
                        ast::FunctionCall {
                            function: Box::new(prefix),
                            as_method: true,
                            name: Some(name),
                            args,
                        },
                    ));
                    continue;
                }
                // TODO: Is this really correct?
                _ => break,
                // None => break,
                // Some(token) => {
                //     return Err(miette!(
                //         labels = vec![token.span.labeled("expected operator")],
                //         "unexpected token"
                //     ))
                // }
            };

            // TODO: Do we have postfix operators? Maybe `:` is one?

            if let Some((l_bp, r_bp)) = infix_binding_power(&op.kind) {
                if l_bp < min_bp {
                    break;
                }

                let op = ast::BinaryOperator::from(&op.kind);
                self.lexer.next();

                let rhs = self.expect_expression_within(r_bp)?;
                lhs = ast::Expression::BinaryOp(ast::BinaryOp {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            }
        }

        Ok(Some(lhs))
    }

    fn parse_function_body(
        &mut self,
        implicit_self_parameter: Option<ast::Name>,
    ) -> miette::Result<ast::FunctionDef> {
        self.lexer.expect(|k| k == &TokenKind::OpenParen, "(")?;
        let mut parameters = Vec::new();
        if let Some(name) = implicit_self_parameter {
            parameters.push(name);
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

        self.lexer.expect(|k| k == &TokenKind::CloseParen, ")")?;
        let block = self.parse_block().wrap_err("in function definition")?;
        self.lexer.expect(|k| k == &TokenKind::End, "end")?;

        Ok(ast::FunctionDef {
            parameters,
            has_varargs,
            block,
        })
    }

    fn parse_name(&mut self) -> miette::Result<ast::Name> {
        let token = self
            .lexer
            .expect(|k| matches!(k, &TokenKind::Identifier(_)), "identifier")?;
        let name = match token.kind {
            TokenKind::Identifier(ident) => ident,
            _ => unreachable!(),
        };

        Ok(ast::Name(name.to_string()))
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
