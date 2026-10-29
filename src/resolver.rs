//! Converts the parser AST into a resolved AST before compilation.
//!
//! Analysis first identifies lexical bindings and all transitive captures. Lowering
//! then embeds finalized binding data in each node and rewrites free names as
//! fields on the lexical `_ENV`. Source-site maps are internal to this pass;
//! the compiler receives only the resolved tree and assigns runtime registers.
use std::collections::HashMap;

use crate::{
    ast::*,
    token::Span,
    value::{LuaString, LuaVariableAttribute},
};

type Site = (usize, usize);
use crate::resolved_ast::BindingId;
fn site(span: Span) -> Site {
    (span.start, span.end)
}

#[derive(Debug)]
struct Binding {
    pub name: LuaString,
    pub attributes: u8,
    function: usize,
}

#[derive(Debug, Default)]
struct Function {
    pub captures: Vec<BindingId>,
    parent: Option<usize>,
}

#[derive(Debug, Default)]
struct Resolution {
    pub bindings: Vec<Binding>,
    pub functions: Vec<Function>,
    declarations: HashMap<Site, BindingId>,
    uses: HashMap<Site, Option<BindingId>>,
    environments: HashMap<Site, BindingId>,
    function_sites: HashMap<Site, usize>,
}

impl Resolution {
    pub fn resolve(block: &TokenTree<Block>, parameters: &[(LuaString, Span)]) -> Self {
        let mut resolver = Resolver {
            result: Self::default(),
            scopes: vec![vec![]],
            function: 0,
        };
        resolver.result.functions.push(Function::default());
        // Binding zero is the main chunk's externally supplied environment.
        resolver.declare(b"_ENV".into(), None, 0);
        for (name, span) in parameters {
            resolver.declare(name.clone(), Some(*span), 0);
        }
        resolver.block(&block.node);
        resolver.result
    }

    pub fn declaration(&self, span: Span) -> BindingId {
        self.declarations[&site(span)]
    }
    pub fn function(&self, span: Span) -> usize {
        self.function_sites[&site(span)]
    }
}

struct Resolver {
    result: Resolution,
    scopes: Vec<Vec<BindingId>>,
    function: usize,
}

impl Resolver {
    fn declare(&mut self, name: LuaString, span: Option<Span>, attributes: u8) {
        let id = self.result.bindings.len();
        self.result.bindings.push(Binding {
            name,
            attributes,
            function: self.function,
        });
        self.scopes.last_mut().unwrap().push(id);
        if let Some(span) = span {
            self.result.declarations.insert(site(span), id);
        }
    }

    fn find(&self, name: &[u8]) -> Option<BindingId> {
        self.scopes
            .iter()
            .rev()
            .flat_map(|s| s.iter().rev())
            .copied()
            .find(|&id| self.result.bindings[id].name == name)
    }

    fn capture(&mut self, id: BindingId) {
        let owner = self.result.bindings[id].function;
        let mut function = self.function;
        while function != owner {
            let info = &mut self.result.functions[function];
            if !info.captures.contains(&id) {
                info.captures.push(id);
            }
            function = info
                .parent
                .expect("binding belongs to an enclosing function");
        }
    }

    fn name(&mut self, name: &TokenTree<Name>) {
        let binding = self.find(&name.node.0);
        self.result.uses.insert(site(name.span), binding);
        if let Some(id) = binding {
            self.capture(id);
        } else {
            let env = self.find(b"_ENV").expect("environment binding");
            self.result.environments.insert(site(name.span), env);
            self.capture(env);
        }
    }

    fn scoped_block(&mut self, block: &Block) {
        self.scopes.push(vec![]);
        self.block(block);
        self.scopes.pop();
    }

    fn block(&mut self, block: &Block) {
        for statement in &block.statements {
            match &statement.node {
                Statement::Assignment { varlist, explist } => {
                    for var in varlist {
                        self.variable(&var.node);
                    }
                    self.expressions(explist);
                }
                Statement::Block(b) => self.scoped_block(&b.node),
                Statement::While { condition, block } => {
                    self.expression(&condition.node);
                    self.scoped_block(&block.node);
                }
                Statement::Repeat { block, condition } => {
                    self.scopes.push(vec![]);
                    self.block(&block.node);
                    self.expression(&condition.node);
                    self.scopes.pop();
                }
                Statement::If {
                    condition,
                    block,
                    else_ifs,
                    else_block,
                } => {
                    self.expression(&condition.node);
                    self.scoped_block(&block.node);
                    for branch in else_ifs {
                        self.expression(&branch.node.condition.node);
                        self.scoped_block(&branch.node.block.node);
                    }
                    if let Some(block) = else_block {
                        self.scoped_block(&block.node);
                    }
                }
                Statement::For { condition, block } => {
                    self.scopes.push(vec![]);
                    match &condition.node {
                        ForCondition::NumericFor(n) => {
                            self.expression(&n.initial.node);
                            self.expression(&n.limit.node);
                            if let Some(step) = &n.step {
                                self.expression(&step.node);
                            }
                            self.declare(n.name.node.0.clone(), Some(n.name.span), 0);
                        }
                        ForCondition::GenericFor { names, expressions } => {
                            self.expressions(expressions);
                            for name in names {
                                self.declare(name.node.0.clone(), Some(name.span), 0);
                            }
                        }
                    }
                    self.scoped_block(&block.node);
                    self.scopes.pop();
                }
                Statement::FunctionCall(call) => self.call(&call.node),
                Statement::LocalDeclaraction(names, expressions) => {
                    self.expressions(expressions);
                    for name in names {
                        let attributes = match name.node.attribute.as_ref().map(|a| &a.node) {
                            Some(LocalAttribute::Const) => LuaVariableAttribute::Constant as u8,
                            Some(LocalAttribute::Close) => {
                                LuaVariableAttribute::Constant as u8
                                    | LuaVariableAttribute::ToBeClosed as u8
                            }
                            None => 0,
                        };
                        self.declare(
                            name.node.name.node.0.clone(),
                            Some(name.node.name.span),
                            attributes,
                        );
                    }
                }
                Statement::LocalFunctionDeclaration(name, function) => {
                    self.declare(name.node.0.clone(), Some(name.span), 0);
                    self.function(function);
                }
                Statement::Goto(_) | Statement::Label(_) | Statement::Break => {}
            }
        }
        if let Some(ret) = &block.return_statement {
            self.expressions(&ret.values);
        }
    }

    fn function(&mut self, function: &TokenTree<FunctionDef>) {
        let parent = self.function;
        self.function = self.result.functions.len();
        self.result
            .function_sites
            .insert(site(function.node.block.span), self.function);
        self.result.functions.push(Function {
            captures: vec![],
            parent: Some(parent),
        });
        self.scopes.push(vec![]);
        for param in &function.node.parameters {
            self.declare(param.node.0.clone(), Some(param.span), 0);
        }
        self.scoped_block(&function.node.block.node);
        self.scopes.pop();
        self.function = parent;
    }

    fn expressions(&mut self, expressions: &[TokenTree<Expression>]) {
        for expression in expressions {
            self.expression(&expression.node);
        }
    }
    fn expression(&mut self, expression: &Expression) {
        match expression {
            Expression::PrefixExpression(p) => self.prefix(&p.node),
            Expression::FunctionDef(f) => self.function(f),
            Expression::TableConstructor(t) => {
                for field in &t.node.fields {
                    match &field.node {
                        Field::Named(_, v) | Field::Value(v) => self.expression(&v.node),
                        Field::Indexed(k, v) => {
                            self.expression(&k.node);
                            self.expression(&v.node);
                        }
                    }
                }
            }
            Expression::BinaryOp { lhs, rhs, .. } => {
                self.expression(&lhs.node);
                self.expression(&rhs.node);
            }
            Expression::UnaryOp { rhs, .. } => self.expression(&rhs.node),
            Expression::Literal(_) | Expression::Ellipsis => {}
        }
    }
    fn prefix(&mut self, prefix: &PrefixExpression) {
        match prefix {
            PrefixExpression::Variable(v) => self.variable(&v.node),
            PrefixExpression::FunctionCall(c) => self.call(&c.node),
            PrefixExpression::Parenthesized(e) => self.expression(&e.node),
        }
    }
    fn variable(&mut self, variable: &Variable) {
        match variable {
            Variable::Name(n) => self.name(n),
            Variable::Indexed(p, e) => {
                self.prefix(&p.node);
                self.expression(&e.node);
            }
            Variable::Field(p, _) => self.prefix(&p.node),
        }
    }
    fn call(&mut self, call: &FunctionCall) {
        self.prefix(&call.function.node);
        self.expressions(&call.args.node);
    }
}

/// Resolve an AST after parsing and any AST optimizations, before compilation.
pub fn resolve(block: TokenTree<Block>) -> crate::resolved_ast::Chunk {
    resolve_with_parameters(block, vec![])
}

/// Resolve a chunk with explicitly supplied initial local parameters.
pub fn resolve_with_parameters(
    block: TokenTree<Block>,
    parameters: Vec<TokenTree<Name>>,
) -> crate::resolved_ast::Chunk {
    let initial = parameters
        .iter()
        .map(|p| (p.node.0.clone(), p.span))
        .collect::<Vec<_>>();
    let resolution = Resolution::resolve(&block, &initial);
    let mut lowerer = Lowerer {
        resolution: &resolution,
        function: 0,
    };
    crate::resolved_ast::Chunk {
        parameters: parameters
            .into_iter()
            .map(|p| lowerer.declaration(p))
            .collect(),
        captures: vec![crate::resolved_ast::Name::Upvalue(lowerer.binding(0))],
        block: lowerer.block(block),
    }
}

use crate::resolved_ast as resolved;

struct Lowerer<'a> {
    resolution: &'a Resolution,
    function: usize,
}

impl Lowerer<'_> {
    fn binding(&self, id: BindingId) -> resolved::Binding {
        let binding = &self.resolution.bindings[id];
        resolved::Binding {
            id,
            name: binding.name.clone(),
            attributes: binding.attributes,
        }
    }

    fn reference(&self, id: BindingId) -> resolved::Name {
        if self.resolution.bindings[id].function == self.function && id != 0 {
            resolved::Name::Local(self.binding(id))
        } else {
            resolved::Name::Upvalue(self.binding(id))
        }
    }

    fn declaration(&self, name: TokenTree<Name>) -> TokenTree<resolved::Name> {
        let id = self.resolution.declaration(name.span);
        TokenTree::new(resolved::Name::Local(self.binding(id)), name.span)
    }

    fn block(&mut self, block: TokenTree<Block>) -> TokenTree<resolved::Block> {
        TokenTree::new(
            resolved::Block {
                statements: block
                    .node
                    .statements
                    .into_iter()
                    .map(|s| self.statement(s))
                    .collect(),
                return_statement: block
                    .node
                    .return_statement
                    .map(|r| resolved::ReturnStatement {
                        return_span: r.return_span,
                        values: self.expressions(r.values),
                    }),
                end_span: block.node.end_span,
            },
            block.span,
        )
    }

    fn statement(&mut self, statement: TokenTree<Statement>) -> TokenTree<resolved::Statement> {
        use resolved::Statement as R;
        let node = match statement.node {
            Statement::Assignment { varlist, explist } => R::Assignment {
                varlist: varlist.into_iter().map(|v| self.variable(v)).collect(),
                explist: self.expressions(explist),
            },
            Statement::Block(b) => R::Block(self.block(b)),
            Statement::While { condition, block } => R::While {
                condition: self.expression(condition),
                block: self.block(block),
            },
            Statement::Repeat { block, condition } => R::Repeat {
                block: self.block(block),
                condition: self.expression(condition),
            },
            Statement::If {
                condition,
                block,
                else_ifs,
                else_block,
            } => R::If {
                condition: self.expression(condition),
                block: self.block(block),
                else_ifs: else_ifs
                    .into_iter()
                    .map(|b| {
                        TokenTree::new(
                            resolved::ElseIf {
                                condition: self.expression(b.node.condition),
                                block: self.block(b.node.block),
                            },
                            b.span,
                        )
                    })
                    .collect(),
                else_block: else_block.map(|b| self.block(b)),
            },
            Statement::For { condition, block } => {
                let node = match condition.node {
                    ForCondition::NumericFor(n) => {
                        resolved::ForCondition::NumericFor(Box::new(resolved::NumericFor {
                            name: self.declaration(n.name),
                            initial: self.expression(n.initial),
                            limit: self.expression(n.limit),
                            step: n.step.map(|e| self.expression(e)),
                        }))
                    }
                    ForCondition::GenericFor { names, expressions } => {
                        resolved::ForCondition::GenericFor {
                            names: names.into_iter().map(|n| self.declaration(n)).collect(),
                            expressions: self.expressions(expressions),
                        }
                    }
                };
                R::For {
                    condition: TokenTree::new(node, condition.span),
                    block: self.block(block),
                }
            }
            Statement::Goto(n) => R::Goto(n),
            Statement::Label(n) => R::Label(n),
            Statement::Break => R::Break,
            Statement::FunctionCall(c) => R::FunctionCall(self.call(c)),
            Statement::LocalDeclaraction(names, expressions) => R::LocalDeclaraction(
                names
                    .into_iter()
                    .map(|n| {
                        TokenTree::new(
                            resolved::AttributedName {
                                name: self.declaration(n.node.name),
                                attribute: n.node.attribute,
                            },
                            n.span,
                        )
                    })
                    .collect(),
                self.expressions(expressions),
            ),
            Statement::LocalFunctionDeclaration(name, function) => {
                R::LocalFunctionDeclaration(self.declaration(name), self.function(function))
            }
        };
        TokenTree::new(node, statement.span)
    }

    fn function(&mut self, function: TokenTree<FunctionDef>) -> TokenTree<resolved::FunctionDef> {
        let parent = self.function;
        self.function = self.resolution.function(function.node.block.span);
        let result = TokenTree::new(
            resolved::FunctionDef {
                name: function.node.name,
                method_name: function.node.method_name,
                parameters: function
                    .node
                    .parameters
                    .into_iter()
                    .map(|p| self.declaration(p))
                    .collect(),
                captures: self.resolution.functions[self.function]
                    .captures
                    .iter()
                    .map(|&id| resolved::Name::Upvalue(self.binding(id)))
                    .collect(),
                varargs: function.node.varargs,
                block: self.block(function.node.block),
            },
            function.span,
        );
        self.function = parent;
        result
    }

    fn expressions(
        &mut self,
        expressions: Vec<TokenTree<Expression>>,
    ) -> Vec<TokenTree<resolved::Expression>> {
        expressions
            .into_iter()
            .map(|e| self.expression(e))
            .collect()
    }

    fn expression(&mut self, expression: TokenTree<Expression>) -> TokenTree<resolved::Expression> {
        use resolved::Expression as R;
        let node = match expression.node {
            Expression::PrefixExpression(p) => R::PrefixExpression(self.prefix(p)),
            Expression::Literal(l) => R::Literal(l),
            Expression::Ellipsis => R::Ellipsis,
            Expression::FunctionDef(f) => R::FunctionDef(self.function(f)),
            Expression::TableConstructor(t) => R::TableConstructor(TokenTree::new(
                resolved::TableConstructor {
                    fields: t
                        .node
                        .fields
                        .into_iter()
                        .map(|f| {
                            TokenTree::new(
                                match f.node {
                                    Field::Named(n, v) => {
                                        resolved::Field::Named(n, self.expression(v))
                                    }
                                    Field::Indexed(k, v) => resolved::Field::Indexed(
                                        self.expression(k),
                                        self.expression(v),
                                    ),
                                    Field::Value(v) => resolved::Field::Value(self.expression(v)),
                                },
                                f.span,
                            )
                        })
                        .collect(),
                },
                t.span,
            )),
            Expression::BinaryOp { op, lhs, rhs } => R::BinaryOp {
                op,
                lhs: Box::new(self.expression(*lhs)),
                rhs: Box::new(self.expression(*rhs)),
            },
            Expression::UnaryOp { op, rhs } => R::UnaryOp {
                op,
                rhs: Box::new(self.expression(*rhs)),
            },
        };
        TokenTree::new(node, expression.span)
    }

    fn prefix(
        &mut self,
        prefix: TokenTree<PrefixExpression>,
    ) -> TokenTree<resolved::PrefixExpression> {
        let node = match prefix.node {
            PrefixExpression::Variable(v) => resolved::PrefixExpression::Variable(self.variable(v)),
            PrefixExpression::FunctionCall(c) => {
                resolved::PrefixExpression::FunctionCall(self.call(c))
            }
            PrefixExpression::Parenthesized(e) => {
                resolved::PrefixExpression::Parenthesized(Box::new(self.expression(*e)))
            }
        };
        TokenTree::new(node, prefix.span)
    }

    fn variable(&mut self, variable: TokenTree<Variable>) -> TokenTree<resolved::Variable> {
        let node = match variable.node {
            Variable::Name(name) => match self.resolution.uses[&site(name.span)] {
                Some(id) => resolved::Variable::Name(TokenTree::new(self.reference(id), name.span)),
                None => {
                    let env = self.resolution.environments[&site(name.span)];
                    let env = TokenTree::new(
                        resolved::Variable::Name(TokenTree::new(self.reference(env), name.span)),
                        name.span,
                    );
                    resolved::Variable::Field(
                        Box::new(TokenTree::new(
                            resolved::PrefixExpression::Variable(env),
                            name.span,
                        )),
                        name,
                    )
                }
            },
            Variable::Indexed(p, e) => resolved::Variable::Indexed(
                Box::new(self.prefix(*p)),
                Box::new(self.expression(*e)),
            ),
            Variable::Field(p, n) => resolved::Variable::Field(Box::new(self.prefix(*p)), n),
        };
        TokenTree::new(node, variable.span)
    }

    fn call(&mut self, call: TokenTree<FunctionCall>) -> TokenTree<resolved::FunctionCall> {
        TokenTree::new(
            resolved::FunctionCall {
                function: Box::new(self.prefix(*call.node.function)),
                method_name: call.node.method_name,
                args: TokenTree::new(self.expressions(call.node.args.node), call.node.args.span),
            },
            call.span,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn resolve(source: &str) -> Resolution {
        let name: LuaString = b"test".into();
        let ast = Parser::new(&name, source.as_bytes()).parse().unwrap();
        Resolution::resolve(&ast, &[])
    }

    #[test]
    fn distinguishes_shadowed_bindings_and_propagates_transitive_captures() {
        let resolution = resolve(
            "local x = 1; local f = function() return function() return x end end; local x = 2; return x",
        );
        let xs = resolution
            .bindings
            .iter()
            .enumerate()
            .filter(|(_, b)| b.name == b"x".as_slice())
            .collect::<Vec<_>>();
        assert_eq!(xs.len(), 2);
        assert_eq!(resolution.functions[1].captures, vec![xs[0].0]);
        assert_eq!(resolution.functions[2].captures, vec![xs[0].0]);
    }

    #[test]
    fn implicit_globals_capture_the_lexical_environment() {
        let resolution =
            resolve("local _ENV = {}; return function() return function() return missing end end");
        assert_eq!(resolution.functions[1].captures, vec![1]);
        assert_eq!(resolution.functions[2].captures, vec![1]);
    }

    fn parse(source: &str) -> TokenTree<Block> {
        crate::parser::Parser::new(b"test", source.as_bytes())
            .parse()
            .unwrap()
    }

    fn environment(expression: &resolved::Expression) -> &resolved::Name {
        let resolved::Expression::PrefixExpression(prefix) = expression else {
            panic!("expected prefix")
        };
        let resolved::PrefixExpression::Variable(variable) = &prefix.node else {
            panic!("expected variable")
        };
        let resolved::Variable::Field(table, key) = &variable.node else {
            panic!("expected environment field")
        };
        assert_eq!(key.node.0, b"print".as_slice());
        let resolved::PrefixExpression::Variable(table) = &table.node else {
            panic!("expected environment")
        };
        let resolved::Variable::Name(name) = &table.node else {
            panic!("expected bound environment")
        };
        &name.node
    }

    #[test]
    fn assignment_contains_a_local_target_and_an_explicit_environment_access() {
        let chunk = super::resolve(parse("local x; x = print"));
        let resolved::Statement::Assignment { varlist, explist } =
            &chunk.block.node.statements[1].node
        else {
            panic!("assignment")
        };
        let resolved::Variable::Name(target) = &varlist[0].node else {
            panic!("local target")
        };
        let resolved::Name::Local(binding) = &target.node else {
            panic!("local binding")
        };
        assert_eq!(binding.name, b"x".as_slice());
        let resolved::Name::Upvalue(env) = environment(&explist[0].node) else {
            panic!("upvalue environment")
        };
        assert_eq!(env.name, b"_ENV".as_slice());
        assert_eq!(env.id, chunk.captures[0].binding().id);
    }

    #[test]
    fn local_environment_is_local_here_and_an_upvalue_inside_a_function() {
        let chunk = super::resolve(parse(
            "local _ENV = {}; local x = print; return function() return print end",
        ));
        let resolved::Statement::LocalDeclaraction(_, values) =
            &chunk.block.node.statements[1].node
        else {
            panic!("declaration")
        };
        let resolved::Name::Local(env) = environment(&values[0].node) else {
            panic!("local environment")
        };
        let resolved::Expression::FunctionDef(function) =
            &chunk.block.node.return_statement.as_ref().unwrap().values[0].node
        else {
            panic!("function")
        };
        assert_eq!(function.node.captures.len(), 1);
        let value = &function
            .node
            .block
            .node
            .return_statement
            .as_ref()
            .unwrap()
            .values[0];
        let resolved::Name::Upvalue(captured) = environment(&value.node) else {
            panic!("captured environment")
        };
        assert_eq!(captured, env);
        assert_eq!(function.node.captures[0].binding(), env);
    }
}
