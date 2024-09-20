use std::{borrow::Cow, path::PathBuf};

use crate::{
    ast::{
        BinaryOperator, Block, Expression, Field, ForCondition, FunctionCall, FunctionDef, Literal,
        Name, Number, PrefixExpression, Statement, TableConstructor, TokenTree, UnaryOperator,
        Variable,
    },
    instruction::Instruction,
    parser::Parser,
    token::Span,
    value::{LuaConst, LuaFunctionDefinition, LuaNumber},
    vm::{Chunk, ConstIndex, VM},
};

const VARARG_LOCAL_NAME: &str = "...";

#[derive(Debug, Clone)]
struct Local {
    name: String,
    depth: u8,
}

#[derive(Debug, Clone)]
struct Upvalue {
    is_local: bool,
    index: u8,
}

#[derive(Debug)]
struct BlockOptions {
    is_loop: bool,
    new_scope: bool,
}

#[derive(Debug, Default)]
struct BlockResult {
    breaks: Vec<usize>,
}

#[derive(Debug)]
struct Frame {
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    scope_depth: u8,
}

impl Frame {
    fn new() -> Self {
        Self {
            locals: vec![],
            upvalues: vec![],
            scope_depth: 0,
        }
    }

    fn resolve_local(&self, name: &str) -> Option<u8> {
        self.locals.iter().enumerate().rev().find_map(|(i, local)| {
            if local.name == name {
                Some(i as u8)
            } else {
                None
            }
        })
    }
}

#[derive(Debug)]
pub struct Compiler<'a, 'source> {
    chunk: Chunk<'source>,
    chunk_index: usize,
    vm: &'a mut VM<'source>,
    frames: Vec<Frame>,
}

impl BlockResult {
    fn new() -> Self {
        Self { breaks: vec![] }
    }

    fn extend(&mut self, other: Self) {
        self.breaks.extend(other.breaks);
    }
}

enum VariableMode {
    Read,
    Write,
}

impl<'a, 'source> Compiler<'a, 'source> {
    pub fn new(vm: &'a mut VM<'source>, filename: PathBuf, source: Cow<'source, str>) -> Self {
        Self {
            chunk: Chunk::new(filename, source),
            chunk_index: vm.get_next_chunk_index(),
            vm,
            // Start at the root of the file.
            frames: vec![Frame::new()],
        }
    }

    pub fn compile(mut self, ast: Option<TokenTree<Block>>) -> miette::Result<usize> {
        let ast = ast.unwrap_or_else(|| {
            Parser::new(self.chunk.get_filename(), self.chunk.get_source())
                .parse()
                .expect("failed to parse")
        });

        let has_return = ast.node.return_statement.is_some();

        self.compile_block(
            ast,
            BlockOptions {
                is_loop: false,
                new_scope: true,
            },
        );

        if !has_return {
            // Add implicit final return
            self.push_load_marker();
            self.push_load_nil();
            self.chunk.push_instruction(Instruction::Return, None);
        }

        Ok(self.vm.add_chunk(self.chunk))
    }

    fn push_load_marker(&mut self) {
        let marker_const_index = self.get_const_index(LuaConst::Marker);
        self.chunk.push_instruction(Instruction::LoadConst, None);
        self.chunk.push_const_index(marker_const_index);
    }

    fn push_load_nil(&mut self) {
        let nil_const_index = self.get_const_index(LuaConst::Nil);
        self.chunk.push_instruction(Instruction::LoadConst, None);
        self.chunk.push_const_index(nil_const_index);
    }

    fn get_const_index(&mut self, lua_const: LuaConst) -> ConstIndex {
        self.vm
            .lookup_const(&lua_const)
            .unwrap_or_else(|| self.vm.register_const(lua_const))
    }

    fn get_global_name_index(&mut self, name: Vec<u8>) -> ConstIndex {
        let lua_const = LuaConst::String(name);
        self.get_const_index(lua_const)
    }

    fn compile_block(&mut self, ast: TokenTree<Block>, options: BlockOptions) -> BlockResult {
        if options.new_scope {
            self.begin_scope();
        }

        let mut block_result = BlockResult { breaks: vec![] };

        for statement in ast.node.statements {
            block_result.extend(self.compile_statement(statement, &options));
        }

        if let Some(return_statement) = ast.node.return_statement {
            self.push_load_marker();
            if return_statement.is_empty() {
                self.push_load_nil();
            } else {
                for return_statement in return_statement {
                    self.compile_expression(return_statement);
                }
            }
            self.chunk.push_instruction(Instruction::Return, None);
        }

        if options.new_scope {
            self.end_scope();
        }

        block_result
    }

    fn compile_statement(
        &mut self,
        statement: TokenTree<Statement>,
        options: &BlockOptions,
    ) -> BlockResult {
        let span = statement.span;
        match statement.node {
            Statement::FunctionCall(function_call) => {
                // Marker for the return values
                self.push_load_marker();
                self.compile_function_call(function_call);
                self.chunk
                    .push_instruction(Instruction::Discard, Some(span));
                BlockResult::new()
            }
            Statement::LocalDeclaraction(names, expressions) => {
                let needs_alignment = names.len() != expressions.len()
                    || expressions.iter().any(|e| {
                        matches!(
                            e.node,
                            Expression::PrefixExpression(TokenTree {
                                // Function calls can produce multiple values
                                node: PrefixExpression::FunctionCall(_),
                                ..
                            })
                        )
                    });

                if needs_alignment {
                    let marker_const_index = self.get_const_index(LuaConst::Marker);
                    self.chunk.push_instruction(Instruction::LoadConst, None);
                    self.chunk.push_const_index(marker_const_index);
                }

                for expression in expressions {
                    self.compile_expression(expression);
                }

                if needs_alignment {
                    // Align number of values with number of variables
                    let var_count = names.len();
                    self.chunk.push_instruction(Instruction::Align, None);
                    self.chunk.push_instruction(var_count as u8, None);
                }

                for name in names {
                    // TODO: Handle the attributes.
                    self.add_local(name.node.name.node.identifier);
                }

                BlockResult::new()
            }
            Statement::Assignment { varlist, explist } => {
                let needs_alignment = varlist.len() != explist.len()
                    || explist.iter().any(|e| {
                        matches!(
                            e.node,
                            Expression::PrefixExpression(TokenTree {
                                // Function calls can produce multiple values
                                node: PrefixExpression::FunctionCall(_),
                                ..
                            })
                        )
                    });

                if needs_alignment {
                    let marker_const_index = self.get_const_index(LuaConst::Marker);
                    self.chunk.push_instruction(Instruction::LoadConst, None);
                    self.chunk.push_const_index(marker_const_index);
                }

                for expression in explist {
                    self.compile_expression(expression);
                }

                if needs_alignment {
                    // Align number of values with number of variables
                    let var_count = varlist.len();
                    self.chunk.push_instruction(Instruction::Align, None);
                    self.chunk.push_instruction(var_count as u8, None);
                }

                // NOTE: We need to iterate in reverse to handle chained assignments correctly. The
                // top of the stack is the last value, so we need to assign the last variable
                // first.
                for variable in varlist.into_iter().rev() {
                    match variable.node {
                        Variable::Name(name) => {
                            self.variable(name, VariableMode::Write);
                        }
                        Variable::Indexed(target, index) => {
                            // `SetTable` expects a stack head of:
                            // [table, index, value]
                            // We need to calculate the value first above, as it e.g. refer to the
                            // old value of `table[index]`. So we push the index, and then the
                            // table:
                            // [value, index, table]
                            // And then we swap the value and the table:
                            // SWAP 2
                            // [value, table, index]
                            self.compile_expression(*index);
                            self.compile_prefix_expression(*target);
                            self.chunk.push_instruction(Instruction::Swap, None);
                            self.chunk.push_instruction(2, None);
                            self.chunk
                                .push_instruction(Instruction::SetTable, Some(span));
                            // SetTable keeps the table on the stack, so we need to pop it
                            self.chunk.push_instruction(Instruction::Pop, None)
                        }
                        Variable::Field(target, name) => {
                            // Same story as above, but with a string key
                            let const_index = self.get_const_index(LuaConst::String(
                                name.node.identifier.into_bytes(),
                            ));
                            self.chunk
                                .push_instruction(Instruction::LoadConst, Some(span));
                            self.chunk.push_const_index(const_index);
                            self.compile_prefix_expression(*target);
                            self.chunk.push_instruction(Instruction::Swap, None);
                            self.chunk.push_instruction(2, None);
                            self.chunk
                                .push_instruction(Instruction::SetTable, Some(span));
                            self.chunk.push_instruction(Instruction::Pop, None)
                        }
                    }
                }

                BlockResult::new()
            }
            Statement::If {
                condition,
                block,
                else_ifs,
                else_block,
            } => {
                let mut block_result = BlockResult::new();
                // Push expression
                self.compile_expression(condition);
                // Jump to the end of the block if the condition is false
                self.chunk.push_instruction(Instruction::JmpFalse, None);
                let jmp_false_addr = self.chunk.push_addr_placeholder();
                // Pop the condition value
                self.chunk.push_instruction(Instruction::Pop, None);
                // Compile the block
                block_result.extend(self.compile_block(
                    block,
                    BlockOptions {
                        is_loop: options.is_loop,
                        new_scope: true,
                    },
                ));
                // Jump to the end of the if statement
                self.chunk.push_instruction(Instruction::Jmp, None);
                let mut jmp_end_addrs = vec![self.chunk.push_addr_placeholder()];
                // Right before the else/elseifs, go here if the condition was false
                self.chunk.patch_addr_placeholder(jmp_false_addr);
                for else_if in else_ifs {
                    // Pop the initial condition
                    self.chunk.push_instruction(Instruction::Pop, None);
                    // Push the new condition
                    self.compile_expression(else_if.node.condition);
                    // Jump to the end of the block if the condition is false
                    self.chunk.push_instruction(Instruction::JmpFalse, None);
                    let jmp_false_addr = self.chunk.push_addr_placeholder();
                    // Pop the condition value
                    self.chunk.push_instruction(Instruction::Pop, None);
                    // Compile the block
                    block_result.extend(self.compile_block(
                        else_if.node.block,
                        BlockOptions {
                            is_loop: options.is_loop,
                            new_scope: true,
                        },
                    ));
                    // Jump to the end of the if statement
                    self.chunk.push_instruction(Instruction::Jmp, None);
                    jmp_end_addrs.push(self.chunk.push_addr_placeholder());
                    // Right before the else/elseifs, go here if the condition was false
                    self.chunk.patch_addr_placeholder(jmp_false_addr);
                }
                let else_jump_addr = if let Some(else_block) = else_block {
                    // Pop the initial condition
                    self.chunk.push_instruction(Instruction::Pop, None);
                    block_result.extend(self.compile_block(
                        else_block,
                        BlockOptions {
                            is_loop: options.is_loop,
                            new_scope: true,
                        },
                    ));
                    self.chunk.push_instruction(Instruction::Jmp, None);
                    Some(self.chunk.push_addr_placeholder())
                } else {
                    None
                };
                self.chunk.push_instruction(Instruction::Pop, None);
                for jmp_end_addr in jmp_end_addrs {
                    self.chunk.patch_addr_placeholder(jmp_end_addr);
                }
                if let Some(else_jump_addr) = else_jump_addr {
                    self.chunk.patch_addr_placeholder(else_jump_addr);
                }
                block_result
            }
            Statement::Break => {
                // TODO: Issue an error if not in a loop

                self.chunk.push_instruction(Instruction::Jmp, Some(span));
                let break_jump = self.chunk.push_addr_placeholder();

                BlockResult {
                    breaks: vec![break_jump],
                }
            }
            Statement::Repeat { block, condition } => {
                // Jump into the block
                self.chunk.push_instruction(Instruction::Jmp, None);
                let inside_block_jump = self.chunk.push_addr_placeholder();
                // Pop the condition value
                let start_addr = self.chunk.get_current_addr();
                self.chunk.push_instruction(Instruction::Pop, None);
                self.chunk.patch_addr_placeholder(inside_block_jump);
                let block_result = self.compile_block(
                    block,
                    BlockOptions {
                        is_loop: true,
                        new_scope: true,
                    },
                );
                self.compile_expression(condition);
                self.chunk.push_instruction(Instruction::JmpFalse, None);
                self.chunk.push_addr(start_addr);
                self.chunk.push_instruction(Instruction::Pop, None);
                for break_jump in block_result.breaks {
                    self.chunk.patch_addr_placeholder(break_jump);
                }
                BlockResult::new()
            }
            Statement::While { condition, block } => {
                let start_addr = self.chunk.get_current_addr();
                self.compile_expression(condition);
                self.chunk.push_instruction(Instruction::JmpFalse, None);
                let jmp_false_addr = self.chunk.push_addr_placeholder();
                let block_result = self.compile_block(
                    block,
                    BlockOptions {
                        is_loop: true,
                        new_scope: true,
                    },
                );
                self.chunk.push_instruction(Instruction::Jmp, None);
                self.chunk.push_addr(start_addr);
                for break_jump in block_result.breaks {
                    self.chunk.patch_addr_placeholder(break_jump);
                }
                self.chunk.patch_addr_placeholder(jmp_false_addr);
                BlockResult::new()
            }
            Statement::For { condition, block } => {
                self.begin_scope();

                let (continue_addr, jmp_false_addr) = match condition.node {
                    ForCondition::NumericFor {
                        name,
                        initial,
                        limit,
                        step,
                    } => {
                        // FIXME: Coerce all to float if initial and step are floats

                        // Define the initial value and variable
                        self.compile_expression(initial);
                        let identifier_local = self.add_local(name.node.identifier);

                        // Create fake variables to hold the limit and step. Note that we use
                        // variable names that are invalid in Lua to avoid conflicts with user
                        // variables.
                        self.compile_expression(limit);
                        let limit_local = self.add_local("#limit".to_string());

                        if let Some(step) = step {
                            self.compile_expression(step);
                        } else {
                            // TODO: Decrementing loops
                            self.compile_load_literal(TokenTree {
                                node: Literal::Number(Number::Integer(1)),
                                span,
                            });
                        }
                        let step_local = self.add_local("#step".to_string());

                        // Label to jump back to the start of the loop
                        let condition_addr = self.chunk.get_current_addr();

                        // Evaluate the limit
                        self.chunk
                            .push_instruction(Instruction::GetLocal, Some(span));
                        self.chunk.push_instruction(identifier_local, None);
                        self.chunk
                            .push_instruction(Instruction::GetLocal, Some(span));
                        self.chunk.push_instruction(limit_local, None);
                        // TODO: Handle decreasing loops
                        self.chunk.push_instruction(Instruction::Le, None);
                        self.chunk.push_instruction(Instruction::JmpFalse, None);
                        let jmp_false_addr = self.chunk.push_addr_placeholder();

                        // Pop the condition value
                        self.chunk.push_instruction(Instruction::Pop, None);

                        // Jump into the loop
                        self.chunk.push_instruction(Instruction::Jmp, None);
                        let inside_block_jump = self.chunk.push_addr_placeholder();

                        // Compile the step
                        let step_addr = self.chunk.get_current_addr();
                        self.chunk
                            .push_instruction(Instruction::GetLocal, Some(span));
                        self.chunk.push_instruction(identifier_local, None);
                        self.chunk
                            .push_instruction(Instruction::GetLocal, Some(span));
                        self.chunk.push_instruction(step_local, None);
                        self.chunk.push_instruction(Instruction::Add, None);
                        self.chunk
                            .push_instruction(Instruction::SetLocal, Some(span));
                        self.chunk.push_instruction(identifier_local, None);

                        // After step, jump to the condition
                        self.chunk.push_instruction(Instruction::Jmp, None);
                        self.chunk.push_addr(condition_addr);

                        // Path the inside block jump
                        self.chunk.patch_addr_placeholder(inside_block_jump);

                        (step_addr, jmp_false_addr)
                    }
                    _ => todo!("compile_statement For {:#?}", condition),
                };

                let block_result = self.compile_block(
                    block,
                    BlockOptions {
                        is_loop: true,
                        new_scope: false,
                    },
                );

                // Jump back to the step evaluation
                self.chunk.push_instruction(Instruction::Jmp, None);
                self.chunk.push_addr(continue_addr);

                // Patch the false condition jump
                self.chunk.patch_addr_placeholder(jmp_false_addr);
                self.chunk.push_instruction(Instruction::Pop, None);

                // Patch any break jumps
                for break_jump in block_result.breaks {
                    self.chunk.patch_addr_placeholder(break_jump);
                }

                self.end_scope();
                BlockResult::new()
            }
            Statement::Block(block) => self.compile_block(
                block,
                BlockOptions {
                    is_loop: options.is_loop,
                    new_scope: true,
                },
            ),
            _ => todo!("compile_statement {:#?}", statement),
        }
    }

    fn compile_function_call(&mut self, function_call: TokenTree<FunctionCall>) {
        // Marker for the start of the function call arguments
        self.push_load_marker();

        // If this is a method call, we need to push the object as the first argument
        if function_call.node.as_method {
            self.compile_prefix_expression(*function_call.node.function);

            // It's unfortunate to duplicate the argument loop here with the else below, but this
            // avoids the borrow checker complaining, and having to `clone()` the function call
            // node above.
            for argument in function_call.node.args {
                self.compile_expression(argument);
            }

            let method_name = function_call.node.name.expect("method call without name");

            // We'll look up the target object as the first item after the marker
            // on the stack, and then retrieve the method as a field on that object.
            self.chunk
                .push_instruction(Instruction::DupFromMarker, None);
            self.chunk.push_instruction(1, None);

            let method_name_const_index =
                self.get_const_index(LuaConst::String(method_name.node.identifier.into_bytes()));
            self.chunk
                .push_instruction(Instruction::LoadConst, Some(function_call.span));
            self.chunk.push_const_index(method_name_const_index);

            self.chunk.push_instruction(Instruction::GetTable, None);
        } else {
            for argument in function_call.node.args {
                self.compile_expression(argument);
            }

            match *function_call.node.function {
                TokenTree {
                    node:
                        PrefixExpression::Variable(TokenTree {
                            node: Variable::Name(name),
                            ..
                        }),
                    ..
                } => {
                    self.variable(name, VariableMode::Read);
                }
                prefix_expression => {
                    self.compile_prefix_expression(prefix_expression);
                }
            }
        }

        self.chunk
            .push_instruction(Instruction::Call, Some(function_call.span));
    }

    fn compile_function_def(&mut self, function_def: TokenTree<FunctionDef>) {
        // We'll issue the bytecode inline here, but jump over it at runtime
        self.chunk.push_instruction(Instruction::Jmp, None);
        let jmp_over_func_addr = self.chunk.push_addr_placeholder();

        // Compile the function
        self.begin_frame();
        self.begin_scope();
        let func_addr = self.chunk.get_current_addr();

        // Calling conventions:
        // The stack will look like this when entering a function:
        // [arg1]
        // [arg2]
        // ...
        // [argN]
        // [num_args]
        // [return_addr]
        // [frame_pointer]
        //
        // The function will push the return values onto the stack, followed by the number of
        // return values. The caller will then pop the return values off the stack.

        // Store the return address
        // self.add_local("#return_addr".to_string());

        // Define the function arguments
        let parameter_count = function_def.node.parameters.len();
        for parameter in function_def.node.parameters {
            self.add_local(parameter.node.identifier);
        }

        if function_def.node.has_varargs {
            self.add_local(VARARG_LOCAL_NAME.to_string());
            self.chunk.push_instruction(Instruction::AlignVararg, None);
        } else {
            self.chunk.push_instruction(Instruction::Align, None);
        }
        self.chunk.push_instruction(parameter_count as u8, None);

        let has_return = function_def.node.block.node.return_statement.is_some();
        self.compile_block(
            function_def.node.block,
            BlockOptions {
                is_loop: false,
                // Already started the function scope above
                new_scope: false,
            },
        );

        // NOTE: We don't call `end_scope` here because we want to keep the locals around for the
        // return values. They're handled by the return statement, when the call frame is dropped.
        // self.end_scope();
        let frame = self.end_frame();

        if !has_return {
            self.push_load_marker();
            self.push_load_nil();
            self.chunk.push_instruction(Instruction::Return, None);
        }

        // Jump over the function
        self.chunk.patch_addr_placeholder(jmp_over_func_addr);

        // Save the function definition
        let const_index = self
            .vm
            .register_const(LuaConst::Function(LuaFunctionDefinition {
                name: function_def.node.name,
                chunk: self.chunk_index,
                ip: func_addr,
                upvalues: frame.upvalues.len(),
            }));
        self.chunk
            .push_instruction(Instruction::LoadClosure, Some(function_def.span));
        self.chunk.push_const_index(const_index);
        for upvalue in frame.upvalues {
            self.chunk.push_instruction(upvalue.is_local, None);
            self.chunk.push_instruction(upvalue.index, None);
        }
    }

    fn compile_expression(&mut self, expression: TokenTree<Expression>) {
        match expression.node {
            Expression::Literal(literal) => {
                self.compile_load_literal(literal);
            }
            Expression::BinaryOp { op, lhs, rhs } => match &op.node {
                BinaryOperator::And => {
                    self.compile_expression(*lhs);
                    self.chunk.push_instruction(Instruction::JmpFalse, None);
                    let jmp_false_addr = self.chunk.push_addr_placeholder();
                    self.compile_expression(*rhs);
                    self.chunk.patch_addr_placeholder(jmp_false_addr);
                }
                BinaryOperator::Or => {
                    self.compile_expression(*lhs);
                    self.chunk.push_instruction(Instruction::JmpTrue, None);
                    let jmp_true_addr = self.chunk.push_addr_placeholder();
                    self.compile_expression(*rhs);
                    self.chunk.patch_addr_placeholder(jmp_true_addr);
                }
                _ => {
                    self.compile_expression(*lhs);
                    self.compile_expression(*rhs);
                    self.compile_binary_operator(op, expression.span);
                }
            },
            Expression::PrefixExpression(function_call) => {
                self.compile_prefix_expression(function_call);
            }
            Expression::UnaryOp { op, rhs } => {
                self.compile_expression(*rhs);
                self.compile_unary_operator(op, expression.span);
            }
            Expression::FunctionDef(function_def) => {
                self.compile_function_def(function_def);
            }
            Expression::TableConstructor(table) => self.compile_table_constructor(table),
            Expression::Ellipsis => {
                let vararg_local_index = self
                    .resolve_local(VARARG_LOCAL_NAME)
                    .expect("parser ensures that vararg is in scope");
                self.chunk
                    .push_instruction(Instruction::LoadVararg, Some(expression.span));
                self.chunk.push_instruction(vararg_local_index, None);
            }
        }
    }

    fn compile_prefix_expression(&mut self, prefix_expression: TokenTree<PrefixExpression>) {
        match prefix_expression.node {
            PrefixExpression::FunctionCall(function_call) => {
                // FIXME: In some contexts we only consume the first return value
                self.compile_function_call(function_call);
            }
            PrefixExpression::Parenthesized(expression) => {
                self.compile_expression(*expression);
            }
            PrefixExpression::Variable(variable) => match variable.node {
                Variable::Name(name) => self.variable(name, VariableMode::Read),
                Variable::Indexed(prefix, index) => {
                    self.compile_prefix_expression(*prefix);
                    self.compile_expression(*index);
                    self.chunk
                        .push_instruction(Instruction::GetTable, Some(prefix_expression.span));
                }
                Variable::Field(prefix, name) => {
                    self.compile_prefix_expression(*prefix);
                    let const_index =
                        self.get_const_index(LuaConst::String(name.node.identifier.into_bytes()));
                    self.chunk
                        .push_instruction(Instruction::LoadConst, Some(prefix_expression.span));
                    self.chunk.push_const_index(const_index);
                    self.chunk
                        .push_instruction(Instruction::GetTable, Some(prefix_expression.span));
                }
            },
        }
    }

    fn compile_table_constructor(&mut self, table: TokenTree<TableConstructor>) {
        self.chunk
            .push_instruction(Instruction::NewTable, Some(table.span));

        let mut index = 1;
        for field in table.node.fields {
            match field.node {
                Field::Named(name, value) => {
                    let const_index =
                        self.get_const_index(LuaConst::String(name.node.identifier.into_bytes()));
                    self.chunk
                        .push_instruction(Instruction::LoadConst, Some(field.span));
                    self.chunk.push_const_index(const_index);
                    self.compile_expression(value);
                }
                Field::Indexed(index_expression, value) => {
                    self.compile_expression(index_expression);
                    self.compile_expression(value);
                }
                Field::Value(value) => {
                    let index_const =
                        self.get_const_index(LuaConst::Number(LuaNumber::Integer(index)));
                    self.chunk
                        .push_instruction(Instruction::LoadConst, Some(field.span));
                    self.chunk.push_const_index(index_const);
                    self.compile_expression(value);

                    index += 1;
                }
            }

            self.chunk
                .push_instruction(Instruction::SetTable, Some(field.span));
        }
    }

    fn compile_load_literal(&mut self, literal: TokenTree<Literal>) {
        let const_index = match literal.node {
            Literal::Nil => self.get_const_index(LuaConst::Nil),
            Literal::Boolean(b) => self.get_const_index(LuaConst::Boolean(b)),
            Literal::Number(Number::Float(f)) => {
                self.get_const_index(LuaConst::Number(LuaNumber::Float(f)))
            }
            Literal::Number(Number::Integer(i)) => {
                self.get_const_index(LuaConst::Number(LuaNumber::Integer(i)))
            }
            Literal::String(s) => self.get_const_index(LuaConst::String(s)),
        };

        self.chunk
            .push_instruction(Instruction::LoadConst, Some(literal.span));
        self.chunk.push_const_index(const_index);
    }

    fn compile_binary_operator(&mut self, op: TokenTree<BinaryOperator>, span: Span) {
        match op.node {
            // Arithmetic
            BinaryOperator::Add => self.chunk.push_instruction(Instruction::Add, Some(span)),
            BinaryOperator::Sub => self.chunk.push_instruction(Instruction::Sub, Some(span)),
            BinaryOperator::Mul => self.chunk.push_instruction(Instruction::Mul, Some(span)),
            BinaryOperator::Div => self.chunk.push_instruction(Instruction::Div, Some(span)),
            BinaryOperator::Mod => self.chunk.push_instruction(Instruction::Mod, Some(span)),
            BinaryOperator::Pow => self.chunk.push_instruction(Instruction::Pow, Some(span)),
            BinaryOperator::FloorDiv => self.chunk.push_instruction(Instruction::IDiv, Some(span)),
            BinaryOperator::BitwiseAnd => {
                self.chunk.push_instruction(Instruction::Band, Some(span))
            }
            BinaryOperator::BitwiseOr => self.chunk.push_instruction(Instruction::Bor, Some(span)),
            BinaryOperator::BitwiseXor => {
                self.chunk.push_instruction(Instruction::Bxor, Some(span))
            }
            BinaryOperator::ShiftLeft => self.chunk.push_instruction(Instruction::Shl, Some(span)),
            BinaryOperator::ShiftRight => self.chunk.push_instruction(Instruction::Shr, Some(span)),

            // Comparison
            BinaryOperator::Equal => self.chunk.push_instruction(Instruction::Eq, Some(span)),
            BinaryOperator::NotEqual => self.chunk.push_instruction(Instruction::Ne, Some(span)),
            BinaryOperator::LessThan => self.chunk.push_instruction(Instruction::Lt, Some(span)),
            BinaryOperator::LessThanOrEqual => {
                self.chunk.push_instruction(Instruction::Le, Some(span))
            }
            BinaryOperator::GreaterThan => self.chunk.push_instruction(Instruction::Gt, Some(span)),
            BinaryOperator::GreaterThanOrEqual => {
                self.chunk.push_instruction(Instruction::Ge, Some(span))
            }

            // Strings
            BinaryOperator::Concat => self.chunk.push_instruction(Instruction::Concat, Some(span)),

            // Logical
            BinaryOperator::And | BinaryOperator::Or => {
                unreachable!(
                    "short-circuiting logical operators should be handled in compile_expression"
                );
            }
        }
    }

    fn compile_unary_operator(&mut self, op: TokenTree<UnaryOperator>, span: Span) {
        match op.node {
            UnaryOperator::Neg => self.chunk.push_instruction(Instruction::Neg, Some(span)),
            UnaryOperator::Not => self.chunk.push_instruction(Instruction::Not, Some(span)),
            UnaryOperator::Length => self.chunk.push_instruction(Instruction::Len, Some(span)),
            UnaryOperator::BitwiseNot => self.chunk.push_instruction(Instruction::BNot, Some(span)),
        }
    }

    fn variable<T>(&mut self, name: TokenTree<Name<T>>, mode: VariableMode) {
        if let Some(local) = self.resolve_local(&name.node.identifier) {
            self.chunk.push_instruction(
                match mode {
                    VariableMode::Read => Instruction::GetLocal,
                    VariableMode::Write => Instruction::SetLocal,
                },
                Some(name.span),
            );
            self.chunk.push_instruction(local, None);
        } else if let Some(upvalue) = self.resolve_upvalue(&name.node.identifier) {
            self.chunk.push_instruction(
                match mode {
                    VariableMode::Read => Instruction::GetUpval,
                    VariableMode::Write => Instruction::SetUpval,
                },
                Some(name.span),
            );
            self.chunk.push_instruction(upvalue, None);
        } else {
            self.chunk.push_instruction(
                match mode {
                    VariableMode::Read => Instruction::GetGlobal,
                    VariableMode::Write => Instruction::SetGlobal,
                },
                Some(name.span),
            );
            let const_index = self.get_global_name_index(name.node.identifier.into_bytes());
            self.chunk.push_const_index(const_index);
        }
    }

    fn begin_frame(&mut self) {
        self.frames.push(Frame::new());
    }

    fn end_frame(&mut self) -> Frame {
        self.frames.pop().expect("frame exists")
    }

    fn begin_scope(&mut self) {
        let current_frame = self.frames.last_mut().unwrap();
        current_frame.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        let current_frame = self.frames.last_mut().unwrap();
        current_frame.scope_depth -= 1;

        while !current_frame.locals.is_empty()
            && current_frame
                .locals
                .last()
                .as_ref()
                .map_or(false, |local| local.depth > current_frame.scope_depth)
        {
            current_frame.locals.pop();
            self.chunk.push_instruction(Instruction::Pop, None);
        }
    }

    fn add_local(&mut self, name: String) -> u8 {
        let current_frame = self.frames.last_mut().unwrap();
        let local = Local {
            name: name.clone(),
            depth: current_frame.scope_depth,
        };

        let index = current_frame.locals.len() as u8;
        current_frame.locals.push(local);

        index
    }

    fn resolve_local(&mut self, name: &str) -> Option<u8> {
        self.frames
            .last()
            .expect("there should always be at least one frame")
            .resolve_local(name)
    }

    fn resolve_upvalue(&mut self, name: &str) -> Option<u8> {
        self.resolve_upvalue_inner(name, self.frames.len() - 1)
    }

    fn resolve_upvalue_inner(&mut self, name: &str, frame_index: usize) -> Option<u8> {
        if frame_index <= 1 {
            return None;
        }

        match self.frames[frame_index - 1].resolve_local(name) {
            Some(local) => Some(self.add_upvalue(frame_index, local, true)),
            None => self
                .resolve_upvalue_inner(name, frame_index - 1)
                .map(|upvalue_index| self.add_upvalue(frame_index, upvalue_index, false)),
        }
    }

    fn add_upvalue(&mut self, frame_index: usize, index: u8, is_local: bool) -> u8 {
        let frame = self.frames.get_mut(frame_index).expect("frame exists");
        if let Some(i) = frame
            .upvalues
            .iter()
            .position(|upvalue| upvalue.is_local == is_local && upvalue.index == index)
        {
            return i as u8;
        }

        frame.upvalues.push(Upvalue { is_local, index });
        frame.upvalues.len() as u8 - 1
    }
}
