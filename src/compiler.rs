use std::path::Path;

use crate::{
    ast::{
        BinaryOperator, Block, Expression, ForCondition, FunctionCall, FunctionDef, Literal, Name,
        Number, PrefixExpression, Statement, TokenTree, UnaryOperator, Variable,
    },
    instruction::Instruction,
    token::Span,
    value::{LuaConst, LuaNumber},
    vm::VM,
};

#[derive(Debug, Clone)]
pub struct Local {
    name: String,
    depth: u8,
}

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
    scope_depth: u8,
}

impl Frame {
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
pub struct Compiler<'path, 'source> {
    vm: VM<'path, 'source>,
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

impl<'path, 'source> Compiler<'path, 'source> {
    pub fn new(filename: &'path Path, source: &'source str) -> Self {
        Self {
            vm: VM::new(filename, source),
            // Start at the root of the file.
            frames: vec![Frame {
                locals: vec![],
                scope_depth: 0,
            }],
        }
    }

    pub fn compile(mut self, ast: TokenTree<Block>) -> VM<'path, 'source> {
        self.compile_block(
            ast,
            BlockOptions {
                is_loop: false,
                new_scope: true,
            },
        );
        self.vm.push_instruction(Instruction::Halt, None);

        self.vm
    }

    fn get_global_name_index(&mut self, name: Vec<u8>) -> u8 {
        let lua_const = LuaConst::String(name);
        self.vm
            .lookup_const(&lua_const)
            .unwrap_or_else(|| self.vm.register_const(lua_const))
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
            todo!("compile_block return_statement {:#?}", return_statement);
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
                self.compile_function_call(function_call);
                BlockResult::new()
            }
            Statement::LocalDeclaraction(names, expressions) => {
                let expression_count = expressions.len();
                for expression in expressions {
                    self.compile_expression(expression);
                }

                // Ensure we have enough expressions to match the names
                for _ in expression_count..names.len() {
                    self.compile_load_literal(TokenTree {
                        node: Literal::Nil,
                        span,
                    });
                }

                for name in names {
                    // TODO: Handle the attributes.
                    self.add_local(name.node.name.node.identifier);
                }

                BlockResult::new()
            }
            Statement::Assignment { varlist, explist } => {
                for expression in explist {
                    self.compile_expression(expression);
                }

                for variable in varlist {
                    match variable.node {
                        Variable::Name(name) => {
                            self.assign_variable(name);
                        }
                        _ => todo!("compile_statement Assignment {:#?}", variable),
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
                self.vm.push_instruction(Instruction::JmpFalse, None);
                let jmp_false_addr = self.vm.push_addr_placeholder();
                // Pop the condition value
                self.vm.push_instruction(Instruction::Pop, None);
                // Compile the block
                block_result.extend(self.compile_block(
                    block,
                    BlockOptions {
                        is_loop: options.is_loop,
                        new_scope: true,
                    },
                ));
                // Jump to the end of the if statement
                self.vm.push_instruction(Instruction::Jmp, None);
                let mut jmp_end_addrs = vec![self.vm.push_addr_placeholder()];
                // Right before the else/elseifs, go here if the condition was false
                self.vm.patch_addr_placeholder(jmp_false_addr);
                for else_if in else_ifs {
                    // Pop the initial condition
                    self.vm.push_instruction(Instruction::Pop, None);
                    // Push the new condition
                    self.compile_expression(else_if.node.condition);
                    // Jump to the end of the block if the condition is false
                    self.vm.push_instruction(Instruction::JmpFalse, None);
                    let jmp_false_addr = self.vm.push_addr_placeholder();
                    // Pop the condition value
                    self.vm.push_instruction(Instruction::Pop, None);
                    // Compile the block
                    block_result.extend(self.compile_block(
                        else_if.node.block,
                        BlockOptions {
                            is_loop: options.is_loop,
                            new_scope: true,
                        },
                    ));
                    // Jump to the end of the if statement
                    self.vm.push_instruction(Instruction::Jmp, None);
                    jmp_end_addrs.push(self.vm.push_addr_placeholder());
                    // Right before the else/elseifs, go here if the condition was false
                    self.vm.patch_addr_placeholder(jmp_false_addr);
                }
                let else_jump_addr = if let Some(else_block) = else_block {
                    // Pop the initial condition
                    self.vm.push_instruction(Instruction::Pop, None);
                    block_result.extend(self.compile_block(
                        else_block,
                        BlockOptions {
                            is_loop: options.is_loop,
                            new_scope: true,
                        },
                    ));
                    self.vm.push_instruction(Instruction::Jmp, None);
                    Some(self.vm.push_addr_placeholder())
                } else {
                    None
                };
                self.vm.push_instruction(Instruction::Pop, None);
                for jmp_end_addr in jmp_end_addrs {
                    self.vm.patch_addr_placeholder(jmp_end_addr);
                }
                if let Some(else_jump_addr) = else_jump_addr {
                    self.vm.patch_addr_placeholder(else_jump_addr);
                }
                block_result
            }
            Statement::Break => {
                // TODO: Issue an error if not in a loop

                self.vm.push_instruction(Instruction::Jmp, Some(span));
                let break_jump = self.vm.push_addr_placeholder();

                BlockResult {
                    breaks: vec![break_jump],
                }
            }
            Statement::Repeat { block, condition } => {
                // Jump into the block
                self.vm.push_instruction(Instruction::Jmp, None);
                let inside_block_jump = self.vm.push_addr_placeholder();
                // Pop the condition value
                let start_addr = self.vm.get_current_addr();
                self.vm.push_instruction(Instruction::Pop, None);
                self.vm.patch_addr_placeholder(inside_block_jump);
                let block_result = self.compile_block(
                    block,
                    BlockOptions {
                        is_loop: true,
                        new_scope: true,
                    },
                );
                self.compile_expression(condition);
                self.vm.push_instruction(Instruction::JmpFalse, None);
                self.vm.push_addr(start_addr);
                self.vm.push_instruction(Instruction::Pop, None);
                for break_jump in block_result.breaks {
                    self.vm.patch_addr_placeholder(break_jump);
                }
                BlockResult::new()
            }
            Statement::While { condition, block } => {
                let start_addr = self.vm.get_current_addr();
                self.compile_expression(condition);
                self.vm.push_instruction(Instruction::JmpFalse, None);
                let jmp_false_addr = self.vm.push_addr_placeholder();
                let block_result = self.compile_block(
                    block,
                    BlockOptions {
                        is_loop: true,
                        new_scope: true,
                    },
                );
                self.vm.push_instruction(Instruction::Jmp, None);
                self.vm.push_addr(start_addr);
                for break_jump in block_result.breaks {
                    self.vm.patch_addr_placeholder(break_jump);
                }
                self.vm.patch_addr_placeholder(jmp_false_addr);
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
                        let condition_addr = self.vm.get_current_addr();

                        // Evaluate the limit
                        self.vm.push_instruction(Instruction::GetLocal, Some(span));
                        self.vm.push_instruction(identifier_local, None);
                        self.vm.push_instruction(Instruction::GetLocal, Some(span));
                        self.vm.push_instruction(limit_local, None);
                        // TODO: Handle decreasing loops
                        self.vm.push_instruction(Instruction::Le, None);
                        self.vm.push_instruction(Instruction::JmpFalse, None);
                        let jmp_false_addr = self.vm.push_addr_placeholder();

                        // Pop the condition value
                        self.vm.push_instruction(Instruction::Pop, None);

                        // Jump into the loop
                        self.vm.push_instruction(Instruction::Jmp, None);
                        let inside_block_jump = self.vm.push_addr_placeholder();

                        // Compile the step
                        let step_addr = self.vm.get_current_addr();
                        self.vm.push_instruction(Instruction::GetLocal, Some(span));
                        self.vm.push_instruction(identifier_local, None);
                        self.vm.push_instruction(Instruction::GetLocal, Some(span));
                        self.vm.push_instruction(step_local, None);
                        self.vm.push_instruction(Instruction::Add, None);
                        self.vm.push_instruction(Instruction::SetLocal, Some(span));
                        self.vm.push_instruction(identifier_local, None);

                        // After step, jump to the condition
                        self.vm.push_instruction(Instruction::Jmp, None);
                        self.vm.push_addr(condition_addr);

                        // Path the inside block jump
                        self.vm.patch_addr_placeholder(inside_block_jump);

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
                self.vm.push_instruction(Instruction::Jmp, None);
                self.vm.push_addr(continue_addr);

                // Patch the false condition jump
                self.vm.patch_addr_placeholder(jmp_false_addr);
                self.vm.push_instruction(Instruction::Pop, None);

                // Patch any break jumps
                for break_jump in block_result.breaks {
                    self.vm.patch_addr_placeholder(break_jump);
                }

                self.end_scope();
                BlockResult::new()
            }
            _ => todo!("compile_statement {:#?}", statement),
        }
    }

    fn compile_function_call(&mut self, function_call: TokenTree<FunctionCall>) {
        let arg_count = function_call.node.args.len();
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
                if name.node.identifier == "print" {
                    // TODO: Handle multiple arguments
                    // TODO: Remove this hack and issue a regular call instruction
                    self.vm
                        .push_instruction(Instruction::Print, Some(function_call.span));
                } else {
                    self.load_variable(name);
                    self.vm
                        .push_instruction(Instruction::Call, Some(function_call.span));
                    self.vm.push_instruction(arg_count as u8, None);
                }
            }

            _ => todo!("compile_function_call for other than `print`"),
        }
    }

    // FIXME: `name` is currently always `None`
    fn compile_function_def(&mut self, function_def: TokenTree<FunctionDef>, name: Option<String>) {
        // We'll issue the bytecode inline here, but jump over it at runtime
        self.vm.push_instruction(Instruction::Jmp, None);
        let jmp_over_func_addr = self.vm.push_addr_placeholder();

        // Compile the function
        self.begin_frame();
        self.begin_scope();
        let func_addr = self.vm.get_current_addr();

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
        //

        let has_return = function_def.node.block.node.return_statement.is_some();
        self.compile_block(
            function_def.node.block,
            BlockOptions {
                is_loop: false,
                // Already started the function scope above
                new_scope: false,
            },
        );

        // TODO: How to handle return values?
        self.end_scope();
        self.end_frame();

        if !has_return {
            // TODO: Implicit return nil
            // let nil_const_index = self.vm.register_const(LuaConst::Nil);
            // self.vm.push_instruction(Instruction::LoadConst, None);
            // self.vm.push_instruction(nil_const_index, None);
            self.vm.push_instruction(Instruction::Return, None);
            // self.vm.push_instruction(1, None);
        }

        // Jump over the function
        self.vm.patch_addr_placeholder(jmp_over_func_addr);

        // Save the function definition
        let const_index = self.vm.register_const(LuaConst::Function(name, func_addr));
        self.vm
            .push_instruction(Instruction::LoadConst, Some(function_def.span));
        self.vm.push_instruction(const_index, None);
    }

    fn compile_expression(&mut self, expression: TokenTree<Expression>) {
        match expression.node {
            Expression::Literal(literal) => {
                self.compile_load_literal(literal);
            }
            Expression::BinaryOp { op, lhs, rhs } => match &op.node {
                BinaryOperator::And => {
                    self.compile_expression(*lhs);
                    self.vm.push_instruction(Instruction::JmpFalse, None);
                    let jmp_false_addr = self.vm.push_addr_placeholder();
                    self.compile_expression(*rhs);
                    self.vm.patch_addr_placeholder(jmp_false_addr);
                }
                BinaryOperator::Or => {
                    self.compile_expression(*lhs);
                    self.vm.push_instruction(Instruction::JmpTrue, None);
                    let jmp_true_addr = self.vm.push_addr_placeholder();
                    self.compile_expression(*rhs);
                    self.vm.patch_addr_placeholder(jmp_true_addr);
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
                self.compile_function_def(function_def, None);
            }
            _ => todo!("compile_expression {:?}", expression),
        }
    }

    fn compile_prefix_expression(&mut self, prefix_expression: TokenTree<PrefixExpression>) {
        match prefix_expression.node {
            PrefixExpression::FunctionCall(function_call) => {
                self.compile_function_call(function_call);
            }
            PrefixExpression::Parenthesized(expression) => {
                self.compile_expression(*expression);
            }
            PrefixExpression::Variable(variable) => match variable.node {
                Variable::Name(name) => self.load_variable(name),
                _ => todo!("compile_prefix_expression Variable {:#?}", variable),
            },
        }
    }

    fn compile_load_literal(&mut self, literal: TokenTree<Literal>) {
        let const_index = match literal.node {
            Literal::Nil => self.vm.register_const(LuaConst::Nil),
            Literal::Boolean(b) => self.vm.register_const(LuaConst::Boolean(b)),
            Literal::Number(Number::Float(f)) => self
                .vm
                .register_const(LuaConst::Number(LuaNumber::Float(f))),
            Literal::Number(Number::Integer(i)) => self
                .vm
                .register_const(LuaConst::Number(LuaNumber::Integer(i))),
            Literal::String(s) => self.vm.register_const(LuaConst::String(s)),
        };

        self.vm
            .push_instruction(Instruction::LoadConst, Some(literal.span));
        self.vm.push_instruction(const_index, None);
    }

    fn compile_binary_operator(&mut self, op: TokenTree<BinaryOperator>, span: Span) {
        match op.node {
            // Arithmetic
            BinaryOperator::Add => self.vm.push_instruction(Instruction::Add, Some(span)),
            BinaryOperator::Sub => self.vm.push_instruction(Instruction::Sub, Some(span)),
            BinaryOperator::Mul => self.vm.push_instruction(Instruction::Mul, Some(span)),
            BinaryOperator::Div => self.vm.push_instruction(Instruction::Div, Some(span)),
            BinaryOperator::Mod => self.vm.push_instruction(Instruction::Mod, Some(span)),
            BinaryOperator::Pow => self.vm.push_instruction(Instruction::Pow, Some(span)),
            BinaryOperator::FloorDiv => self.vm.push_instruction(Instruction::IDiv, Some(span)),
            BinaryOperator::BitwiseAnd => self.vm.push_instruction(Instruction::Band, Some(span)),
            BinaryOperator::BitwiseOr => self.vm.push_instruction(Instruction::Bor, Some(span)),
            BinaryOperator::BitwiseXor => self.vm.push_instruction(Instruction::Bxor, Some(span)),
            BinaryOperator::ShiftLeft => self.vm.push_instruction(Instruction::Shl, Some(span)),
            BinaryOperator::ShiftRight => self.vm.push_instruction(Instruction::Shr, Some(span)),

            // Comparison
            BinaryOperator::Equal => self.vm.push_instruction(Instruction::Eq, Some(span)),
            BinaryOperator::NotEqual => self.vm.push_instruction(Instruction::Ne, Some(span)),
            BinaryOperator::LessThan => self.vm.push_instruction(Instruction::Lt, Some(span)),
            BinaryOperator::LessThanOrEqual => {
                self.vm.push_instruction(Instruction::Le, Some(span))
            }
            BinaryOperator::GreaterThan => self.vm.push_instruction(Instruction::Gt, Some(span)),
            BinaryOperator::GreaterThanOrEqual => {
                self.vm.push_instruction(Instruction::Ge, Some(span))
            }

            // Strings
            BinaryOperator::Concat => self.vm.push_instruction(Instruction::Concat, Some(span)),

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
            UnaryOperator::Neg => self.vm.push_instruction(Instruction::Neg, Some(span)),
            UnaryOperator::Not => self.vm.push_instruction(Instruction::Not, Some(span)),
            UnaryOperator::Length => self.vm.push_instruction(Instruction::Len, Some(span)),
            UnaryOperator::BitwiseNot => self.vm.push_instruction(Instruction::BNot, Some(span)),
        }
    }

    fn assign_variable<T>(&mut self, name: TokenTree<Name<T>>) {
        if let Some(local) = self.resolve_local(&name.node.identifier) {
            self.vm
                .push_instruction(Instruction::SetLocal, Some(name.span));
            self.vm.push_instruction(local, None);
        } else {
            let const_index = self.get_global_name_index(name.node.identifier.into_bytes());
            self.vm
                .push_instruction(Instruction::SetGlobal, Some(name.span));
            self.vm.push_instruction(const_index, None);
        }
    }

    fn load_variable<T>(&mut self, name: TokenTree<Name<T>>) {
        if let Some(local) = self.resolve_local(&name.node.identifier) {
            self.vm
                .push_instruction(Instruction::GetLocal, Some(name.span));
            self.vm.push_instruction(local, None);
        } else {
            let const_index = self.get_global_name_index(name.node.identifier.into_bytes());
            self.vm
                .push_instruction(Instruction::GetGlobal, Some(name.span));
            self.vm.push_instruction(const_index, None);
        }
    }

    pub fn begin_frame(&mut self) {
        self.frames.push(Frame {
            locals: vec![],
            scope_depth: 0,
        });
    }

    pub fn end_frame(&mut self) {
        self.frames.pop();
    }

    pub fn begin_scope(&mut self) {
        let current_frame = self.frames.last_mut().unwrap();
        current_frame.scope_depth += 1;
    }

    pub fn end_scope(&mut self) {
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
            self.vm.push_instruction(Instruction::Pop, None);
        }
    }

    pub fn add_local(&mut self, name: String) -> u8 {
        let current_frame = self.frames.last_mut().unwrap();
        let local = Local {
            name: name.clone(),
            depth: current_frame.scope_depth,
        };

        let index = current_frame.locals.len() as u8;
        current_frame.locals.push(local);

        index
    }

    pub fn resolve_local(&mut self, name: &str) -> Option<u8> {
        self.frames.last().unwrap().resolve_local(name)
    }
}
