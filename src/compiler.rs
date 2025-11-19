use std::{
    borrow::Cow,
    collections::HashMap,
    path::PathBuf,
    sync::{Arc, RwLock},
};

use crate::{
    ast::{
        BinaryOperator, Block, Expression, Field, ForCondition, FunctionCall, FunctionDef, Literal,
        LocalAttribute, Name, Number, PrefixExpression, Statement, TableConstructor, TokenTree,
        UnaryOperator, Variable,
    },
    error::{lua_error, RuntimeError},
    instruction::Instruction,
    parser::Parser,
    token::Span,
    value::{
        LuaConst, LuaFunctionDefinition, LuaNumber, LuaObject, LuaTable, LuaVariableAttribute,
    },
    vm::{Chunk, ConstIndex, JumpAddr, VM},
};

const VARARG_LOCAL_NAME: &[u8] = b"...";

#[derive(Debug)]
struct Goto {
    addr: JumpAddr,
    span: Span,
    locals: usize,
    depth: u8,
    scope_id: usize,
    to_end_of_scope: bool,
}

#[derive(Debug)]
struct Label {
    addr: JumpAddr,
    locals: usize,
    depth: u8,
}

#[derive(Debug, Clone)]
struct Local {
    name: Vec<u8>,
    is_parameter: bool,
    depth: u8,
    span: Option<Span>,
}

#[derive(Debug, Clone)]
struct Upvalue {
    is_local: bool,
    index: u8,
}

#[derive(Debug)]
struct BlockOptions {
    is_loop: bool,
    loop_scope_depth: u8,
    new_scope: bool,
}

#[derive(Debug, Default)]
struct BreakJump {
    addr: JumpAddr,
    locals: usize,
}

#[derive(Debug, Default)]
struct BlockResult {
    breaks: Vec<BreakJump>,
    any_new_locals: Option<NewLocalsAfterGoto>,
}

#[derive(Debug)]
struct NewLocalsAfterGoto {
    goto: (Vec<u8>, Span),
    local: (Vec<u8>, Option<Span>),
}

#[derive(Debug)]
struct Frame {
    ip: JumpAddr,
    name: Option<Vec<u8>>,
    method_name: Option<Vec<u8>>,
    locals: Vec<Local>,
    gotos: HashMap<Vec<u8>, Vec<Goto>>,
    labels: HashMap<Vec<u8>, Label>,
    upvalues: Vec<Upvalue>,
    scope_depth: u8,
    scope_id_counter: usize,
    scope_ids: Vec<usize>,
}

impl Frame {
    fn new(ip: JumpAddr, name: Option<Vec<u8>>, method_name: Option<Vec<u8>>) -> Self {
        Self {
            ip,
            name,
            method_name,
            locals: vec![],
            gotos: HashMap::new(),
            labels: HashMap::new(),
            upvalues: vec![],
            scope_depth: 0,
            scope_id_counter: 1,
            scope_ids: vec![0],
        }
    }

    fn scope_id(&self) -> usize {
        *self.scope_ids.last().expect("scope_ids is not empty")
    }

    fn resolve_local(&self, name: &[u8]) -> Option<u8> {
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
        Self {
            breaks: vec![],
            any_new_locals: None,
        }
    }

    fn extend(&mut self, other: Self) {
        self.breaks.extend(other.breaks);
        if self.any_new_locals.is_none() {
            self.any_new_locals = other.any_new_locals;
        }
    }

    fn with_breaks(mut self, breaks: Vec<BreakJump>) -> Self {
        self.breaks = breaks;
        self
    }

    fn with_new_locals(mut self, any_new_locals: Option<NewLocalsAfterGoto>) -> Self {
        self.any_new_locals = any_new_locals;
        self
    }

    fn assert_no_new_locals(&self) -> crate::Result<()> {
        if let Some(new_local) = &self.any_new_locals {
            let mut labels = vec![new_local.goto.1.labeled("this goto statement")];
            if let Some(span) = new_local.local.1 {
                labels.push(span.labeled("this local variable definition"));
            }
            return Err(lua_error!(
                labels = labels,
                "<goto {}> jumps into the scope of local '{}'",
                String::from_utf8_lossy(&new_local.goto.0),
                String::from_utf8_lossy(&new_local.local.0)
            ));
        }
        Ok(())
    }
}

enum VariableMode {
    Read,
    Write,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ExpressionResult {
    Single,
    Multiple,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ExpressionListLength {
    Constant,
    /// Expression list ends in an expression that _may_ result in multiple values.
    MultRes,
}

impl<'a, 'source> Compiler<'a, 'source> {
    pub fn new(
        vm: &'a mut VM<'source>,
        global_env: Option<Arc<RwLock<LuaObject>>>,
        filename: Option<PathBuf>,
        chunk_name: String,
        source: Cow<'source, [u8]>,
    ) -> Self {
        Self {
            chunk: Chunk::new(
                global_env.unwrap_or_else(|| vm.get_global_env()),
                filename,
                chunk_name,
                source,
            ),
            chunk_index: vm.get_next_chunk_index(),
            vm,
            // Start at the root of the file.
            frames: vec![Frame::new(0, None, None)],
        }
    }

    pub fn compile(mut self, ast: Option<TokenTree<Block>>) -> crate::Result<usize> {
        let ast = match ast {
            Some(ast) => Ok(ast),
            None => Parser::new(self.chunk.get_filename(), self.chunk.get_source()).parse(),
        }?;

        let has_return = ast.node.return_statement.is_some();

        self.compile_block(
            ast,
            true,
            BlockOptions {
                is_loop: false,
                loop_scope_depth: 0,
                new_scope: true,
            },
        )?;

        if !has_return {
            // Add implicit final return
            self.chunk.push_instruction(Instruction::Return0, None);
        }

        debug_assert!(
            self.frames.len() == 1,
            "should have only the root frame left"
        );
        debug_assert!(
            self.frames[0].locals.is_empty(),
            "all locals should be popped"
        );

        // Get rid of the root frame, to resolve any pending gotos and such.
        self.end_frame()?;

        Ok(self.vm.add_chunk(self.chunk))
    }

    fn push_load_marker(&mut self) {
        let marker_const_index = self.get_const_index(LuaConst::Marker);
        self.chunk.push_instruction(Instruction::LoadConst, None);
        self.chunk.push_const_index(marker_const_index);
    }

    fn push_load_nil(&mut self, span: Option<Span>) {
        let nil_const_index = self.get_const_index(LuaConst::Nil);
        self.chunk.push_instruction(Instruction::LoadConst, span);
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

    fn compile_block(
        &mut self,
        ast: TokenTree<Block>,
        block_is_final_statement: bool,
        options: BlockOptions,
    ) -> crate::Result<BlockResult> {
        if options.new_scope {
            self.begin_scope();
        }

        let has_return = ast.node.return_statement.is_some();
        let num_statements = ast.node.statements.len();

        let mut block_result = BlockResult::new();

        for (i, statement) in ast.node.statements.into_iter().enumerate() {
            if !matches!(&statement.node, Statement::Label(_)) {
                block_result.assert_no_new_locals()?;
            }

            let is_final_statement =
                block_is_final_statement && i == num_statements - 1 && !has_return;
            block_result.extend(self.compile_statement(statement, &options, is_final_statement)?);
        }

        if let Some(return_statement) = ast.node.return_statement {
            let mut handled_as_tail_call = false;
            if return_statement.len() == 1 {
                if let Expression::PrefixExpression(TokenTree {
                    node: PrefixExpression::FunctionCall(function_call),
                    ..
                }) = &return_statement[0].node
                {
                    if self.compile_tail_call(function_call.clone())? {
                        handled_as_tail_call = true;
                    }
                }
            }

            if !handled_as_tail_call {
                self.compile_return_statement(return_statement)?;
            }
        }

        if options.new_scope {
            self.end_scope()?;
            Ok(block_result.with_new_locals(None))
        } else {
            Ok(block_result)
        }
    }

    fn compile_statement(
        &mut self,
        statement: TokenTree<Statement>,
        options: &BlockOptions,
        is_final_statement: bool,
    ) -> crate::Result<BlockResult> {
        let span = statement.span;
        Ok(match statement.node {
            Statement::FunctionCall(function_call) => {
                if is_final_statement && self.compile_tail_call(function_call.clone())? {
                    return Ok(BlockResult::new());
                }

                // Marker for the return values
                self.push_load_marker();
                self.compile_function_call(function_call, ExpressionResult::Multiple)?;
                self.chunk.push_instruction(Instruction::Discard, None);
                BlockResult::new()
            }
            Statement::LocalDeclaraction(names, expressions) => {
                let empty_expressions = expressions.is_empty();
                let needs_alignment = !empty_expressions
                    && (names.len() != expressions.len()
                        || matches!(
                            expressions.last().unwrap().node,
                            Expression::PrefixExpression(TokenTree {
                                // Function calls can produce multiple values
                                node: PrefixExpression::FunctionCall(_),
                                ..
                            }) | Expression::Ellipsis
                        ));

                if needs_alignment {
                    self.push_load_marker();
                }

                self.compile_expression_list(expressions)?;

                if needs_alignment {
                    // Align number of values with number of variables
                    let var_count = names.len();
                    self.chunk.push_instruction(Instruction::Align, None);
                    self.chunk.push_instruction(var_count as u8, None);
                }

                for name in names {
                    if empty_expressions {
                        self.push_load_nil(Some(name.span));
                    }
                    let local_index = self.add_local(name.node.name.node.0, Some(name.span));
                    match name.node.attribute {
                        Some(TokenTree {
                            node: LocalAttribute::Const,
                            ..
                        }) => {
                            self.chunk
                                .push_instruction(Instruction::SetLocalAttr, Some(span));
                            self.chunk.push_instruction(local_index, None);
                            self.chunk
                                .push_instruction(LuaVariableAttribute::Constant, None);
                        }
                        Some(TokenTree {
                            node: LocalAttribute::Close,
                            ..
                        }) => {
                            self.chunk
                                .push_instruction(Instruction::SetLocalAttr, Some(span));
                            self.chunk.push_instruction(local_index, None);
                            self.chunk
                                .push_instruction(LuaVariableAttribute::ToBeClosed, None);
                        }
                        None => {}
                    }
                }

                BlockResult::new()
            }
            Statement::LocalFunctionDeclaration(name, function_def) => {
                // The statement `local function f () body end` translates to `local f; f =
                // function () body end`, not to `local f = function () body end` (This only makes
                // a difference when the body of the function contains references to f.)
                self.push_load_nil(Some(name.span));
                let local_index = self.add_local(name.node.0, Some(name.span));

                self.compile_function_def(function_def)?;
                self.chunk
                    .push_instruction(Instruction::SetLocal, Some(span));
                self.chunk.push_instruction(local_index, None);

                BlockResult::new()
            }
            Statement::Assignment { varlist, explist } => {
                let needs_alignment = varlist.len() != explist.len()
                    || explist.last().is_some_and(|e| {
                        matches!(
                            e.node,
                            Expression::PrefixExpression(TokenTree {
                                // Function calls can produce multiple values
                                node: PrefixExpression::FunctionCall(_),
                                ..
                            }) | Expression::Ellipsis
                        )
                    });

                if needs_alignment {
                    self.push_load_marker();
                }

                self.compile_expression_list(explist)?;

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
                            self.compile_expression(*index, ExpressionResult::Single)?;
                            self.compile_prefix_expression(*target, ExpressionResult::Single)?;
                            self.chunk.push_instruction(Instruction::Swap, None);
                            self.chunk.push_instruction(2, None);
                            self.chunk
                                .push_instruction(Instruction::SetTable, Some(span));
                            // SetTable keeps the table on the stack, so we need to pop it
                            self.chunk.push_instruction(Instruction::Pop, Some(span))
                        }
                        Variable::Field(target, name) => {
                            // Same story as above, but with a string key
                            let const_index = self.get_const_index(LuaConst::String(name.node.0));
                            self.chunk
                                .push_instruction(Instruction::LoadConst, Some(span));
                            self.chunk.push_const_index(const_index);
                            self.compile_prefix_expression(*target, ExpressionResult::Single)?;
                            self.chunk.push_instruction(Instruction::Swap, None);
                            self.chunk.push_instruction(2, None);
                            self.chunk
                                .push_instruction(Instruction::SetTable, Some(span));
                            self.chunk.push_instruction(Instruction::Pop, Some(span))
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
                self.compile_expression(condition, ExpressionResult::Single)?;
                // Jump to the end of the block if the condition is false
                self.chunk.push_instruction(Instruction::JmpFalse, None);
                let jmp_false_addr = self.chunk.push_addr_placeholder();
                // Pop the condition value
                self.chunk.push_instruction(Instruction::Pop, None);
                // Compile the block
                let block_has_return = block.node.return_statement.is_some();
                block_result.extend(self.compile_block(
                    block,
                    is_final_statement,
                    BlockOptions {
                        is_loop: options.is_loop,
                        loop_scope_depth: options.loop_scope_depth,
                        new_scope: true,
                    },
                )?);
                // Jump to the end of the if statement
                let mut jmp_end_addrs = vec![];
                if !block_has_return {
                    self.chunk.push_instruction(Instruction::Jmp, None);
                    jmp_end_addrs.push(self.chunk.push_addr_placeholder());
                }
                // Right before the else/elseifs, go here if the condition was false
                self.chunk.patch_addr_placeholder(jmp_false_addr);
                for else_if in else_ifs {
                    // Pop the initial condition
                    self.chunk.push_instruction(Instruction::Pop, None);
                    // Push the new condition
                    self.compile_expression(else_if.node.condition, ExpressionResult::Single)?;
                    // Jump to the end of the block if the condition is false
                    self.chunk.push_instruction(Instruction::JmpFalse, None);
                    let jmp_false_addr = self.chunk.push_addr_placeholder();
                    // Pop the condition value
                    self.chunk.push_instruction(Instruction::Pop, None);
                    // Compile the block
                    let block_has_return = else_if.node.block.node.return_statement.is_some();
                    block_result.extend(self.compile_block(
                        else_if.node.block,
                        is_final_statement,
                        BlockOptions {
                            is_loop: options.is_loop,
                            loop_scope_depth: options.loop_scope_depth,
                            new_scope: true,
                        },
                    )?);
                    // Jump to the end of the if statement
                    if !block_has_return {
                        self.chunk.push_instruction(Instruction::Jmp, None);
                        jmp_end_addrs.push(self.chunk.push_addr_placeholder());
                    }
                    // Right before the else/elseifs, go here if the condition was false
                    self.chunk.patch_addr_placeholder(jmp_false_addr);
                }
                let else_jump_addr = if let Some(else_block) = else_block {
                    // Pop the initial condition
                    self.chunk.push_instruction(Instruction::Pop, None);
                    let block_has_return = else_block.node.return_statement.is_some();
                    block_result.extend(self.compile_block(
                        else_block,
                        is_final_statement,
                        BlockOptions {
                            is_loop: options.is_loop,
                            loop_scope_depth: options.loop_scope_depth,
                            new_scope: true,
                        },
                    )?);
                    if !block_has_return {
                        self.chunk.push_instruction(Instruction::Jmp, None);
                        Some(self.chunk.push_addr_placeholder())
                    } else {
                        None
                    }
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
                if !options.is_loop {
                    return Err(lua_error!(
                        labels = vec![span.labeled("here")],
                        "cannot break outside of a loop"
                    ));
                }

                // Check the number of locals defined so far.
                let locals = self
                    .frames
                    .last()
                    .expect("frames is not empty")
                    .locals
                    .iter()
                    .filter(|local| local.depth >= options.loop_scope_depth)
                    .count();

                // Jump past the rest of the loop (sort of, we do some intermediate jumps to clean
                // up all locals along the way, both in the scope of the loop itself, and any
                // underlying ones such as if statements).
                self.chunk.push_instruction(Instruction::Jmp, Some(span));
                let break_jump = self.chunk.push_addr_placeholder();

                BlockResult::new().with_breaks(vec![BreakJump {
                    addr: break_jump,
                    locals,
                }])
            }
            Statement::Repeat { block, condition } => {
                // Jump into the block
                self.chunk.push_instruction(Instruction::Jmp, None);
                let inside_block_jump = self.chunk.push_addr_placeholder();

                // Pop the condition value
                let start_addr = self.chunk.get_current_addr();
                self.chunk.push_instruction(Instruction::Pop, None);
                self.chunk.patch_addr_placeholder(inside_block_jump);

                let loop_scope_depth = self.begin_scope();
                let block_result = self.compile_block(
                    block,
                    is_final_statement,
                    BlockOptions {
                        is_loop: true,
                        loop_scope_depth,
                        new_scope: false,
                    },
                )?;

                block_result.assert_no_new_locals()?;

                self.compile_expression(condition, ExpressionResult::Single)?;
                self.chunk.push_instruction(Instruction::JmpFalse, None);
                self.chunk.push_addr(start_addr);
                self.chunk.push_instruction(Instruction::Pop, None);

                self.end_scope()?;

                let jmps: Vec<_> = block_result
                    .breaks
                    .into_iter()
                    .map(|jump| {
                        // Skip over the extra pops here for non-breaks
                        self.chunk.push_instruction(Instruction::Jmp, None);
                        let jmp = self.chunk.push_addr_placeholder();

                        // Pop the locals defined in the loop before the break
                        self.chunk.patch_addr_placeholder(jump.addr);
                        for _ in 0..jump.locals {
                            self.chunk.push_instruction(Instruction::Pop, None);
                        }

                        jmp
                    })
                    .collect();
                for addr in jmps {
                    self.chunk.patch_addr_placeholder(addr);
                }

                BlockResult::new()
            }
            Statement::While { condition, block } => {
                let start_addr = self.chunk.get_current_addr();
                self.compile_expression(condition, ExpressionResult::Single)?;
                self.chunk.push_instruction(Instruction::JmpFalse, None);
                let jmp_false_addr = self.chunk.push_addr_placeholder();
                self.chunk.push_instruction(Instruction::Pop, None);

                let loop_scope_depth = self.begin_scope();
                let block_result = self.compile_block(
                    block,
                    is_final_statement,
                    BlockOptions {
                        is_loop: true,
                        loop_scope_depth,
                        new_scope: false,
                    },
                )?;
                self.end_scope()?;

                self.chunk.push_instruction(Instruction::Jmp, None);
                self.chunk.push_addr(start_addr);

                let jmps: Vec<_> = block_result
                    .breaks
                    .into_iter()
                    .map(|jump| {
                        // Skip over the extra pops here for non-breaks
                        self.chunk.push_instruction(Instruction::Jmp, None);
                        let jmp = self.chunk.push_addr_placeholder();

                        // Pop the locals defined in the loop before the break
                        self.chunk.patch_addr_placeholder(jump.addr);
                        for _ in 0..jump.locals {
                            self.chunk.push_instruction(Instruction::Pop, None);
                        }

                        jmp
                    })
                    .collect();
                for addr in jmps {
                    self.chunk.patch_addr_placeholder(addr);
                }

                self.chunk.push_instruction(Instruction::Jmp, None);
                let jmp_exit_loop_addr = self.chunk.push_addr_placeholder();
                self.chunk.patch_addr_placeholder(jmp_false_addr);
                // Pop the loop condition value
                self.chunk.push_instruction(Instruction::Pop, None);
                self.chunk.patch_addr_placeholder(jmp_exit_loop_addr);

                BlockResult::new()
            }
            Statement::For { condition, block } => {
                let loop_scope_depth = self.begin_scope();

                let (continue_addr, jmp_exit_loop_addr) = match condition.node {
                    ForCondition::NumericFor {
                        name,
                        initial,
                        limit,
                        step,
                    } => {
                        // FIXME: Coerce all to float if initial and step are floats

                        // Define the initial value and variable
                        self.compile_expression(initial, ExpressionResult::Single)?;
                        let identifier_local = self.add_local(name.node.0, Some(name.span));

                        // Create fake variables to hold the limit and step. Note that we use
                        // variable names that are invalid in Lua to avoid conflicts with user
                        // variables.
                        self.compile_expression(limit, ExpressionResult::Single)?;
                        let limit_local = self.add_local(b"#limit".to_vec(), None);

                        let step_local = if let Some(step) = step {
                            let step_span = step.span;
                            self.compile_expression(step, ExpressionResult::Single)?;
                            let step_local = self.add_local(b"#step".to_vec(), None);

                            // Raise an error if the step value is equal to zero
                            self.chunk.push_instruction(Instruction::GetLocal, None);
                            self.chunk.push_instruction(step_local, None);
                            self.compile_load_literal(Literal::Number(Number::Integer(0)), None);
                            self.chunk.push_instruction(Instruction::Eq, None);
                            self.chunk.push_instruction(Instruction::JmpFalse, None);
                            let jmp_false_addr = self.chunk.push_addr_placeholder();
                            self.chunk
                                .push_instruction(Instruction::Error, Some(step_span));
                            self.chunk
                                .push_instruction(RuntimeError::ForLoopLimitIsZero, None);

                            // Raise error
                            self.chunk.patch_addr_placeholder(jmp_false_addr);
                            self.chunk.push_instruction(Instruction::Pop, None);

                            step_local
                        } else {
                            self.compile_load_literal(Literal::Number(Number::Integer(1)), None);
                            self.add_local(b"#step".to_vec(), None)
                        };

                        // Create a "variable" to remember whether this is a decreasing loop.
                        self.chunk.push_instruction(Instruction::GetLocal, None);
                        self.chunk.push_instruction(step_local, None);
                        self.compile_load_literal(Literal::Number(Number::Integer(0)), None);
                        self.chunk.push_instruction(Instruction::Lt, None);
                        let decreasing_local = self.add_local(b"#decreasing".to_vec(), None);

                        // Label to jump back to the start of the loop
                        let condition_addr = self.chunk.get_current_addr();

                        // Evaluate the limit
                        self.chunk
                            .push_instruction(Instruction::GetLocal, Some(name.span));
                        self.chunk.push_instruction(identifier_local, None);
                        self.chunk.push_instruction(Instruction::GetLocal, None);
                        self.chunk.push_instruction(limit_local, None);

                        // Choose either "<=" or ">=" depending on whether the loop is increasing
                        // or decreasing.
                        self.chunk.push_instruction(Instruction::GetLocal, None);
                        self.chunk.push_instruction(decreasing_local, None);
                        self.chunk.push_instruction(Instruction::JmpTrue, None);
                        let jmp_decreasing = self.chunk.push_addr_placeholder();
                        self.chunk.push_instruction(Instruction::Pop, None);
                        self.chunk.push_instruction(Instruction::Le, None);
                        self.chunk.push_instruction(Instruction::Jmp, None);
                        let jmp_after_ge = self.chunk.push_addr_placeholder();
                        self.chunk.patch_addr_placeholder(jmp_decreasing);
                        self.chunk.push_instruction(Instruction::Pop, None);
                        self.chunk.push_instruction(Instruction::Ge, None);
                        self.chunk.patch_addr_placeholder(jmp_after_ge);

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
                            .push_instruction(Instruction::GetLocal, Some(name.span));
                        self.chunk.push_instruction(identifier_local, None);
                        self.chunk.push_instruction(Instruction::GetLocal, None);
                        self.chunk.push_instruction(step_local, None);
                        self.chunk.push_instruction(Instruction::Add, None);
                        self.chunk.push_instruction(Instruction::SetLocal, None);
                        self.chunk.push_instruction(identifier_local, None);

                        // After step, jump to the condition
                        self.chunk.push_instruction(Instruction::Jmp, None);
                        self.chunk.push_addr(condition_addr);

                        // Path the inside block jump
                        self.chunk.patch_addr_placeholder(inside_block_jump);

                        (step_addr, jmp_false_addr)
                    }
                    ForCondition::GenericFor { names, expressions } => {
                        // Evaluate the expressions and make sure there are three
                        self.push_load_marker();
                        self.compile_expression_list(expressions)?;
                        self.chunk.push_instruction(Instruction::Align, None);
                        self.chunk.push_instruction(4, None);

                        // Save the result as locals: the iterator function, the state, the initial
                        // value for the control variable (which is the first name).
                        let iterator_local = self.add_local(b"#iterator".to_vec(), None);
                        let state_local = self.add_local(b"#state".to_vec(), None);
                        // NOTE: The syntax ensures there is at least one name
                        let control_local =
                            self.add_local(names[0].node.0.clone(), Some(names[0].span));
                        let closing_local = self.add_local(b"#closing".to_vec(), None);
                        self.chunk.push_instruction(Instruction::SetLocalAttr, None);
                        self.chunk.push_instruction(closing_local, None);
                        self.chunk
                            .push_instruction(LuaVariableAttribute::ToBeClosed, None);

                        // Initialize the other variables to nil
                        for name in &names[1..] {
                            self.push_load_nil(Some(name.span));
                            self.add_local(name.node.0.clone(), Some(name.span));
                        }

                        // Call the iterator function
                        let step_addr = self.chunk.get_current_addr();
                        // Results marker
                        self.push_load_marker();
                        self.chunk.push_instruction(Instruction::GetLocal, None);
                        self.chunk.push_instruction(state_local, None);
                        self.chunk.push_instruction(Instruction::GetLocal, None);
                        self.chunk.push_instruction(control_local, None);
                        self.chunk.push_instruction(Instruction::GetLocal, None);
                        self.chunk.push_instruction(iterator_local, None);
                        self.chunk.push_instruction(Instruction::Call, None);
                        self.chunk.push_instruction(0, None);
                        self.chunk.push_instruction(2, None);
                        // Align to number of names
                        self.chunk.push_instruction(Instruction::Align, None);
                        self.chunk.push_instruction(names.len() as u8, None);
                        // Then store each result in the corresponding local
                        // NOTE: We need to iterate in reverse to handle chained assignments
                        // correctly.
                        for name in names.iter().rev() {
                            self.chunk
                                .push_instruction(Instruction::SetLocal, Some(span));
                            let local_index = self
                                .resolve_local(&name.node.0)
                                .expect("parser ensures that variable is in scope");
                            self.chunk.push_instruction(local_index, None);
                        }

                        // Now we need to evaluate whether any value was returned. If not, we
                        // should break out of the loop.
                        self.chunk
                            .push_instruction(Instruction::GetLocal, Some(span));
                        self.chunk.push_instruction(control_local, None);
                        self.push_load_nil(None);
                        self.chunk.push_instruction(Instruction::Eq, None);
                        self.chunk.push_instruction(Instruction::JmpTrue, None);
                        let jmp_true_addr = self.chunk.push_addr_placeholder();

                        // Pop the condition value
                        self.chunk.push_instruction(Instruction::Pop, None);

                        (step_addr, jmp_true_addr)
                    }
                };

                let block_result = self.compile_block(
                    block,
                    is_final_statement,
                    BlockOptions {
                        is_loop: true,
                        loop_scope_depth,
                        new_scope: true,
                    },
                )?;

                // Jump back to the step evaluation
                self.chunk.push_instruction(Instruction::Jmp, None);
                self.chunk.push_addr(continue_addr);

                // Patch the false condition jump
                self.chunk.patch_addr_placeholder(jmp_exit_loop_addr);
                self.chunk.push_instruction(Instruction::Pop, None);

                self.end_scope()?;

                // Patch any break jumps
                let jmps: Vec<_> = block_result
                    .breaks
                    .into_iter()
                    .map(|jump| {
                        // Skip over the extra pops here for non-breaks
                        self.chunk.push_instruction(Instruction::Jmp, None);
                        let jmp = self.chunk.push_addr_placeholder();

                        // Pop the locals defined in the loop before the break
                        self.chunk.patch_addr_placeholder(jump.addr);
                        for _ in 0..jump.locals {
                            self.chunk.push_instruction(Instruction::Pop, None);
                        }

                        jmp
                    })
                    .collect();
                for addr in jmps {
                    self.chunk.patch_addr_placeholder(addr);
                }

                BlockResult::new()
            }
            Statement::Block(block) => self.compile_block(
                block,
                is_final_statement,
                BlockOptions {
                    is_loop: options.is_loop,
                    loop_scope_depth: options.loop_scope_depth,
                    new_scope: true,
                },
            )?,
            Statement::Goto(name) => {
                let frame = self.frames.last_mut().expect("frames is not empty");
                let label = frame.labels.get(&name.node.0);
                let locals = frame
                    .locals
                    .iter()
                    .filter(|local| local.depth <= frame.scope_depth)
                    .count();
                match label {
                    Some(label) => {
                        // Pop all locals that were defined in between
                        if locals > label.locals {
                            let intermediate_locals = locals - label.locals;
                            for _ in 0..intermediate_locals {
                                self.chunk.push_instruction(Instruction::Pop, None);
                            }
                        }
                        self.chunk.push_instruction(Instruction::Jmp, Some(span));
                        self.chunk.push_addr(label.addr);
                    }
                    None => {
                        // Add to frame to resolve the jump later
                        self.chunk.push_instruction(Instruction::Jmp, Some(span));
                        let addr = self.chunk.push_addr_placeholder();

                        let scope_id = frame.scope_id();
                        let goto = Goto {
                            addr,
                            locals,
                            span,
                            depth: frame.scope_depth,
                            scope_id,
                            to_end_of_scope: false,
                        };
                        frame.gotos.entry(name.node.0).or_default().push(goto);
                    }
                }

                BlockResult::new()
            }
            Statement::Label(label) => {
                let frame = self.frames.last_mut().expect("frames is not empty");
                if frame.labels.contains_key(&label.node.0) {
                    return Err(lua_error!(
                        labels = vec![label.span.labeled("here")],
                        "label '{}' already defined",
                        String::from_utf8_lossy(&label.node.0)
                    ));
                }

                let addr = self.chunk.get_current_addr();
                let locals = frame
                    .locals
                    .iter()
                    .filter(|local| local.depth <= frame.scope_depth)
                    .count();

                let scope_id = frame.scope_id();
                let (any_new_locals, resolved_all_gotos) =
                    if let Some(gotos) = frame.gotos.get_mut(&label.node.0) {
                        let mut any_new_locals = None;
                        for goto in gotos.iter_mut() {
                            if goto.depth < frame.scope_depth {
                                return Err(lua_error!(
                                    labels = vec![
                                        goto.span.labeled("this goto statement"),
                                        label.span.labeled("this label")
                                    ],
                                    "cannot goto label '{}' in a narrower scope",
                                    String::from_utf8_lossy(&label.node.0)
                                ));
                            }

                            if goto.depth > frame.scope_depth || goto.scope_id == scope_id {
                                if locals > goto.locals && any_new_locals.is_none() {
                                    let first_new_local = frame
                                        .locals
                                        .iter()
                                        .filter(|local| local.depth <= frame.scope_depth)
                                        .nth(goto.locals)
                                        .expect("local exists");
                                    any_new_locals = Some(NewLocalsAfterGoto {
                                        goto: (label.node.0.clone(), goto.span),
                                        local: (first_new_local.name.clone(), first_new_local.span),
                                    });

                                    goto.to_end_of_scope = true;
                                } else {
                                    self.chunk.patch_addr_placeholder_with(goto.addr, addr);
                                }
                            }
                        }

                        gotos.retain(|goto| {
                            (goto.depth <= frame.scope_depth && goto.scope_id != scope_id)
                                || goto.to_end_of_scope
                        });

                        (any_new_locals, gotos.is_empty())
                    } else {
                        (None, false)
                    };

                if resolved_all_gotos {
                    frame.gotos.remove(&label.node.0);
                }

                frame.labels.insert(
                    label.node.0,
                    Label {
                        addr,
                        locals,
                        depth: frame.scope_depth,
                    },
                );

                BlockResult::new().with_new_locals(any_new_locals)
            }
        })
    }

    fn compile_return_statement(
        &mut self,
        return_statement: Vec<TokenTree<Expression>>,
    ) -> crate::Result {
        if return_statement.is_empty() {
            self.chunk.push_instruction(Instruction::Return0, None);
            return Ok(());
        }

        let (num_returns, is_variable) = {
            (
                return_statement.len(),
                // The number of return values is variable if the last expression is
                // a function call or an ellipsis (things that can result in
                // multiple values themselves).
                matches!(
                    return_statement.last(),
                    Some(TokenTree {
                        node: Expression::PrefixExpression(TokenTree {
                            node: PrefixExpression::FunctionCall(..),
                            ..
                        }) | Expression::Ellipsis,
                        ..
                    })
                ),
            )
        };

        let expression_list_length = self.compile_expression_list(return_statement)?;

        if num_returns == 1 && !is_variable {
            self.chunk.push_instruction(Instruction::Return1, None);
        } else {
            self.chunk.push_instruction(
                match expression_list_length {
                    ExpressionListLength::Constant => Instruction::Return,
                    ExpressionListLength::MultRes => Instruction::ReturnM,
                },
                None,
            );
            self.chunk.push_instruction(
                match expression_list_length {
                    ExpressionListLength::Constant => num_returns as u8,
                    ExpressionListLength::MultRes => (num_returns - 1) as u8,
                },
                None,
            );
        }

        Ok(())
    }

    fn compile_function_call(
        &mut self,
        function_call: TokenTree<FunctionCall>,
        result_mode: ExpressionResult,
    ) -> crate::Result {
        let num_args = function_call.node.args.node.len();

        // If this is a method call, we need to push the object as the first argument
        let expression_list_length = if let Some(method_name) = function_call.node.method_name {
            self.compile_prefix_expression(*function_call.node.function, ExpressionResult::Single)?;

            // It's unfortunate to duplicate the argument loop here with the else below, but this
            // avoids the borrow checker complaining, and having to `clone()` the function call
            // node above.
            let expression_list_length =
                self.compile_expression_list(function_call.node.args.node)?;

            // We'll look up the target object as the first item after the marker
            // on the stack, and then retrieve the method as a field on that object.
            self.chunk
                .push_instruction(Instruction::DupFromMarker, None);
            self.chunk.push_instruction(1, None);

            let method_name_const_index =
                self.get_const_index(LuaConst::String(method_name.node.0));
            self.chunk
                .push_instruction(Instruction::LoadConst, Some(function_call.span));
            self.chunk.push_const_index(method_name_const_index);

            self.chunk.push_instruction(
                Instruction::GetTable,
                Some(Span::new(function_call.span.start, method_name.span.end)),
            );

            expression_list_length
        } else {
            let expression_list_length =
                self.compile_expression_list(function_call.node.args.node)?;

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
                    self.compile_prefix_expression(prefix_expression, ExpressionResult::Single)?;
                }
            }

            expression_list_length
        };

        // Instruction: [CALL|CALLM] [NUM_RESULTS] [NUM_KNOWN_ARGUMENTS]
        // CALL for regular calls, CALLM for calls with a multres expression at
        // the end of its arguments.
        // NUM_RESULTS is 1 for single result, 0 for multres.
        // NUM_KNOWN_ARGUMENTS is the number of known arguments, not counting
        // the multres at the end (if any), plus 1.
        self.chunk.push_instruction(
            match expression_list_length {
                ExpressionListLength::Constant => Instruction::Call,
                ExpressionListLength::MultRes => Instruction::CallM,
            },
            Some(function_call.span),
        );
        self.chunk.push_instruction(
            if result_mode == ExpressionResult::Single {
                1
            } else {
                0
            },
            None,
        );
        self.chunk.push_instruction(
            match expression_list_length {
                ExpressionListLength::Constant => (num_args + 1) as u8,
                ExpressionListLength::MultRes => num_args as u8,
            },
            None,
        );

        Ok(())
    }

    fn compile_tail_call(&mut self, function_call: TokenTree<FunctionCall>) -> crate::Result<bool> {
        // Check if this function call refers to the current function
        let frame = self.frames.last().expect("frames is not empty");
        let is_tail_call = match &function_call.node.function.node {
            PrefixExpression::Variable(TokenTree {
                node: Variable::Name(name),
                ..
            }) => {
                (Some(&name.node.0) == frame.name.as_ref()
                    && function_call.node.method_name.is_none())
                    || (&name.node.0 == b"self"
                        && function_call.node.method_name.map(|n| n.node.0) == frame.method_name)
            }
            _ => false,
        };

        if !is_tail_call {
            return Ok(false);
        }

        let is_method = frame.method_name.is_some();
        let parameter_offset = if is_method { 1 } else { 0 };
        let num_parameters = frame
            .locals
            .iter()
            .filter(|local| local.is_parameter)
            .count()
            - parameter_offset;
        let function_has_varargs = frame
            .locals
            .iter()
            .any(|local| local.is_parameter && local.name == VARARG_LOCAL_NAME);
        let num_locals = frame.locals.len();
        let frame_ip = frame.ip;

        self.push_load_marker();
        self.compile_expression_list(function_call.node.args.node)?;

        if function_has_varargs {
            self.chunk.push_instruction(Instruction::AlignVararg, None);
        } else {
            self.chunk.push_instruction(Instruction::Align, None);
        }
        self.chunk.push_instruction(num_parameters as u8, None);

        // Now we just need to overwrite the current frame's local parameters with the new
        // values, and then jump back to the start of the function.
        if function_has_varargs {
            self.chunk.push_instruction(Instruction::SetLocal, None);
            self.chunk
                .push_instruction((num_parameters + parameter_offset) as u8, None);
        }
        for i in 0..num_parameters {
            self.chunk.push_instruction(Instruction::SetLocal, None);
            self.chunk
                .push_instruction((num_parameters - i - 1 + parameter_offset) as u8, None);
        }

        // Pop off the rest of the stack
        let extraneous_locals = num_locals
            - num_parameters
            - if function_has_varargs { 1 } else { 0 }
            - parameter_offset;
        for _ in 0..extraneous_locals {
            self.chunk.push_instruction(Instruction::Pop, None);
        }

        self.chunk.push_instruction(Instruction::Jmp, None);
        // We don't have to do the alignment, since we already ensured the correct number of
        // arguments above.
        self.chunk.push_addr(frame_ip + 2);

        Ok(true)
    }

    fn compile_function_def(&mut self, function_def: TokenTree<FunctionDef>) -> crate::Result<()> {
        // We'll issue the bytecode inline here, but jump over it at runtime
        self.chunk.push_instruction(Instruction::Jmp, None);
        let jmp_over_func_addr = self.chunk.push_addr_placeholder();

        // Compile the function
        let func_addr = self.chunk.get_current_addr();
        self.begin_frame(
            self.chunk.get_current_addr(),
            function_def.node.name.clone(),
            function_def.node.method_name,
        );
        self.begin_scope();

        // Define the function arguments
        let parameter_count = function_def.node.parameters.len();
        for parameter in function_def.node.parameters {
            self.add_parameter(parameter.node.0, Some(parameter.span));
        }

        if function_def.node.has_varargs {
            // TODO: There actually should be a span for varargs, right?
            self.add_parameter(VARARG_LOCAL_NAME.to_vec(), None);
            self.chunk.push_instruction(Instruction::AlignVararg, None);
        } else {
            self.chunk.push_instruction(Instruction::Align, None);
        }
        self.chunk.push_instruction(parameter_count as u8, None);

        let has_return = function_def.node.block.node.return_statement.is_some();
        self.compile_block(
            function_def.node.block,
            true,
            BlockOptions {
                is_loop: false,
                loop_scope_depth: 0,
                // Already started the function scope above
                new_scope: false,
            },
        )?;

        // NOTE: We don't call `end_scope` here because we want to keep the locals around for the
        // return values. They're handled by the return statement, when the call frame is dropped.
        let frame = self.end_frame()?;

        if !has_return {
            self.chunk.push_instruction(Instruction::Return0, None);
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

        Ok(())
    }

    fn compile_expression_list(
        &mut self,
        expressions: Vec<TokenTree<Expression>>,
    ) -> crate::Result<ExpressionListLength> {
        let num_expressions = expressions.len();
        let mut multres = false;
        for (i, expression) in expressions.into_iter().enumerate() {
            let is_last = i == num_expressions - 1;
            if matches!(
                self.compile_expression(
                    expression,
                    if is_last {
                        ExpressionResult::Multiple
                    } else {
                        ExpressionResult::Single
                    },
                )?,
                ExpressionResult::Multiple
            ) {
                multres = true;
            }
        }

        Ok(if multres {
            ExpressionListLength::MultRes
        } else {
            ExpressionListLength::Constant
        })
    }

    fn compile_expression(
        &mut self,
        expression: TokenTree<Expression>,
        result_mode: ExpressionResult,
    ) -> crate::Result<ExpressionResult> {
        let span = expression.span;
        Ok(match expression.node {
            Expression::Literal(literal) => {
                self.compile_load_literal(literal.node, Some(literal.span));

                ExpressionResult::Single
            }
            Expression::BinaryOp { op, lhs, rhs } => {
                match &op.node {
                    BinaryOperator::And => {
                        self.compile_expression(*lhs, ExpressionResult::Single)?;
                        self.chunk.push_instruction(Instruction::JmpFalse, None);
                        let jmp_false_addr = self.chunk.push_addr_placeholder();
                        self.compile_expression(*rhs, ExpressionResult::Single)?;
                        self.chunk.push_instruction(Instruction::And, Some(span));
                        self.chunk.patch_addr_placeholder(jmp_false_addr)
                    }
                    BinaryOperator::Or => {
                        self.compile_expression(*lhs, ExpressionResult::Single)?;
                        self.chunk.push_instruction(Instruction::JmpTrue, None);
                        let jmp_true_addr = self.chunk.push_addr_placeholder();
                        self.compile_expression(*rhs, ExpressionResult::Single)?;
                        self.chunk.push_instruction(Instruction::Or, Some(span));
                        self.chunk.patch_addr_placeholder(jmp_true_addr);
                    }
                    _ => {
                        self.compile_expression(*lhs, ExpressionResult::Single)?;
                        self.compile_expression(*rhs, ExpressionResult::Single)?;
                        self.compile_binary_operator(op, expression.span);
                    }
                }

                ExpressionResult::Single
            }
            Expression::PrefixExpression(function_call) => {
                self.compile_prefix_expression(function_call, result_mode)?
            }
            Expression::UnaryOp { op, rhs } => {
                self.compile_expression(*rhs, ExpressionResult::Single)?;
                self.compile_unary_operator(op, expression.span);

                ExpressionResult::Single
            }
            Expression::FunctionDef(function_def) => {
                self.compile_function_def(function_def)?;

                ExpressionResult::Single
            }
            Expression::TableConstructor(table) => {
                self.compile_table_constructor(table)?;

                ExpressionResult::Single
            }
            Expression::Ellipsis => {
                let vararg_local_index = self
                    .resolve_local(VARARG_LOCAL_NAME)
                    .expect("parser ensures that vararg is in scope");
                self.chunk
                    .push_instruction(Instruction::LoadVararg, Some(expression.span));
                self.chunk.push_instruction(vararg_local_index, None);
                self.chunk.push_instruction(
                    if result_mode == ExpressionResult::Single {
                        1
                    } else {
                        0
                    },
                    None,
                );

                result_mode
            }
        })
    }

    fn compile_prefix_expression(
        &mut self,
        prefix_expression: TokenTree<PrefixExpression>,
        result_mode: ExpressionResult,
    ) -> crate::Result<ExpressionResult> {
        Ok(match prefix_expression.node {
            PrefixExpression::FunctionCall(function_call) => {
                self.compile_function_call(function_call, result_mode)?;

                result_mode
            }
            PrefixExpression::Parenthesized(expression) => {
                self.compile_expression(*expression, ExpressionResult::Single)?;

                ExpressionResult::Single
            }
            PrefixExpression::Variable(variable) => {
                match variable.node {
                    Variable::Name(name) => self.variable(name, VariableMode::Read),
                    Variable::Indexed(prefix, index) => {
                        self.compile_prefix_expression(*prefix, ExpressionResult::Single)?;
                        self.compile_expression(*index, ExpressionResult::Single)?;
                        self.chunk
                            .push_instruction(Instruction::GetTable, Some(prefix_expression.span));
                    }
                    Variable::Field(prefix, name) => {
                        self.compile_prefix_expression(*prefix, ExpressionResult::Single)?;
                        let const_index = self.get_const_index(LuaConst::String(name.node.0));
                        self.chunk
                            .push_instruction(Instruction::LoadConst, Some(prefix_expression.span));
                        self.chunk.push_const_index(const_index);
                        self.chunk
                            .push_instruction(Instruction::GetTable, Some(prefix_expression.span));
                    }
                }

                ExpressionResult::Single
            }
        })
    }

    fn compile_table_constructor(
        &mut self,
        table: TokenTree<TableConstructor>,
    ) -> crate::Result<()> {
        // Optimization: if all of the fields are literals, we can just create the table here and
        // load it as a constant.
        if table.node.fields.iter().all(|field| match &field.node {
            Field::Named(_, value) => matches!(value.node, Expression::Literal(_)),
            Field::Indexed(index, value) => {
                matches!(index.node, Expression::Literal(_))
                    && matches!(value.node, Expression::Literal(_))
            }
            Field::Value(value) => matches!(value.node, Expression::Literal(_)),
        }) {
            let mut new_table = LuaTable::new();
            for fields in table.node.fields {
                match fields.node {
                    Field::Named(name, value) => {
                        let value = match value.node {
                            Expression::Literal(literal) => literal.node,
                            _ => unreachable!("parser ensures that field value is a literal"),
                        };
                        new_table.insert(name.node.0.into(), value.into());
                    }
                    Field::Indexed(index, value) => {
                        let index = match index.node {
                            Expression::Literal(literal) => literal.node,
                            _ => unreachable!("parser ensures that field index is a literal"),
                        };
                        let value = match value.node {
                            Expression::Literal(literal) => literal.node,
                            _ => unreachable!("parser ensures that field value is a literal"),
                        };
                        new_table.insert(index.into(), value.into());
                    }
                    Field::Value(value) => {
                        let value = match value.node {
                            Expression::Literal(literal) => literal.node,
                            _ => unreachable!("parser ensures that field value is a literal"),
                        };
                        new_table.append(value.into());
                    }
                }
            }

            let const_index = self.get_const_index(LuaConst::Table(new_table));
            self.chunk
                .push_instruction(Instruction::LoadConst, Some(table.span));
            self.chunk.push_const_index(const_index);

            return Ok(());
        }

        self.chunk
            .push_instruction(Instruction::NewTable, Some(table.span));

        let num_fields = table.node.fields.len();
        let mut previous_was_value_field = false;
        for (i, field) in table.node.fields.into_iter().enumerate() {
            if previous_was_value_field && !matches!(&field.node, Field::Value(_)) {
                self.chunk
                    .push_instruction(Instruction::AppendToTable, Some(field.span));
                previous_was_value_field = false;
            }

            match field.node {
                Field::Named(name, value) => {
                    let const_index = self.get_const_index(LuaConst::String(name.node.0));
                    self.chunk
                        .push_instruction(Instruction::LoadConst, Some(field.span));
                    self.chunk.push_const_index(const_index);
                    self.compile_expression(value, ExpressionResult::Single)?;
                    self.chunk
                        .push_instruction(Instruction::SetTable, Some(field.span));
                }
                Field::Indexed(index_expression, value) => {
                    self.compile_expression(index_expression, ExpressionResult::Single)?;
                    self.compile_expression(value, ExpressionResult::Single)?;
                    self.chunk
                        .push_instruction(Instruction::SetTable, Some(field.span));
                }
                Field::Value(value) => {
                    if !previous_was_value_field {
                        self.push_load_marker();
                        previous_was_value_field = true;
                    }

                    let is_last_field = i == num_fields - 1;
                    self.compile_expression(
                        value,
                        if is_last_field {
                            ExpressionResult::Multiple
                        } else {
                            ExpressionResult::Single
                        },
                    )?;
                }
            }
        }

        if previous_was_value_field {
            self.chunk
                .push_instruction(Instruction::AppendToTable, None);
        }

        Ok(())
    }

    fn compile_load_literal(&mut self, literal: Literal, span: Option<Span>) {
        let const_index = match literal {
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

        self.chunk.push_instruction(Instruction::LoadConst, span);
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

    fn variable(&mut self, name: TokenTree<Name>, mode: VariableMode) {
        if let Some(local) = self.resolve_local(&name.node.0) {
            self.chunk.push_instruction(
                match mode {
                    VariableMode::Read => Instruction::GetLocal,
                    VariableMode::Write => Instruction::SetLocal,
                },
                Some(name.span),
            );
            self.chunk.push_instruction(local, None);
        } else if let Some(upvalue) = self.resolve_upvalue(&name.node.0) {
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
            let const_index = self.get_global_name_index(name.node.0);
            self.chunk.push_const_index(const_index);
        }
    }

    fn begin_frame(&mut self, ip: JumpAddr, name: Option<Vec<u8>>, method_name: Option<Vec<u8>>) {
        self.frames.push(Frame::new(ip, name, method_name));
    }

    fn end_frame(&mut self) -> crate::Result<Frame> {
        let frame = self.frames.pop().expect("frame exists");

        if let Some((name, goto)) = frame
            .gotos
            .iter()
            .flat_map(|(name, jumps)| jumps.iter().map(move |jump| (name, jump)))
            .next()
        {
            return Err(lua_error!(
                labels = vec![goto.span.labeled("here")],
                "label '{}' not found",
                String::from_utf8_lossy(name)
            ));
        }

        Ok(frame)
    }

    fn begin_scope(&mut self) -> u8 {
        let current_frame = self.frames.last_mut().unwrap();
        current_frame.scope_depth += 1;
        current_frame.scope_ids.push(current_frame.scope_id_counter);
        current_frame.scope_id_counter += 1;
        current_frame.scope_depth
    }

    fn end_scope(&mut self) -> crate::Result<()> {
        let current_frame = self.frames.last_mut().unwrap();
        let scope_id = current_frame.scope_id();
        current_frame.scope_ids.pop();
        let parent_scope_id = current_frame.scope_id();
        current_frame.scope_depth -= 1;

        while !current_frame.locals.is_empty()
            && current_frame
                .locals
                .last()
                .as_ref()
                .is_some_and(|local| local.depth > current_frame.scope_depth)
        {
            current_frame.locals.pop();
            self.chunk.push_instruction(Instruction::Pop, None);
        }

        // Remove any labels that are no longer in scope
        let mut to_remove = Vec::new();
        for (name, label) in current_frame.labels.iter() {
            if label.depth > current_frame.scope_depth {
                to_remove.push(name.clone());
            }

            // Patch gotos that go to this label
            let all_jumps_resolved = if let Some(jumps) = current_frame.gotos.get_mut(name) {
                for jump in jumps.iter() {
                    if (jump.depth > label.depth || jump.scope_id == scope_id)
                        && !jump.to_end_of_scope
                    {
                        if jump.depth < label.depth {
                            return Err(lua_error!(
                                labels = vec![jump.span.labeled("here")],
                                "label '{}' not found",
                                String::from_utf8_lossy(name)
                            ));
                        }

                        self.chunk
                            .patch_addr_placeholder_with(jump.addr, label.addr);
                    }
                }

                jumps.retain(|jump| {
                    jump.depth < label.depth || jump.scope_id != scope_id || jump.to_end_of_scope
                });
                jumps.is_empty()
            } else {
                false
            };
            if all_jumps_resolved {
                current_frame.gotos.remove(name);
            }
        }

        for name in to_remove {
            current_frame.labels.remove(&name);
        }

        // If we have any gotos still pending, they're going to jump out of this scope. So we need
        // to make sure that they will correctly pop any locals that were in scope at the point of
        // the goto.
        for gotos in current_frame.gotos.values_mut() {
            for goto in gotos.iter_mut() {
                if goto.scope_id == scope_id || goto.to_end_of_scope {
                    if goto.locals > 0 {
                        self.chunk.push_instruction(Instruction::Jmp, None);
                        let after_goto_pops = self.chunk.push_addr_placeholder();
                        self.chunk.patch_addr_placeholder(goto.addr);
                        let locals_to_pop = goto.locals - current_frame.locals.len();
                        for _ in 0..locals_to_pop {
                            self.chunk.push_instruction(Instruction::Pop, None);
                        }
                        if !goto.to_end_of_scope {
                            self.chunk.push_instruction(Instruction::Jmp, None);
                            goto.addr = self.chunk.push_addr_placeholder();
                        }
                        self.chunk.patch_addr_placeholder(after_goto_pops);
                    } else if goto.to_end_of_scope {
                        self.chunk.patch_addr_placeholder(goto.addr);
                    }

                    goto.scope_id = parent_scope_id;
                    goto.locals = current_frame
                        .locals
                        .iter()
                        .filter(|local| local.depth <= current_frame.scope_depth)
                        .count();
                }
            }

            gotos.retain(|goto| !goto.to_end_of_scope);
        }

        current_frame.gotos.retain(|_, gotos| !gotos.is_empty());

        Ok(())
    }

    fn add_parameter(&mut self, name: Vec<u8>, span: Option<Span>) -> u8 {
        self.add_local_inner(name, true, span)
    }

    fn add_local(&mut self, name: Vec<u8>, span: Option<Span>) -> u8 {
        self.add_local_inner(name, false, span)
    }

    fn add_local_inner(&mut self, name: Vec<u8>, is_parameter: bool, span: Option<Span>) -> u8 {
        let current_frame = self.frames.last_mut().unwrap();
        let local = Local {
            name: name.clone(),
            is_parameter,
            depth: current_frame.scope_depth,
            span,
        };

        let index = current_frame.locals.len() as u8;
        current_frame.locals.push(local);

        index
    }

    fn resolve_local(&mut self, name: &[u8]) -> Option<u8> {
        self.frames
            .last()
            .expect("there should always be at least one frame")
            .resolve_local(name)
    }

    fn resolve_upvalue(&mut self, name: &[u8]) -> Option<u8> {
        self.resolve_upvalue_inner(name, self.frames.len() - 1)
    }

    fn resolve_upvalue_inner(&mut self, name: &[u8], frame_index: usize) -> Option<u8> {
        if frame_index < 1 {
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
