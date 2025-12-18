use std::{
    borrow::Cow,
    collections::HashMap,
    path::PathBuf,
    sync::{Arc, RwLock},
};

use crate::{
    ast::{
        BinaryOperator, Block, Expression, Field, ForCondition, FunctionCall, FunctionDef, Literal,
        LocalAttribute, Name, Number, NumericFor, PrefixExpression, Statement, TableConstructor,
        TokenTree, UnaryOperator, Variable,
    },
    bytecode::{Bytecode, JumpToUndecidedAddress},
    chunk::Chunk,
    error::{RuntimeError, lua_error},
    parser::Parser,
    token::Span,
    value::{
        LuaConst, LuaFunctionDefinition, LuaNumber, LuaObject, LuaTable, LuaVariableAttribute,
    },
    vm::{ConstIndex, JumpAddr, VM},
};

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
    current_register: u8,
    depth: u8,
}

#[derive(Debug, Clone)]
struct Local {
    name: Vec<u8>,
    depth: u8,
    span: Option<Span>,
}

#[derive(Debug, Clone)]
pub(crate) struct Upvalue {
    pub(crate) is_local: bool,
    pub(crate) index: u8,
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
    name: Option<Vec<u8>>,
    method_name: Option<Vec<u8>>,
    locals: Vec<Local>,
    gotos: HashMap<Vec<u8>, Vec<Goto>>,
    labels: HashMap<Vec<u8>, Label>,
    upvalues: Vec<Upvalue>,
    scope_depth: u8,
    scope_id_counter: usize,
    scope_ids: Vec<usize>,
    scope_starting_registers: Vec<u8>,
    register_index: u8,
    max_registers: u8,
}

impl Frame {
    fn new(name: Option<Vec<u8>>, method_name: Option<Vec<u8>>) -> Self {
        Self {
            name,
            method_name,
            locals: vec![],
            gotos: HashMap::new(),
            labels: HashMap::new(),
            upvalues: vec![],
            scope_depth: 0,
            scope_id_counter: 1,
            scope_ids: vec![0],
            scope_starting_registers: vec![0],
            register_index: 0,
            max_registers: 0,
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

    fn take_register(&mut self) -> u8 {
        let reg = self.register_index;
        self.register_index += 1;
        if self.register_index > self.max_registers {
            self.max_registers = self.register_index;
        }
        reg
    }

    fn reserve_register(&mut self, index: u8) {
        if index >= self.register_index {
            self.register_index = index + 1;
        }
    }

    fn release_register(&mut self) {
        assert!(self.register_index > 0, "no registers to release");
        self.register_index -= 1;
    }

    fn release_registers_from(&mut self, index: u8) {
        assert!(
            index <= self.register_index,
            "cannot reset registers to a higher index ({index} > {})",
            self.register_index
        );
        self.register_index = index;
    }

    fn set_register_index(&mut self, index: u8) {
        self.register_index = index;
        if self.register_index > self.max_registers {
            self.max_registers = self.register_index;
        }
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

    fn assert_no_new_locals(&self) -> crate::Result {
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
    Ref,
    Read,
    Write { value_register: u8 },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExpressionMode {
    /// When possible, just return a register reference to the value. This will
    /// mainly benefit references to variables, in usages like `SetTable`, where
    /// you need the register holding the table value. Expressions such as
    /// calls or operations that produce new values will always return a new
    /// register.
    Ref,
    /// Always copy the value into a new register, even if we have a register
    /// already holding the value. This is useful e.g. for function calls,
    /// where the layout is expected to be [..., function, arg1, arg2, ...].
    Copy,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExpressionResultMode {
    Single,
    Multiple,
}

impl ExpressionResultMode {
    fn into_result(self, register: u8) -> ExpressionResult {
        match self {
            ExpressionResultMode::Single => ExpressionResult::Single { register },
            ExpressionResultMode::Multiple => ExpressionResult::Multiple {
                from_register: register,
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExpressionResult {
    Single { register: u8 },
    Multiple { from_register: u8 },
}

impl ExpressionResult {
    pub fn get_register(&self) -> u8 {
        match self {
            ExpressionResult::Single { register } => *register,
            ExpressionResult::Multiple { from_register } => *from_register,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ExpressionListLength {
    Constant {
        from_register: u8,
    },
    /// Expression list ends in an expression that _may_ result in multiple values.
    MultRes {
        from_register: u8,
    },
}

impl ExpressionListLength {
    pub fn get_register(&self) -> u8 {
        match self {
            Self::Constant { from_register } => *from_register,
            Self::MultRes { from_register } => *from_register,
        }
    }
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
            frames: vec![Frame::new(None, None)],
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
            self.chunk.push_bytecode(Bytecode::Return0, None);
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
        let frame = self.end_frame()?;
        self.chunk.max_registers = frame.max_registers;

        Ok(self.vm.add_chunk(self.chunk))
    }

    fn push_load_nil(&mut self, span: Option<Span>) -> u8 {
        let const_index = self.get_const_index(LuaConst::Nil);
        let register = self.frames.last_mut().unwrap().take_register();
        self.chunk.push_bytecode(
            Bytecode::LoadConst {
                register,
                const_index,
            },
            span,
        );
        register
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
            if return_statement.len() == 1
                && let Expression::PrefixExpression(TokenTree {
                    node: PrefixExpression::FunctionCall(function_call),
                    ..
                }) = &return_statement[0].node
                && self.compile_tail_call(function_call.clone())?
            {
                handled_as_tail_call = true;
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
        let current_register = self.frames.last().unwrap().register_index;

        Ok(match statement.node {
            Statement::FunctionCall(function_call) => {
                if is_final_statement && self.compile_tail_call(function_call.clone())? {
                    return Ok(BlockResult::new());
                }

                self.compile_function_call(function_call, ExpressionResultMode::Multiple)?;
                self.frames
                    .last_mut()
                    .unwrap()
                    .release_registers_from(current_register);
                BlockResult::new()
            }
            Statement::LocalDeclaraction(names, expressions) => {
                let num_names = names.len();
                let num_expressions = expressions.len();
                let expression_list_can_be_multres =
                    Self::expression_list_can_be_multres(&expressions);
                if expression_list_can_be_multres {
                    // Pre-initialize any values beyond (num_expressions - 1 - num_names) to `nil`,
                    // as they may end up never getting set. The `- 1` accounts for the last
                    // variable expression.
                    // For example, in `local a, b, c, d = 1, ...`, we'll:
                    // - Initialize the registers for b, c, and d to nil.
                    //   `LoadNil 0 Rb Rd`
                    // - Then compile the expression list, which ends up loading the actual values.
                    // - If `...` results in fewer than 3 values, c and d will still be correctly
                    //   initialized.
                    //
                    // local a, b = 1, ...
                    // exprs = 2, num_names = 2, from = 1
                    // local a = ...
                    // exprs = 1, num_names = 1, from = 0
                    // local a = 1, 2, 3, ...
                    // exprs = 4, num_names = 1, don't
                    let num_non_multres_expressions = expressions.len() - 1;
                    if num_non_multres_expressions < num_names {
                        let preinit_from = (num_names - num_non_multres_expressions) as u8;
                        let preinit_to = num_names as u8;
                        self.chunk.push_bytecode(
                            Bytecode::LoadNil {
                                from_register: current_register + preinit_from,
                                to_register: current_register + preinit_to - 1,
                            },
                            None,
                        );
                    }
                } else if num_expressions == 0 {
                    // Then just `LoadNil` all names.
                    self.chunk.push_bytecode(
                        Bytecode::LoadNil {
                            from_register: current_register,
                            to_register: current_register + num_names as u8 - 1,
                        },
                        None,
                    );
                }

                self.compile_expression_list(expressions)?;

                for (index, name) in names.into_iter().enumerate() {
                    let name_register = current_register + index as u8;
                    if index >= num_expressions
                        && !expression_list_can_be_multres
                        && num_expressions > 0
                    {
                        // Opposite of above, if it's not multres and we didn't pre-initialize `nil`
                        // values, and there are fewer expressions than local names, set the value
                        // here.
                        // For example, in `local a, b = 1`:
                        // - We compile the expression list, which loads `1` into `Ra`.
                        // - Then for `b`, we need to set it to `nil`.
                        self.push_load_nil(Some(name.span));
                    }
                    self.add_local(name.node.name.node.0, name_register, Some(name.span));

                    match name.node.attribute {
                        Some(TokenTree {
                            node: LocalAttribute::Const,
                            ..
                        }) => {
                            self.chunk.push_bytecode(
                                Bytecode::SetLocalAttr {
                                    register: name_register,
                                    attr_index: LuaVariableAttribute::Constant,
                                },
                                Some(span),
                            );
                        }
                        Some(TokenTree {
                            node: LocalAttribute::Close,
                            ..
                        }) => {
                            self.chunk.push_bytecode(
                                Bytecode::SetLocalAttr {
                                    register: name_register,
                                    attr_index: LuaVariableAttribute::ToBeClosed,
                                },
                                Some(span),
                            );
                        }
                        None => {}
                    }
                }

                self.frames
                    .last_mut()
                    .unwrap()
                    .release_registers_from(current_register + num_names as u8);

                BlockResult::new()
            }
            Statement::LocalFunctionDeclaration(name, function_def) => {
                // The statement `local function f () body end` translates to `local f; f =
                // function () body end`, not to `local f = function () body end` (This only makes
                // a difference when the body of the function contains references to f.)
                let name_r = self.push_load_nil(Some(name.span));
                self.add_local(name.node.0, name_r, Some(name.span));

                self.compile_function_def(function_def, Some(name_r))?;

                self.frames
                    .last_mut()
                    .unwrap()
                    .release_registers_from(name_r + 1);

                BlockResult::new()
            }
            Statement::Assignment { varlist, explist } => {
                self.compile_expression_list(explist)?;

                for (index, variable) in varlist.into_iter().enumerate() {
                    match variable.node {
                        Variable::Name(name) => {
                            self.variable(
                                name,
                                VariableMode::Write {
                                    value_register: current_register + index as u8,
                                },
                            );
                        }
                        Variable::Indexed(target, target_index) => {
                            // `SetTable` expects a stack head of:
                            // [table, index, value]
                            // We need to calculate the value first above, as it e.g. refer to the
                            // old value of `table[index]`. So we push the index, and then the
                            // table:
                            // [value, index, table]
                            // And then we swap the value and the table:
                            // SWAP 2
                            // [value, table, index]
                            let target_r = self
                                .compile_prefix_expression(
                                    *target,
                                    ExpressionMode::Ref,
                                    ExpressionResultMode::Single,
                                )?
                                .get_register();
                            let index_r = self
                                .compile_expression(
                                    *target_index,
                                    ExpressionMode::Ref,
                                    ExpressionResultMode::Single,
                                )?
                                .get_register();
                            self.chunk.push_bytecode(
                                Bytecode::SetTable {
                                    table_register: target_r,
                                    key_register: index_r,
                                    value_register: current_register + index as u8,
                                },
                                Some(span),
                            );
                        }
                        Variable::Field(target, name) => {
                            // Same story as above, but with a string key
                            let target_r = self
                                .compile_prefix_expression(
                                    *target,
                                    ExpressionMode::Ref,
                                    ExpressionResultMode::Single,
                                )?
                                .get_register();
                            let const_index = self.get_const_index(LuaConst::String(name.node.0));
                            let const_r = self.frames.last_mut().unwrap().take_register();
                            self.chunk.push_bytecode(
                                Bytecode::LoadConst {
                                    register: const_r,
                                    const_index,
                                },
                                Some(span),
                            );

                            self.chunk.push_bytecode(
                                Bytecode::SetTable {
                                    table_register: target_r,
                                    key_register: const_r,
                                    value_register: current_register + index as u8,
                                },
                                Some(span),
                            );
                        }
                    }
                }

                self.frames
                    .last_mut()
                    .unwrap()
                    .release_registers_from(current_register);

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
                let condition_r = self
                    .compile_expression(
                        condition,
                        ExpressionMode::Ref,
                        ExpressionResultMode::Single,
                    )?
                    .get_register();

                // Jump to the end of the block if the condition is false
                let jmp_false_addr = self.chunk.push_undecided_jump(
                    JumpToUndecidedAddress::JmpFalse {
                        condition_register: condition_r,
                    },
                    None,
                );

                // Release the condition register
                self.frames
                    .last_mut()
                    .unwrap()
                    .release_registers_from(current_register);

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
                if !block_has_return && (!else_ifs.is_empty() || else_block.is_some()) {
                    jmp_end_addrs.push(
                        self.chunk
                            .push_undecided_jump(JumpToUndecidedAddress::Jmp, None),
                    );
                }

                // Right before the else/elseifs, go here if the condition was false
                self.chunk.patch_addr_placeholder(jmp_false_addr);
                for else_if in else_ifs {
                    // Push the new condition
                    let condition_r = self
                        .compile_expression(
                            else_if.node.condition,
                            ExpressionMode::Ref,
                            ExpressionResultMode::Single,
                        )?
                        .get_register();
                    // Jump to the end of the block if the condition is false
                    let jmp_false_addr = self.chunk.push_undecided_jump(
                        JumpToUndecidedAddress::JmpFalse {
                            condition_register: condition_r,
                        },
                        None,
                    );
                    // Release the condition register
                    self.frames
                        .last_mut()
                        .unwrap()
                        .release_registers_from(condition_r);
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
                        jmp_end_addrs.push(
                            self.chunk
                                .push_undecided_jump(JumpToUndecidedAddress::Jmp, None),
                        );
                    }
                    // Right before the else/elseifs, go here if the condition was false
                    self.chunk.patch_addr_placeholder(jmp_false_addr);
                }
                let else_jump_addr = if let Some(else_block) = else_block {
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
                        Some(
                            self.chunk
                                .push_undecided_jump(JumpToUndecidedAddress::Jmp, None),
                        )
                    } else {
                        None
                    }
                } else {
                    None
                };
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
                // let locals = self
                //     .frames
                //     .last()
                //     .expect("frames is not empty")
                //     .locals
                //     .iter()
                //     .filter(|local| local.depth >= options.loop_scope_depth)
                //     .count();

                // Jump past the rest of the loop (sort of, we do some intermediate jumps to clean
                // up all locals along the way, both in the scope of the loop itself, and any
                // underlying ones such as if statements).
                let break_jump = self
                    .chunk
                    .push_undecided_jump(JumpToUndecidedAddress::Jmp, Some(span));

                BlockResult::new().with_breaks(vec![BreakJump { addr: break_jump }])
            }
            Statement::Repeat { block, condition } => {
                let start_addr = self.chunk.get_current_addr();

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

                let expr_r = self
                    .compile_expression(
                        condition,
                        ExpressionMode::Ref,
                        ExpressionResultMode::Single,
                    )?
                    .get_register();
                self.chunk.push_bytecode(
                    Bytecode::JmpFalse {
                        condition_register: expr_r,
                        address: start_addr,
                    },
                    None,
                );

                self.end_scope()?;

                for break_ in block_result.breaks {
                    self.chunk.patch_addr_placeholder(break_.addr);
                }

                BlockResult::new()
            }
            Statement::While { condition, block } => {
                let start_addr = self.chunk.get_current_addr();
                let expr_r = self
                    .compile_expression(
                        condition,
                        ExpressionMode::Ref,
                        ExpressionResultMode::Single,
                    )?
                    .get_register();
                let jmp_false_addr = self.chunk.push_undecided_jump(
                    JumpToUndecidedAddress::JmpFalse {
                        condition_register: expr_r,
                    },
                    None,
                );
                self.frames
                    .last_mut()
                    .unwrap()
                    .release_registers_from(current_register);

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

                self.chunk.push_bytecode(
                    Bytecode::Jmp {
                        address: start_addr,
                    },
                    None,
                );

                let jmps: Vec<_> = block_result
                    .breaks
                    .into_iter()
                    .map(|jump| {
                        // Skip over the extra pops here for non-breaks
                        let jmp = self
                            .chunk
                            .push_undecided_jump(JumpToUndecidedAddress::Jmp, None);

                        // Pop the locals defined in the loop before the break
                        self.chunk.patch_addr_placeholder(jump.addr);

                        jmp
                    })
                    .collect();
                for addr in jmps {
                    self.chunk.patch_addr_placeholder(addr);
                }

                let jmp_exit_loop_addr = self
                    .chunk
                    .push_undecided_jump(JumpToUndecidedAddress::Jmp, None);
                self.chunk.patch_addr_placeholder(jmp_false_addr);
                // Pop the loop condition value
                // self.chunk.push_instruction(Instruction::Pop, None);
                self.chunk.patch_addr_placeholder(jmp_exit_loop_addr);

                BlockResult::new()
            }
            Statement::For { condition, block } => {
                let loop_scope_depth = self.begin_scope();

                let (continue_addr, jmp_exit_loop_addr) = match condition.node {
                    ForCondition::NumericFor(numeric_for) => {
                        let NumericFor {
                            name,
                            initial,
                            limit,
                            step,
                        } = *numeric_for;

                        // FIXME: Coerce all to float if initial and step are floats

                        // let limit_r = self.frames.last_mut().unwrap().take_register();
                        // let step_r = self.frames.last_mut().unwrap().take_register();
                        // let limit_r = self.frames.last_mut().unwrap().take_register();

                        // Define the initial value and variable
                        let ident_r = self
                            .compile_expression(
                                initial,
                                ExpressionMode::Copy,
                                ExpressionResultMode::Single,
                            )?
                            .get_register();
                        self.add_local(name.node.0, ident_r, Some(name.span));

                        // Create fake variables to hold the limit and step. Note that we use
                        // variable names that are invalid in Lua to avoid conflicts with user
                        // variables.
                        let limit_r = self
                            .compile_expression(
                                limit,
                                ExpressionMode::Copy,
                                ExpressionResultMode::Single,
                            )?
                            .get_register();
                        self.add_local(b"#limit".to_vec(), limit_r, None);

                        let step_r = if let Some(step) = step {
                            let step_span = step.span;
                            let step_r = self
                                .compile_expression(
                                    step,
                                    ExpressionMode::Copy,
                                    ExpressionResultMode::Single,
                                )?
                                .get_register();
                            self.add_local(b"#step".to_vec(), step_r, None);

                            // Raise an error if the step value is equal to zero
                            let zero_r = self
                                .compile_load_literal(Literal::Number(Number::Integer(0)), None);
                            self.chunk.push_bytecode(
                                Bytecode::Eq {
                                    dest_register: zero_r,
                                    left_register: step_r,
                                    right_register: zero_r,
                                },
                                None,
                            );
                            let jmp_false_addr = self.chunk.push_undecided_jump(
                                JumpToUndecidedAddress::JmpFalse {
                                    condition_register: zero_r,
                                },
                                None,
                            );

                            // Raise error
                            self.chunk.push_bytecode(
                                Bytecode::Error {
                                    code: RuntimeError::ForLoopLimitIsZero,
                                },
                                Some(step_span),
                            );

                            // Release `zero_r`.
                            self.frames.last_mut().unwrap().release_register();

                            // Otherwise, move past it
                            self.chunk.patch_addr_placeholder(jmp_false_addr);
                            step_r
                        } else {
                            let step_r = self
                                .compile_load_literal(Literal::Number(Number::Integer(1)), None);
                            self.add_local(b"#step".to_vec(), step_r, None);
                            step_r
                        };

                        // Create a "variable" to remember whether this is a decreasing loop.
                        // TODO: Optimization, if the step is statically known (e.g. the default of
                        // 1, or a constant integer value), we don't need to do any of this.
                        let zero_r =
                            self.compile_load_literal(Literal::Number(Number::Integer(0)), None);
                        let decreasing_r = zero_r; // We can reuse this slot
                        self.add_local(b"#decreasing".to_vec(), decreasing_r, None);
                        self.chunk.push_bytecode(
                            Bytecode::Lt {
                                dest_register: decreasing_r,
                                left_register: step_r,
                                right_register: zero_r,
                            },
                            None,
                        );

                        // Label to jump back to the start of the loop
                        let condition_addr = self.chunk.get_current_addr();

                        // Evaluate the limit
                        // Choose either "<=" or ">=" depending on whether the loop is increasing
                        // or decreasing.
                        let jmp_decreasing = self.chunk.push_undecided_jump(
                            JumpToUndecidedAddress::JmpTrue {
                                condition_register: decreasing_r,
                            },
                            None,
                        );

                        let limit_eval_r = self.frames.last_mut().unwrap().take_register();

                        // Loop is increasing, check if ident <= limit
                        self.chunk.push_bytecode(
                            Bytecode::Le {
                                dest_register: limit_eval_r,
                                left_register: ident_r,
                                right_register: limit_r,
                            },
                            None,
                        );
                        let jmp_after_le = self
                            .chunk
                            .push_undecided_jump(JumpToUndecidedAddress::Jmp, None);

                        self.chunk.patch_addr_placeholder(jmp_decreasing);

                        // Loop is decreasing, check if limit <= ident
                        self.chunk.push_bytecode(
                            Bytecode::Le {
                                dest_register: limit_eval_r,
                                left_register: limit_r,
                                right_register: ident_r,
                            },
                            None,
                        );

                        self.chunk.patch_addr_placeholder(jmp_after_le);

                        let jmp_false_addr = self.chunk.push_undecided_jump(
                            JumpToUndecidedAddress::JmpFalse {
                                condition_register: limit_eval_r,
                            },
                            None,
                        );

                        // Jump into the loop
                        let inside_block_jump = self
                            .chunk
                            .push_undecided_jump(JumpToUndecidedAddress::Jmp, None);

                        // Compile the step
                        let step_addr = self.chunk.get_current_addr();
                        self.chunk.push_bytecode(
                            Bytecode::Add {
                                dest_register: ident_r,
                                left_register: ident_r,
                                right_register: step_r,
                            },
                            None,
                        );

                        // After step, jump to the condition
                        self.chunk.push_bytecode(
                            Bytecode::Jmp {
                                address: condition_addr,
                            },
                            None,
                        );

                        // Path the inside block jump
                        self.chunk.patch_addr_placeholder(inside_block_jump);

                        // Release the limit_eval_r register.
                        self.frames.last_mut().unwrap().release_register();

                        (step_addr, jmp_false_addr)
                    }
                    ForCondition::GenericFor { names, expressions } => {
                        // Evaluate the expressions and keep no more than 4
                        // Pre-fill `nil` values as well
                        self.chunk.push_bytecode(
                            Bytecode::LoadNil {
                                from_register: current_register,
                                to_register: current_register + 3,
                            },
                            None,
                        );
                        self.compile_expression_list(expressions)?;
                        self.frames
                            .last_mut()
                            .unwrap()
                            .set_register_index(current_register + 4);

                        // Save the result as locals: the iterator function, the state, the initial
                        // value for the control variable (which is the first name).
                        let iterator_local = current_register;
                        let state_local = current_register + 1;
                        let control_local = current_register + 2;
                        let closing_local = current_register + 3;
                        self.add_local(b"#iterator".to_vec(), iterator_local, None);
                        self.add_local(b"#state".to_vec(), state_local, None);
                        // NOTE: The syntax ensures there is at least one name
                        self.add_local(names[0].node.0.clone(), control_local, Some(names[0].span));
                        self.add_local(b"#closing".to_vec(), closing_local, None);
                        self.chunk.push_bytecode(
                            Bytecode::SetLocalAttr {
                                register: closing_local,
                                attr_index: LuaVariableAttribute::ToBeClosed,
                            },
                            None,
                        );

                        // Initialize the other variables to nil
                        for name in &names[1..] {
                            let register = self.push_load_nil(Some(name.span));
                            self.add_local(name.node.0.clone(), register, Some(name.span));
                        }

                        // Call the iterator function
                        let step_addr = self.chunk.get_current_addr();
                        let frame = self.frames.last_mut().unwrap();
                        let func_r = frame.take_register();
                        let state_r = frame.take_register();
                        let control_r = frame.take_register();
                        self.chunk.push_bytecode(
                            Bytecode::Mov {
                                dest_register: func_r,
                                src_register: iterator_local,
                            },
                            None,
                        );
                        self.chunk.push_bytecode(
                            Bytecode::Mov {
                                dest_register: state_r,
                                src_register: state_local,
                            },
                            None,
                        );
                        self.chunk.push_bytecode(
                            Bytecode::Mov {
                                dest_register: control_r,
                                src_register: control_local,
                            },
                            None,
                        );
                        self.chunk.push_bytecode(
                            Bytecode::Call {
                                func_register: func_r,
                                result_mode: ExpressionResultMode::Multiple,
                                num_args: 2,
                            },
                            None,
                        );

                        // Then store each result in the corresponding local
                        for (index, name) in names.into_iter().enumerate() {
                            let local_register = self
                                .resolve_local(&name.node.0)
                                .expect("parser ensures that variable is in scope");
                            self.chunk.push_bytecode(
                                Bytecode::Mov {
                                    dest_register: local_register,
                                    src_register: func_r + index as u8,
                                },
                                Some(span),
                            );
                        }

                        self.frames
                            .last_mut()
                            .unwrap()
                            .release_registers_from(func_r);

                        // Now we need to evaluate whether any value was returned. If not, we
                        // should break out of the loop.
                        let nil_r = self.push_load_nil(None);
                        self.chunk.push_bytecode(
                            Bytecode::Eq {
                                dest_register: nil_r,
                                left_register: control_local,
                                right_register: nil_r,
                            },
                            None,
                        );
                        let jmp_true_addr = self.chunk.push_undecided_jump(
                            JumpToUndecidedAddress::JmpTrue {
                                condition_register: nil_r,
                            },
                            None,
                        );

                        self.frames.last_mut().unwrap().release_register();

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
                self.chunk.push_bytecode(
                    Bytecode::Jmp {
                        address: continue_addr,
                    },
                    None,
                );

                // Patch the false condition jump
                self.chunk.patch_addr_placeholder(jmp_exit_loop_addr);

                self.end_scope()?;

                // Patch any break jumps
                // let jmps: Vec<_> = block_result
                //     .breaks
                //     .into_iter()
                //     .map(|jump| {
                //         // Skip over the extra pops here for non-breaks
                //         self.chunk.push_instruction(Instruction::Jmp, None);
                //         let jmp = self.chunk.push_addr_placeholder();

                // // Pop the locals defined in the loop before the break
                // self.chunk.patch_addr_placeholder(jump.addr);
                // for _ in 0..jump.locals {
                //     self.chunk.push_instruction(Instruction::Pop, None);
                // }

                //         jmp
                //     })
                //     .collect();
                // for addr in jmps {
                //     self.chunk.patch_addr_placeholder(addr);
                // }

                // Patch any break jumps
                for jump in block_result.breaks {
                    self.chunk.patch_addr_placeholder(jump.addr);
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
                        self.chunk.push_bytecode(
                            Bytecode::JmpClose {
                                address: label.addr,
                                close_from_register: label.current_register,
                            },
                            Some(span),
                        );
                    }
                    None => {
                        // Add to frame to resolve the jump later
                        let addr = self
                            .chunk
                            .push_undecided_jump(JumpToUndecidedAddress::Jmp, None);

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
                        current_register: frame.register_index,
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
            self.chunk.push_bytecode(Bytecode::Return0, None);
            return Ok(());
        }

        let (num_returns, is_variable) = (
            return_statement.len(),
            Self::expression_list_can_be_multres(&return_statement),
        );

        let register = self.frames.last().unwrap().register_index;
        let expression_list_length = self.compile_expression_list(return_statement)?;

        if num_returns == 1 && !is_variable {
            self.chunk.push_bytecode(
                Bytecode::Return1 {
                    src_register: register,
                },
                None,
            );
        } else {
            self.chunk.push_bytecode(
                match expression_list_length {
                    ExpressionListLength::Constant { from_register } => Bytecode::Return {
                        from_register,
                        num_values: num_returns as u8,
                    },
                    ExpressionListLength::MultRes { from_register } => Bytecode::ReturnM {
                        from_register,
                        num_fixed_values: num_returns as u8 - 1,
                    },
                },
                None,
            );
        }

        Ok(())
    }

    fn compile_function_call(
        &mut self,
        function_call: TokenTree<FunctionCall>,
        result_mode: ExpressionResultMode,
    ) -> crate::Result<u8> {
        // If this is a method call, we need to push the object as the first argument
        let (function_register, self_arg_n) =
            if let Some(method_name) = function_call.node.method_name {
                let func_r = self.frames.last_mut().unwrap().take_register();
                let table_r = self
                    .compile_prefix_expression(
                        *function_call.node.function,
                        ExpressionMode::Copy,
                        ExpressionResultMode::Single,
                    )?
                    .get_register();

                let method_name_const_index =
                    self.get_const_index(LuaConst::String(method_name.node.0));
                let method_name_r = self.frames.last_mut().unwrap().take_register();
                self.chunk.push_bytecode(
                    Bytecode::LoadConst {
                        register: method_name_r,
                        const_index: method_name_const_index,
                    },
                    Some(function_call.span),
                );

                self.chunk.push_bytecode(
                    Bytecode::GetTable {
                        dest_register: func_r,
                        table_register: table_r,
                        key_register: method_name_r,
                    },
                    Some(Span::new(function_call.span.start, method_name.span.end)),
                );

                self.frames
                    .last_mut()
                    .unwrap()
                    .release_registers_from(method_name_r);

                // Account for one extra parameter (the `self` argument)
                (func_r, 1)
            } else {
                let function_register = self
                    .compile_prefix_expression(
                        *function_call.node.function,
                        ExpressionMode::Copy,
                        ExpressionResultMode::Single,
                    )?
                    .get_register();

                (function_register, 0)
            };

        let num_args = function_call.node.args.node.len() + self_arg_n;
        let expression_list_length = self.compile_expression_list(function_call.node.args.node)?;

        self.chunk.push_call_instruction(
            function_register,
            matches!(expression_list_length, ExpressionListLength::MultRes { .. }),
            result_mode,
            match expression_list_length {
                ExpressionListLength::Constant { .. } => num_args as u8,
                // Subtract one, because the last argument will be accounted for via multres
                ExpressionListLength::MultRes { .. } => num_args as u8 - 1,
            },
            Some(function_call.span),
        );

        self.frames
            .last_mut()
            .unwrap()
            .release_registers_from(function_register + 1);

        Ok(function_register)
    }

    fn compile_tail_call(&mut self, function_call: TokenTree<FunctionCall>) -> crate::Result<bool> {
        // Check if this function call refers to the current function
        let frame = self.frames.last_mut().expect("frames is not empty");
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

        let mut num_args = function_call.node.args.node.len();
        let (parameters_register, is_self_call) = if frame.method_name.is_some() {
            // Load the `self` argument into the first parameter register.
            let register = frame.take_register();
            self.chunk.push_bytecode(
                Bytecode::Mov {
                    dest_register: register,
                    // NOTE: `self` is always in the first register of method-style functions.
                    src_register: 0,
                },
                Some(function_call.span),
            );
            num_args += 1;
            (register, true)
        } else {
            (frame.register_index, false)
        };
        let expression_list_length = self.compile_expression_list(function_call.node.args.node)?;
        if !is_self_call {
            debug_assert_eq!(parameters_register, expression_list_length.get_register());
        }

        let parameters_register = if is_self_call {
            parameters_register
        } else {
            expression_list_length.get_register()
        };

        self.chunk.push_callt_instruction(
            parameters_register,
            matches!(expression_list_length, ExpressionListLength::MultRes { .. }),
            // TODO: Is this right? Does it matter?
            ExpressionResultMode::Multiple,
            match expression_list_length {
                ExpressionListLength::Constant { .. } => num_args as u8,
                // Subtract one, because the last argument will be accounted for via multres
                ExpressionListLength::MultRes { .. } => num_args as u8 - 1,
            },
            Some(function_call.span),
        );

        Ok(true)
    }

    fn compile_function_def(
        &mut self,
        function_def: TokenTree<FunctionDef>,
        closure_r: Option<u8>,
    ) -> crate::Result<u8> {
        // We'll issue the bytecode inline here, but jump over it at runtime
        let jmp_over_func_addr = self
            .chunk
            .push_undecided_jump(JumpToUndecidedAddress::Jmp, None);

        // Compile the function
        let func_addr = self.chunk.get_current_addr();
        self.begin_frame(
            function_def.node.name.clone(),
            function_def.node.method_name.clone(),
        );
        self.begin_scope();

        // Define the function arguments
        let parameter_count = function_def.node.parameters.len();
        for (index, parameter) in function_def.node.parameters.into_iter().enumerate() {
            self.add_local(parameter.node.0, index as u8, Some(parameter.span));
        }

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
            self.chunk.push_bytecode(Bytecode::Return0, None);
        }

        // Jump over the function
        self.chunk.patch_addr_placeholder(jmp_over_func_addr);

        // Save the function definition
        let const_index = self
            .vm
            .register_const(LuaConst::Function(LuaFunctionDefinition {
                name: function_def
                    .node
                    .method_name
                    .or(function_def.node.name)
                    .map(|mut name| {
                        name.extend_from_slice(b"()");
                        name
                    }),
                chunk: self.chunk_index,
                ip: func_addr,
                upvalues: frame.upvalues.len(),
                num_params: parameter_count as u8,
                has_varargs: function_def.node.varargs.is_some(),
                max_registers: frame.max_registers,
            }));
        let closure_r =
            closure_r.unwrap_or_else(|| self.frames.last_mut().unwrap().take_register());
        self.chunk.push_bytecode(
            Bytecode::LoadClosure {
                register: closure_r,
                const_index,
                upvalues: frame.upvalues,
            },
            Some(function_def.span),
        );

        Ok(closure_r)
    }

    fn compile_expression_list(
        &mut self,
        expressions: Vec<TokenTree<Expression>>,
    ) -> crate::Result<ExpressionListLength> {
        let num_expressions = expressions.len();
        let from_register = self.frames.last().unwrap().register_index;
        let mut multres = false;
        for (i, expression) in expressions.into_iter().enumerate() {
            let is_last = i == num_expressions - 1;
            if matches!(
                self.compile_expression(
                    expression,
                    ExpressionMode::Copy,
                    if is_last {
                        ExpressionResultMode::Multiple
                    } else {
                        ExpressionResultMode::Single
                    },
                )?,
                ExpressionResult::Multiple { .. }
            ) {
                multres = true;
            }
        }

        Ok(if multres {
            ExpressionListLength::MultRes { from_register }
        } else {
            ExpressionListLength::Constant { from_register }
        })
    }

    fn compile_expression(
        &mut self,
        expression: TokenTree<Expression>,
        expression_mode: ExpressionMode,
        result_mode: ExpressionResultMode,
    ) -> crate::Result<ExpressionResult> {
        Ok(match expression.node {
            Expression::Literal(literal) => {
                let register = self.compile_load_literal(literal.node, Some(literal.span));

                ExpressionResult::Single { register }
            }
            Expression::BinaryOp { op, lhs, rhs } => {
                let register = match &op.node {
                    BinaryOperator::And => {
                        let lhs_r = self
                            .compile_expression(
                                *lhs,
                                ExpressionMode::Copy,
                                ExpressionResultMode::Single,
                            )?
                            .get_register();
                        let jmp_false_addr = self.chunk.push_undecided_jump(
                            JumpToUndecidedAddress::JmpFalse {
                                condition_register: lhs_r,
                            },
                            None,
                        );

                        self.frames
                            .last_mut()
                            .unwrap()
                            .release_registers_from(lhs_r);

                        let rhs_r = self
                            .compile_expression(
                                *rhs,
                                ExpressionMode::Copy,
                                ExpressionResultMode::Single,
                            )?
                            .get_register();
                        self.chunk.patch_addr_placeholder(jmp_false_addr);

                        debug_assert_eq!(lhs_r, rhs_r, "AND rhs must reuse lhs register");

                        lhs_r
                    }
                    BinaryOperator::Or => {
                        let lhs_r = self
                            .compile_expression(
                                *lhs,
                                ExpressionMode::Copy,
                                ExpressionResultMode::Single,
                            )?
                            .get_register();
                        let jmp_true_addr = self.chunk.push_undecided_jump(
                            JumpToUndecidedAddress::JmpTrue {
                                condition_register: lhs_r,
                            },
                            None,
                        );

                        self.frames
                            .last_mut()
                            .unwrap()
                            .release_registers_from(lhs_r);

                        let rhs_r = self
                            .compile_expression(
                                *rhs,
                                ExpressionMode::Copy,
                                ExpressionResultMode::Single,
                            )?
                            .get_register();
                        self.chunk.patch_addr_placeholder(jmp_true_addr);

                        debug_assert_eq!(lhs_r, rhs_r, "OR rhs must reuse lhs register");

                        lhs_r
                    }
                    _ => {
                        let dest_r = self.frames.last_mut().unwrap().take_register();
                        let lhs_r = self
                            .compile_expression(
                                *lhs,
                                ExpressionMode::Ref,
                                ExpressionResultMode::Single,
                            )?
                            .get_register();
                        let rhs_r = self
                            .compile_expression(
                                *rhs,
                                ExpressionMode::Ref,
                                ExpressionResultMode::Single,
                            )?
                            .get_register();
                        self.compile_binary_operator(op, dest_r, lhs_r, rhs_r, expression.span);

                        self.frames
                            .last_mut()
                            .unwrap()
                            .release_registers_from(dest_r + 1);

                        dest_r
                    }
                };

                ExpressionResult::Single { register }
            }
            Expression::PrefixExpression(prefix_expression) => {
                self.compile_prefix_expression(prefix_expression, expression_mode, result_mode)?
            }
            Expression::UnaryOp { op, rhs } => {
                let current_r = self.frames.last().unwrap().register_index;
                let expr_r = self
                    .compile_expression(*rhs, ExpressionMode::Ref, ExpressionResultMode::Single)?
                    .get_register();
                let dest_r = if current_r == expr_r {
                    expr_r
                } else {
                    self.frames.last_mut().unwrap().take_register()
                };
                self.compile_unary_operator(op, expression.span, dest_r, expr_r);

                ExpressionResult::Single { register: dest_r }
            }
            Expression::FunctionDef(function_def) => {
                let register = self.compile_function_def(function_def, None)?;

                ExpressionResult::Single { register }
            }
            Expression::TableConstructor(table) => {
                let register = self.compile_table_constructor(table)?;

                ExpressionResult::Single { register }
            }
            Expression::Ellipsis => {
                let register = self.frames.last_mut().unwrap().take_register();
                self.chunk.push_bytecode(
                    Bytecode::LoadVararg {
                        dest_register: register,
                        single_value: matches!(result_mode, ExpressionResultMode::Single),
                    },
                    Some(expression.span),
                );

                result_mode.into_result(register)
            }
        })
    }

    fn compile_prefix_expression(
        &mut self,
        prefix_expression: TokenTree<PrefixExpression>,
        expression_mode: ExpressionMode,
        result_mode: ExpressionResultMode,
    ) -> crate::Result<ExpressionResult> {
        Ok(match prefix_expression.node {
            PrefixExpression::FunctionCall(function_call) => {
                let register = self.compile_function_call(function_call, result_mode)?;

                result_mode.into_result(register)
            }
            PrefixExpression::Parenthesized(expression) => {
                self.compile_expression(*expression, expression_mode, ExpressionResultMode::Single)?
            }
            PrefixExpression::Variable(variable) => {
                let register = match variable.node {
                    Variable::Name(name) => self.variable(
                        name,
                        match expression_mode {
                            ExpressionMode::Ref => VariableMode::Ref,
                            ExpressionMode::Copy => VariableMode::Read,
                        },
                    ),
                    Variable::Indexed(prefix, index) => {
                        let dest_r = self.frames.last_mut().unwrap().take_register();
                        let table_r = self
                            .compile_prefix_expression(
                                *prefix,
                                ExpressionMode::Ref,
                                ExpressionResultMode::Single,
                            )?
                            .get_register();
                        let index_r = self
                            .compile_expression(
                                *index,
                                ExpressionMode::Ref,
                                ExpressionResultMode::Single,
                            )?
                            .get_register();
                        self.chunk.push_bytecode(
                            Bytecode::GetTable {
                                dest_register: dest_r,
                                table_register: table_r,
                                key_register: index_r,
                            },
                            Some(prefix_expression.span),
                        );
                        self.frames
                            .last_mut()
                            .unwrap()
                            .release_registers_from(dest_r + 1);

                        dest_r
                    }
                    Variable::Field(prefix, name) => {
                        let current_r = self.frames.last().unwrap().register_index;
                        let table_r = self
                            .compile_prefix_expression(
                                *prefix,
                                ExpressionMode::Ref,
                                ExpressionResultMode::Single,
                            )?
                            .get_register();

                        let const_index = self.get_const_index(LuaConst::String(name.node.0));
                        let name_r = self.frames.last_mut().unwrap().take_register();
                        self.chunk.push_bytecode(
                            Bytecode::LoadConst {
                                register: name_r,
                                const_index,
                            },
                            Some(name.span),
                        );
                        let dest_r = if table_r == current_r {
                            table_r
                        } else {
                            name_r
                        };
                        self.chunk.push_bytecode(
                            Bytecode::GetTable {
                                dest_register: dest_r,
                                table_register: table_r,
                                key_register: name_r,
                            },
                            Some(prefix_expression.span),
                        );

                        self.frames
                            .last_mut()
                            .unwrap()
                            .release_registers_from(dest_r + 1);

                        dest_r
                    }
                };

                ExpressionResult::Single { register }
            }
        })
    }

    fn compile_table_constructor(
        &mut self,
        table: TokenTree<TableConstructor>,
    ) -> crate::Result<u8> {
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
            let table_r = self.frames.last_mut().unwrap().take_register();
            self.chunk.push_bytecode(
                Bytecode::LoadConst {
                    register: table_r,
                    const_index,
                },
                Some(table.span),
            );

            return Ok(table_r);
        }

        let table_r = self.frames.last_mut().unwrap().take_register();
        self.chunk.push_bytecode(
            Bytecode::NewTable {
                dest_register: table_r,
            },
            Some(table.span),
        );

        let num_fields = table.node.fields.len();
        let mut previous_was_value_field = false;
        let mut num_values = 0;
        let mut previous_was_multires = false;
        for (i, field) in table.node.fields.into_iter().enumerate() {
            if previous_was_value_field && !matches!(&field.node, Field::Value(_)) {
                // TODO: Do we specify the starting register here for the values? Or is it ALWAYS
                // table_r + 1?
                self.chunk.push_bytecode(
                    if !previous_was_multires {
                        Bytecode::AppendToTable {
                            table_register: table_r,
                            num_values,
                        }
                    } else {
                        Bytecode::AppendToTableM {
                            table_register: table_r,
                            num_values,
                        }
                    },
                    Some(field.span),
                );
                previous_was_value_field = false;
                previous_was_multires = false;
            }

            match field.node {
                Field::Named(name, value) => {
                    let const_index = self.get_const_index(LuaConst::String(name.node.0));
                    let const_r = self.frames.last_mut().unwrap().take_register();
                    self.chunk.push_bytecode(
                        Bytecode::LoadConst {
                            register: const_r,
                            const_index,
                        },
                        Some(field.span),
                    );
                    let value_r = self
                        .compile_expression(
                            value,
                            ExpressionMode::Ref,
                            ExpressionResultMode::Single,
                        )?
                        .get_register();
                    self.chunk.push_bytecode(
                        Bytecode::SetTable {
                            table_register: table_r,
                            key_register: const_r,
                            value_register: value_r,
                        },
                        Some(field.span),
                    );

                    self.frames
                        .last_mut()
                        .unwrap()
                        .release_registers_from(const_r);
                }
                Field::Indexed(index_expression, value) => {
                    let index_r = self
                        .compile_expression(
                            index_expression,
                            ExpressionMode::Ref,
                            ExpressionResultMode::Single,
                        )?
                        .get_register();
                    let value_r = self
                        .compile_expression(
                            value,
                            ExpressionMode::Ref,
                            ExpressionResultMode::Single,
                        )?
                        .get_register();
                    self.chunk.push_bytecode(
                        Bytecode::SetTable {
                            table_register: table_r,
                            key_register: index_r,
                            value_register: value_r,
                        },
                        Some(field.span),
                    );

                    self.frames
                        .last_mut()
                        .unwrap()
                        .release_registers_from(table_r + 1);
                }
                Field::Value(value) => {
                    previous_was_value_field = true;

                    let is_last_field = i == num_fields - 1;
                    if matches!(
                        self.compile_expression(
                            value,
                            ExpressionMode::Copy,
                            if is_last_field {
                                ExpressionResultMode::Multiple
                            } else {
                                ExpressionResultMode::Single
                            },
                        )?,
                        ExpressionResult::Multiple { .. }
                    ) {
                        previous_was_multires = true;
                    } else {
                        num_values += 1;
                    }
                }
            }
        }

        if previous_was_value_field {
            self.chunk.push_bytecode(
                if !previous_was_multires {
                    Bytecode::AppendToTable {
                        table_register: table_r,
                        num_values,
                    }
                } else {
                    Bytecode::AppendToTableM {
                        table_register: table_r,
                        num_values,
                    }
                },
                None,
            );
        }

        self.frames
            .last_mut()
            .unwrap()
            .release_registers_from(table_r + 1);

        Ok(table_r)
    }

    fn compile_load_literal(&mut self, literal: Literal, span: Option<Span>) -> u8 {
        let register = self.frames.last_mut().unwrap().take_register();
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

        self.chunk.push_bytecode(
            Bytecode::LoadConst {
                register,
                const_index,
            },
            span,
        );

        register
    }

    fn compile_binary_operator(
        &mut self,
        op: TokenTree<BinaryOperator>,
        dest_register: u8,
        left_register: u8,
        right_register: u8,
        span: Span,
    ) {
        self.chunk.push_bytecode(
            match op.node {
                // Arithmetic
                BinaryOperator::Add => Bytecode::Add {
                    dest_register,
                    left_register,
                    right_register,
                },
                BinaryOperator::Sub => Bytecode::Sub {
                    dest_register,
                    left_register,
                    right_register,
                },
                BinaryOperator::Mul => Bytecode::Mul {
                    dest_register,
                    left_register,
                    right_register,
                },
                BinaryOperator::Div => Bytecode::Div {
                    dest_register,
                    left_register,
                    right_register,
                },
                BinaryOperator::Mod => Bytecode::Mod {
                    dest_register,
                    left_register,
                    right_register,
                },
                BinaryOperator::Pow => Bytecode::Pow {
                    dest_register,
                    left_register,
                    right_register,
                },
                BinaryOperator::FloorDiv => Bytecode::IDiv {
                    dest_register,
                    left_register,
                    right_register,
                },
                BinaryOperator::BitwiseAnd => Bytecode::Band {
                    dest_register,
                    left_register,
                    right_register,
                },
                BinaryOperator::BitwiseOr => Bytecode::Bor {
                    dest_register,
                    left_register,
                    right_register,
                },
                BinaryOperator::BitwiseXor => Bytecode::Bxor {
                    dest_register,
                    left_register,
                    right_register,
                },
                BinaryOperator::ShiftLeft => Bytecode::Shl {
                    dest_register,
                    left_register,
                    right_register,
                },
                BinaryOperator::ShiftRight => Bytecode::Shr {
                    dest_register,
                    left_register,
                    right_register,
                },

                // Comparison
                BinaryOperator::Equal => Bytecode::Eq {
                    dest_register,
                    left_register,
                    right_register,
                },
                BinaryOperator::NotEqual => Bytecode::Ne {
                    dest_register,
                    left_register,
                    right_register,
                },
                BinaryOperator::LessThan => Bytecode::Lt {
                    dest_register,
                    left_register,
                    right_register,
                },
                BinaryOperator::LessThanOrEqual => Bytecode::Le {
                    dest_register,
                    left_register,
                    right_register,
                },
                // https://www.lua.org/manual/5.4/manual.html#3.4.4
                // A comparison a > b is translated to b < a and a >= b is translated to b <= a.
                BinaryOperator::GreaterThan => {
                    // std::mem::swap(&mut left_register, &mut right_register);
                    // self.chunk.push_instruction(Instruction::Lt, Some(span));
                    Bytecode::Lt {
                        dest_register,
                        left_register: right_register,
                        right_register: left_register,
                    }
                }
                BinaryOperator::GreaterThanOrEqual => {
                    // self.chunk.push_instruction(Instruction::Le, Some(span));
                    // std::mem::swap(&mut left_register, &mut right_register);
                    Bytecode::Le {
                        dest_register,
                        left_register: right_register,
                        right_register: left_register,
                    }
                }

                // Strings
                BinaryOperator::Concat => Bytecode::Concat {
                    dest_register,
                    left_register,
                    right_register,
                },

                // Logical
                BinaryOperator::And | BinaryOperator::Or => {
                    unreachable!(
                        "short-circuiting logical operators should be handled in compile_expression"
                    );
                }
            },
            Some(span),
        );
    }

    fn compile_unary_operator(
        &mut self,
        op: TokenTree<UnaryOperator>,
        span: Span,
        dest_register: u8,
        src_register: u8,
    ) {
        self.chunk.push_bytecode(
            match op.node {
                UnaryOperator::Neg => Bytecode::Neg {
                    dest_register,
                    src_register,
                },
                UnaryOperator::Not => Bytecode::Not {
                    dest_register,
                    src_register,
                },
                UnaryOperator::Length => Bytecode::Len {
                    dest_register,
                    src_register,
                },
                UnaryOperator::BitwiseNot => Bytecode::BNot {
                    dest_register,
                    src_register,
                },
            },
            Some(span),
        );
    }

    /// Check if the list of expressions could be "multres", i.e. result in a variable
    /// amount of values produced.
    fn expression_list_can_be_multres(expressions: &[TokenTree<Expression>]) -> bool {
        // The number of return values is variable if the last expression is
        // a function call or an ellipsis (things that can result in
        // multiple values themselves).
        expressions.last().is_some_and(|expr| match &expr.node {
            Expression::PrefixExpression(prefix_expr) => {
                matches!(&prefix_expr.node, PrefixExpression::FunctionCall(_))
            }
            Expression::Ellipsis => true,
            _ => false,
        })
    }

    fn variable(&mut self, name: TokenTree<Name>, mode: VariableMode) -> u8 {
        let span = name.span;
        let (bytecode, register) = if let Some(local) = self.resolve_local(&name.node.0) {
            match mode {
                VariableMode::Ref => {
                    return local;
                }
                VariableMode::Read => {
                    let register = self.frames.last_mut().unwrap().take_register();
                    (
                        Bytecode::Mov {
                            dest_register: register,
                            src_register: local,
                        },
                        register,
                    )
                }
                VariableMode::Write { value_register } => (
                    Bytecode::Mov {
                        dest_register: local,
                        src_register: value_register,
                    },
                    0,
                ),
            }
        } else if let Some(upvalue) = self.resolve_upvalue(&name.node.0) {
            match mode {
                VariableMode::Read | VariableMode::Ref => {
                    let register = self.frames.last_mut().unwrap().take_register();
                    (
                        Bytecode::GetUpval {
                            dest_register: register,
                            upval_index: upvalue,
                        },
                        register,
                    )
                }
                VariableMode::Write { value_register } => (
                    Bytecode::SetUpval {
                        upval_index: upvalue,
                        src_register: value_register,
                    },
                    0,
                ),
            }
        } else {
            let const_index = self.get_global_name_index(name.node.0.clone());
            match mode {
                VariableMode::Read | VariableMode::Ref => {
                    let register = self.frames.last_mut().unwrap().take_register();
                    (
                        Bytecode::GetGlobal {
                            dest_register: register,
                            name_index: const_index,
                        },
                        register,
                    )
                }
                VariableMode::Write { value_register } => (
                    Bytecode::SetGlobal {
                        src_register: value_register,
                        name_index: const_index,
                    },
                    0,
                ),
            }
        };

        self.chunk.push_bytecode(bytecode, Some(span));
        register
    }

    fn begin_frame(&mut self, name: Option<Vec<u8>>, method_name: Option<Vec<u8>>) {
        self.frames.push(Frame::new(name, method_name));
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
        current_frame
            .scope_starting_registers
            .push(current_frame.register_index);
        current_frame.scope_id_counter += 1;
        current_frame.scope_depth
    }

    fn end_scope(&mut self) -> crate::Result {
        let current_frame = self.frames.last_mut().unwrap();
        let scope_id = current_frame.scope_id();
        current_frame.scope_ids.pop();
        let parent_scope_id = current_frame.scope_id();
        current_frame.scope_depth -= 1;
        let starting_register = current_frame
            .scope_starting_registers
            .pop()
            .expect("there should be a starting register for the scope");
        current_frame.release_registers_from(starting_register);

        while !current_frame.locals.is_empty()
            && current_frame
                .locals
                .last()
                .as_ref()
                .is_some_and(|local| local.depth > current_frame.scope_depth)
        {
            current_frame.locals.pop();
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
                        // FIXME: This doesn't need to `pop` anything, but if any locals of this
                        // scope have the `ToBeClosed` attribute, we need to call their __close
                        // metamethod.
                        self.chunk.patch_addr_placeholder(goto.addr);
                        if !goto.to_end_of_scope {
                            goto.addr = self
                                .chunk
                                .push_undecided_jump(JumpToUndecidedAddress::Jmp, None);
                        }
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

    fn add_local(&mut self, name: Vec<u8>, register: u8, span: Option<Span>) {
        let current_frame = self.frames.last_mut().unwrap();
        current_frame.reserve_register(register);
        let local = Local {
            name: name.clone(),
            depth: current_frame.scope_depth,
            span,
        };

        let index = current_frame.locals.len() as u8;
        debug_assert!(
            index == register,
            "registers must be allocated sequentially (index: {index}, register: {register}), when adding local {}",
            String::from_utf8_lossy(&name),
        );
        current_frame.locals.push(local);
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
