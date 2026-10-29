use std::{
    collections::HashMap,
    path::PathBuf,
    sync::{Arc, RwLock},
};

use cranelift::{
    codegen::ir::{AbiParam, FuncRef, InstBuilder, Value, types},
    jit::JITBuilder,
    module::Linkage,
};
use cranelift::{frontend::FunctionBuilder, module::Module};
use cranelift::{frontend::FunctionBuilderContext, jit::JITModule};

use crate::{
    GlobalState,
    chunk::Chunk,
    compiler::{
        Compiler, Runnable,
        bytecode::BytecodeCompiler,
        frame::{BindingLocation, Frame, UpvalueLocation},
        goto::{Goto, Label},
        variable::{Local, Upvalue},
    },
    resolved_ast::{
        self, Block, Expression, Literal, Name, PrefixExpression, Statement, TokenTree, Variable,
    },
    token::Span,
    value::{JitLuaFunction, LuaString, LuaValue, LuaVariableAttribute},
};

/// Try Compile
macro_rules! tc {
    ($t:expr) => {{
        match $t? {
            Some(v) => v,
            None => {
                return Ok(None);
            }
        }
    }};
}

use super::CompiledChunk;

mod binary;
mod helpers;
mod int;

#[derive(Debug)]
pub(super) struct CraneliftCompiler {
    fallback: super::bytecode::BytecodeCompiler,
    global_state: Arc<RwLock<GlobalState>>,
    chunk: Chunk,
    method_name: Option<LuaString>,
    frame: Frame,
    to_close: Vec<u8>,
    gotos: HashMap<LuaString, Vec<Goto>>,
    labels: HashMap<LuaString, Label>,
    scope_depth: u8,
    scope_id_counter: usize,
    scope_ids: Vec<usize>,
    scope_starting_registers: Vec<u8>,
    register_index: u8,
    max_registers: u8,
    print_bytecode: bool,
}

impl Compiler for CraneliftCompiler {
    fn compile(&mut self, ast: resolved_ast::Chunk) -> crate::Result<Runnable> {
        match self.compile_chunk(ast)? {
            CompiledChunk::Bytecode(_) => todo!(),
            CompiledChunk::Jit(f) => Ok(Runnable::Jit(f)),
        }
    }

    fn compile_chunk(&mut self, ast: resolved_ast::Chunk) -> crate::Result<CompiledChunk> {
        dbg!(&ast.parameters);
        // self.chunk.num_params = ast.parameters.len() as u8;
        // for parameter in ast.parameters {
        //     let register = self.take_register();
        //     self.add_binding(parameter, register);
        // }
        // for capture in ast.captures {
        //     let binding = capture.binding();
        //     self.frame.upvalues.insert(
        //         binding.id,
        //         Upvalue {
        //             index: self.frame.upvalues.len() as u8,
        //             source: UpvalueLocation::Upvalue { index: 0 },
        //             attributes: binding.attributes,
        //             name: binding.name.clone(),
        //         },
        //     );
        // }
        match self.compile_body(ast.clone())? {
            Some(f) => Ok(CompiledChunk::Jit(f)),
            None => {
                // Fall back to regular bytecode compiler
                self.fallback.compile_chunk(ast)
            }
        }
    }
}

impl CraneliftCompilerInner<'_, '_> {
    fn add_binding(&mut self, name: &TokenTree<Name>, register: u8) {
        let binding = name.node.binding().clone();
        self.add_local(
            binding.name.clone(),
            register,
            binding.attributes,
            Some(name.span),
        );
        let local = self.frame.locals.last_mut().unwrap();
        local.binding = Some(binding.id);
        local.captured = binding.captured;
    }

    fn add_local(&mut self, name: LuaString, register: u8, attributes: u8, span: Option<Span>) {
        self.reserve_register(register);
        // let local_var_index = self.chunk.local_vars.len();
        // self.chunk.local_vars.push(LocalVarInfo {
        //     name: name.clone(),
        //     register,
        //     start_ip: self.chunk.get_current_addr() as usize,
        //     end_ip: usize::MAX,
        // });

        let index = self.frame.locals.len() as u8;
        debug_assert!(
            index + 1 == register,
            "registers must be allocated sequentially (index: {index}, register: {register}), when adding local {}",
            String::from_utf8_lossy(&name),
        );

        let local = Local {
            binding: None,
            captured: false,
            name,
            register,
            depth: self.scope_depth,
            span,
            attributes,
            // var_index: local_var_index,
            var_index: 0,
        };
        self.frame.locals.push(local);

        if attributes & (LuaVariableAttribute::ToBeClosed as u8) != 0 {
            unimplemented!()
            // self.to_close.push(register);
        }
    }
    fn binding_location(&self, name: &Name) -> BindingLocation<'_> {
        self.frame.binding_location(name.binding().id)
    }

    fn scope_id(&self) -> usize {
        *self.scope_ids.last().expect("scope_ids is not empty")
    }

    fn register(&mut self, register: u8) -> Value {
        self.builder.ins().iconst(types::I32, register as i64)
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

impl CraneliftCompiler {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        global_state: Arc<RwLock<GlobalState>>,
        chunk_name: LuaString,
        filename: Option<impl Into<PathBuf>>,
        method_name: Option<LuaString>,
        source: Arc<Vec<u8>>,
        has_varargs: bool,
        print_bytecode: bool,
    ) -> Self {
        let filename = filename.map(|f| f.into());
        Self {
            fallback: BytecodeCompiler::new(
                Arc::clone(&global_state),
                chunk_name.clone(),
                filename.clone(),
                method_name.clone(),
                Arc::clone(&source),
                has_varargs,
                print_bytecode,
            ),
            global_state,
            chunk: Chunk::new(chunk_name, filename, source, 0, has_varargs, vec![]),
            method_name,
            gotos: HashMap::new(),
            labels: HashMap::new(),
            to_close: vec![],
            frame: Frame::new(),
            scope_depth: 0,
            scope_id_counter: 1,
            scope_ids: vec![0],
            scope_starting_registers: vec![0],
            register_index: 1,
            max_registers: 0,
            print_bytecode,
        }
    }

    fn compile_body(
        &mut self,
        chunk: resolved_ast::Chunk,
    ) -> crate::Result<Option<JitLuaFunction>> {
        let mut builder = JITBuilder::new(cranelift::module::default_libcall_names())
            .map_err(|e| Box::new(e.into()))?;

        // Make our Rust functions available to generated code.
        builder.symbol("jit_get_local", helpers::jit_get_local as *const u8);
        builder.symbol("jit_set_local", helpers::jit_set_local as *const u8);
        builder.symbol("jit_binary_add", helpers::jit_binary_add as *const u8);
        builder.symbol("jit_return_one", helpers::jit_return_one as *const u8);

        let mut module = JITModule::new(builder);

        // Main function: (LuaState*) -> void
        let mut sig = module.make_signature();
        let ptr = module.target_config().pointer_type();
        sig.params.push(AbiParam::new(ptr));
        // sig.returns.push(AbiParam::new(types::I64));

        // let func_id = module
        //     .declare_anonymous_function(&sig)
        //     .map_err(|e| Box::new(e.into()))?;
        let name = self.chunk.name.to_string();
        let func_id = module
            .declare_function(&name, Linkage::Local, &sig)
            .map_err(|e| Box::new(e.into()))?;

        // get_local: (LuaState*, i32) -> i64
        let mut get_sig = module.make_signature();
        get_sig.params.push(AbiParam::new(ptr));
        get_sig.params.push(AbiParam::new(types::I32));
        get_sig.returns.push(AbiParam::new(types::I64));

        let get_id = module
            .declare_function("jit_get_local", Linkage::Import, &get_sig)
            .map_err(|e| Box::new(e.into()))?;

        // set_local: (LuaState*, i32, i64) -> void
        let mut set_sig = module.make_signature();
        set_sig.params.push(AbiParam::new(ptr));
        set_sig.params.push(AbiParam::new(types::I32));
        set_sig.params.push(AbiParam::new(types::I64));

        let set_id = module
            .declare_function("jit_set_local", Linkage::Import, &set_sig)
            .map_err(|e| Box::new(e.into()))?;

        // binary_add: (LuaState*, i64, i64) -> i64
        let mut binary_add_sig = module.make_signature();
        binary_add_sig.params.push(AbiParam::new(ptr));
        binary_add_sig.params.push(AbiParam::new(types::I64));
        binary_add_sig.params.push(AbiParam::new(types::I64));
        binary_add_sig.returns.push(AbiParam::new(types::I64));

        let binary_add_id = module
            .declare_function("jit_binary_add", Linkage::Import, &binary_add_sig)
            .map_err(|e| Box::new(e.into()))?;

        // return_one: (LuaState*, i64) -> void
        let mut return_sig = module.make_signature();
        return_sig.params.push(AbiParam::new(ptr));
        return_sig.params.push(AbiParam::new(types::I64));

        let return_id = module
            .declare_function("jit_return_one", Linkage::Import, &return_sig)
            .map_err(|e| Box::new(e.into()))?;

        // Construct the Cranelift function.
        let mut ctx = module.make_context();
        ctx.func.signature = sig;

        // Import each helper into this function's IR.
        let runtime = RuntimeFunctions {
            get_local: module.declare_func_in_func(get_id, &mut ctx.func),
            set_local: module.declare_func_in_func(set_id, &mut ctx.func),
            binary_add: module.declare_func_in_func(binary_add_id, &mut ctx.func),
            return_one: module.declare_func_in_func(return_id, &mut ctx.func),
        };

        let mut builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);

        let entry = builder.create_block();

        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);

        // The sole native argument is *mut LuaState.
        let state = builder.block_params(entry)[0];
        let mut compiler = CraneliftCompilerInner::new(&mut builder, state, runtime, &chunk);

        // This is the actual AST-driven compilation!
        // compiler.compile_function(function);
        tc!(compiler.compile_block(chunk.block));

        builder.seal_all_blocks();
        builder.finalize(module.target_config());

        println!("Generated CLIF:\n{}", ctx.func.display());

        // Compile into executable machine code.
        module
            .define_function(func_id, &mut ctx)
            .map_err(|e| Box::new(e.into()))?;
        module.clear_context(&mut ctx);
        module
            .finalize_definitions()
            .map_err(|e| Box::new(e.into()))?;

        let ptr = module.get_finalized_function(func_id);

        // SAFETY: the generated function's signature matches JitFn.
        Ok(Some(unsafe {
            std::mem::transmute::<*const u8, JitLuaFunction>(ptr)
        }))
    }
}

struct RuntimeFunctions {
    get_local: FuncRef,
    set_local: FuncRef,
    return_one: FuncRef,
    binary_add: FuncRef,
}

struct CraneliftCompilerInner<'a, 'b> {
    builder: &'a mut FunctionBuilder<'b>,
    frame: Frame,
    scope_depth: u8,
    scope_id_counter: usize,
    scope_ids: Vec<usize>,
    scope_starting_registers: Vec<u8>,
    register_index: u8,
    max_registers: u8,
    state: Value,
    runtime: RuntimeFunctions,
}

impl<'a, 'b> CraneliftCompilerInner<'a, 'b> {
    fn new(
        builder: &'a mut FunctionBuilder<'b>,
        state: Value,
        runtime: RuntimeFunctions,
        chunk: &resolved_ast::Chunk,
    ) -> Self {
        let mut result = Self {
            builder,
            frame: Frame::new(),
            scope_depth: 0,
            scope_id_counter: 1,
            scope_ids: vec![0],
            scope_starting_registers: vec![0],
            register_index: 1,
            max_registers: 0,
            state,
            runtime,
        };

        for parameter in &chunk.parameters {
            let register = result.take_register();
            result.add_binding(parameter, register);
        }
        for capture in &chunk.captures {
            let binding = capture.binding();
            result.frame.upvalues.insert(
                binding.id,
                Upvalue {
                    index: result.frame.upvalues.len() as u8,
                    source: UpvalueLocation::Upvalue { index: 0 },
                    attributes: binding.attributes,
                    name: binding.name.clone(),
                },
            );
        }

        result
    }
}

impl CraneliftCompilerInner<'_, '_> {
    fn compile_block(&mut self, ast: TokenTree<Block>) -> crate::Result<Option<()>> {
        let terminated = tc!(self.compile_statements(ast.node.statements));
        if !terminated {
            self.builder.ins().return_(&[]);
        }

        Ok(Some(()))
    }

    /// Returns true if all execution paths terminate.
    fn compile_statements(
        &mut self,
        stmts: Vec<TokenTree<Statement>>,
    ) -> crate::Result<Option<bool>> {
        for stmt in stmts {
            if tc!(self.compile_stmt(stmt)) {
                return Ok(Some(true));
            }
        }

        Ok(Some(false))
    }

    fn compile_stmt(&mut self, stmt: TokenTree<Statement>) -> crate::Result<Option<bool>> {
        match stmt.node {
            Statement::LocalDeclaraction(names, exprs) => {
                if names.len() > 1 || exprs.len() > 1 {
                    return Ok(None);
                }

                let value = match exprs.first() {
                    Some(e) => {
                        tc!(self.compile_expr(e))
                    }
                    None => {
                        // Store `nil`
                        self.builder
                            .ins()
                            .iconst(types::I64, LuaValue::NIL.into_raw())
                    }
                };

                let name_register = self.take_register();
                let register = self.register(name_register);
                self.builder
                    .ins()
                    .call(self.runtime.set_local, &[self.state, register, value]);

                self.add_binding(&names[0].node.name, name_register);
            }
            s => todo!("compile_stmt: {s:#?}"),
        }

        Ok(Some(false))
    }

    fn compile_expr(&mut self, expr: &TokenTree<Expression>) -> crate::Result<Option<Value>> {
        match &expr.node {
            Expression::PrefixExpression(e) => self.compile_prefix_expr(e),
            Expression::Literal(lit) => self.compile_literal(lit),
            Expression::BinaryOp { op, lhs, rhs } => {
                let lhs = tc!(self.compile_expr(lhs));
                let rhs = tc!(self.compile_expr(rhs));

                self.compile_binary_op(op, lhs, rhs)
            }
            e => todo!("compile_expr: {e:#?}"),
        }
    }

    fn compile_prefix_expr(
        &mut self,
        expr: &TokenTree<PrefixExpression>,
    ) -> crate::Result<Option<Value>> {
        match &expr.node {
            PrefixExpression::Variable(v) => self.compile_variable(v),
            p => {
                todo!("compile_prefix_expr: {p:#?}")
            }
        }
    }

    fn compile_variable(&mut self, var: &TokenTree<Variable>) -> crate::Result<Option<Value>> {
        match &var.node {
            Variable::Name(name) => match self.binding_location(&name.node) {
                BindingLocation::Local { register, local } => {
                    let register = self.register(register);
                    let call = self
                        .builder
                        .ins()
                        .call(self.runtime.get_local, &[self.state, register]);

                    Ok(Some(self.builder.inst_results(call)[0]))
                }
                BindingLocation::Upvalue { index, attributes } => todo!(),
            },
            _ => todo!(),
        }
    }

    fn compile_literal(&mut self, lit: &TokenTree<Literal>) -> crate::Result<Option<Value>> {
        Ok(Some(self.builder.ins().iconst(
            types::I64,
            match lit.node {
                Literal::Nil => LuaValue::NIL.into_raw(),
                Literal::Boolean(b) => {
                    let v = LuaValue::from_bool(b);
                    v.into_raw()
                }
                Literal::Number(resolved_ast::Number::Integer(i)) => {
                    let v = LuaValue::from_integer(i);
                    v.into_raw()
                }
                Literal::Number(resolved_ast::Number::Float(f)) => {
                    let v = LuaValue::from_float(f);
                    v.into_raw()
                }
                Literal::String(_) => {
                    eprintln!("string literal not yet supported");
                    return Ok(None);
                }
            },
        )))
    }
}
