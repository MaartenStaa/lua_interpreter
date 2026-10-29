mod block;
mod bytecode;
mod cranelift;
mod expression;
mod frame;
mod goto;
mod variable;

use std::{
    path::PathBuf,
    sync::{Arc, RwLock},
};

pub(crate) use expression::ExpressionResultMode;

use crate::{
    GlobalState, LuaState, resolved_ast,
    value::{JitLuaFunction, LuaString, LuaValue},
    vm::ChunkIndex,
};

#[derive(Debug)]
pub enum CompiledChunk {
    Bytecode(ChunkIndex),
    Jit(JitLuaFunction),
}

#[derive(Debug)]
pub enum Runnable {
    State(LuaState),
    Jit(JitLuaFunction),
}

impl Runnable {
    pub fn run(&mut self) -> crate::Result<Vec<LuaValue>> {
        match self {
            Runnable::State(lua_state) => lua_state.run(),
            Runnable::Jit(f) => todo!("run {f:#?}"),
        }
    }
}

pub trait Compiler {
    /// Compile the given AST into a function that can be called. If `ast` is `None`,
    /// the compiler will use the previously provided source code (from calling
    /// [`factory`]) to parse and build the AST.
    /// TODO: Return a function instead of a lua state?
    // fn compile(&mut self, ast: Option<TokenTree<Block>>) -> crate::Result<LuaState>;
    fn compile(&mut self, ast: resolved_ast::Chunk) -> crate::Result<Runnable>;

    fn compile_chunk(&mut self, ast: resolved_ast::Chunk) -> crate::Result<CompiledChunk>;
}

#[allow(clippy::too_many_arguments)]
pub fn factory(
    global_state: Arc<RwLock<GlobalState>>,
    chunk_name: LuaString,
    filename: Option<impl Into<PathBuf>>,
    method_name: Option<LuaString>,
    source: Arc<Vec<u8>>,
    has_varargs: bool,
    print_bytecode: bool,
) -> Box<dyn Compiler> {
    if cfg!(feature = "cranelift") {
        return Box::new(cranelift::CraneliftCompiler::new(
            global_state,
            chunk_name,
            filename,
            method_name,
            source,
            has_varargs,
            print_bytecode,
        ));
    }

    Box::new(bytecode::BytecodeCompiler::new(
        global_state,
        chunk_name,
        filename,
        method_name,
        source,
        has_varargs,
        print_bytecode,
    ))
}
