use std::{os::unix::ffi::OsStrExt, path::PathBuf};

use lua_interpreter::{compiler::Compiler, optimizer::optimize, parser::Parser, value::LuaString};
use rstest::*;

#[rstest]
fn test_files(#[files("tests/resources/**/*.lua")] file: PathBuf) {
    use std::sync::Arc;

    use lua_interpreter::{env, vm::VM};

    let source = std::fs::read(&file).expect("failed to read source code file");
    let chunk_name: LuaString = file
        .file_name()
        .map(|n| n.as_bytes().into())
        .unwrap_or_else(|| b"<file>".into());

    let mut parser = Parser::new(&chunk_name, &source);

    let ast = optimize(parser.parse().expect("parsing should succeed"));

    let global_env = env::create_global_env();
    let mut vm = VM::new(global_env, false);
    let chunk = Compiler::new(
        &mut vm,
        chunk_name,
        Some(file),
        None,
        Arc::new(source.into()),
        vec![],
        true,
    )
    .compile_chunk(Some(ast))
    .expect("compiling should succeed");

    vm.run_chunk(chunk.chunk_index)
        .expect("running chunk should succeed");
}
