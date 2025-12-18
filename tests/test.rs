use std::path::PathBuf;

use lua_interpreter::{compiler::Compiler, optimizer::optimize, parser::Parser};
use rstest::*;

#[rstest]
fn test_files(#[files("tests/resources/**/*.lua")] file: PathBuf) {
    use std::sync::{Arc, RwLock};

    use lua_interpreter::{env, value::LuaObject, vm::VM};

    let source = std::fs::read(&file).expect("failed to read source code file");
    let mut parser = Parser::new(Some(&file), &source);

    let ast = optimize(parser.parse().expect("parsing should succeed"));
    let chunk_name = file
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| "<file>".to_string());

    let global_env = Arc::new(RwLock::new(LuaObject::Table(env::create_global_env())));
    let mut vm = VM::new(Arc::clone(&global_env), false);
    let chunk = Compiler::new(
        &mut vm,
        Some(global_env),
        Some(file),
        chunk_name,
        source.into(),
    )
    .compile(Some(ast))
    .expect("compiling should succeed");

    vm.run_chunk(chunk).expect("running chunk should succeed");
}
