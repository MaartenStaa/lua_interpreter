use std::{os::unix::ffi::OsStrExt, path::PathBuf};

use lua_interpreter::{
    GlobalState, compiler::Compiler, optimizer::optimize, parser::Parser, resolver::resolve,
    value::LuaString,
};
use rstest::*;

#[rstest]
fn test_files(#[files("tests/resources/**/*.lua")] file: PathBuf) {
    use std::sync::Arc;

    let source = std::fs::read(&file).expect("failed to read source code file");
    let chunk_name: LuaString = file
        .file_name()
        .map(|n| n.as_bytes().into())
        .unwrap_or_else(|| b"<file>".into());

    let mut parser = Parser::new(&chunk_name, &source);

    let ast = optimize(parser.parse().expect("parsing should succeed"));

    let global_state = GlobalState::get();
    Compiler::new(
        global_state,
        chunk_name,
        Some(file),
        None,
        Arc::new(source),
        true,
        false,
    )
    .compile(resolve(ast))
    .expect("compiling should succeed")
    .run()
    .expect("running chunk should succeed");
}
