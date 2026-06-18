use std::{
    borrow::Cow,
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
    sync::Arc,
};

use clap::Parser;
use lua_interpreter::{
    compiler::Compiler, env, lexer::Lexer, optimizer::optimize as optimize_ast, parser,
    token::TokenKind, value::LuaString, vm::VM,
};
use miette::{LabeledSpan, NamedSource};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Input {
    /// The Lua source file to run
    filename: PathBuf,

    /// Debug the lexer, printing out each token. Does not parse or execute the file.
    #[clap(long, default_value = "false")]
    debug_lexer: bool,

    /// Debug the parser, printing out the AST. Does not execute the file.
    #[clap(long, default_value = "false")]
    debug_parser: bool,

    /// Disable the optimizer, e.g. skipping constant folding.
    #[clap(long, default_value = "false")]
    disable_optimizer: bool,

    /// Print the bytecode instructions before executing them.
    #[clap(long, default_value = "false")]
    print_bytecode: bool,
}

fn main() {
    let Input {
        filename,
        debug_lexer,
        debug_parser,
        disable_optimizer,
        print_bytecode,
    } = Input::parse();

    let Ok(filename) = filename.canonicalize() else {
        eprintln!("failed to canonicalize filename {}", filename.display());
        std::process::exit(1);
    };
    let source = std::fs::read(&filename).expect("failed to read source code file");
    let chunk_name: LuaString = filename
        .file_name()
        .map(|n| n.as_bytes().into())
        .unwrap_or_else(|| b"<file>".into());

    if debug_lexer {
        let lexer = Lexer::new(&chunk_name, &source);

        run_debug_lexer(lexer, &filename, &source);
        return;
    }

    let mut parser = parser::Parser::new(&chunk_name, &source);
    if debug_parser {
        run_debug_parser(parser);
        return;
    }

    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{:?}", e);
            std::process::exit(1);
        }
    };
    let ast = if disable_optimizer {
        ast
    } else {
        optimize_ast(ast)
    };

    let mut vm = VM::new(env::create_global_env(), print_bytecode);
    let chunk = match Compiler::new(
        &mut vm,
        chunk_name.clone(),
        Some(filename),
        None,
        Arc::new(Cow::Borrowed(&source)),
        vec![],
        true,
    )
    .compile_chunk(Some(ast))
    {
        Ok(c) => c,
        Err(e) => {
            let e = e.with_source_code(NamedSource::new(
                String::from_utf8_lossy(&chunk_name),
                source,
            ));
            eprintln!("compilation failed: {e:?}");
            std::process::exit(1);
        }
    };

    vm.run(chunk);
}

fn run_debug_lexer(lexer: Lexer, filename: &Path, source: &[u8]) {
    let filename = filename
        .file_name()
        .map(|f| f.to_string_lossy().to_string())
        .unwrap_or_else(|| "<file>".to_string());
    let source = String::from_utf8_lossy(source).to_string();
    for token in lexer {
        match token {
            Ok(t) => {
                let kind = match t.kind {
                    TokenKind::String(s) => format!("String(\"{}\")", String::from_utf8_lossy(&s)),
                    _ => format!("{:?}", t.kind),
                };
                let diag = miette::miette!(
                    labels = vec![LabeledSpan::at(t.span.start..t.span.end, kind)],
                    severity = miette::Severity::Advice,
                    "found a token",
                )
                .with_source_code(NamedSource::new(filename.clone(), source.clone()));
                eprintln!("{:?}", diag);
            }
            Err(e) => {
                eprintln!("{:?}", e);
                std::process::exit(1);
            }
        }
    }
}

fn run_debug_parser(mut parser: parser::Parser) {
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{:?}", e);
            std::process::exit(1);
        }
    };

    println!("{:#?}", ast);
}
