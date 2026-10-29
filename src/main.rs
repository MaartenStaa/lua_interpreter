use std::{
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
    sync::Arc,
};

use clap::{Parser, Subcommand};
use lua_interpreter::{
    GlobalState, compiler::Compiler, lexer::Lexer, optimizer::optimize as optimize_ast, parser,
    resolver::resolve, token::TokenKind, value::LuaString,
};
use miette::{LabeledSpan, NamedSource};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Input {
    #[command(subcommand)]
    command: Option<Command>,

    /// The Lua source file to run
    filename: Option<PathBuf>,

    /// Disable the optimizer, e.g. skipping constant folding.
    #[clap(long, default_value = "false")]
    disable_optimizer: bool,

    /// Print the bytecode instructions before executing them.
    #[clap(long, default_value = "false")]
    print_bytecode: bool,

    /// Print the execution steps of the VM.
    #[clap(long, default_value = "false")]
    trace_vm: bool,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Debug the lexer, printing out each token. Does not parse or execute the file.
    DebugLexer,

    /// Debug the parser, printing out the AST. Does not execute the file.
    DebugParser,

    #[cfg(feature = "debugger")]
    /// Launch the interactive debugger.
    Debug {
        /// Path to the Lua source file to debug.
        filename: PathBuf,

        /// Pause on the very first instruction.
        #[clap(long, default_value = "false")]
        break_start: bool,

        /// Set breakpoints. Format: "file.lua:20" or "c:2:15" (chunk:instruction).
        #[clap(long, short = 'b')]
        breakpoint: Vec<String>,
    },
}

fn main() {
    let Input {
        filename,
        command,
        disable_optimizer,
        print_bytecode,
        trace_vm,
    } = Input::parse();

    let filename = {
        #[cfg(feature = "debugger")]
        if let Some(Command::Debug { filename, .. }) = &command {
            filename.clone()
        } else {
            extract_filename(filename)
        }
        #[cfg(not(feature = "debugger"))]
        extract_filename(filename)
    };

    let Ok(filename) = filename.canonicalize() else {
        eprintln!("failed to canonicalize filename {}", filename.display());
        std::process::exit(1);
    };
    let source = std::fs::read(&filename).expect("failed to read source code file");
    let chunk_name: LuaString = filename
        .file_name()
        .map_or_else(|| b"<file>".into(), |n| n.as_bytes().into());

    if let Some(command) = command {
        match command {
            Command::DebugLexer => {
                let lexer = Lexer::new(&chunk_name, &source);

                run_debug_lexer(lexer, &filename, &source);
                return;
            }
            Command::DebugParser => {
                run_debug_parser(parser::Parser::new(&chunk_name, &source));
                return;
            }
            #[cfg(feature = "debugger")]
            Command::Debug {
                break_start,
                breakpoint: breakpoint_args,
                ..
            } => {
                run_debugger(&filename, break_start, &breakpoint_args);
                return;
            }
        }
    }

    let ast = match parser::Parser::new(&chunk_name, &source).parse() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{e:?}");
            std::process::exit(1);
        }
    };
    let ast = if disable_optimizer {
        ast
    } else {
        optimize_ast(ast)
    };

    let source = Arc::new(source);
    if match Compiler::new(
        GlobalState::get(),
        chunk_name.clone(),
        Some(filename),
        None,
        Arc::clone(&source),
        true,
        print_bytecode,
    )
    .compile(resolve(ast))
    {
        Ok(c) => c,
        Err(e) => {
            let e = e.with_source_code(NamedSource::new(
                String::from_utf8_lossy(&chunk_name),
                (*source).clone(),
            ));
            eprintln!("compilation failed: {e:?}");
            std::process::exit(1);
        }
    }
    .with_tracer(trace_vm)
    .run()
    .is_err()
    {
        std::process::exit(1);
    }
}

fn extract_filename(filename: Option<PathBuf>) -> PathBuf {
    let Some(filename) = filename else {
        eprintln!("error: a filename is required when not using a subcommand");
        std::process::exit(1);
    };
    filename
}

fn run_debug_lexer(lexer: Lexer, filename: &Path, source: &[u8]) {
    let filename = filename
        .file_name()
        .map_or_else(|| "<file>".to_string(), |f| f.to_string_lossy().to_string());
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
                eprintln!("{diag:?}");
            }
            Err(e) => {
                eprintln!("{e:?}");
                std::process::exit(1);
            }
        }
    }
}

fn run_debug_parser(mut parser: parser::Parser) {
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{e:?}");
            std::process::exit(1);
        }
    };
    let resolved = resolve(ast);

    println!("{resolved:#?}");
}

#[cfg(feature = "debugger")]
fn run_debugger(filename: &Path, break_start: bool, breakpoint_args: &[String]) {
    use lua_interpreter::debugger;

    let Ok(filename) = filename.canonicalize() else {
        eprintln!("failed to canonicalize filename {}", filename.display());
        std::process::exit(1);
    };
    let source = std::fs::read(&filename).expect("failed to read source code file");
    let chunk_name: LuaString = filename
        .file_name()
        .map_or_else(|| b"<file>".into(), |n| n.as_bytes().into());

    let mut parser = parser::Parser::new(&chunk_name, &source);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{e:?}");
            std::process::exit(1);
        }
    };
    let ast = optimize_ast(ast);

    let source = Arc::new(source);
    let state = match Compiler::new(
        GlobalState::get(),
        chunk_name.clone(),
        Some(filename),
        None,
        Arc::clone(&source),
        true,
        false,
    )
    .compile(resolve(ast))
    {
        Ok(c) => c,
        Err(e) => {
            let e = e.with_source_code(NamedSource::new(
                String::from_utf8_lossy(&chunk_name),
                (*source).clone(),
            ));
            eprintln!("compilation failed: {e:?}");
            std::process::exit(1);
        }
    };

    let breakpoints: Vec<_> = breakpoint_args
        .iter()
        .filter_map(|s| {
            let bp = debugger::parse_breakpoint(s);
            if bp.is_none() {
                eprintln!("warning: invalid breakpoint format: {s}");
            }
            bp
        })
        .collect();

    debugger::launch_debugger(state, breakpoints, break_start);
}
