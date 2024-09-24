use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};

use clap::Parser;
use lua_interpreter::{
    compiler::Compiler, debug, lexer::Lexer, optimizer::optimize as optimize_ast, parser,
    token::TokenKind, vm::VM,
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

    let source = std::fs::read_to_string(&filename).expect("failed to read source code file");

    if debug_lexer {
        let lexer = Lexer::new(Some(&filename), &source);

        run_debug_lexer(lexer, &filename, &source);
        return;
    }

    let mut parser = parser::Parser::new(Some(&filename), &source);
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

    let chunk_name = filename
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| "<file>".to_string());

    let mut vm = VM::new();
    if let Err(e) = Compiler::new(&mut vm, Some(filename), chunk_name, Cow::Borrowed(&source))
        .compile(Some(ast))
    {
        eprintln!("compilation failed: {e:?}");
        std::process::exit(1);
    }
    if print_bytecode {
        debug::print_instructions(&vm);
    }

    vm.run();
}

fn run_debug_lexer(lexer: Lexer, filename: &Path, source: &str) {
    let filename = filename
        .file_name()
        .map(|f| f.to_string_lossy().to_string())
        .unwrap_or_else(|| "<file>".to_string());
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
                .with_source_code(NamedSource::new(filename.clone(), source.to_owned()));
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
