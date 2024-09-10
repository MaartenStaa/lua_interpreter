use std::path::PathBuf;

use clap::Parser;
use lua_interpreter::{lexer, parser, token::TokenKind};
use miette::LabeledSpan;

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
}

fn main() {
    let Input {
        filename,
        debug_lexer,
        debug_parser,
    } = Input::parse();

    let source = std::fs::read_to_string(&filename).expect("failed to read source code file");

    if debug_lexer {
        let lexer = lexer::Lexer::new(filename.clone(), &source);
        let source_code = lexer.get_source_code();

        run_debug_lexer(lexer, source_code);
        return;
    }

    if debug_parser {
        let parser = parser::Parser::new(filename, &source);
        run_debug_parser(parser);
        return;
    }

    unimplemented!();
}

fn run_debug_lexer(lexer: lexer::Lexer, source_code: miette::NamedSource<String>) {
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
                .with_source_code(source_code.clone());
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

    dbg!(ast);
}
