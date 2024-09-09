use std::path::PathBuf;

use lua_interpreter::{lexer, token::TokenKind};
use miette::LabeledSpan;

fn main() {
    let filename = std::env::args()
        .nth(1)
        .expect("expected source code file as first argument");
    let filename = PathBuf::from(filename);
    let source = std::fs::read_to_string(&filename).expect("failed to read source code file");
    let lexer = lexer::Lexer::new(filename.clone(), &source);
    let lexer2 = lexer::Lexer::new(filename, &source);

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
                .with_source_code(lexer2.get_source_code());
                eprintln!("{:?}", diag);
            }
            Err(e) => {
                eprintln!("{:?}", e);
                std::process::exit(1);
            }
        }
    }
}
