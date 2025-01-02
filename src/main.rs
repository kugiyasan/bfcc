// https://www.sigbus.info/compilerbook
// https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm

mod analyzer;
mod codegen;
mod lexer;
mod parser;

use std::env;

use crate::analyzer::SemanticVisitor;
use crate::codegen::Codegen;
use crate::lexer::Lexer;
use crate::parser::Parser;

fn main() {
    let mut env: Vec<_> = env::args_os().map(|s| s.into_string().unwrap()).collect();
    if env.len() != 2 {
        eprintln!("Wrong number of arguments!");
        return;
    }

    let user_input = env.pop().unwrap();
    let tokens = Lexer::tokenize(&user_input);
    let token_kinds = tokens.iter().map(|t| &t.kind).collect::<Vec<_>>();
    dbg!(token_kinds);

    let mut parser = Parser::new(tokens);
    let mut translation_unit = parser.parse();
    // dbg!(&translation_unit);

    let mut visitor = SemanticVisitor::new();
    let symbol_table = visitor.visit_translation_unit(&mut translation_unit);
    dbg!(&translation_unit);
    dbg!(&symbol_table);

    let mut codegen = Codegen::new(symbol_table);
    codegen.generate(translation_unit);
}
