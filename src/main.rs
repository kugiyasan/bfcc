// https://www.sigbus.info/compilerbook
// https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm

mod lexer;
mod parser;
mod analyzer;
mod codegen;

use std::env;

use crate::lexer::Tokens;
use crate::parser::Ast;
use crate::analyzer::SemanticVisitor;
use crate::codegen::Codegen;

fn main() {
    let mut env: Vec<_> = env::args_os().map(|s| s.into_string().unwrap()).collect();
    if env.len() != 2 {
        eprintln!("Wrong number of arguments!");
        return;
    }

    let user_input = env.pop().unwrap();
    let tokens = Tokens::tokenize(user_input);
    let token_kinds = tokens.iter().map(|t| &t.kind).collect::<Vec<_>>();
    dbg!(token_kinds);

    let mut ast = Ast::new(tokens);
    let program = ast.parse();
    dbg!(&program);

    let visitor = SemanticVisitor::new();
    visitor.visit_program(&program);

    let mut codegen = Codegen::new();
    codegen.generate(program);
}
