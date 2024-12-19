// https://www.sigbus.info/compilerbook

mod ast;
mod codegen;
mod token;

use std::env;

use crate::ast::Ast;
use crate::codegen::Codegen;
use crate::token::Tokens;

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

    let mut codegen = Codegen::new();
    codegen.generate(program);
}
