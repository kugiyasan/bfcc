// https://www.sigbus.info/compilerbook

mod token;
mod ast;
mod machine_code;

use std::env;

use crate::token::Tokens;
use crate::ast::Ast;

fn main() {
    let mut env: Vec<String> = env::args_os().map(|s| s.into_string().unwrap()).collect();
    if env.len() != 2 {
        eprintln!("Wrong number of arguments!");
        return;
    }

    let user_input = env.pop().unwrap();

    let tokens = Tokens::tokenize(user_input);
    let mut ast = Ast::new(tokens);
    let node = ast.parse();

    machine_code::generate(node);
}
