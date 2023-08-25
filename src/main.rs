// https://www.sigbus.info/compilerbook

mod token;

use std::env;

use crate::token::Tokens;

fn main() {
    let mut env: Vec<String> = env::args_os().map(|s| s.into_string().unwrap()).collect();
    if env.len() != 2 {
        eprintln!("Wrong number of arguments!");
        return;
    }

    println!(".intel_syntax noprefix");
    println!(".globl main");
    println!("main:");

    let mut tokens = Tokens::tokenize(env.pop().unwrap());

    println!("  mov rax, {}", tokens.expect_number());

    while !tokens.at_eof() {
        let c: &str = &tokens.get_current_token_str();
        match c {
            "+" => {
                tokens.expect('+');
                println!("  add rax, {}", tokens.expect_number());
            }
            "-" => {
                tokens.expect('-');
                println!("  sub rax, {}", tokens.expect_number());
            }
            _ => panic!("unexpected character: {}", c),
        }
    }

    println!("  ret");
}
