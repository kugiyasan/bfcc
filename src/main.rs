// https://www.sigbus.info/compilerbook

mod ast;
mod machine_code;
mod token;

use std::env;

use ast::{Node, NodeKind};

use crate::ast::Ast;
use crate::token::Tokens;

fn dbg_stmt(node: &Option<Node>) {
    if node.is_none() {
        return;
    }
    let mut stmt = vec![];
    let mut stack = vec![node.as_ref().unwrap()];

    while !stack.is_empty() {
        let node = stack.pop().unwrap();
        if node.kind == NodeKind::Stmt {
            stack.push(&node.left.as_ref().unwrap());
            stack.push(&node.right.as_ref().unwrap());
        } else {
            stmt.push(node);
        }
    }

    stmt.reverse();
    dbg!(stmt);
}

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
    let node = Ast::parse(tokens);
    dbg_stmt(&node);
    machine_code::generate(node);
}
