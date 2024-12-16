use crate::{ast::Node, token::TokenKind};

fn _generate(node: Node) {
    if let TokenKind::Num(num) = node.kind {
        println!("  push {}", num);
        return;
    }

    if let Some(left) = node.left {
        _generate(*left);
    }
    if let Some(right) = node.right {
        _generate(*right);
    }

    println!("  pop rdi");
    println!("  pop rax");

    match node.kind {
        TokenKind::Add => println!("  add rax, rdi"),
        TokenKind::Sub => println!("  sub rax, rdi"),
        TokenKind::Mul => println!("  imul rax, rdi"),
        TokenKind::Div => println!("  cqo\n  idiv rdi"),
        TokenKind::LessThan => println!("  cmp rdi, rax\n  setl al\n  movzb rax, al"),
        TokenKind::LessEqual => println!("  cmp rdi, rax\n  setle al\n  movzb rax, al"),
        TokenKind::NotEqual => println!("  cmp rdi, rax\n  setne al\n  movzb rax, al"),
        TokenKind::Equal => println!("  cmp rdi, rax\n  sete al\n  movzb rax, al"),
        n => panic!("Unknown node kind to generate: {:?}", n),
    };

    println!("  push rax");
}

pub fn generate(node: Node) {
    println!(".intel_syntax noprefix");
    println!(".globl main");
    println!("main:");
    _generate(node);
    println!("  pop rax");
    println!("  ret");
}
