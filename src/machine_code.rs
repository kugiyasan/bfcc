use crate::ast::{Node, NodeKind};

fn _generate(node: Node) {
    if let NodeKind::Num(num) = node.kind {
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
        NodeKind::Add => println!("  add rax, rdi"),
        NodeKind::Sub => println!("  sub rax, rdi"),
        NodeKind::Mul => println!("  imul rax, rdi"),
        NodeKind::Div => println!("  cqo\n  idiv rdi"),
        NodeKind::LessThan => println!("  cmp rdi, rax\n  setl al\n  movzb rax, al"),
        NodeKind::LessEqual => println!("  cmp rdi, rax\n  setle al\n  movzb rax, al"),
        NodeKind::NotEqual => println!("  cmp rdi, rax\n  setne al\n  movzb rax, al"),
        NodeKind::DoubleEqual => println!("  cmp rdi, rax\n  sete al\n  movzb rax, al"),
        n => panic!("Unknown node kind to generate: {:?}", n),
    };

    println!("  push rax");
}

pub fn generate(node: Option<Node>) {
    println!(".intel_syntax noprefix");
    println!(".globl main");
    println!("main:");

    if let Some(n) = node {
        _generate(n);
    } else {
        println!("  push 0");
    }

    println!("  pop rax");
    println!("  ret");
}
