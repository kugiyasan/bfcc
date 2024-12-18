use crate::ast::{Node, NodeKind};

fn gen_lval(node: Node) {
    if let NodeKind::LVar { offset } = node.kind {
        println!("  mov rax, rbp");
        println!("  sub rax, {}", offset);
        println!("  push rax");
        return;
    }
    panic!("Node is not an l-value: {node:?}");
}

fn _generate(node: Node) {
    match node.kind {
        NodeKind::Num(num) => {
            println!("  push {}", num);
            return;
        }
        NodeKind::LVar { offset: _ } => {
            gen_lval(node);
            println!("  pop rax");
            println!("  mov rax, [rax]");
            println!("  push rax");
            return;
        }
        NodeKind::Assign => {
            gen_lval(*node.left.unwrap());
            _generate(*node.right.unwrap());
            println!("  pop rdi");
            println!("  pop rax");
            println!("  mov [rax], rdi");
            println!("  push rdi");
            return;
        }
        NodeKind::Stmt => {
            _generate(*node.left.unwrap());
            println!("  pop rax");
            _generate(*node.right.unwrap());
            return;
        }
        NodeKind::Return => {
            _generate(*node.left.unwrap());
            println!("  pop rax");
            epilogue();
            return;
        }
        _ => (),
    };

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

fn prologue(offset: i32) {
    println!("  push rbp");
    println!("  mov rbp, rsp");
    println!("  sub rsp, {}", offset);
}

fn epilogue() {
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}

pub fn generate(node: Option<Node>, offset: i32) {
    println!(".intel_syntax noprefix");
    println!(".globl main");
    println!("main:");
    prologue(offset);

    if let Some(n) = node {
        _generate(n);
        println!("  pop rax");
    }

    epilogue();
}
