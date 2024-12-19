use crate::ast::{Add, Assign, Equality, Expr, Mul, Primary, Program, Relational, Stmt, Unary};

fn gen_oneop(s: &str) {
    println!("  pop rax");
    println!("{}", s);
    println!("  push rax");
}

fn gen_binop(s: &str) {
    println!("  pop rdi");
    println!("  pop rax");
    println!("{}", s);
    println!("  push rax");
}

fn gen_lval(offset: i32) {
    println!("  mov rax, rbp");
    println!("  sub rax, {}", offset);
    println!("  push rax");
}

fn gen_program(program: Program) {
    if program.0.is_empty() {
        return;
    }

    for stmt in program.0 {
        match stmt {
            Stmt::If(expr, stmt, else_stmt) => {
                todo!();
            }
            Stmt::While(expr, stmt) => {
                todo!()
            }
            Stmt::For(expr1, expr2, expr3, stmt) => {
                todo!()
            }
            Stmt::Return(expr) => {
                gen_expr(expr);
                println!("  pop rax");
                epilogue();
            }
            Stmt::Expr(expr) => {
                gen_expr(expr);
                println!("  pop rax");
            }
        };
    }
}

fn gen_expr(expr: Expr) {
    match expr {
        Expr::Assign(assign) => gen_assign(assign),
    }
}

fn gen_assign(assign: Assign) {
    if let Some(a) = assign.assign {
        if let Equality::Identity(Relational::Identity(Add::Identity(Mul::Identity(Unary::Pos(
            Primary::Ident(offset),
        ))))) = assign.eq
        {
            gen_lval(offset);
        } else {
            panic!("Invalid l-value for assignment: {:?}", assign.eq);
        }
        gen_assign(*a);
        println!("  pop rdi");
        println!("  pop rax");
        println!("  mov [rax], rdi");
        println!("  push rdi");
        return;
    }

    gen_equality(assign.eq);
}

fn gen_equality(equality: Equality) {
    match equality {
        Equality::Identity(rel) => {
            gen_relational(rel);
        }
        Equality::Equal(rel, eq) => {
            gen_relational(rel);
            gen_equality(*eq);
            gen_binop("  cmp rdi, rax\n  sete al\n  movzb rax, al");
        }
        Equality::NotEqual(rel, eq) => {
            gen_relational(rel);
            gen_equality(*eq);
            gen_binop("  cmp rdi, rax\n  setne al\n  movzb rax, al");
        }
    }
}

fn gen_relational(relational: Relational) {
    match relational {
        Relational::Identity(add) => {
            gen_add(add);
        }
        Relational::LessThan(add, rel) => {
            gen_add(add);
            gen_relational(*rel);
            gen_binop("  cmp rdi, rax\n  setl al\n  movzb rax, al");
        }
        Relational::LessEqual(add, rel) => {
            gen_add(add);
            gen_relational(*rel);
            gen_binop("  cmp rdi, rax\n  setle al\n  movzb rax, al");
        }
        Relational::GreaterThan(add, rel) => {
            gen_add(add);
            gen_relational(*rel);
            gen_binop("  cmp rdi, rax\n  setg al\n  movzb rax, al");
        }
        Relational::GreaterEqual(add, rel) => {
            gen_add(add);
            gen_relational(*rel);
            gen_binop("  cmp rdi, rax\n  setge al\n  movzb rax, al");
        }
    }
}

fn gen_add(add: Add) {
    match add {
        Add::Identity(mul) => gen_mul(mul),
        Add::Add(mul, a) => {
            gen_mul(mul);
            gen_add(*a);
            gen_binop("  add rax, rdi");
        }
        Add::Sub(mul, a) => {
            gen_mul(mul);
            gen_add(*a);
            gen_binop("  sub rax, rdi");
        }
    }
}

fn gen_mul(mul: Mul) {
    match mul {
        Mul::Identity(unary) => gen_unary(unary),
        Mul::Mul(unary, m) => {
            gen_unary(unary);
            gen_mul(*m);
            gen_binop("  imul rax, rdi");
        }
        Mul::Div(unary, m) => {
            gen_unary(unary);
            gen_mul(*m);
            gen_binop("  cqo\n  idiv rdi");
        }
    }
}

fn gen_unary(unary: Unary) {
    match unary {
        Unary::Pos(primary) => {
            gen_primary(primary);
        }
        Unary::Neg(primary) => {
            gen_primary(primary);
            gen_oneop("  neg rax");
        }
    }
}

fn gen_primary(primary: Primary) {
    match primary {
        Primary::Num(num) => println!("  push {}", num),
        Primary::Ident(offset) => {
            gen_lval(offset);
            println!("  pop rax");
            println!("  mov rax, [rax]");
            println!("  push rax");
        }
        Primary::Expr(expr) => gen_expr(*expr),
    }
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

pub fn generate(program: Program, offset: i32) {
    println!(".intel_syntax noprefix");
    println!(".globl main");
    println!("main:");
    prologue(offset);

    gen_program(program);

    epilogue();
}
