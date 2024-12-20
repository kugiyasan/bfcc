use crate::ast::{
    Add, Assign, Equality, Expr, Func, Mul, Primary, Program, Relational, Stmt, Unary,
};

const ARGUMENT_REGISTERS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

pub struct Codegen {
    label_index: usize,
}

fn epilogue() {
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}

impl Codegen {
    pub fn new() -> Self {
        Self { label_index: 0 }
    }

    pub fn generate(&mut self, program: Program) {
        println!(".intel_syntax noprefix");
        println!(".globl main");
        self.gen_program(program);
    }

    fn new_label(&mut self) -> String {
        let s = format!(".L{:0>3}", self.label_index);
        self.label_index += 1;
        s
    }

    fn gen_oneop(&mut self, s: &str) {
        println!("  pop rax");
        println!("{}", s);
        println!("  push rax");
    }

    fn gen_binop(&mut self, s: &str) {
        println!("  pop rdi");
        println!("  pop rax");
        println!("{}", s);
        println!("  push rax");
    }

    fn gen_lval(&mut self, offset: usize) {
        println!("  mov rax, rbp");
        println!("  sub rax, {}", offset);
        println!("  push rax");
    }

    fn gen_program(&mut self, program: Program) {
        for func in program.0 {
            self.gen_func(func);
        }
    }

    fn gen_func(
        &mut self,
        Func {
            name,
            args,
            stmts,
            local_offset,
        }: Func,
    ) {
        println!("{name}:");
        println!("  push rbp");
        println!("  mov rbp, rsp");
        for reg in ARGUMENT_REGISTERS.iter().take(args.len()) {
            println!("  push {reg}");
        }
        println!("  sub rsp, {}", local_offset);

        for s in stmts {
            self.gen_stmt(s);
        }

        epilogue();
    }

    fn gen_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.gen_expr(expr);
                println!("  pop rax");
            }
            Stmt::Block(stmt) => {
                for s in stmt {
                    self.gen_stmt(s);
                }
            }
            Stmt::If(expr, stmt, None) => self.gen_if(expr, *stmt),
            Stmt::If(expr, stmt, Some(else_stmt)) => self.gen_if_else(expr, *stmt, *else_stmt),
            Stmt::While(expr, stmt) => self.gen_while(expr, *stmt),
            Stmt::For(expr1, expr2, expr3, stmt) => self.gen_for(expr1, expr2, expr3, *stmt),
            Stmt::Return(expr) => {
                self.gen_expr(expr);
                println!("  pop rax");
                epilogue();
            }
        };
    }

    fn gen_if(&mut self, expr: Expr, stmt: Stmt) {
        let end_label = self.new_label();
        self.gen_expr(expr);
        println!("  pop rax");
        println!("  cmp rax, 0");
        println!("  je {end_label}");
        self.gen_stmt(stmt);
        println!("{end_label}:");
    }

    fn gen_if_else(&mut self, expr: Expr, stmt: Stmt, else_stmt: Stmt) {
        let end_label = self.new_label();
        let else_label = self.new_label();

        self.gen_expr(expr);
        println!("  pop rax");
        println!("  cmp rax, 0");
        println!("  je {else_label}");
        self.gen_stmt(stmt);
        println!("  jmp {end_label}");
        println!("{else_label}:",);
        self.gen_stmt(else_stmt);
        println!("{end_label}:");
        return;
    }

    fn gen_while(&mut self, expr: Expr, stmt: Stmt) {
        let begin_label = self.new_label();
        let end_label = self.new_label();

        println!("{begin_label}:");
        self.gen_expr(expr);
        println!("  pop rax");
        println!("  cmp rax, 0");
        println!("  je {end_label}");
        self.gen_stmt(stmt);
        println!("  jmp {begin_label}");
        println!("{end_label}:");
    }

    fn gen_for(
        &mut self,
        expr1: Option<Expr>,
        expr2: Option<Expr>,
        expr3: Option<Expr>,
        stmt: Stmt,
    ) {
        let begin_label = self.new_label();
        let end_label = self.new_label();

        if let Some(e) = expr1 {
            self.gen_expr(e);
        }
        println!("{begin_label}:");
        if let Some(e) = expr2 {
            self.gen_expr(e);
        }
        println!("  pop rax");
        println!("  cmp rax, 0");
        println!("  je {end_label}");
        self.gen_stmt(stmt);
        if let Some(e) = expr3 {
            self.gen_expr(e);
        }
        println!("  jmp {begin_label}");
        println!("{end_label}:");
    }

    fn gen_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Assign(assign) => self.gen_assign(assign),
        }
    }

    fn gen_assign(&mut self, assign: Assign) {
        if let Some(a) = assign.assign {
            if let Equality::Identity(Relational::Identity(Add::Identity(Mul::Identity(
                Unary::Pos(Primary::Ident(offset)),
            )))) = assign.eq
            {
                self.gen_lval(offset);
            } else {
                panic!("Invalid l-value for assignment: {:?}", assign.eq);
            }
            self.gen_assign(*a);
            println!("  pop rdi");
            println!("  pop rax");
            println!("  mov [rax], rdi");
            println!("  push rdi");
            return;
        }

        self.gen_equality(assign.eq);
    }

    fn gen_equality(&mut self, equality: Equality) {
        match equality {
            Equality::Identity(rel) => {
                self.gen_relational(rel);
            }
            Equality::Equal(rel, eq) => {
                self.gen_relational(rel);
                self.gen_equality(*eq);
                self.gen_binop("  cmp rdi, rax\n  sete al\n  movzb rax, al");
            }
            Equality::NotEqual(rel, eq) => {
                self.gen_relational(rel);
                self.gen_equality(*eq);
                self.gen_binop("  cmp rdi, rax\n  setne al\n  movzb rax, al");
            }
        }
    }

    fn gen_relational(&mut self, relational: Relational) {
        match relational {
            Relational::Identity(add) => {
                self.gen_add(add);
            }
            Relational::LessThan(add, rel) => {
                self.gen_add(add);
                self.gen_relational(*rel);
                self.gen_binop("  cmp rax, rdi\n  setl al\n  movzb rax, al");
            }
            Relational::LessEqual(add, rel) => {
                self.gen_add(add);
                self.gen_relational(*rel);
                self.gen_binop("  cmp rax, rdi\n  setle al\n  movzb rax, al");
            }
            Relational::GreaterThan(add, rel) => {
                self.gen_add(add);
                self.gen_relational(*rel);
                self.gen_binop("  cmp rax, rdi\n  setg al\n  movzb rax, al");
            }
            Relational::GreaterEqual(add, rel) => {
                self.gen_add(add);
                self.gen_relational(*rel);
                self.gen_binop("  cmp rax, rdi\n  setge al\n  movzb rax, al");
            }
        }
    }

    fn gen_add(&mut self, add: Add) {
        match add {
            Add::Identity(mul) => self.gen_mul(mul),
            Add::Add(mul, a) => {
                self.gen_mul(mul);
                self.gen_add(*a);
                self.gen_binop("  add rax, rdi");
            }
            Add::Sub(mul, a) => {
                self.gen_mul(mul);
                self.gen_add(*a);
                self.gen_binop("  sub rax, rdi");
            }
        }
    }

    fn gen_mul(&mut self, mul: Mul) {
        match mul {
            Mul::Identity(unary) => self.gen_unary(unary),
            Mul::Mul(unary, m) => {
                self.gen_unary(unary);
                self.gen_mul(*m);
                self.gen_binop("  imul rax, rdi");
            }
            Mul::Div(unary, m) => {
                self.gen_unary(unary);
                self.gen_mul(*m);
                self.gen_binop("  cqo\n  idiv rdi");
            }
        }
    }

    fn gen_unary(&mut self, unary: Unary) {
        match unary {
            Unary::Pos(primary) => {
                self.gen_primary(primary);
            }
            Unary::Neg(primary) => {
                self.gen_primary(primary);
                self.gen_oneop("  neg rax");
            }
        }
    }

    fn gen_primary(&mut self, primary: Primary) {
        match primary {
            Primary::Num(num) => println!("  push {}", num),
            Primary::Ident(offset) => {
                self.gen_lval(offset);
                println!("  pop rax");
                println!("  mov rax, [rax]");
                println!("  push rax");
            }
            Primary::FunctionCall(name, args) => {
                let n_args = args.len();
                if n_args > 6 {
                    panic!("Function has too many arguments (max 6), got {}", n_args);
                }
                for arg in args {
                    self.gen_expr(arg);
                }

                for reg in ARGUMENT_REGISTERS.iter().take(n_args).rev() {
                    println!("  pop {}", reg);
                }

                println!("  push rbp");
                println!("  mov rbp, rsp");
                println!("  and rsp, {}", u64::MAX - 15);
                println!("  call {name}");
                println!("  mov rsp, rbp");
                println!("  pop rbp");
                println!("  push rax");
            }
            Primary::Expr(expr) => self.gen_expr(*expr),
        }
    }
}
