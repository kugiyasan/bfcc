use crate::{
    analyzer::{LvarOffset, SymbolTable, Ty},
    parser::{
        Assign, AssignOpKind, BinOpKind, CompoundStmt, ConstantExpr, Declaration,
        DeclarationOrStmt, DirectDeclarator, Expr, ExprKind, ExternalDeclaration, FuncDef,
        InitDeclarator, Primary, Stmt, TranslationUnit, Unary,
    },
};

const ARGUMENT_REGISTERS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

pub struct Codegen {
    label_index: usize,
    symbol_table: SymbolTable,
}

fn epilogue() {
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}

fn gen_deref(size: usize) {
    // todo: if ty.is_signed() { "movsx" } else { "movzx" };
    let instruction = match size {
        8 => "mov",
        _ => "movsx",
    };
    let size_directive = match size {
        1 => "BYTE PTR",
        2 => "WORD PTR",
        4 => "DWORD PTR",
        8 => "QWORD PTR",
        _ => panic!("Unexpected variable type size: {}", size),
    };

    println!("  pop rax");
    println!("  {} rax, {} [rax]", instruction, size_directive);
    println!("  push rax");
}

impl Codegen {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            label_index: 0,
            symbol_table,
        }
    }

    pub fn generate(&mut self, translation_unit: TranslationUnit) {
        println!(".intel_syntax noprefix");
        println!(".globl main");
        self.gen_strings();
        self.gen_translation_unit(translation_unit);
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

    fn gen_lval(&mut self, name: &str) {
        match self.symbol_table.get_lvar_offset(name) {
            LvarOffset::Local(offset) => {
                println!("  mov rax, rbp");
                println!("  sub rax, {}", offset);
                println!("  push rax");
            }
            LvarOffset::Global => {
                println!("  lea rax, QWORD PTR {}[rip]", name);
                println!("  push rax");
            }
            LvarOffset::String(string_id) => {
                let name = format!(".LC{}", string_id);
                println!("  lea rax, QWORD PTR {}[rip]", name);
                println!("  push rax");
            }
        }
    }

    fn gen_strings(&mut self) {
        for (s, i) in self.symbol_table.get_strings() {
            println!(".LC{i}:");
            println!("  .string \"{}\"", s);
        }
    }

    fn gen_translation_unit(&mut self, translation_unit: TranslationUnit) {
        for external_declaration in translation_unit.0 {
            self.gen_external_declaration(external_declaration);
        }
    }

    fn gen_external_declaration(&mut self, external_declaration: ExternalDeclaration) {
        match external_declaration {
            ExternalDeclaration::FuncDef(f) => self.gen_func_def(f),
            ExternalDeclaration::Declaration(Declaration { specs, inits }) => {
                let InitDeclarator::Declarator(ref declarator) = inits[0] else {
                    todo!();
                };
                let ty = Ty::from_specs_and_declarator(&specs, declarator);

                println!(".data");
                println!("{}:", declarator.direct.get_name());
                println!("  .zero {}", ty.sizeof());
            }
        }
    }

    fn gen_func_def(
        &mut self,
        FuncDef {
            specs: _,
            declarator,
            stmt,
        }: FuncDef,
    ) {
        self.symbol_table
            .set_current_func(declarator.direct.get_name());

        let DirectDeclarator::ParamTypeList(dd, param_type_list) = declarator.direct else {
            panic!(
                "DirectDeclarator is not a ParamTypeList: {:?}",
                declarator.direct
            );
        };
        let DirectDeclarator::Ident(ident) = *dd else {
            panic!("Function name is not an identifier: {:?}", dd);
        };

        println!(".text");
        println!("{ident}:");
        println!("  push rbp");
        println!("  mov rbp, rsp");
        for reg in ARGUMENT_REGISTERS.iter().take(param_type_list.params.len()) {
            println!("  push {reg}");
        }
        let local_offset = self.symbol_table.get_offset(&ident);
        println!("  sub rsp, {}", local_offset);

        self.gen_compound_stmt(stmt);
        epilogue();
    }

    fn gen_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::SemiColon => (),
            Stmt::Expr(expr) => {
                self.gen_expr(expr);
                println!("  pop rax");
            }
            Stmt::Label(ident, stmt) => {
                println!("{}:", ident);
                self.gen_stmt(*stmt);
            }
            Stmt::Compound(stmt) => self.gen_compound_stmt(stmt),
            Stmt::If(expr, stmt, None) => self.gen_if(expr, *stmt),
            Stmt::If(expr, stmt, Some(else_stmt)) => self.gen_if_else(expr, *stmt, *else_stmt),
            Stmt::While(expr, stmt) => self.gen_while(expr, *stmt),
            Stmt::DoWhile(stmt, expr) => self.gen_do_while(*stmt, expr),
            Stmt::For(expr1, expr2, expr3, stmt) => self.gen_for(expr1, expr2, expr3, *stmt),
            Stmt::Goto(ident) => {
                println!("  jmp {}", ident);
            }
            Stmt::Return(expr) => {
                self.gen_expr(expr);
                println!("  pop rax");
                epilogue();
            }
            _ => todo!(),
        };
    }

    fn gen_compound_stmt(&mut self, stmt: CompoundStmt) {
        for ds in stmt.0 {
            match ds {
                DeclarationOrStmt::Stmt(s) => self.gen_stmt(s),
                DeclarationOrStmt::Declaration(_) => (),
            }
        }
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

    fn gen_do_while(&mut self, stmt: Stmt, expr: Expr) {
        let begin_label = self.new_label();

        println!("{begin_label}:");
        self.gen_stmt(stmt);
        self.gen_expr(expr);
        println!("  pop rax");
        println!("  cmp rax, 0");
        println!("  jne {begin_label}");
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
        for assign in expr.0 {
            self.gen_assign(assign);
        }
    }

    fn gen_assign(&mut self, assign: Assign) {
        match assign {
            Assign::Const(c) => self.gen_constant_expr(c),
            Assign::Assign(unary, kind, a) => {
                let mut var_type_size = 999;
                if let Unary::Deref(u) = unary {
                    var_type_size = u.get_type(&self.symbol_table).get_inner().unwrap().sizeof();
                    self.gen_unary(*u);
                } else if let Unary::Identity(Primary::Ident(ident)) = unary {
                    var_type_size = self.symbol_table.get_var_type(&ident).sizeof();
                    self.gen_lval(&ident);
                } else if let Unary::Field(u, f) = unary {
                    let Ty::Struct(sds) = u.get_type(&self.symbol_table) else {
                        unreachable!()
                    };
                    let mut offset = 0;
                    for (s, ty) in sds {
                        if s == f {
                            var_type_size = ty.sizeof();
                            break;
                        }
                        offset += ty.sizeof();
                    }

                    self.gen_unary(*u);
                    println!("  pop rax");
                    println!("  add rax, {}", offset);
                    println!("  push rax");
                } else {
                    panic!("Invalid l-value for assignment: {:?}", unary);
                }
                self.gen_assign(*a);

                match kind {
                    AssignOpKind::Assign => {
                        let src_reg = match var_type_size {
                            1 => "dil",
                            2 => "di",
                            4 => "edi",
                            8 => "rdi",
                            _ => panic!("Unexpected variable type size: {}", var_type_size),
                        };
                        println!("  pop rdi");
                        println!("  pop rax");
                        println!("  mov [rax], {}", src_reg);
                        println!("  push rdi");
                    }
                    _ => todo!(),
                }
            }
        }
    }

    fn gen_constant_expr(&mut self, c: ConstantExpr) {
        match c {
            ConstantExpr::Identity(expr_kind) => self.gen_expr_kind(expr_kind),
            ConstantExpr::Ternary(_kind, _expr, _c) => todo!(),
        }
    }

    fn gen_expr_kind(&mut self, expr_kind: ExprKind) {
        match expr_kind {
            ExprKind::Binary(kind, left, right) => {
                self.gen_expr_kind(*left);
                self.gen_expr_kind(*right);
                self.gen_bin_op_kind(kind);
            }
            ExprKind::Unary(unary) => self.gen_unary(unary),
        }
    }

    fn gen_bin_op_kind(&mut self, kind: BinOpKind) {
        match kind {
            BinOpKind::Equal => self.gen_binop("  cmp rdi, rax\n  sete al\n  movzb rax, al"),
            BinOpKind::NotEqual => self.gen_binop("  cmp rdi, rax\n  setne al\n  movzb rax, al"),

            BinOpKind::LessThan => self.gen_binop("  cmp rax, rdi\n  setl al\n  movzb rax, al"),
            BinOpKind::LessEqual => self.gen_binop("  cmp rax, rdi\n  setle al\n  movzb rax, al"),
            BinOpKind::GreaterThan => self.gen_binop("  cmp rax, rdi\n  setg al\n  movzb rax, al"),
            BinOpKind::GreaterEqual => {
                self.gen_binop("  cmp rax, rdi\n  setge al\n  movzb rax, al")
            }

            BinOpKind::Add => self.gen_binop("  add rax, rdi"),
            BinOpKind::Sub => self.gen_binop("  sub rax, rdi"),
            BinOpKind::Mul => self.gen_binop("  imul rax, rdi"),
            BinOpKind::Div => self.gen_binop("  cqo\n  idiv rdi"),
            BinOpKind::Mod => self.gen_binop("  cqo\n  idiv rdi\nmov rax, rdx"),

            _ => todo!(),
        }
    }

    fn gen_unary(&mut self, unary: Unary) {
        match unary {
            Unary::Identity(primary) => {
                self.gen_primary(primary);
            }
            Unary::Neg(unary) => {
                self.gen_unary(*unary);
                self.gen_oneop("  neg rax");
            }
            Unary::Ref(unary) => match *unary {
                Unary::Identity(Primary::Ident(ident)) => self.gen_lval(&ident),
                Unary::Deref(u) => self.gen_unary(*u),
                _ => todo!("properly gen_lval only when the expression is an l-value"),
            },
            Unary::Deref(unary) => {
                let ty = unary.get_type(&self.symbol_table);
                self.gen_unary(*unary);
                gen_deref(ty.get_inner().unwrap().sizeof());
            }
            Unary::Call(unary, expr) => {
                let Unary::Identity(Primary::Ident(ident)) = *unary else {
                    panic!("Expected single identifier function names");
                };
                let args = if let Some(a) = expr { a.0 } else { vec![] };
                let n_args = args.len();
                if n_args > 6 {
                    panic!("Function has too many arguments (max 6), got {}", n_args);
                }
                for arg in args {
                    self.gen_assign(arg);
                }

                for reg in ARGUMENT_REGISTERS.iter().take(n_args).rev() {
                    println!("  pop {}", reg);
                }

                println!("  push rbp");
                println!("  mov rbp, rsp");
                println!("  and rsp, {}", u64::MAX - 15);
                println!("  call {ident}");
                println!("  mov rsp, rbp");
                println!("  pop rbp");
                println!("  push rax");
            }
            Unary::Field(unary, field) => {
                let Ty::Struct(sds) = unary.get_type(&self.symbol_table) else {
                    unreachable!()
                };
                let mut offset = 0;
                let mut size = 0;
                for (s, ty) in sds {
                    if s == field {
                        size = ty.sizeof();
                        break;
                    }
                    offset += ty.sizeof();
                }

                self.gen_unary(*unary);
                println!("  pop rax");
                println!("  add rax, {}", offset);
                println!("  push rax");
                gen_deref(size);
            }
            _ => todo!(),
        }
    }

    fn gen_primary(&mut self, primary: Primary) {
        match primary {
            Primary::Ident(ident) => {
                self.gen_lval(&ident);
                let ty = self.symbol_table.get_var_type(&ident);
                if let Ty::Array(_, _) = ty {
                    return;
                }

                gen_deref(ty.sizeof());
            }
            Primary::Num(num) => println!("  push {}", num),
            Primary::String(b) => self.gen_lval(&b.iter().map(|&b| b as char).collect::<String>()),
            Primary::Expr(expr) => self.gen_expr(*expr),
        }
    }
}
