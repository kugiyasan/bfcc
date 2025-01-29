use crate::{
    analyzer::{LvarOffset, SymbolTable, Ty},
    parser::{
        Assign, AssignOpKind, BinOp, BinOpKind, CompoundStmt, ConstantExpr, Declaration,
        DeclarationOrStmt, DirectDeclarator, Expr, ExternalDeclaration, FuncDef, InitDeclarator,
        Initializer, Primary, Stmt, TranslationUnit, Unary,
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

    fn _gen_binop(&mut self, s: &str) {
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

    fn gen_deref(&mut self, ty: &Ty) {
        let size = ty.sizeof(&self.symbol_table);

        let instruction_and_src = match (size, ty.is_signed()) {
            (8, _) => "mov rax",
            (4, false) => "mov eax",
            (_, true) => "movsx rax",
            (_, false) => "movzx rax",
        };
        let size_directive = match size {
            1 => "BYTE PTR",
            2 => "WORD PTR",
            4 => "DWORD PTR",
            8 => "QWORD PTR",
            _ => panic!("Unexpected variable type size: {}", size),
        };

        self.gen_oneop(&format!(
            "  {}, {} [rax]",
            instruction_and_src, size_directive
        ));
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
                if specs.iter().any(|s| s.is_typedef() || s.is_extern()) {
                    return;
                }
                for init in inits {
                    match init {
                        InitDeclarator::Declarator(d) => {
                            let ty = self.symbol_table.from_specs_and_declarator(&specs, &d);

                            if let Ty::Func(_, _) = ty {
                                return;
                            }

                            println!(".data");
                            println!("{}:", d.direct.get_name());
                            println!("  .zero {}", ty.sizeof(&self.symbol_table));
                        }
                        InitDeclarator::DeclaratorAndInitializer(d, i) => {
                            let ty = self.symbol_table.from_specs_and_declarator(&specs, &d);

                            if let Ty::Func(_, _) = ty {
                                return;
                            }

                            println!(".data");
                            println!("{}:", d.direct.get_name());
                            self.gen_global_initializer(i, &ty);
                        }
                    }
                }
            }
        }
    }

    fn gen_global_initializer(&mut self, initializer: Initializer, ty: &Ty) {
        match initializer {
            Initializer::Assign(a) => {
                let primary = a.constant_fold(&mut self.symbol_table);
                let assign_ty = a.get_type(&mut self.symbol_table);
                match primary {
                    Primary::Num(value) => match assign_ty.sizeof(&self.symbol_table) {
                        1 => println!("  .byte {}", value),
                        2 => println!("  .value {}", value),
                        4 => println!("  .long {}", value),
                        8 => println!("  .quad {}", value),
                        s => panic!("Invalid type size: {}", s),
                    },
                    Primary::String(s) => {
                        let s = s.iter().map(|c| *c as char).collect::<String>();
                        let label = self.symbol_table.get_strings().get(&s).unwrap();
                        println!("  .quad .LC{}", label);
                    }
                    _ => unreachable!(),
                };
            }
            Initializer::Vec(v) => {
                for i in v {
                    self.gen_global_initializer(i, ty);
                }
            }
        }
    }

    fn gen_declaration(&mut self, declaration: Declaration) {
        for init in declaration.inits {
            match init {
                InitDeclarator::Declarator(_) => (),
                InitDeclarator::DeclaratorAndInitializer(d, i) => {
                    let ty = self
                        .symbol_table
                        .from_specs_and_declarator(&declaration.specs, &d);
                    let var_type_size = ty.sizeof(&self.symbol_table);
                    self.gen_local_initializer(i, var_type_size)
                }
            }
        }
    }

    fn gen_local_initializer(&mut self, initializer: Initializer, var_type_size: usize) {
        match initializer {
            Initializer::Assign(a) => {
                self._gen_assign(a, var_type_size);
            }
            Initializer::Vec(_) => todo!(),
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
            Stmt::ForWithDeclaration(decl, expr2, expr3, stmt) => {
                self.gen_for_with_declaration(decl, expr2, expr3, *stmt)
            }
            Stmt::Goto(ident) => {
                println!("  jmp {}", ident);
            }
            Stmt::Return(Some(expr)) => {
                self.gen_expr(expr);
                println!("  pop rax");
                epilogue();
            }
            Stmt::Return(None) => {
                epilogue();
            }
            s => todo!("{:?}", s),
        };
    }

    fn gen_compound_stmt(&mut self, stmt: CompoundStmt) {
        for ds in stmt.0 {
            match ds {
                DeclarationOrStmt::Stmt(s) => self.gen_stmt(s),
                DeclarationOrStmt::Declaration(d) => self.gen_declaration(d),
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

    fn gen_for_with_declaration(
        &mut self,
        decl: Declaration,
        expr2: Option<Expr>,
        expr3: Option<Expr>,
        stmt: Stmt,
    ) {
        let begin_label = self.new_label();
        let end_label = self.new_label();

        self.gen_declaration(decl);
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
        let len = expr.0.len();
        for (i, assign) in expr.0.into_iter().enumerate() {
            self.gen_assign(assign);
            if i + 1 < len {
                println!("  pop rax");
            }
        }
    }

    fn gen_assign(&mut self, assign: Assign) {
        match assign {
            Assign::Const(c) => self.gen_constant_expr(c),
            Assign::Assign(unary, kind, a) => {
                if kind != AssignOpKind::Assign {
                    unreachable!("Complex assignments should be desugared by SemanticVisitor");
                }

                let var_type_size;
                if let Unary::Deref(u) = unary {
                    var_type_size = u
                        .get_type(&mut self.symbol_table)
                        .get_inner()
                        .unwrap()
                        .sizeof(&self.symbol_table);
                    self.gen_unary(*u);
                } else if let Unary::Identity(Primary::Ident(ident)) = unary {
                    var_type_size = self
                        .symbol_table
                        .get_var_type(&ident)
                        .sizeof(&self.symbol_table);
                    self.gen_lval(&ident);
                } else {
                    panic!("Invalid l-value for assignment: {:?}", unary);
                }
                self._gen_assign(*a, var_type_size);
            }
        }
    }

    fn _gen_assign(&mut self, assign: Assign, var_type_size: usize) {
        self.gen_assign(assign);

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

    fn gen_constant_expr(&mut self, c: ConstantExpr) {
        match c {
            ConstantExpr::Identity(binop) => self.gen_binop(binop),
            ConstantExpr::Ternary(cond, expr, constant_expr) => {
                let false_label = self.new_label();
                let end_label = self.new_label();

                self.gen_binop(cond);
                println!("  pop rax");
                println!("  cmp al, 0");
                println!("  je {}", false_label);

                self.gen_expr(expr);
                println!("  jmp {}", end_label);

                println!("{}:", false_label);
                self.gen_constant_expr(*constant_expr);
                println!("{}:", end_label);
            }
        }
    }

    fn gen_binop(&mut self, binop: BinOp) {
        match binop {
            BinOp::Binary(BinOpKind::LogicalOr, left, right) => self.gen_logical_or(*left, *right),
            BinOp::Binary(BinOpKind::LogicalAnd, left, right) => {
                self.gen_logical_and(*left, *right)
            }
            BinOp::Binary(kind, left, right) => {
                self.gen_binop(*left);
                self.gen_binop(*right);
                self.gen_bin_op_kind(kind);
            }
            BinOp::Unary(unary) => self.gen_unary(unary),
        }
    }

    fn gen_logical_or(&mut self, left: BinOp, right: BinOp) {
        let true_label = self.new_label();
        let false_label = self.new_label();
        let end_label = self.new_label();

        self.gen_binop(left);
        println!("  pop rax");
        println!("  cmp rax, 0");
        println!("  jne {}", true_label);

        self.gen_binop(right);
        println!("  pop rax");
        println!("  cmp rax, 0");
        println!("  je {}", false_label);

        println!("{}:", true_label);
        println!("  push 1");
        println!("  jmp {}", end_label);
        println!("{}:", false_label);
        println!("  push 0");
        println!("{}:", end_label);
    }

    fn gen_logical_and(&mut self, left: BinOp, right: BinOp) {
        let false_label = self.new_label();
        let end_label = self.new_label();

        self.gen_binop(left);
        println!("  pop rax");
        println!("  cmp rax, 0");
        println!("  je {}", false_label);

        self.gen_binop(right);
        println!("  pop rax");
        println!("  cmp rax, 0");
        println!("  je {}", false_label);

        println!("  push 1");
        println!("  jmp {}", end_label);
        println!("{}:", false_label);
        println!("  push 0");
        println!("{}:", end_label);
    }

    fn gen_bin_op_kind(&mut self, kind: BinOpKind) {
        match kind {
            BinOpKind::LogicalOr => unreachable!(),
            BinOpKind::LogicalAnd => unreachable!(),
            BinOpKind::BitwiseOr => self._gen_binop("  or rax, rdi"),
            BinOpKind::BitwiseXor => self._gen_binop("  xor rax, rdi"),
            BinOpKind::BitwiseAnd => self._gen_binop("  and rax, rdi"),

            BinOpKind::Equal => self._gen_binop("  cmp rdi, rax\n  sete al\n  movzb rax, al"),
            BinOpKind::NotEqual => self._gen_binop("  cmp rdi, rax\n  setne al\n  movzb rax, al"),

            BinOpKind::LessThan => self._gen_binop("  cmp rax, rdi\n  setl al\n  movzb rax, al"),
            BinOpKind::LessEqual => self._gen_binop("  cmp rax, rdi\n  setle al\n  movzb rax, al"),
            BinOpKind::GreaterThan => self._gen_binop("  cmp rax, rdi\n  setg al\n  movzb rax, al"),
            BinOpKind::GreaterEqual => {
                self._gen_binop("  cmp rax, rdi\n  setge al\n  movzb rax, al")
            }

            BinOpKind::LeftShift => self._gen_binop("  mov rcx, rdi\n  sal rax, cl"),
            BinOpKind::RightShift => self._gen_binop("  mov rcx, rdi\n  sar rax, cl"),

            BinOpKind::Add => self._gen_binop("  add rax, rdi"),
            BinOpKind::Sub => self._gen_binop("  sub rax, rdi"),
            BinOpKind::Mul => self._gen_binop("  imul rax, rdi"),
            BinOpKind::Div => self._gen_binop("  cqo\n  idiv rdi"),
            BinOpKind::Mod => self._gen_binop("  cqo\n  idiv rdi\nmov rax, rdx"),
        }
    }

    fn gen_unary(&mut self, unary: Unary) {
        match unary {
            Unary::Identity(primary) => self.gen_primary(primary),
            Unary::Cast(_, u) => self.gen_unary(*u),
            Unary::Neg(unary) => {
                self.gen_unary(*unary);
                self.gen_oneop("  neg rax");
            }
            Unary::Ref(unary) => match *unary {
                Unary::Identity(Primary::Ident(ident)) => self.gen_lval(&ident),
                Unary::Deref(u) => self.gen_unary(*u),
                _ => panic!("properly gen_lval only when the expression is an l-value"),
            },
            Unary::Deref(unary) => {
                let ty = unary.get_type(&mut self.symbol_table);
                self.gen_unary(*unary);
                let inner = ty.get_inner();
                if matches!(inner, Some(Ty::Struct(_) | Ty::Union(_))) {
                    return;
                }
                self.gen_deref(&inner.unwrap());
            }
            Unary::BitwiseNot(u) => {
                self.gen_unary(*u);
                self.gen_oneop("  not rax")
            }
            Unary::LogicalNot(u) => {
                self.gen_unary(*u);
                println!("  pop rax");
                println!("  cmp rax, 0");
                println!("  sete al");
                println!("  movzx eax, al");
                println!("  push rax");
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
            u => unreachable!("{:?} should have been desugared", u),
        }
    }

    fn gen_primary(&mut self, primary: Primary) {
        match primary {
            Primary::Ident(ident) => {
                self.gen_lval(&ident);
                let ty = self.symbol_table.get_var_type(&ident);
                if matches!(ty, Ty::Array(_, _) | Ty::Struct(_) | Ty::Union(_)) {
                    return;
                }

                self.gen_deref(&ty);
            }
            Primary::Num(num) if !(-0x80000000..=0x80000000).contains(&num) => {
                println!("  mov rax, {}\n  push rax", num)
            }
            Primary::Num(num) => println!("  push {}", num),
            Primary::String(b) => self.gen_lval(&b.iter().map(|&b| b as char).collect::<String>()),
            Primary::Expr(expr) => self.gen_expr(*expr),
        }
    }
}
