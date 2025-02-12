use std::io::{BufWriter, Write};

use crate::{
    analyzer::{LvarOffset, SymbolTable, Ty},
    parser::{
        Assign, AssignOpKind, BinOp, BinOpKind, CompoundStmt, ConstantExpr, Declaration,
        DeclarationOrStmt, DirectDeclarator, Expr, ExternalDeclaration, FuncDef, InitDeclarator,
        Initializer, Primary, Stmt, TranslationUnit, Unary,
    },
};

const ARGUMENT_REGISTERS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

macro_rules! w {
    ($dst:expr, $($arg:tt)*) => {
        writeln!($dst, $($arg)*).unwrap()
    };
}

#[derive(Debug, Default)]
struct SwitchData {
    cases: Vec<(i64, String)>,
    default: Option<String>,
}

impl SwitchData {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_case(&mut self, n: i64, label: String) {
        if self.cases.iter().any(|(c, _)| *c == n) {
            panic!("2 cases with the same value");
        }
        self.cases.push((n, label));
    }

    pub fn add_default(&mut self, label: String) {
        if self.default.is_some() {
            panic!("2 default in switch");
        }
        self.default = Some(label);
    }

    pub fn merge(&mut self, sd: SwitchData) {
        for (n, label) in sd.cases {
            self.add_case(n, label);
        }

        if let Some(label) = sd.default {
            self.add_default(label);
        }
    }
}

pub struct Codegen {
    w: BufWriter<Box<dyn Write>>,
    label_index: usize,
    symbol_table: SymbolTable,
}

impl Codegen {
    pub fn new(w: Box<dyn Write>, symbol_table: SymbolTable) -> Self {
        Self {
            w: BufWriter::new(w),
            label_index: 0,
            symbol_table,
        }
    }

    pub fn generate(&mut self, translation_unit: TranslationUnit) {
        w!(&mut self.w, ".intel_syntax noprefix");
        self.gen_strings();
        self.gen_translation_unit(translation_unit);
    }

    fn epilogue(&mut self) {
        w!(&mut self.w, "  mov rsp, rbp");
        w!(&mut self.w, "  pop rbp");
        w!(&mut self.w, "  ret");
    }

    fn new_label(&mut self) -> String {
        let s = format!(".L{:0>3}", self.label_index);
        self.label_index += 1;
        s
    }

    fn gen_oneop(&mut self, s: &str) {
        w!(&mut self.w, "  pop rax");
        w!(&mut self.w, "{}", s);
        w!(&mut self.w, "  push rax");
    }

    fn _gen_binop(&mut self, s: &str) {
        w!(&mut self.w, "  pop rdi");
        w!(&mut self.w, "  pop rax");
        w!(&mut self.w, "{}", s);
        w!(&mut self.w, "  push rax");
    }

    fn gen_lval(&mut self, name: &str) {
        match self.symbol_table.get_lvar_offset(name) {
            LvarOffset::Local(offset) => {
                w!(&mut self.w, "  mov rax, rbp");
                w!(&mut self.w, "  sub rax, {}", offset);
                w!(&mut self.w, "  push rax");
            }
            LvarOffset::Global => {
                w!(&mut self.w, "  lea rax, QWORD PTR {}[rip]", name);
                w!(&mut self.w, "  push rax");
            }
            LvarOffset::String(string_id) => {
                let name = format!(".LC{}", string_id);
                w!(&mut self.w, "  lea rax, QWORD PTR {}[rip]", name);
                w!(&mut self.w, "  push rax");
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
            w!(&mut self.w, ".LC{i}:");
            w!(&mut self.w, "  .string \"{}\"", s);
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

                            let name = d.direct.get_name();
                            if !specs.iter().any(|s| s.is_static()) {
                                w!(&mut self.w, ".globl {}", name);
                            }
                            w!(&mut self.w, ".data");
                            w!(&mut self.w, "{}:", name);
                            w!(&mut self.w, "  .zero {}", ty.sizeof(&self.symbol_table));
                        }
                        InitDeclarator::DeclaratorAndInitializer(d, i) => {
                            let ty = self.symbol_table.from_specs_and_declarator(&specs, &d);

                            if let Ty::Func(_, _) = ty {
                                return;
                            }

                            let name = d.direct.get_name();
                            if !specs.iter().any(|s| s.is_static()) {
                                w!(&mut self.w, ".globl {}", name);
                            }
                            w!(&mut self.w, ".data");
                            w!(&mut self.w, "{}:", name);
                            self.gen_global_initializer(i, &ty);
                        }
                    }
                }
            }
        }
    }

    fn gen_global_initializer(&mut self, initializer: Initializer, expected_ty: &Ty) {
        match initializer {
            Initializer::Assign(a) => {
                if let Assign::Const(ConstantExpr::Identity(BinOp::Unary(Unary::Ref(u)))) = a {
                    let Unary::Identity(Primary::Ident(i)) = *u else {
                        panic!("Not an identifier");
                    };
                    w!(&mut self.w, "  .quad {}", i);
                    return;
                }

                let primary = a.constant_fold(&mut self.symbol_table);
                match primary {
                    Primary::Num(value) => match expected_ty.sizeof(&self.symbol_table) {
                        1 => w!(&mut self.w, "  .byte {}", value),
                        2 => w!(&mut self.w, "  .value {}", value),
                        4 => w!(&mut self.w, "  .long {}", value),
                        8 => w!(&mut self.w, "  .quad {}", value),
                        s => panic!("Invalid type size: {}", s),
                    },
                    Primary::String(s) => {
                        let s = s.iter().map(|c| *c as char).collect::<String>();
                        let label = self.symbol_table.get_strings().get(&s).unwrap();
                        w!(&mut self.w, "  .quad .LC{}", label);
                    }
                    _ => unreachable!(),
                };
            }
            Initializer::Vec(v) => {
                if let Ty::Array(ty, _) = expected_ty {
                    for i in v {
                        self.gen_global_initializer(i, ty);
                    }
                } else if let Ty::Struct(name) = expected_ty {
                    let sds = self.symbol_table.get_struct_definition(name).clone();
                    for (i, (_, ty)) in v.into_iter().zip(sds) {
                        self.gen_global_initializer(i, &ty);
                    }
                } else {
                    panic!("Illegal Vec initializer for ty {:?}", expected_ty);
                }
            }
        }
    }

    fn gen_declaration(&mut self, declaration: Declaration) {
        for init in declaration.inits {
            match init {
                InitDeclarator::Declarator(_) => (),
                InitDeclarator::DeclaratorAndInitializer(d, i) => {
                    self.gen_lval(&d.direct.get_name());

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
            specs,
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
        let DirectDeclarator::Ident(name) = *dd else {
            panic!("Function name is not an identifier: {:?}", dd);
        };

        w!(&mut self.w, ".text");
        if !specs.iter().any(|s| s.is_static()) {
            w!(&mut self.w, ".globl {}", name);
        }
        w!(&mut self.w, "{}:", name);
        w!(&mut self.w, "  push rbp");
        w!(&mut self.w, "  mov rbp, rsp");
        for reg in ARGUMENT_REGISTERS.iter().take(param_type_list.params.len()) {
            w!(&mut self.w, "  push {reg}");
        }
        let local_offset = self.symbol_table.get_offset(&name);
        w!(&mut self.w, "  sub rsp, {}", local_offset);

        self.gen_compound_stmt(stmt, None, None);
        self.epilogue();
    }

    fn gen_stmt(
        &mut self,
        stmt: Stmt,
        break_label: Option<&str>,
        continue_label: Option<&str>,
    ) -> SwitchData {
        match stmt {
            Stmt::Label(label, stmt) => {
                w!(&mut self.w, "{}:", label);
                self.gen_stmt(*stmt, break_label, continue_label)
            }
            Stmt::Case(c, stmt) => {
                let label = self.new_label();
                w!(&mut self.w, "{}:", label);
                let mut sd = self.gen_stmt(*stmt, break_label, continue_label);
                let n = c.constant_fold(&mut self.symbol_table).expect_num();
                sd.add_case(n, label);
                sd
            }
            Stmt::Default(stmt) => {
                let label = self.new_label();
                w!(&mut self.w, "{}:", label);
                let mut sd = self.gen_stmt(*stmt, break_label, continue_label);
                sd.add_default(label);
                sd
            }

            Stmt::SemiColon => SwitchData::new(),
            Stmt::Expr(expr) => {
                self.gen_expr(expr);
                w!(&mut self.w, "  pop rax");
                SwitchData::new()
            }
            Stmt::Compound(stmt) => self.gen_compound_stmt(stmt, break_label, continue_label),

            Stmt::If(expr, stmt, None) => self.gen_if(expr, *stmt, break_label, continue_label),
            Stmt::If(expr, stmt, Some(else_stmt)) => {
                self.gen_if_else(expr, *stmt, *else_stmt, break_label, continue_label)
            }
            Stmt::Switch(expr, stmt) => {
                self.gen_switch(expr, *stmt, continue_label);
                SwitchData::new()
            }

            Stmt::While(expr, stmt) => self.gen_while(expr, *stmt),
            Stmt::DoWhile(stmt, expr) => self.gen_do_while(*stmt, expr),
            Stmt::For(expr1, expr2, expr3, stmt) => self.gen_for(expr1, expr2, expr3, *stmt),
            Stmt::ForWithDeclaration(decl, expr2, expr3, stmt) => {
                self.gen_for_with_declaration(decl, expr2, expr3, *stmt)
            }

            Stmt::Goto(ident) => {
                w!(&mut self.w, "  jmp {}", ident);
                SwitchData::new()
            }
            Stmt::Continue => {
                let continue_label =
                    continue_label.unwrap_or_else(|| panic!("Continue not in a loop"));
                w!(&mut self.w, "  jmp {}", continue_label);
                SwitchData::new()
            }
            Stmt::Break => {
                let break_label = break_label.unwrap_or_else(|| panic!("Break not in a loop"));
                w!(&mut self.w, "  jmp {}", break_label);
                SwitchData::new()
            }
            Stmt::Return(Some(expr)) => {
                self.gen_expr(expr);
                w!(&mut self.w, "  pop rax");
                self.epilogue();
                SwitchData::new()
            }
            Stmt::Return(None) => {
                self.epilogue();
                SwitchData::new()
            }
        }
    }

    fn gen_compound_stmt(
        &mut self,
        stmt: CompoundStmt,
        break_label: Option<&str>,
        continue_label: Option<&str>,
    ) -> SwitchData {
        let mut switch_data = SwitchData::new();
        for ds in stmt.0 {
            match ds {
                DeclarationOrStmt::Stmt(s) => {
                    let sd = self.gen_stmt(s, break_label, continue_label);
                    switch_data.merge(sd);
                }
                DeclarationOrStmt::Declaration(d) => self.gen_declaration(d),
            }
        }

        switch_data
    }

    fn gen_if(
        &mut self,
        expr: Expr,
        stmt: Stmt,
        break_label: Option<&str>,
        continue_label: Option<&str>,
    ) -> SwitchData {
        let end_label = self.new_label();
        self.gen_expr(expr);
        w!(&mut self.w, "  pop rax");
        w!(&mut self.w, "  cmp rax, 0");
        w!(&mut self.w, "  je {end_label}");
        let sd = self.gen_stmt(stmt, break_label, continue_label);
        w!(&mut self.w, "{end_label}:");
        sd
    }

    fn gen_if_else(
        &mut self,
        expr: Expr,
        stmt: Stmt,
        else_stmt: Stmt,
        break_label: Option<&str>,
        continue_label: Option<&str>,
    ) -> SwitchData {
        let end_label = self.new_label();
        let else_label = self.new_label();

        self.gen_expr(expr);
        w!(&mut self.w, "  pop rax");
        w!(&mut self.w, "  cmp rax, 0");
        w!(&mut self.w, "  je {else_label}");
        let mut sd1 = self.gen_stmt(stmt, break_label, continue_label);
        w!(&mut self.w, "  jmp {end_label}");
        w!(&mut self.w, "{else_label}:",);
        let sd2 = self.gen_stmt(else_stmt, break_label, continue_label);
        w!(&mut self.w, "{end_label}:");

        sd1.merge(sd2);
        sd1
    }

    fn gen_switch(&mut self, expr: Expr, stmt: Stmt, continue_label: Option<&str>) {
        let break_label = self.new_label();
        let switch_label = self.new_label();

        self.gen_expr(expr);
        w!(&mut self.w, "  jmp {}", switch_label);
        let sd = self.gen_stmt(stmt, Some(&break_label), continue_label);

        w!(&mut self.w, "  jmp {}", break_label);
        w!(&mut self.w, "{switch_label}:");
        for (n, label) in sd.cases {
            w!(&mut self.w, "  cmp rax, {}", n);
            w!(&mut self.w, "  je {}", label);
        }
        if let Some(label) = sd.default {
            w!(&mut self.w, "  jmp {}", label);
        }

        w!(&mut self.w, "{break_label}:");
    }

    fn gen_while(&mut self, expr: Expr, stmt: Stmt) -> SwitchData {
        let begin_label = self.new_label();
        let end_label = self.new_label();

        w!(&mut self.w, "{begin_label}:");
        self.gen_expr(expr);
        w!(&mut self.w, "  pop rax");
        w!(&mut self.w, "  cmp rax, 0");
        w!(&mut self.w, "  je {end_label}");

        let sd = self.gen_stmt(stmt, Some(&end_label), Some(&begin_label));

        w!(&mut self.w, "  jmp {begin_label}");
        w!(&mut self.w, "{end_label}:");

        sd
    }

    fn gen_do_while(&mut self, stmt: Stmt, expr: Expr) -> SwitchData {
        let begin_label = self.new_label();
        let end_label = self.new_label();

        w!(&mut self.w, "{begin_label}:");

        let sd = self.gen_stmt(stmt, Some(&end_label), Some(&begin_label));

        self.gen_expr(expr);
        w!(&mut self.w, "  pop rax");
        w!(&mut self.w, "  cmp rax, 0");
        w!(&mut self.w, "  jne {begin_label}");

        w!(&mut self.w, "{end_label}:");

        sd
    }

    fn gen_for(
        &mut self,
        expr1: Option<Expr>,
        expr2: Option<Expr>,
        expr3: Option<Expr>,
        stmt: Stmt,
    ) -> SwitchData {
        let begin_label = self.new_label();
        let continue_label = self.new_label();
        let end_label = self.new_label();

        if let Some(e) = expr1 {
            self.gen_expr(e);
        }
        w!(&mut self.w, "{begin_label}:");
        if let Some(e) = expr2 {
            self.gen_expr(e);
            w!(&mut self.w, "  pop rax");
            w!(&mut self.w, "  cmp rax, 0");
            w!(&mut self.w, "  je {end_label}");
        }

        let sd = self.gen_stmt(stmt, Some(&end_label), Some(&continue_label));

        w!(&mut self.w, "{continue_label}:");
        if let Some(e) = expr3 {
            self.gen_expr(e);
        }
        w!(&mut self.w, "  jmp {begin_label}");
        w!(&mut self.w, "{end_label}:");
        sd
    }

    fn gen_for_with_declaration(
        &mut self,
        decl: Declaration,
        expr2: Option<Expr>,
        expr3: Option<Expr>,
        stmt: Stmt,
    ) -> SwitchData {
        let begin_label = self.new_label();
        let continue_label = self.new_label();
        let end_label = self.new_label();

        self.gen_declaration(decl);
        w!(&mut self.w, "{begin_label}:");
        if let Some(e) = expr2 {
            self.gen_expr(e);
            w!(&mut self.w, "  pop rax");
            w!(&mut self.w, "  cmp rax, 0");
            w!(&mut self.w, "  je {end_label}");
        }

        let sd = self.gen_stmt(stmt, Some(&end_label), Some(&continue_label));

        w!(&mut self.w, "{continue_label}:");
        if let Some(e) = expr3 {
            self.gen_expr(e);
        }
        w!(&mut self.w, "  jmp {begin_label}");
        w!(&mut self.w, "{end_label}:");

        sd
    }

    fn gen_expr(&mut self, expr: Expr) {
        let len = expr.0.len();
        for (i, assign) in expr.0.into_iter().enumerate() {
            self.gen_assign(assign);
            if i + 1 < len {
                w!(&mut self.w, "  pop rax");
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
        w!(&mut self.w, "  pop rdi");
        w!(&mut self.w, "  pop rax");
        w!(&mut self.w, "  mov [rax], {}", src_reg);
        w!(&mut self.w, "  push rdi");
    }

    fn gen_constant_expr(&mut self, c: ConstantExpr) {
        match c {
            ConstantExpr::Identity(binop) => self.gen_binop(binop),
            ConstantExpr::Ternary(cond, expr, constant_expr) => {
                let false_label = self.new_label();
                let end_label = self.new_label();

                self.gen_binop(cond);
                w!(&mut self.w, "  pop rax");
                w!(&mut self.w, "  cmp al, 0");
                w!(&mut self.w, "  je {}", false_label);

                self.gen_expr(expr);
                w!(&mut self.w, "  jmp {}", end_label);

                w!(&mut self.w, "{}:", false_label);
                self.gen_constant_expr(*constant_expr);
                w!(&mut self.w, "{}:", end_label);
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
        w!(&mut self.w, "  pop rax");
        w!(&mut self.w, "  cmp rax, 0");
        w!(&mut self.w, "  jne {}", true_label);

        self.gen_binop(right);
        w!(&mut self.w, "  pop rax");
        w!(&mut self.w, "  cmp rax, 0");
        w!(&mut self.w, "  je {}", false_label);

        w!(&mut self.w, "{}:", true_label);
        w!(&mut self.w, "  push 1");
        w!(&mut self.w, "  jmp {}", end_label);
        w!(&mut self.w, "{}:", false_label);
        w!(&mut self.w, "  push 0");
        w!(&mut self.w, "{}:", end_label);
    }

    fn gen_logical_and(&mut self, left: BinOp, right: BinOp) {
        let false_label = self.new_label();
        let end_label = self.new_label();

        self.gen_binop(left);
        w!(&mut self.w, "  pop rax");
        w!(&mut self.w, "  cmp rax, 0");
        w!(&mut self.w, "  je {}", false_label);

        self.gen_binop(right);
        w!(&mut self.w, "  pop rax");
        w!(&mut self.w, "  cmp rax, 0");
        w!(&mut self.w, "  je {}", false_label);

        w!(&mut self.w, "  push 1");
        w!(&mut self.w, "  jmp {}", end_label);
        w!(&mut self.w, "{}:", false_label);
        w!(&mut self.w, "  push 0");
        w!(&mut self.w, "{}:", end_label);
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
                u => {
                    dbg!(u);
                    panic!("properly gen_lval only when the expression is an l-value")
                }
            },
            Unary::Deref(unary) => {
                let ty = unary.get_type(&mut self.symbol_table);
                self.gen_unary(*unary);
                let inner = ty.get_inner().unwrap();
                if matches!(inner, Ty::Array(_, _) | Ty::Struct(_) | Ty::Union(_)) {
                    return;
                }
                self.gen_deref(&inner);
            }
            Unary::BitwiseNot(u) => {
                self.gen_unary(*u);
                self.gen_oneop("  not rax")
            }
            Unary::LogicalNot(u) => {
                self.gen_unary(*u);
                w!(&mut self.w, "  pop rax");
                w!(&mut self.w, "  cmp rax, 0");
                w!(&mut self.w, "  sete al");
                w!(&mut self.w, "  movzx eax, al");
                w!(&mut self.w, "  push rax");
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
                    w!(&mut self.w, "  pop {}", reg);
                }

                w!(&mut self.w, "  push rbp");
                w!(&mut self.w, "  mov rbp, rsp");
                w!(&mut self.w, "  and rsp, {}", u64::MAX - 15);
                w!(&mut self.w, "  call {ident}");
                w!(&mut self.w, "  mov rsp, rbp");
                w!(&mut self.w, "  pop rbp");
                w!(&mut self.w, "  push rax");
            }
            u => unreachable!("{:?} should have been desugared", u),
        }
    }

    fn gen_primary(&mut self, primary: Primary) {
        match primary {
            Primary::Ident(ident) => {
                if let Some(value) = self.symbol_table.get_enum_value(&ident) {
                    w!(&mut self.w, "  push {}", value);
                    return;
                }
                self.gen_lval(&ident);
                let ty = self.symbol_table.get_var_type(&ident);
                if matches!(ty, Ty::Array(_, _) | Ty::Struct(_) | Ty::Union(_)) {
                    return;
                }

                self.gen_deref(&ty);
            }
            Primary::Num(num) if !(-0x80000000..=0x80000000).contains(&num) => {
                w!(&mut self.w, "  mov rax, {}\n  push rax", num)
            }
            Primary::Num(num) => w!(&mut self.w, "  push {}", num),
            Primary::String(b) => self.gen_lval(&b.iter().map(|&b| b as char).collect::<String>()),
            Primary::Expr(expr) => self.gen_expr(*expr),
        }
    }
}
