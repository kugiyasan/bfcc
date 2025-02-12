use crate::parser::{
    AbstractDeclarator, Assign, AssignOpKind, BinOp, BinOpKind, CompoundStmt, ConstantExpr,
    Declaration, DeclarationOrStmt, Declarator, DirectAbstractDeclarator, DirectDeclarator, Expr,
    ExternalDeclaration, FuncDef, InitDeclarator, Initializer, ParamDeclaration, Primary, Stmt,
    TranslationUnit, Typedefs, Unary,
};

use super::{symbol_table::SymbolTable, Ty};

#[derive(Default)]
pub struct SemanticVisitor {
    symbol_table: SymbolTable,
}

impl SemanticVisitor {
    pub fn new(typedefs: Typedefs) -> Self {
        Self {
            symbol_table: SymbolTable::new(typedefs),
        }
    }

    pub fn visit_translation_unit(
        &mut self,
        translation_unit: &mut TranslationUnit,
    ) -> SymbolTable {
        for external_declaration in translation_unit.0.iter_mut() {
            self.visit_external_declaration(external_declaration);
        }

        self.symbol_table.clone()
    }

    fn visit_external_declaration(&mut self, external_declaration: &mut ExternalDeclaration) {
        match external_declaration {
            ExternalDeclaration::FuncDef(f) => self.visit_func_def(f),
            ExternalDeclaration::Declaration(d) => self.visit_declaration(d, true),
        }
    }

    fn visit_func_def(&mut self, func_def: &mut FuncDef) {
        self.symbol_table
            .declare_global(&func_def.specs, &func_def.declarator);

        let DirectDeclarator::ParamTypeList(dd, ptl) = &mut func_def.declarator.direct else {
            panic!("Function declarator should be a ParamTypeList");
        };

        self.symbol_table.declare_func(dd.get_name());
        self.visit_direct_declarator(dd);

        for p in ptl.params.iter_mut() {
            self.visit_param_declaration(p);
        }

        for ds in func_def.stmt.0.iter_mut() {
            self.visit_declaration_or_stmt(ds);
        }
        self.symbol_table.set_current_func("".to_string());
    }

    fn visit_declarator(&mut self, declarator: &mut Declarator) {
        // declarator.pointer;
        self.visit_direct_declarator(&mut declarator.direct);
    }

    fn visit_direct_declarator(&mut self, direct_declarator: &mut DirectDeclarator) {
        match direct_declarator {
            DirectDeclarator::Ident(_) => (),
            DirectDeclarator::Declarator(d) => self.visit_declarator(d),
            DirectDeclarator::Array(dd, expr) => {
                self.visit_direct_declarator(dd);
                if let Some(c) = expr {
                    self.visit_constant_expr(c);
                }
            }
            DirectDeclarator::ParamTypeList(dd, ptl) => {
                self.visit_direct_declarator(dd);
                for p in ptl.params.iter_mut() {
                    self.visit_param_declaration(p);
                }
            }
        }
    }

    fn visit_param_declaration(&mut self, param_declaration: &mut ParamDeclaration) {
        match param_declaration {
            ParamDeclaration::Declarator(specs, d) => {
                self.visit_declarator(d);
                self.symbol_table
                    .declare_local_with_offset(specs.clone(), *d.clone(), 8);
            }
            ParamDeclaration::AbstractDeclarator(_, ad) => {
                if let Some(a) = ad {
                    self.visit_abstract_declarator(a);
                }
            }
        };
    }

    fn visit_abstract_declarator(&mut self, ad: &mut AbstractDeclarator) {
        match ad {
            AbstractDeclarator::Pointer(_) => (),
            AbstractDeclarator::DirectAbstractDeclarator(_, dad) => {
                self.visit_direct_abstract_declarator(dad)
            }
        }
    }

    fn visit_direct_abstract_declarator(&mut self, dad: &mut DirectAbstractDeclarator) {
        match dad {
            DirectAbstractDeclarator::AbstractDeclarator(ad) => self.visit_abstract_declarator(ad),
            DirectAbstractDeclarator::Array(dad, c) => {
                if let Some(dad) = dad {
                    self.visit_direct_abstract_declarator(dad.as_mut());
                }
                if let Some(c) = c {
                    self.visit_constant_expr(c);
                }
            }
            DirectAbstractDeclarator::ParamTypeList(dad, ptl) => {
                if let Some(dad) = dad {
                    self.visit_direct_abstract_declarator(dad.as_mut());
                }
                todo!("{:?}", ptl);
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::SemiColon => (),
            Stmt::Expr(expr) => {
                self.visit_expr(expr);
            }
            Stmt::Label(ident, stmt) => {
                self.symbol_table.add_label(ident.clone());
                self.visit_stmt(stmt.as_mut())
            }
            Stmt::Case(c, stmt) => {
                self.visit_constant_expr(c);
                self.visit_stmt(stmt.as_mut());
            }
            Stmt::Default(stmt) => {
                self.visit_stmt(stmt.as_mut());
            }
            Stmt::Compound(CompoundStmt(stmts)) => {
                for ds in stmts {
                    self.visit_declaration_or_stmt(ds);
                }
            }
            Stmt::If(expr, stmt, None) => {
                self.visit_expr(expr);
                self.visit_stmt(stmt);
            }
            Stmt::If(expr, stmt, Some(else_stmt)) => {
                self.visit_expr(expr);
                self.visit_stmt(stmt);
                self.visit_stmt(else_stmt);
            }
            Stmt::Switch(expr, stmt) => {
                self.visit_expr(expr);
                self.visit_stmt(stmt);
            }
            Stmt::While(expr, stmt) => {
                self.visit_expr(expr);
                self.visit_stmt(stmt);
            }
            Stmt::DoWhile(stmt, expr) => {
                self.visit_stmt(stmt);
                self.visit_expr(expr);
            }
            Stmt::For(expr1, expr2, expr3, stmt) => {
                if let Some(e) = expr1 {
                    self.visit_expr(e);
                }
                if let Some(e) = expr2 {
                    self.visit_expr(e);
                }
                if let Some(e) = expr3 {
                    self.visit_expr(e);
                }
                self.visit_stmt(stmt);
            }
            Stmt::ForWithDeclaration(d, expr2, expr3, stmt) => {
                self.visit_declaration(d, false);
                if let Some(e) = expr2 {
                    self.visit_expr(e);
                }
                if let Some(e) = expr3 {
                    self.visit_expr(e);
                }
                self.visit_stmt(stmt);
            }
            Stmt::Goto(_) => (),
            Stmt::Continue => (),
            Stmt::Break => (),
            Stmt::Return(Some(expr)) => {
                self.visit_expr(expr);
            }
            Stmt::Return(None) => (),
        };
    }

    fn visit_declaration_or_stmt(&mut self, ds: &mut DeclarationOrStmt) {
        match ds {
            DeclarationOrStmt::Declaration(d) => self.visit_declaration(d, false),
            DeclarationOrStmt::Stmt(s) => self.visit_stmt(s),
        }
    }

    fn visit_declaration(&mut self, declaration: &mut Declaration, is_global: bool) {
        if declaration.specs.iter().any(|s| s.is_typedef()) {
            return;
        }

        self.symbol_table.parse_primary_type(&declaration.specs);

        for init in declaration.inits.iter_mut() {
            match init {
                InitDeclarator::Declarator(d) => {
                    if is_global {
                        self.symbol_table.declare_global(&declaration.specs, d);
                    } else {
                        self.symbol_table.declare_local(&declaration.specs, d);
                    }
                }
                InitDeclarator::DeclaratorAndInitializer(d, i) => {
                    let left_ty = self
                        .symbol_table
                        .from_specs_and_declarator(&declaration.specs, d);

                    if let Some(size) = self.visit_initializer(i, &left_ty) {
                        if let DirectDeclarator::Array(_, ce @ None) = &mut d.direct {
                            *ce = Some(ConstantExpr::Identity(BinOp::Unary(Unary::Identity(
                                Primary::Num(size as i64),
                            ))));
                        }
                    }

                    if is_global {
                        self.symbol_table.declare_global(&declaration.specs, d);
                    } else {
                        self.symbol_table.declare_local(&declaration.specs, d);
                    }
                }
            }
        }
    }

    fn visit_initializer(
        &mut self,
        initializer: &mut Initializer,
        expected_ty: &Ty,
    ) -> Option<usize> {
        match initializer {
            Initializer::Assign(a) => {
                let ty = self.visit_assign(a);
                expected_ty.assert_compatible(&ty);
                None
            }
            Initializer::Vec(inits) => match expected_ty {
                Ty::Array(inner_ty, size) => {
                    if let Some(s) = size {
                        if *s > inits.len() {
                            eprintln!("Initializer contains too many elements");
                            inits.truncate(*s);
                        }
                        if *s < inits.len() {
                            todo!("Fill the array up to the expected size");
                        }
                    }
                    let len = inits.len();
                    for init in inits {
                        self.visit_initializer(init, inner_ty);
                    }
                    Some(len)
                }
                Ty::Struct(name) => {
                    let sds = self.symbol_table.get_struct_definition(name).clone();
                    for ((_field, field_ty), init) in sds.iter().zip(inits) {
                        self.visit_initializer(init, field_ty);
                    }
                    None
                }
                ty => todo!("{:?}", ty),
            },
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Ty {
        let mut t = Ty::Void;
        for assign in expr.0.iter_mut() {
            t = self.visit_assign(assign);
        }
        t
    }

    fn visit_assign(&mut self, assign: &mut Assign) -> Ty {
        match assign {
            Assign::Const(c) => self.visit_constant_expr(c),
            Assign::Assign(u, kind, a) => {
                if *kind != AssignOpKind::Assign {
                    // desugar from u += a to u = u + a
                    let bok = kind.to_bin_op_kind();
                    let assign_wrapped =
                        BinOp::Unary(Unary::Identity(Primary::Expr(Box::new(Expr(vec![
                            *a.clone()
                        ])))));
                    let a = Assign::Const(ConstantExpr::Identity(BinOp::Binary(
                        bok,
                        Box::new(BinOp::Unary(u.clone())),
                        Box::new(assign_wrapped),
                    )));
                    *assign = Assign::Assign(u.clone(), AssignOpKind::Assign, Box::new(a));
                    return self.visit_assign(assign);
                }

                let t1 = self.visit_unary(u);
                let t2 = self.visit_assign(a);

                match (t1, t2) {
                    (Ty::Ptr(p), Ty::Array(a, _)) => {
                        p.assert_compatible(&a);
                        *p
                    }
                    (t1, t2) => {
                        t1.assert_compatible(&t2);
                        t1
                    }
                }
            }
        }
    }

    fn visit_constant_expr(&mut self, c: &mut ConstantExpr) -> Ty {
        match c {
            ConstantExpr::Identity(e) => self.visit_binop(e),
            ConstantExpr::Ternary(binop, expr, constant_expr) => {
                self.visit_binop(binop);
                let t2 = self.visit_expr(expr);
                let t3 = self.visit_constant_expr(constant_expr);
                t2.assert_compatible(&t3);
                t2
            }
        }
    }

    fn visit_binop(&mut self, binop: &mut BinOp) -> Ty {
        match binop {
            BinOp::Binary(kind, left, right) => {
                let t1 = self.visit_binop(left);
                let t2 = self.visit_binop(right);

                match (t1, t2) {
                    (Ty::Ptr(ref p), t2) if t2.is_numeric() => {
                        let size = BinOp::Unary(Unary::Identity(Primary::Num(
                            p.sizeof(&self.symbol_table) as i64,
                        )));
                        *right =
                            Box::new(BinOp::Binary(BinOpKind::Mul, right.clone(), Box::new(size)));
                        Ty::Ptr(p.clone())
                    }
                    (t2, Ty::Ptr(ref p)) if t2.is_numeric() => {
                        let size = BinOp::Unary(Unary::Identity(Primary::Num(
                            p.sizeof(&self.symbol_table) as i64,
                        )));
                        *left =
                            Box::new(BinOp::Binary(BinOpKind::Mul, left.clone(), Box::new(size)));
                        Ty::Ptr(p.clone())
                    }
                    (Ty::Array(t, size), t2) if t2.is_numeric() => {
                        let s = BinOp::Unary(Unary::Identity(Primary::Num(
                            t.sizeof(&self.symbol_table) as i64,
                        )));
                        *right =
                            Box::new(BinOp::Binary(BinOpKind::Mul, right.clone(), Box::new(s)));
                        Ty::Array(t, size)
                    }
                    (t2, Ty::Array(t, size)) if t2.is_numeric() => {
                        let s = BinOp::Unary(Unary::Identity(Primary::Num(
                            t.sizeof(&self.symbol_table) as i64,
                        )));
                        *left = Box::new(BinOp::Binary(BinOpKind::Mul, left.clone(), Box::new(s)));
                        Ty::Array(t, size)
                    }
                    (Ty::Ptr(p1), Ty::Ptr(p2)) if p1 == p2 => {
                        assert!(!matches!(
                            kind,
                            BinOpKind::Add | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod
                        ));

                        if *kind == BinOpKind::Sub {
                            let size = BinOp::Unary(Unary::Identity(Primary::Num(
                                p1.sizeof(&self.symbol_table) as i64,
                            )));
                            *binop = BinOp::Binary(
                                BinOpKind::Div,
                                Box::new(binop.clone()),
                                Box::new(size),
                            );
                        }

                        Ty::I32
                    }
                    (t1, t2) => {
                        if *kind == BinOpKind::LogicalAnd || *kind == BinOpKind::LogicalOr {
                            return Ty::I64;
                        }
                        t1.assert_compatible(&t2);
                        t1
                    }
                }
            }
            BinOp::Unary(unary) => self.visit_unary(unary),
        }
    }

    fn visit_unary(&mut self, unary: &mut Unary) -> Ty {
        match unary {
            Unary::Identity(primary) => self.visit_primary(primary),
            Unary::Cast(tn, u) => {
                self.visit_unary(u);
                self.symbol_table
                    .from_specs_and_abstract_declarator(&tn.specs, &tn.declarator)
            }
            Unary::Neg(u) => self.visit_unary(u.as_mut()),
            Unary::Ref(u) => {
                let t = self.visit_unary(u.as_mut());
                Ty::Ptr(Box::new(t))
            }
            Unary::Deref(u) => {
                let ty = self.visit_unary(u.as_mut());
                match ty {
                    Ty::Ptr(p) => *p,
                    Ty::Array(t, _) => *t,
                    t => panic!("Tried to dereference non-pointer variable: {:?}", t),
                }
            }
            Unary::BitwiseNot(u) => self.visit_unary(u.as_mut()),
            Unary::LogicalNot(u) => self.visit_unary(u.as_mut()),
            Unary::PrefixIncrement(u) => {
                // desugar from ++u to (u += 1)
                let a = Assign::Const(ConstantExpr::Identity(BinOp::Unary(Unary::Identity(
                    Primary::Num(1),
                ))));
                let expr = Expr(vec![Assign::Assign(
                    *u.clone(),
                    AssignOpKind::AddAssign,
                    Box::new(a),
                )]);
                *unary = Unary::Identity(Primary::Expr(Box::new(expr)));
                self.visit_unary(unary)
            }
            Unary::PrefixDecrement(u) => {
                // desugar from --u to (u -= 1)
                let a = Assign::Const(ConstantExpr::Identity(BinOp::Unary(Unary::Identity(
                    Primary::Num(1),
                ))));
                let expr = Expr(vec![Assign::Assign(
                    *u.clone(),
                    AssignOpKind::SubAssign,
                    Box::new(a),
                )]);
                *unary = Unary::Identity(Primary::Expr(Box::new(expr)));
                self.visit_unary(unary)
            }
            Unary::Sizeof(u) => {
                let ty = self.visit_unary(u.as_mut());
                *unary = Unary::Identity(Primary::Num(ty.sizeof(&self.symbol_table) as i64));
                Ty::I32
            }
            Unary::SizeofType(tn) => {
                let ty = self
                    .symbol_table
                    .from_specs_and_abstract_declarator(&tn.specs, &tn.declarator);
                *unary = Unary::Identity(Primary::Num(ty.sizeof(&self.symbol_table) as i64));
                Ty::I32
            }

            Unary::Index(u, e) => {
                // desugar from u[e] to *(u + e)
                let u = BinOp::Unary(*u.clone());
                let e = BinOp::Unary(Unary::Identity(Primary::Expr(Box::new(e.clone()))));
                let binary = BinOp::Binary(BinOpKind::Add, Box::new(u), Box::new(e));
                let expr = Expr(vec![Assign::Const(ConstantExpr::Identity(binary))]);
                *unary = Unary::Deref(Box::new(Unary::Identity(Primary::Expr(Box::new(expr)))));
                self.visit_unary(unary)
            }
            Unary::Call(u, expr) => {
                if let Some(e) = expr {
                    self.visit_expr(e);
                }
                let ty = u.get_type(&mut self.symbol_table);
                let Ty::Func(return_ty, _) = ty else {
                    panic!("Tried to call invalid type {:?}", ty);
                };
                *return_ty
            }
            Unary::Field(u, f) => {
                // desugar from u.f to *(T*)u if u is an union
                if let Ty::Union(name) = u.get_type(&mut self.symbol_table) {
                    let ty = self.symbol_table.get_union_field(&name, f);
                    let ty = ty.clone();

                    let tn = Ty::Ptr(Box::new(ty.clone())).to_typename();
                    let binop = BinOp::Unary(Unary::Cast(Box::new(tn), Box::new(*u.clone())));
                    let expr = Expr(vec![Assign::Const(ConstantExpr::Identity(binop))]);
                    *unary = Unary::Deref(Box::new(Unary::Identity(Primary::Expr(Box::new(expr)))));
                    self.visit_unary(unary);
                    return ty;
                }

                // desugar from u.f to *(T*)((char*)u + offset) if u is a struct
                let Ty::Struct(name) = u.get_type(&mut self.symbol_table) else {
                    panic!("Accessing a field on a non-struct type");
                };
                let (offset, ty) = self.symbol_table.get_struct_field(&name, f);

                let tn = Ty::Ptr(Box::new(Ty::I8)).to_typename();
                let u = Unary::Cast(Box::new(tn), Box::new(*u.clone()));
                let offset = Box::new(BinOp::Unary(Unary::Identity(Primary::Num(offset as i64))));
                let binop = BinOp::Binary(BinOpKind::Add, Box::new(BinOp::Unary(u)), offset);
                let expr = Expr(vec![Assign::Const(ConstantExpr::Identity(binop))]);

                let tn = Ty::Ptr(Box::new(ty.clone())).to_typename();
                let cast = Unary::Cast(
                    Box::new(tn),
                    Box::new(Unary::Identity(Primary::Expr(Box::new(expr)))),
                );

                let binop = BinOp::Unary(cast);
                let expr = Expr(vec![Assign::Const(ConstantExpr::Identity(binop))]);
                *unary = Unary::Deref(Box::new(Unary::Identity(Primary::Expr(Box::new(expr)))));
                self.visit_unary(unary)
            }
            Unary::PointerField(u, f) => {
                // desugar from u->f to (*u).f
                *unary = Unary::Field(Box::new(Unary::Deref(u.clone())), f.clone());
                self.visit_unary(unary)
            }
            Unary::PostfixIncrement(u) => {
                // desugar from u++ to (u += 1, u - 1)
                let one = BinOp::Unary(Unary::Identity(Primary::Num(1)));
                let a = Assign::Const(ConstantExpr::Identity(one.clone()));
                let a1 = Assign::Assign(*u.clone(), AssignOpKind::AddAssign, Box::new(a));
                let a2 = Assign::Const(ConstantExpr::Identity(BinOp::Binary(
                    BinOpKind::Sub,
                    Box::new(BinOp::Unary(*u.clone())),
                    Box::new(one),
                )));
                let expr = Expr(vec![a1, a2]);
                *unary = Unary::Identity(Primary::Expr(Box::new(expr)));
                self.visit_unary(unary)
            }
            Unary::PostfixDecrement(u) => {
                // desugar from u-- to (u -= 1, u + 1)
                let one = BinOp::Unary(Unary::Identity(Primary::Num(1)));
                let a = Assign::Const(ConstantExpr::Identity(one.clone()));
                let a1 = Assign::Assign(*u.clone(), AssignOpKind::SubAssign, Box::new(a));
                let a2 = Assign::Const(ConstantExpr::Identity(BinOp::Binary(
                    BinOpKind::Add,
                    Box::new(BinOp::Unary(*u.clone())),
                    Box::new(one),
                )));
                let expr = Expr(vec![a1, a2]);
                *unary = Unary::Identity(Primary::Expr(Box::new(expr)));
                self.visit_unary(unary)
            }
        }
    }

    fn visit_primary(&mut self, primary: &mut Primary) -> Ty {
        match primary {
            Primary::Ident(ident) => self.symbol_table.get_var_type(ident),
            Primary::Num(_) => Ty::I32,
            Primary::String(b) => {
                self.symbol_table.declare_string(b.clone());
                Ty::Ptr(Box::new(Ty::I8))
            }
            Primary::Expr(expr) => self.visit_expr(expr.as_mut()),
        }
    }
}
