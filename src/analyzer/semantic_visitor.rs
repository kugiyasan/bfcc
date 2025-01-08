use core::panic;

use crate::parser::{
    Assign, BinOpKind, CompoundStmt, ConstantExpr, Declaration, DeclarationOrStmt, Declarator,
    DirectDeclarator, Expr, ExprKind, ExternalDeclaration, FuncDef, InitDeclarator,
    ParamDeclaration, Primary, Stmt, TranslationUnit, Unary,
};

use super::{symbol_table::SymbolTable, Ty};

pub struct SemanticVisitor {
    symbol_table: SymbolTable,
}

impl SemanticVisitor {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
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
            ExternalDeclaration::Declaration(d) => self.visit_declaration(d),
        }
    }

    fn visit_func_def(&mut self, func_def: &mut FuncDef) {
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
                    .declare_var_with_offset(specs.clone(), *d.clone(), 8);
            }
            ParamDeclaration::AbstractDeclarator(_, _ad) => todo!(),
        };
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
            Stmt::Goto(_) => (),
            Stmt::Continue => todo!(),
            Stmt::Break => todo!(),
            Stmt::Return(expr) => {
                self.visit_expr(expr);
            }
        };
    }

    fn visit_declaration_or_stmt(&mut self, ds: &mut DeclarationOrStmt) {
        match ds {
            DeclarationOrStmt::Declaration(d) => self.visit_declaration(d),
            DeclarationOrStmt::Stmt(s) => self.visit_stmt(s),
        }
    }

    fn visit_declaration(&mut self, declaration: &mut Declaration) {
        for init in declaration.inits.iter_mut() {
            if let InitDeclarator::Declarator(d) = init {
                self.symbol_table
                    .declare_var(declaration.specs.clone(), d.clone());
            }
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
            Assign::Assign(unary, _kind, a) => {
                let t1 = self.visit_unary(unary);
                let t2 = self.visit_assign(a);

                match (t1, t2) {
                    (Ty::Ptr(p), Ty::Array(a, _)) => {
                        assert!(p.is_compatible(&a));
                        *p
                    }
                    (t1, t2) => {
                        assert!(t1.is_compatible(&t2));
                        t1
                    }
                }
            }
        }
    }

    fn visit_constant_expr(&mut self, c: &mut ConstantExpr) -> Ty {
        match c {
            ConstantExpr::Identity(e) => self.visit_expr_kind(e),
            ConstantExpr::Ternary(expr_kind, expr, constant_expr) => {
                self.visit_expr_kind(expr_kind);
                let t2 = self.visit_expr(expr);
                let t3 = self.visit_constant_expr(constant_expr);
                assert_eq!(t2, t3);
                t2
            }
        }
    }

    fn visit_expr_kind(&mut self, expr_kind: &mut ExprKind) -> Ty {
        match expr_kind {
            ExprKind::Binary(_kind, left, right) => {
                let t1 = self.visit_expr_kind(left);
                let t2 = self.visit_expr_kind(right);

                match (t1, t2) {
                    (Ty::Ptr(ref p), t2) if p.is_compatible(&t2) => {
                        let size =
                            ExprKind::Unary(Unary::Identity(Primary::Num(t2.sizeof() as i32)));
                        *right = Box::new(ExprKind::Binary(
                            BinOpKind::Mul,
                            right.clone(),
                            Box::new(size),
                        ));
                        Ty::Ptr(Box::new(t2))
                    }
                    (t2, Ty::Ptr(ref p)) if p.is_compatible(&t2) => {
                        let size =
                            ExprKind::Unary(Unary::Identity(Primary::Num(t2.sizeof() as i32)));
                        *left = Box::new(ExprKind::Binary(
                            BinOpKind::Mul,
                            left.clone(),
                            Box::new(size),
                        ));
                        Ty::Ptr(Box::new(t2))
                    }
                    (Ty::Array(t, size), t2) if t.is_compatible(&t2) => {
                        let s = ExprKind::Unary(Unary::Identity(Primary::Num(t.sizeof() as i32)));
                        *right =
                            Box::new(ExprKind::Binary(BinOpKind::Mul, right.clone(), Box::new(s)));
                        Ty::Array(t, size)
                    }
                    (t2, Ty::Array(t, size)) if t.is_compatible(&t2) => {
                        let s = ExprKind::Unary(Unary::Identity(Primary::Num(t.sizeof() as i32)));
                        *left =
                            Box::new(ExprKind::Binary(BinOpKind::Mul, left.clone(), Box::new(s)));
                        Ty::Array(t, size)
                    }
                    (t1, t2) => {
                        assert!(t1.is_compatible(&t2));
                        t1
                    }
                }
            }
            ExprKind::Unary(unary) => self.visit_unary(unary),
        }
    }

    fn visit_unary(&mut self, unary: &mut Unary) -> Ty {
        match unary {
            Unary::Identity(primary) => self.visit_primary(primary),
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
            Unary::Sizeof(u) => {
                let t = self.visit_unary(u.as_mut());
                *unary = Unary::Identity(Primary::Num(t.sizeof() as i32));
                Ty::Int
            }
            Unary::Call(_, None) => Ty::Void, // todo
            Unary::Call(_, Some(expr)) => self.visit_expr(expr),
            Unary::Index(u, e) => {
                // desugar to *(u + e)
                let u = ExprKind::Unary(*u.clone());
                let e = ExprKind::Unary(Unary::Identity(Primary::Expr(Box::new(e.clone()))));
                let binary = ExprKind::Binary(BinOpKind::Add, Box::new(u), Box::new(e));
                let expr = Expr(vec![Assign::Const(ConstantExpr::Identity(binary))]);
                *unary = Unary::Deref(Box::new(Unary::Identity(Primary::Expr(Box::new(expr)))));
                self.visit_unary(unary)
            }
            u => todo!("{u:?}"),
        }
    }

    fn visit_primary(&mut self, primary: &mut Primary) -> Ty {
        match primary {
            Primary::Ident(ident) => self.symbol_table.get_var_type(ident),
            Primary::Num(_) => Ty::Int,
            Primary::String(b) => {
                self.symbol_table.declare_string(b.clone());
                Ty::Array(Box::new(Ty::Char), b.len())
            }
            Primary::Expr(expr) => self.visit_expr(expr.as_mut()),
        }
    }
}
