use core::panic;

use crate::parser::{
    Assign, BinOpKind, CompoundStmt, ConstantExpr, Declaration, DeclarationOrStmt, Declarator,
    DirectDeclarator, Expr, ExprKind, ExternalDeclaration, FuncDef, Identifier, InitDeclarator,
    ParamDeclaration, Primary, Stmt, TranslationUnit, Unary,
};

use super::symbol_table::SymbolTable;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Void,
    Char,
    Int,
    Ptr(Box<Type>),
    Array(Box<Type>, usize),
}

impl Type {
    pub fn sizeof(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::Char => 1,
            Type::Int => 8,
            Type::Ptr(_) => 8,
            Type::Array(t, size) => t.sizeof() * size,
        }
    }

    pub fn is_compatible_type(&self, other: &Self) -> bool {
        match (self, other) {
            (t1, t2) if t1 == t2 => true,
            (Type::Int, Type::Char) => true,
            (Type::Char, Type::Int) => true,
            _ => false,
        }
    }
}

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
                self.symbol_table.declare_var(specs.clone(), *d.clone());
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
            Stmt::Compound(CompoundStmt(stmts)) => {
                for ds in stmts {
                    self.visit_declaration_or_stmt(ds);
                }
            }
            Stmt::If(expr, stmt, None) => self.visit_if(expr, stmt.as_mut()),
            Stmt::If(expr, stmt, Some(else_stmt)) => {
                self.visit_if_else(expr, stmt.as_mut(), else_stmt.as_mut())
            }
            Stmt::While(expr, stmt) => self.visit_while(expr, stmt.as_mut()),
            Stmt::For(expr1, expr2, expr3, stmt) => {
                self.visit_for(expr1, expr2, expr3, stmt.as_mut())
            }
            Stmt::Return(expr) => {
                self.visit_expr(expr);
            }
            _ => todo!(),
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

    fn visit_if(&mut self, expr: &mut Expr, stmt: &mut Stmt) {
        self.visit_expr(expr);
        self.visit_stmt(stmt);
    }

    fn visit_if_else(&mut self, expr: &mut Expr, stmt: &mut Stmt, else_stmt: &mut Stmt) {
        self.visit_expr(expr);
        self.visit_stmt(stmt);
        self.visit_stmt(else_stmt);
    }

    fn visit_while(&mut self, expr: &mut Expr, stmt: &mut Stmt) {
        self.visit_expr(expr);
        self.visit_stmt(stmt);
    }

    fn visit_for(
        &mut self,
        expr1: &mut Option<Expr>,
        expr2: &mut Option<Expr>,
        expr3: &mut Option<Expr>,
        stmt: &mut Stmt,
    ) {
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

    fn visit_expr(&mut self, expr: &mut Expr) -> Type {
        let mut t = Type::Void;
        for assign in expr.0.iter_mut() {
            t = self.visit_assign(assign);
        }
        t
    }

    fn visit_assign(&mut self, assign: &mut Assign) -> Type {
        match assign {
            Assign::Const(c) => self.visit_constant_expr(c),
            Assign::Assign(unary, _kind, a) => {
                let t1 = self.visit_unary(unary);
                let t2 = self.visit_assign(a);

                match (t1, t2) {
                    (Type::Ptr(p), Type::Array(a, _)) => {
                        assert!(p.is_compatible_type(&a));
                        *p
                    }
                    (t1, t2) => {
                        assert!(t1.is_compatible_type(&t2));
                        t1
                    }
                }
            }
        }
    }

    fn visit_constant_expr(&mut self, c: &mut ConstantExpr) -> Type {
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

    fn visit_expr_kind(&mut self, expr_kind: &mut ExprKind) -> Type {
        match expr_kind {
            ExprKind::Binary(_kind, left, right) => {
                let t1 = self.visit_expr_kind(left);
                let t2 = self.visit_expr_kind(right);

                match (t1, t2) {
                    (Type::Ptr(ref p), t2) if p.is_compatible_type(&t2) => {
                        let size =
                            ExprKind::Unary(Unary::Identity(Primary::Num(t2.sizeof() as i32)));
                        *right = Box::new(ExprKind::Binary(
                            BinOpKind::Mul,
                            right.clone(),
                            Box::new(size),
                        ));
                        Type::Ptr(Box::new(t2))
                    }
                    (t2, Type::Ptr(ref p)) if p.is_compatible_type(&t2) => {
                        let size =
                            ExprKind::Unary(Unary::Identity(Primary::Num(t2.sizeof() as i32)));
                        *left = Box::new(ExprKind::Binary(
                            BinOpKind::Mul,
                            left.clone(),
                            Box::new(size),
                        ));
                        Type::Ptr(Box::new(t2))
                    }
                    (Type::Array(t, size), t2) if t.is_compatible_type(&t2) => {
                        let s = ExprKind::Unary(Unary::Identity(Primary::Num(t.sizeof() as i32)));
                        *right =
                            Box::new(ExprKind::Binary(BinOpKind::Mul, right.clone(), Box::new(s)));
                        Type::Array(t, size)
                    }
                    (t2, Type::Array(t, size)) if t.is_compatible_type(&t2) => {
                        let s = ExprKind::Unary(Unary::Identity(Primary::Num(t.sizeof() as i32)));
                        *left =
                            Box::new(ExprKind::Binary(BinOpKind::Mul, left.clone(), Box::new(s)));
                        Type::Array(t, size)
                    }
                    (t1, t2) => {
                        assert!(t1.is_compatible_type(&t2));
                        t1
                    }
                }
            }
            ExprKind::Unary(unary) => self.visit_unary(unary),
        }
    }

    fn visit_unary(&mut self, unary: &mut Unary) -> Type {
        match unary {
            Unary::Identity(primary) => self.visit_primary(primary),
            Unary::Neg(u) => self.visit_unary(u.as_mut()),
            Unary::Ref(u) => {
                let t = self.visit_unary(u.as_mut());
                Type::Ptr(Box::new(t))
            }
            Unary::Deref(u) => {
                let ty = self.visit_unary(u.as_mut());
                match ty {
                    Type::Ptr(p) => *p,
                    Type::Array(t, _) => *t,
                    t => panic!("Tried to dereference non-pointer variable: {:?}", t),
                }
            }
            Unary::Sizeof(u) => {
                let t = self.visit_unary(u.as_mut());
                *unary = Unary::Identity(Primary::Num(t.sizeof() as i32));
                Type::Int
            }
            Unary::Call(_, None) => Type::Void, // todo
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

    fn visit_primary(&mut self, primary: &mut Primary) -> Type {
        match primary {
            Primary::Num(_num) => Type::Int,
            Primary::Ident(Identifier { name }) => self.symbol_table.get_var_type(name),
            Primary::Expr(expr) => self.visit_expr(expr.as_mut()),
        }
    }
}
