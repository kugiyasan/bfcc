use core::panic;

use crate::parser::{
    Assign, BinOpKind, CompoundStmt, ConstantExpr, Declaration, DeclarationOrStmt, Expr, ExprKind,
    FuncDef, Identifier, InitDeclarator, Primary, Stmt, TranslationUnit, Unary,
};

use super::symbol_table::SymbolTable;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Void,
    Int,
    Ptr(Box<Type>),
    Array(Box<Type>, usize),
}

impl Type {
    pub fn sizeof(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::Int => 4,
            Type::Ptr(_) => 8,
            Type::Array(t, size) => t.sizeof() * size,
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
        for func_def in translation_unit.0.iter_mut() {
            self.visit_func_def(func_def);
        }

        self.symbol_table.clone()
    }

    pub fn visit_func_def(&mut self, func_def: &mut FuncDef) {
        self.symbol_table
            .declare_func(func_def.declarator.direct.get_name());

        for d in func_def.declarations.iter_mut() {
            self.visit_declaration(d);
        }

        for ds in func_def.stmt.0.iter_mut() {
            self.visit_declaration_or_stmt(ds);
        }
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
                assert_eq!(t1, t2);
                t1
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
        dbg!(&expr_kind);
        match expr_kind {
            ExprKind::Binary(_kind, left, right) => {
                let t1 = self.visit_expr_kind(left);
                let t2 = self.visit_expr_kind(right);

                dbg!((&t1, &t2));
                // todo also check t2 t1
                if let Type::Ptr(ref p) = t1 {
                    if **p == t2 {
                        let size =
                            ExprKind::Unary(Unary::Identity(Primary::Num(t2.sizeof() as i32)));
                        *right = Box::new(ExprKind::Binary(
                            BinOpKind::Mul,
                            right.clone(),
                            Box::new(size),
                        ));
                    }
                    return t1;
                }

                assert_eq!(t1, t2);
                t1
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
                let t = self.visit_unary(u.as_mut());
                let Type::Ptr(p) = t else {
                    panic!("Tried to dereference non-pointer variable")
                };
                *p
            }
            Unary::Sizeof(u) => {
                let t = self.visit_unary(u.as_mut());
                *unary = Unary::Identity(Primary::Num(t.sizeof() as i32));
                Type::Int
            }
            _ => todo!(),
        }
    }

    fn visit_primary(&mut self, primary: &mut Primary) -> Type {
        match primary {
            Primary::Num(_num) => Type::Int,
            Primary::Ident(Identifier { name }) => self.symbol_table.get_var_type(name),
            Primary::FunctionCall(_name, None) => Type::Void, // todo
            Primary::FunctionCall(_name, Some(expr)) => self.visit_expr(expr),
            Primary::Expr(expr) => self.visit_expr(expr.as_mut()),
        }
    }
}
