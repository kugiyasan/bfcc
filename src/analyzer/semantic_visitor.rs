use crate::parser::{
    Assign, CompoundStmt, ConstantExpr, Declaration, DeclarationOrStmt, Expr, ExprKind, FuncDef,
    Primary, Stmt, TranslationUnit, Unary,
};

pub struct SemanticVisitor {}

impl SemanticVisitor {
    pub fn new() -> Self {
        Self {}
    }

    pub fn visit_program(&self, translation_unit: &TranslationUnit) {
        for func_def in translation_unit.0.iter() {
            self.visit_func_def(func_def);
        }
    }

    pub fn visit_func_def(&self, func_def: &FuncDef) {
        for ds in func_def.stmt.0.iter() {
            self.visit_declaration_or_stmt(ds);
        }
    }

    fn visit_stmt(&self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => self.visit_expr(expr),
            Stmt::Compound(CompoundStmt(stmts)) => {
                for ds in stmts {
                    self.visit_declaration_or_stmt(ds);
                }
            }
            Stmt::If(expr, stmt, None) => self.visit_if(expr, &stmt),
            Stmt::If(expr, stmt, Some(else_stmt)) => self.visit_if_else(expr, &stmt, &else_stmt),
            Stmt::While(expr, stmt) => self.visit_while(expr, &stmt),
            Stmt::For(expr1, expr2, expr3, stmt) => self.visit_for(expr1, expr2, expr3, &stmt),
            Stmt::Return(expr) => self.visit_expr(expr),
            _ => todo!(),
        };
    }

    fn visit_declaration_or_stmt(&self, ds: &DeclarationOrStmt) {
        match ds {
            DeclarationOrStmt::Declaration(d) => self.visit_declaration(d),
            DeclarationOrStmt::Stmt(s) => self.visit_stmt(s),
        }
    }

    fn visit_declaration(&self, declaration: &Declaration) {}

    fn visit_if(&self, expr: &Expr, stmt: &Stmt) {
        self.visit_expr(expr);
        self.visit_stmt(stmt);
    }

    fn visit_if_else(&self, expr: &Expr, stmt: &Stmt, else_stmt: &Stmt) {
        self.visit_expr(expr);
        self.visit_stmt(stmt);
        self.visit_stmt(else_stmt);
    }

    fn visit_while(&self, expr: &Expr, stmt: &Stmt) {
        self.visit_expr(expr);
        self.visit_stmt(stmt);
    }

    fn visit_for(
        &self,
        expr1: &Option<Expr>,
        expr2: &Option<Expr>,
        expr3: &Option<Expr>,
        stmt: &Stmt,
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

    fn visit_expr(&self, expr: &Expr) {
        for assign in expr.0.iter() {
            self.visit_assign(assign);
        }
    }

    fn visit_assign(&self, assign: &Assign) {
        match assign {
            Assign::Const(c) => self.visit_constant_expr(c),
            Assign::Assign(unary, _kind, a) => {
                self.visit_unary(unary);
                self.visit_assign(a);
            }
        }
    }

    fn visit_constant_expr(&self, c: &ConstantExpr) {
        match c {
            ConstantExpr::Identity(e) => self.visit_expr_kind(e),
            ConstantExpr::Ternary(expr_kind, expr, constant_expr) => {
                self.visit_expr_kind(expr_kind);
                self.visit_expr(expr);
                self.visit_constant_expr(constant_expr);
            }
        }
    }

    fn visit_expr_kind(&self, expr_kind: &ExprKind) {
        match expr_kind {
            ExprKind::Binary(_kind, left, right) => {
                self.visit_expr_kind(left);
                self.visit_expr_kind(right);
            }
            ExprKind::Unary(unary) => self.visit_unary(unary),
        }
    }

    fn visit_unary(&self, unary: &Unary) {
        match unary {
            Unary::Identity(primary) => self.visit_primary(primary),
            Unary::Neg(unary) => self.visit_unary(&unary),
            Unary::Ref(unary) => self.visit_unary(&unary),
            Unary::Deref(unary) => self.visit_unary(&unary),
            _ => todo!(),
        }
    }

    fn visit_primary(&self, primary: &Primary) {
        match primary {
            Primary::Num(_num) => (),
            Primary::Ident(_offset) => (),
            Primary::FunctionCall(_name, None) => (),
            Primary::FunctionCall(_name, Some(expr)) => self.visit_expr(expr),
            Primary::Expr(expr) => self.visit_expr(&expr),
        }
    }
}
