use crate::parser::{
    Add, Assign, Equality, Expr, FuncDef, Mul, Primary, Relational, Stmt, TranslationUnit, Unary,
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
        for stmt in func_def.stmts.iter() {
            self.visit_stmt(stmt);
        }
    }

    fn visit_stmt(&self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => self.visit_expr(expr),
            Stmt::Block(stmt) => {
                for s in stmt {
                    self.visit_stmt(s);
                }
            }
            Stmt::If(expr, stmt, None) => self.visit_if(expr, &stmt),
            Stmt::If(expr, stmt, Some(else_stmt)) => self.visit_if_else(expr, &stmt, &else_stmt),
            Stmt::While(expr, stmt) => self.visit_while(expr, &stmt),
            Stmt::For(expr1, expr2, expr3, stmt) => self.visit_for(expr1, expr2, expr3, &stmt),
            Stmt::Return(expr) => self.visit_expr(expr),
        };
    }

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
        match expr {
            Expr::Assign(assign) => self.visit_assign(assign),
        }
    }

    fn visit_assign(&self, assign: &Assign) {
        if let Some(a) = assign.assign.as_ref() {
            self.visit_assign(a);
        }

        self.visit_equality(&assign.eq);
    }

    fn visit_equality(&self, equality: &Equality) {
        match equality {
            Equality::Identity(rel) => {
                self.visit_relational(rel);
            }
            Equality::Equal(rel, eq) => {
                self.visit_relational(rel);
                self.visit_equality(&eq);
            }
            Equality::NotEqual(rel, eq) => {
                self.visit_relational(rel);
                self.visit_equality(&eq);
            }
        }
    }

    fn visit_relational(&self, relational: &Relational) {
        match relational {
            Relational::Identity(add) => {
                self.visit_add(add);
            }
            Relational::LessThan(add, rel) => {
                self.visit_add(add);
                self.visit_relational(&rel);
            }
            Relational::LessEqual(add, rel) => {
                self.visit_add(add);
                self.visit_relational(&rel);
            }
            Relational::GreaterThan(add, rel) => {
                self.visit_add(add);
                self.visit_relational(&rel);
            }
            Relational::GreaterEqual(add, rel) => {
                self.visit_add(add);
                self.visit_relational(&rel);
            }
        }
    }

    fn visit_add(&self, add: &Add) {
        match add {
            Add::Identity(mul) => self.visit_mul(mul),
            Add::Add(mul, a) => {
                self.visit_mul(mul);
                self.visit_add(&a);
            }
            Add::Sub(mul, a) => {
                self.visit_mul(mul);
                self.visit_add(&a);
            }
        }
    }

    fn visit_mul(&self, mul: &Mul) {
        match mul {
            Mul::Identity(unary) => self.visit_unary(unary),
            Mul::Mul(unary, m) => {
                self.visit_unary(unary);
                self.visit_mul(&m);
            }
            Mul::Div(unary, m) => {
                self.visit_unary(unary);
                self.visit_mul(&m);
            }
        }
    }

    fn visit_unary(&self, unary: &Unary) {
        match unary {
            Unary::Identity(primary) => self.visit_primary(primary),
            Unary::Neg(unary) => self.visit_unary(&unary),
            Unary::Ref(unary) => self.visit_unary(&unary),
            Unary::Deref(unary) => self.visit_unary(&unary),
        }
    }

    fn visit_primary(&self, primary: &Primary) {
        match primary {
            Primary::Num(_num) => (),
            Primary::Ident(_offset) => (),
            Primary::FunctionCall(_name, args) => {
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Primary::Expr(expr) => self.visit_expr(&expr),
        }
    }
}
