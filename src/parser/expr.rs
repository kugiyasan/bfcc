use crate::analyzer::{SymbolTable, Ty};

use super::TypeName;

#[derive(Clone, Debug, PartialEq)]
pub struct Expr(pub Vec<Assign>);

impl Expr {
    pub fn get_type(&self, symbol_table: &mut SymbolTable) -> Ty {
        self.0
            .last()
            .map(|a| a.get_type(symbol_table))
            .unwrap_or(Ty::Void)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AssignOpKind {
    Assign,
    MulAssign,
    DivAssign,
    ModAssign,
    AddAssign,
    SubAssign,
    LeftShiftAssign,
    RightShiftAssign,
    AndAssign,
    XorAssign,
    OrAssign,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Assign {
    Const(ConstantExpr),
    Assign(Unary, AssignOpKind, Box<Assign>),
}

impl Assign {
    pub fn get_type(&self, symbol_table: &mut SymbolTable) -> Ty {
        match self {
            Assign::Const(c) => c.get_type(symbol_table),
            Assign::Assign(_, _, a) => a.get_type(symbol_table),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstantExpr {
    Identity(ExprKind),
    Ternary(ExprKind, Expr, Box<ConstantExpr>),
}

impl ConstantExpr {
    pub fn get_type(&self, symbol_table: &mut SymbolTable) -> Ty {
        match self {
            ConstantExpr::Identity(e) => e.get_type(symbol_table),
            ConstantExpr::Ternary(_, e, _) => e.get_type(symbol_table),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinOpKind {
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    LeftShift,
    RightShift,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Unary(Unary),
    Binary(BinOpKind, Box<ExprKind>, Box<ExprKind>),
}

impl ExprKind {
    pub fn get_type(&self, symbol_table: &mut SymbolTable) -> Ty {
        match self {
            ExprKind::Unary(u) => u.get_type(symbol_table),
            ExprKind::Binary(_, e1, e2) => {
                let t1 = e1.get_type(symbol_table);
                let t2 = e2.get_type(symbol_table);
                match (t1, t2) {
                    (t1, t2) if t1 == t2 => t1,
                    (Ty::Ptr(t), _) | (_, Ty::Ptr(t)) => Ty::Ptr(t),
                    (Ty::Array(t, s), _) | (_, Ty::Array(t, s)) => Ty::Array(t, s),
                    _ => panic!(),
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Unary {
    Identity(Primary),
    Cast(Box<TypeName>, Box<Unary>),

    Neg(Box<Unary>),
    Ref(Box<Unary>),
    Deref(Box<Unary>),
    BitwiseNot(Box<Unary>),
    LogicalNot(Box<Unary>),
    PrefixIncrement(Box<Unary>),
    PrefixDecrement(Box<Unary>),
    Sizeof(Box<Unary>),
    SizeofType(Box<TypeName>),

    Index(Box<Unary>, Expr),
    Call(Box<Unary>, Option<Expr>),
    Field(Box<Unary>, String),
    PointerField(Box<Unary>, String),
    PostfixIncrement(Box<Unary>),
    PostfixDecrement(Box<Unary>),
}

impl Unary {
    pub fn get_type(&self, symbol_table: &mut SymbolTable) -> Ty {
        match self {
            Unary::Identity(p) => p.get_type(symbol_table),
            Unary::Cast(tn, _) => symbol_table.from_type_name(tn),
            Unary::Call(_, _) => todo!(),
            Unary::Index(_, _) => panic!("Semantic visitor should have desugared indexing"),
            Unary::Field(u, f) => {
                let Ty::Struct(name) = u.get_type(symbol_table) else {
                    panic!("Accessing a field on a non-struct type");
                };
                let (_, ty) = symbol_table.get_struct_field(&name, f);
                ty.clone()
            }
            Unary::PointerField(_, _) => unreachable!(),

            Unary::Ref(u) => Ty::Ptr(Box::new(u.get_type(symbol_table))),
            Unary::Deref(u) => u.get_type(symbol_table).get_inner().unwrap(),
            Unary::Neg(u)
            | Unary::BitwiseNot(u)
            | Unary::LogicalNot(u)
            | Unary::PrefixIncrement(u)
            | Unary::PrefixDecrement(u)
            | Unary::Sizeof(u)
            | Unary::PostfixIncrement(u)
            | Unary::PostfixDecrement(u) => u.get_type(symbol_table),

            Unary::SizeofType(_) => todo!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Primary {
    Num(i32),
    Ident(String),
    String(Vec<u8>),
    Expr(Box<Expr>),
}

impl Primary {
    pub fn get_type(&self, symbol_table: &mut SymbolTable) -> Ty {
        match self {
            Primary::Num(_) => Ty::Int, // todo
            Primary::Ident(name) => symbol_table.get_var_type(name),
            Primary::String(b) => Ty::Array(Box::new(Ty::Char), b.len()),
            Primary::Expr(e) => e.get_type(symbol_table),
        }
    }
}
