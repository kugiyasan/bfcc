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

    pub fn constant_fold(&self, symbol_table: &mut SymbolTable) -> i64 {
        if self.0.len() != 1 {
            panic!("Constant folded Expr should not have multiple Assigns");
        }
        self.0[0].constant_fold(symbol_table)
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

impl AssignOpKind {
    pub fn to_bin_op_kind(&self) -> BinOpKind {
        match self {
            AssignOpKind::Assign => unreachable!(),
            AssignOpKind::MulAssign => BinOpKind::Mul,
            AssignOpKind::DivAssign => BinOpKind::Div,
            AssignOpKind::ModAssign => BinOpKind::Mod,
            AssignOpKind::AddAssign => BinOpKind::Add,
            AssignOpKind::SubAssign => BinOpKind::Sub,
            AssignOpKind::LeftShiftAssign => BinOpKind::LeftShift,
            AssignOpKind::RightShiftAssign => BinOpKind::RightShift,
            AssignOpKind::AndAssign => BinOpKind::BitwiseAnd,
            AssignOpKind::XorAssign => BinOpKind::BitwiseXor,
            AssignOpKind::OrAssign => BinOpKind::BitwiseOr,
        }
    }
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

    pub fn constant_fold(&self, symbol_table: &mut SymbolTable) -> i64 {
        match self {
            Assign::Const(c) => c.constant_fold(symbol_table),
            Assign::Assign(_, _, _) => panic!("Constant folded Assign cannot do assignments"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstantExpr {
    Identity(BinOp),
    Ternary(BinOp, Expr, Box<ConstantExpr>),
}

impl ConstantExpr {
    pub fn get_type(&self, symbol_table: &mut SymbolTable) -> Ty {
        match self {
            ConstantExpr::Identity(binop) => binop.get_type(symbol_table),
            ConstantExpr::Ternary(_, e, _) => e.get_type(symbol_table),
        }
    }

    pub fn constant_fold(&self, symbol_table: &mut SymbolTable) -> i64 {
        match self {
            ConstantExpr::Identity(binop) => binop.constant_fold(symbol_table),
            ConstantExpr::Ternary(binop, e, c) => {
                if binop.constant_fold(symbol_table) == 1 {
                    e.constant_fold(symbol_table)
                } else {
                    c.constant_fold(symbol_table)
                }
            }
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
pub enum BinOp {
    Unary(Unary),
    Binary(BinOpKind, Box<BinOp>, Box<BinOp>),
}

impl BinOp {
    pub fn get_type(&self, symbol_table: &mut SymbolTable) -> Ty {
        match self {
            BinOp::Unary(u) => u.get_type(symbol_table),
            BinOp::Binary(_, e1, e2) => {
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

    pub fn constant_fold(&self, symbol_table: &mut SymbolTable) -> i64 {
        match self {
            BinOp::Unary(u) => u.constant_fold(symbol_table),
            BinOp::Binary(kind, b1, b2) => {
                let n1 = b1.constant_fold(symbol_table);
                let n2 = b2.constant_fold(symbol_table);
                match kind {
                    BinOpKind::LogicalOr => (n1 != 0 || n2 != 0) as i64,
                    BinOpKind::LogicalAnd => (n1 != 0 && n2 != 0) as i64,
                    BinOpKind::BitwiseOr => n1 | n2,
                    BinOpKind::BitwiseXor => n1 ^ n2,
                    BinOpKind::BitwiseAnd => n1 & n2,
                    BinOpKind::Equal => (n1 == n2) as i64,
                    BinOpKind::NotEqual => (n1 != n2) as i64,
                    BinOpKind::LessThan => (n1 < n2) as i64,
                    BinOpKind::LessEqual => (n1 <= n2) as i64,
                    BinOpKind::GreaterThan => (n1 > n2) as i64,
                    BinOpKind::GreaterEqual => (n1 >= n2) as i64,
                    BinOpKind::LeftShift => n1 << n2,
                    BinOpKind::RightShift => n1 >> n2,
                    BinOpKind::Add => n1 + n2,
                    BinOpKind::Sub => n1 - n2,
                    BinOpKind::Mul => n1 * n2,
                    BinOpKind::Div => n1 / n2,
                    BinOpKind::Mod => n1 % n2,
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
            Unary::Cast(tn, _) => {
                symbol_table.from_specs_and_abstract_declarator(&tn.specs, &tn.declarator)
            }
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

    pub fn constant_fold(&self, symbol_table: &mut SymbolTable) -> i64 {
        match self {
            Unary::Identity(p) => p.constant_fold(symbol_table),
            Unary::Cast(_, u) => u.constant_fold(symbol_table),
            Unary::Neg(u) => -u.constant_fold(symbol_table),
            Unary::BitwiseNot(u) => !u.constant_fold(symbol_table),
            Unary::LogicalNot(u) => !u.constant_fold(symbol_table),
            Unary::Sizeof(u) => u.get_type(symbol_table).sizeof(symbol_table) as i64,
            Unary::SizeofType(tn) => symbol_table
                .from_specs_and_abstract_declarator(&tn.specs, &tn.declarator)
                .sizeof(symbol_table) as i64,
            u => panic!("Cannot constant fold {:?}", u),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Primary {
    Num(i64),
    Ident(String),
    String(Vec<u8>),
    Expr(Box<Expr>),
}

impl Primary {
    pub fn get_type(&self, symbol_table: &mut SymbolTable) -> Ty {
        match self {
            Primary::Num(_) => Ty::I32, // todo
            Primary::Ident(name) => symbol_table.get_var_type(name),
            Primary::String(b) => Ty::Array(Box::new(Ty::I8), b.len()),
            Primary::Expr(e) => e.get_type(symbol_table),
        }
    }

    pub fn constant_fold(&self, symbol_table: &mut SymbolTable) -> i64 {
        match self {
            Primary::Num(n) => *n,
            Primary::Ident(i) => panic!("Found identifier {:?} while trying to constant fold", i),
            Primary::String(s) => panic!("Found string {:?} while trying to constant fold", s),
            Primary::Expr(e) => e.constant_fold(symbol_table),
        }
    }
}
