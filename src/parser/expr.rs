use crate::analyzer::{SymbolTable, Ty};

#[derive(Clone, Debug, PartialEq)]
pub struct Expr(pub Vec<Assign>);

impl Expr {
    pub fn get_type(&self, symbol_table: &SymbolTable) -> Ty {
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
    pub fn get_type(&self, symbol_table: &SymbolTable) -> Ty {
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
    pub fn get_type(&self, symbol_table: &SymbolTable) -> Ty {
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
    pub fn get_type(&self, symbol_table: &SymbolTable) -> Ty {
        match self {
            ExprKind::Unary(u) => u.get_type(symbol_table),
            ExprKind::Binary(_, e, _) => e.get_type(symbol_table),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Unary {
    Identity(Primary),
    Cast((), Box<Unary>), // todo

    Neg(Box<Unary>),
    Ref(Box<Unary>),
    Deref(Box<Unary>),
    BitwiseNot(Box<Unary>),
    LogicalNot(Box<Unary>),
    PrefixIncrement(Box<Unary>),
    PrefixDecrement(Box<Unary>),
    Sizeof(Box<Unary>),

    Index(Box<Unary>, Expr),
    Call(Box<Unary>, Option<Expr>),
    Field(Box<Unary>, Identifier),
    PointerField(Box<Unary>, Identifier),
    PostfixIncrement(Box<Unary>),
    PostfixDecrement(Box<Unary>),
}

impl Unary {
    pub fn get_type(&self, symbol_table: &SymbolTable) -> Ty {
        match self {
            Unary::Identity(p) => p.get_type(symbol_table),
            Unary::Cast(_, _) | Unary::Call(_, _) => {
                panic!("lvalue required as left operand of assignment")
            }
            Unary::Index(_, _) => panic!("Semantic visitor should have desugared indexing"),
            Unary::Field(_, _) => todo!(),
            Unary::PointerField(_, _) => todo!(),

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
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Primary {
    Num(i32),
    Ident(Identifier),
    String(Vec<u8>),
    Expr(Box<Expr>),
}

impl Primary {
    pub fn get_type(&self, symbol_table: &SymbolTable) -> Ty {
        match self {
            Primary::Num(_) => Ty::Int, // todo
            Primary::Ident(Identifier { name }) => symbol_table.get_var_type(name),
            Primary::String(b) => Ty::Array(Box::new(Ty::Char), b.len()),
            Primary::Expr(e) => e.get_type(symbol_table),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
    pub name: String,
}
