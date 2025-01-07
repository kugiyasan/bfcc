#![allow(dead_code)]

use crate::analyzer::{SymbolTable, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct TranslationUnit(pub Vec<ExternalDeclaration>);

#[derive(Clone, Debug, PartialEq)]
pub enum ExternalDeclaration {
    FuncDef(FuncDef),
    Declaration(Declaration),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDef {
    pub specs: Vec<DeclarationSpecifier>,
    pub declarator: Declarator,
    pub stmt: CompoundStmt,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DeclarationSpecifier {
    StorageClassSpecifier(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
}

#[derive(Clone, Debug, PartialEq)]
pub enum StorageClassSpecifier {
    Auto,
    Register,
    Static,
    Extern,
    Typedef,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    StructOrUnionSpecifier(),
    EnumSpecifier(),
    TypedefName(),
}

#[derive(Clone, Debug, PartialEq)]
pub enum SpecifierQualifier {
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declarator {
    pub pointer: Option<Pointer>,
    pub direct: DirectDeclarator,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Pointer {
    pub types: Vec<TypeQualifier>,
    pub pointer: Box<Option<Pointer>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeQualifier {
    Const,
    Volatile,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DirectDeclarator {
    Ident(Identifier),
    Declarator(Box<Declarator>),
    Array(Box<DirectDeclarator>, Option<ConstantExpr>),
    ParamTypeList(Box<DirectDeclarator>, ParamTypeList),
}

impl DirectDeclarator {
    pub fn get_name(&self) -> String {
        match self {
            DirectDeclarator::Ident(Identifier { name }) => name.to_string(),
            DirectDeclarator::Declarator(d) => d.direct.get_name(),
            DirectDeclarator::Array(dd, _) => dd.get_name(),
            DirectDeclarator::ParamTypeList(dd, _) => dd.get_name(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParamTypeList {
    pub params: Vec<ParamDeclaration>,
    pub variadic: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParamDeclaration {
    Declarator(Vec<DeclarationSpecifier>, Box<Declarator>),
    AbstractDeclarator(Vec<DeclarationSpecifier>, Box<Declarator>), // todo
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Label(Identifier, Box<Stmt>),
    Case(ConstantExpr, Box<Stmt>),
    Default(Box<Stmt>),

    SemiColon,
    Expr(Expr),
    Compound(CompoundStmt),

    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Switch(Expr, Box<Stmt>),

    While(Expr, Box<Stmt>),
    DoWhile(Box<Stmt>, Expr),
    For(Option<Expr>, Option<Expr>, Option<Expr>, Box<Stmt>),

    Goto(Identifier),
    Continue,
    Break,
    Return(Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompoundStmt(pub Vec<DeclarationOrStmt>);

#[derive(Clone, Debug, PartialEq)]
pub enum DeclarationOrStmt {
    Declaration(Declaration),
    Stmt(Stmt),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declaration {
    pub specs: Vec<DeclarationSpecifier>,
    pub inits: Vec<InitDeclarator>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum InitDeclarator {
    Declarator(Declarator),
    DeclaratorAndInitializer(Declarator, ()),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr(pub Vec<Assign>);

impl Expr {
    pub fn get_type(&self, symbol_table: &SymbolTable) -> Type {
        self.0.last().map(|a| a.get_type(symbol_table)).unwrap_or(Type::Void)
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
    pub fn get_type(&self, symbol_table: &SymbolTable) -> Type {
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
    pub fn get_type(&self, symbol_table: &SymbolTable) -> Type {
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
    pub fn get_type(&self, symbol_table: &SymbolTable) -> Type {
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
    pub fn get_type(&self, symbol_table: &SymbolTable) -> Type {
        match self {
            Unary::Identity(p) => p.get_type(symbol_table),
            Unary::Cast(_, _) | Unary::Call(_, _) => panic!("lvalue required as left operand of assignment"),
            Unary::Index(_, _) => panic!("Semantic visitor should have desugared indexing"),
            Unary::Field(_, _) => todo!(),
            Unary::PointerField(_, _) => todo!(),

            Unary::Ref(u) => Type::Ptr(Box::new(u.get_type(symbol_table))),
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
    pub fn get_type(&self, symbol_table: &SymbolTable) -> Type {
        match self {
            Primary::Num(_) => Type::Int, // todo
            Primary::Ident(Identifier { name }) => symbol_table.get_var_type(name),
            Primary::String(b) => Type::Array(Box::new(Type::Char), b.len()),
            Primary::Expr(e) => e.get_type(symbol_table),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
    pub name: String,
}
