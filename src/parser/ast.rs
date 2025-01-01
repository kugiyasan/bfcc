#![allow(dead_code)]

#[derive(Clone, Debug)]
pub struct TranslationUnit(pub Vec<FuncDef>);

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub specs: Vec<DeclarationSpecifier>,
    pub declarator: Declarator,
    pub stmt: CompoundStmt,
}

#[derive(Clone, Debug)]
pub enum DeclarationSpecifier {
    StorageClassSpecifier(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
}

#[derive(Clone, Debug)]
pub enum StorageClassSpecifier {
    Auto,
    Register,
    Static,
    Extern,
    Typedef,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum SpecifierQualifier {
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
}

#[derive(Clone, Debug)]
pub struct Declarator {
    pub pointer: Option<Pointer>,
    pub direct: DirectDeclarator,
}

#[derive(Clone, Debug)]
pub struct Pointer {
    pub types: Vec<TypeQualifier>,
    pub pointer: Box<Option<Pointer>>,
}

#[derive(Clone, Debug)]
pub enum TypeQualifier {
    Const,
    Volatile,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct ParamTypeList {
    pub params: Vec<ParamDeclaration>,
    pub variadic: bool,
}

#[derive(Clone, Debug)]
pub enum ParamDeclaration {
    Declarator(Vec<DeclarationSpecifier>, Box<Declarator>),
    AbstractDeclarator(Vec<DeclarationSpecifier>, Box<Declarator>), // todo
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Label(Identifier, Box<Stmt>),
    Case(Expr, Box<Stmt>), // todo
    Default(Expr, Box<Stmt>),

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

#[derive(Clone, Debug)]
pub struct CompoundStmt(pub Vec<DeclarationOrStmt>);

#[derive(Clone, Debug)]
pub enum DeclarationOrStmt {
    Declaration(Declaration),
    Stmt(Stmt),
}

#[derive(Clone, Debug)]
pub struct Declaration {
    pub specs: Vec<DeclarationSpecifier>,
    pub inits: Vec<InitDeclarator>,
}

#[derive(Clone, Debug)]
pub enum InitDeclarator {
    Declarator(Declarator),
    DeclaratorAndInitializer(Declarator, ()),
}

#[derive(Clone, Debug)]
pub struct Expr(pub Vec<Assign>);

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum Assign {
    Const(ConstantExpr),
    Assign(Unary, AssignOpKind, Box<Assign>),
}

#[derive(Clone, Debug)]
pub enum ConstantExpr {
    Identity(ExprKind),
    Ternary(ExprKind, Expr, Box<ConstantExpr>),
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum ExprKind {
    Unary(Unary),
    Binary(BinOpKind, Box<ExprKind>, Box<ExprKind>),
}

#[derive(Clone, Debug)]
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
}

#[derive(Clone, Debug)]
pub enum Primary {
    Num(i32),
    Ident(Identifier),
    FunctionCall(String, Option<Expr>),
    Expr(Box<Expr>),
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub name: String,
}
