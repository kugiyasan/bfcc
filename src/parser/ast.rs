#![allow(dead_code)]

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

#[derive(Clone, Debug, PartialEq)]
pub enum ConstantExpr {
    Identity(ExprKind),
    Ternary(ExprKind, Expr, Box<ConstantExpr>),
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

#[derive(Clone, Debug, PartialEq)]
pub enum Primary {
    Num(i32),
    Ident(Identifier),
    String(Vec<u8>),
    Expr(Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
    pub name: String,
}
