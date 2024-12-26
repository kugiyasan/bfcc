#[derive(Debug)]
pub struct TranslationUnit(pub Vec<FuncDef>);

#[derive(Debug)]
pub struct FuncDef {
    pub name: String,
    pub args: Vec<usize>,
    pub stmt: CompoundStmt,
    pub local_offset: usize,
}

#[derive(Debug)]
pub enum DeclarationSpecifier {
    StorageClassSpecifier(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
}

#[derive(Debug)]
pub enum StorageClassSpecifier {
    Auto,
    Register,
    Static,
    Extern,
    Typedef,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum SpecifierQualifier {
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
}

#[derive(Debug)]
pub struct Declarator {
    pub pointer: Option<Pointer>,
    pub direct: DirectDeclarator,
}

#[derive(Debug)]
pub struct Pointer {
    pub types: Vec<TypeQualifier>,
    pub pointer: Box<Option<Pointer>>,
}

#[derive(Debug)]
pub enum TypeQualifier {
    Const,
    Volatile,
}

#[derive(Debug)]
pub enum DirectDeclarator {
    Ident(Identifier),
    Declarator(Box<Declarator>),
    Array(Box<DirectDeclarator>, Option<Expr>), // todo
    ParamList(Box<DirectDeclarator>, ParamList),
    Identifiers(Box<DirectDeclarator>, Vec<Identifier>),
}

#[derive(Debug)]
pub struct ParamList {
    pub params: Vec<ParamDeclaration>,
    pub variadic: bool,
}

#[derive(Debug)]
pub enum ParamDeclaration {
    Declarator(Vec<DeclarationSpecifier>, Box<Declarator>),
    AbstractDeclarator(Vec<DeclarationSpecifier>, Box<Declarator>), // todo
    Identity(Vec<DeclarationSpecifier>),
}

#[derive(Debug)]
pub enum Stmt {
    Label(Identifier, Box<Stmt>),
    Case(Expr, Box<Stmt>), // todo
    Default(Expr, Box<Stmt>),

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

#[derive(Debug)]
pub struct CompoundStmt(pub Vec<DeclarationOrStmt>);

#[derive(Debug)]
pub enum DeclarationOrStmt {
    Declaration(Declaration),
    Stmt(Stmt),
}

#[derive(Debug)]
pub struct Declaration {
    pub specs: Vec<DeclarationSpecifier>,
    pub inits: Vec<InitDeclarator>,
}

#[derive(Debug)]
pub enum InitDeclarator {
    Declarator(Declarator),
}

#[derive(Debug)]
pub struct Expr(pub Vec<Assign>);

#[derive(Debug)]
pub struct Assign {
    pub eq: Equality,
    pub assign: Option<Box<Assign>>,
}

#[derive(Debug)]
pub enum Equality {
    Identity(Relational),
    Equal(Relational, Box<Equality>),
    NotEqual(Relational, Box<Equality>),
}

#[derive(Debug)]
pub enum Relational {
    Identity(Add),
    LessThan(Add, Box<Relational>),
    LessEqual(Add, Box<Relational>),
    GreaterThan(Add, Box<Relational>),
    GreaterEqual(Add, Box<Relational>),
}

#[derive(Debug)]
pub enum Add {
    Identity(Mul),
    Add(Mul, Box<Add>),
    Sub(Mul, Box<Add>),
}

#[derive(Debug)]
pub enum Mul {
    Identity(Unary),
    Mul(Unary, Box<Mul>),
    Div(Unary, Box<Mul>),
}

#[derive(Debug)]
pub enum Unary {
    Identity(Primary),
    Neg(Box<Unary>),
    Ref(Box<Unary>),
    Deref(Box<Unary>),
}

#[derive(Debug)]
pub enum Primary {
    Num(i32),
    Ident(Identifier),
    FunctionCall(String, Option<Expr>),
    Expr(Box<Expr>),
}

#[derive(Debug)]
pub struct Identifier {
    pub offset: usize,
}
