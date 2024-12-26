#[derive(Debug)]
pub struct TranslationUnit(pub Vec<FuncDef>);

#[derive(Debug)]
pub struct FuncDef {
    pub name: String,
    pub args: Vec<usize>,
    pub stmts: Vec<Stmt>,
    pub local_offset: usize,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    For(Option<Expr>, Option<Expr>, Option<Expr>, Box<Stmt>),
    Return(Expr),
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
    Ident(usize),
    FunctionCall(String, Vec<Expr>),
    Expr(Box<Expr>),
}
