use std::collections::HashMap;

use crate::token::{Token, TokenKind};

#[derive(Debug)]
struct LocalVariables {
    locals: HashMap<String, usize>,
    last_offset: usize,
}

#[derive(Debug)]
pub struct Ast {
    tokens: Vec<Token>,
    index: usize,
    locals: LocalVariables,
}

impl LocalVariables {
    pub fn new() -> Self {
        Self {
            locals: HashMap::new(),
            last_offset: 0,
        }
    }

    pub fn reset(&mut self) {
        self.locals.clear();
        self.last_offset = 0;
    }

    pub fn get_lvar_offset(&mut self, ident: &str) -> usize {
        if let Some(offset) = self.locals.get(ident) {
            return *offset;
        }
        self.last_offset += 8;
        let offset = self.last_offset;
        self.locals.insert(ident.to_string(), offset);
        offset
    }
}

#[derive(Debug)]
pub struct Program(pub Vec<Func>);

#[derive(Debug)]
pub struct Func {
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
pub enum Expr {
    Assign(Assign),
}

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
    Pos(Primary),
    Neg(Primary),
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

impl Ast {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: 0,
            locals: LocalVariables::new(),
        }
    }

    pub fn parse(&mut self) -> Program {
        self.parse_program()
    }

    fn is_eof(&mut self) -> bool {
        self.index >= self.tokens.len()
    }

    fn consume(&mut self, kind: &TokenKind) -> bool {
        if self.is_eof() {
            return false;
        }
        let t = &self.tokens[self.index];
        if &t.kind != kind {
            return false;
        }
        self.index += 1;
        return true;
    }

    fn expect(&mut self, kind: &TokenKind) {
        if self.is_eof() {
            panic!("Expected {:?}, however received EOF", kind);
        }
        let t = &self.tokens[self.index];
        if &t.kind != kind {
            panic!(
                "Unexpected token at index {}: {:?} (was expecting {:?})",
                self.index, t.kind, kind
            );
        }
        self.index += 1;
    }

    fn expect_ident(&mut self) -> String {
        if self.is_eof() {
            panic!("Unexpected EOF");
        }
        let k = self.tokens[self.index].kind.clone();
        if let TokenKind::Ident(name) = k {
            self.index += 1;
            return name;
        }
        panic!(
            "Unexpected token at index {}: {:?} (was expecting Ident)",
            self.index, k
        );
    }

    /// program = func*
    fn parse_program(&mut self) -> Program {
        let mut funcs = vec![];

        while self.index < self.tokens.len() {
            funcs.push(self.parse_func());
        }

        Program(funcs)
    }

    /// func = "int" name "(" args ")" "{" stmt* "}"
    fn parse_func(&mut self) -> Func {
        self.locals.reset();
        self.expect(&TokenKind::Int);

        let t = self.tokens[self.index].clone();
        let TokenKind::Ident(name) = t.kind else {
            panic!("Expected function definition, got {:?}", t.kind);
        };

        self.index += 1;
        let args = self.parse_args();

        let mut stmts = vec![];
        self.expect(&TokenKind::OpenCurlyBrace);
        while !self.consume(&TokenKind::CloseCurlyBrace) {
            stmts.push(self.parse_stmt());
        }

        Func {
            name,
            args,
            stmts,
            local_offset: self.locals.last_offset,
        }
    }

    /// args = ("int" ident ("," "int" ident)*)?
    fn parse_args(&mut self) -> Vec<usize> {
        let mut args = vec![];
        self.expect(&TokenKind::OpenParen);

        if !self.consume(&TokenKind::CloseParen) {
            self.expect(&TokenKind::Int);
            let ident = self.expect_ident();
            let offset = self.locals.get_lvar_offset(&ident);
            args.push(offset);
            while self.consume(&TokenKind::Comma) {
                self.expect(&TokenKind::Int);
                let ident = self.expect_ident();
                let offset = self.locals.get_lvar_offset(&ident);
                args.push(offset);
            }
            self.expect(&TokenKind::CloseParen);
        }

        args
    }

    /// stmt = expr ";"
    ///      | "{" stmt* "}"
    ///      | "if" "(" expr ")" stmt ("else" stmt)?
    ///      | "while" "(" expr ")" stmt
    ///      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
    ///      | "return" expr ";"
    fn parse_stmt(&mut self) -> Stmt {
        if self.consume(&TokenKind::OpenCurlyBrace) {
            let mut stmt = vec![];
            while !self.consume(&TokenKind::CloseCurlyBrace) {
                stmt.push(self.parse_stmt());
            }
            Stmt::Block(stmt)
        } else if self.consume(&TokenKind::If) {
            self.expect(&TokenKind::OpenParen);
            let expr = self.parse_expr();
            self.expect(&TokenKind::CloseParen);
            let stmt = self.parse_stmt();
            let else_stmt = if self.consume(&TokenKind::Else) {
                Some(Box::new(self.parse_stmt()))
            } else {
                None
            };
            Stmt::If(expr, Box::new(stmt), else_stmt)
        } else if self.consume(&TokenKind::While) {
            self.expect(&TokenKind::OpenParen);
            let expr = self.parse_expr();
            self.expect(&TokenKind::CloseParen);
            let stmt = self.parse_stmt();
            Stmt::While(expr, Box::new(stmt))
        } else if self.consume(&TokenKind::For) {
            self.parse_for()
        } else if self.consume(&TokenKind::Return) {
            let expr = self.parse_expr();
            self.expect(&TokenKind::SemiColon);
            Stmt::Return(expr)
        } else {
            let expr = self.parse_expr();
            self.expect(&TokenKind::SemiColon);
            Stmt::Expr(expr)
        }
    }

    fn parse_for(&mut self) -> Stmt {
        self.expect(&TokenKind::OpenParen);
        let expr1 = if !self.consume(&TokenKind::SemiColon) {
            let expr = Some(self.parse_expr());
            self.expect(&TokenKind::SemiColon);
            expr
        } else {
            None
        };
        let expr2 = if !self.consume(&TokenKind::SemiColon) {
            let expr = Some(self.parse_expr());
            self.expect(&TokenKind::SemiColon);
            expr
        } else {
            None
        };
        let expr3 = if !self.consume(&TokenKind::CloseParen) {
            let expr = Some(self.parse_expr());
            self.expect(&TokenKind::CloseParen);
            expr
        } else {
            None
        };
        let stmt = self.parse_stmt();
        Stmt::For(expr1, expr2, expr3, Box::new(stmt))
    }

    /// expr = assign
    fn parse_expr(&mut self) -> Expr {
        Expr::Assign(self.parse_assign())
    }

    /// assign = equality ("=" assign)?
    fn parse_assign(&mut self) -> Assign {
        let eq = self.parse_equality();
        let assign = if self.consume(&TokenKind::Equal) {
            Some(Box::new(self.parse_assign()))
        } else {
            None
        };

        Assign { eq, assign }
    }

    /// equality = relational ("==" relational | "!=" relational)*
    fn parse_equality(&mut self) -> Equality {
        let rel = self.parse_relational();

        if self.consume(&TokenKind::DoubleEqual) {
            Equality::Equal(rel, Box::new(self.parse_equality()))
        } else if self.consume(&TokenKind::NotEqual) {
            Equality::NotEqual(rel, Box::new(self.parse_equality()))
        } else {
            Equality::Identity(rel)
        }
    }

    /// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    fn parse_relational(&mut self) -> Relational {
        let add = self.parse_add();

        if self.consume(&TokenKind::LessThan) {
            Relational::LessThan(add, Box::new(self.parse_relational()))
        } else if self.consume(&TokenKind::LessEqual) {
            Relational::LessEqual(add, Box::new(self.parse_relational()))
        } else if self.consume(&TokenKind::GreaterThan) {
            Relational::GreaterThan(add, Box::new(self.parse_relational()))
        } else if self.consume(&TokenKind::GreaterEqual) {
            Relational::GreaterEqual(add, Box::new(self.parse_relational()))
        } else {
            Relational::Identity(add)
        }
    }

    /// add = mul ("+" mul | "-" mul)*
    fn parse_add(&mut self) -> Add {
        let mul = self.parse_mul();

        if self.consume(&TokenKind::Plus) {
            Add::Add(mul, Box::new(self.parse_add()))
        } else if self.consume(&TokenKind::Minus) {
            Add::Sub(mul, Box::new(self.parse_add()))
        } else {
            Add::Identity(mul)
        }
    }

    /// mul = unary ("*" unary | "/" unary)*
    fn parse_mul(&mut self) -> Mul {
        let unary = self.parse_unary();

        if self.consume(&TokenKind::Star) {
            Mul::Mul(unary, Box::new(self.parse_mul()))
        } else if self.consume(&TokenKind::Slash) {
            Mul::Div(unary, Box::new(self.parse_mul()))
        } else {
            Mul::Identity(unary)
        }
    }

    /// unary = "+"? primary
    ///       | "-"? primary
    ///       | "*" unary
    ///       | "&" unary
    fn parse_unary(&mut self) -> Unary {
        match self.tokens[self.index].kind {
            TokenKind::Plus => {
                self.index += 1;
                Unary::Pos(self.parse_primary())
            }
            TokenKind::Minus => {
                self.index += 1;
                Unary::Neg(self.parse_primary())
            }
            TokenKind::Ampersand => {
                self.index += 1;
                Unary::Ref(Box::new(self.parse_unary()))
            }
            TokenKind::Star => {
                self.index += 1;
                Unary::Deref(Box::new(self.parse_unary()))
            }
            _ => Unary::Pos(self.parse_primary()),
        }
    }

    /// primary = num
    ///         | ident ("(" (expr (, expr)*)? ")")?
    ///         | "(" expr ")"
    fn parse_primary(&mut self) -> Primary {
        match &self.tokens[self.index].kind {
            TokenKind::OpenParen => {
                self.index += 1;
                let expr = self.parse_expr();
                self.expect(&TokenKind::CloseParen);
                Primary::Expr(Box::new(expr))
            }
            TokenKind::Num(num) => {
                self.index += 1;
                Primary::Num(*num)
            }
            TokenKind::Ident(ident) => {
                self.index += 1;
                self.parse_ident(ident.clone())
            }

            t => panic!("Unexpected token: {:?}", t),
        }
    }

    fn parse_ident(&mut self, name: String) -> Primary {
        if self.consume(&TokenKind::OpenParen) {
            let mut args = vec![];

            if self.consume(&TokenKind::CloseParen) {
                Primary::FunctionCall(name, args)
            } else {
                args.push(self.parse_expr());
                while self.consume(&TokenKind::Comma) {
                    args.push(self.parse_expr());
                }
                self.expect(&TokenKind::CloseParen);
                Primary::FunctionCall(name, args)
            }
        } else {
            let offset = self.locals.get_lvar_offset(&name);
            Primary::Ident(offset)
        }
    }
}
