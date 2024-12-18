use crate::token::{Token, TokenKind};

#[derive(Clone, Debug, PartialEq)]
pub enum NodeKind {
    Add,
    Sub,
    Mul,
    Div,
    Num(i32),

    LessThan,
    LessEqual,
    DoubleEqual,
    NotEqual,

    LVar { offset: i32 },
    Stmt,
    Assign,
}

#[derive(Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub left: Option<Box<Node>>,
    pub right: Option<Box<Node>>,
}

#[derive(Debug)]
pub struct Ast {
    tokens: Vec<Token>,
    index: usize,
}

impl Node {
    pub fn new_number(num: i32) -> Self {
        Self {
            kind: NodeKind::Num(num),
            left: None,
            right: None,
        }
    }

    pub fn new_ident(ident: String) -> Self {
        let c = ident.bytes().next().unwrap();
        let offset = (c - b'a' + 1) as i32 * 8;

        Self {
            kind: NodeKind::LVar { offset },
            left: None,
            right: None,
        }
    }
}

/// program    = stmt*
/// stmt       = expr ";"
/// expr       = assign
/// assign     = equality ("=" assign)?
/// equality   = relational ("==" relational | "!=" relational)*
/// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
/// add        = mul ("+" mul | "-" mul)*
/// mul        = unary ("*" unary | "/" unary)*
/// unary      = ("+" | "-")? primary
/// primary    = num | ident | "(" expr ")"
impl Ast {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    pub fn parse(tokens: Vec<Token>) -> Option<Node> {
        let mut ast = Ast::new(tokens);
        ast.parse_program()
    }

    fn expect(&mut self, kind: &TokenKind) {
        let t = &self.tokens[self.index];
        if &t.kind != kind {
            panic!("Unexpected token: {:?}", t);
        }
        self.index += 1;
    }

    fn parse_program(&mut self) -> Option<Node> {
        if self.tokens.len() == 0 {
            return None;
        }

        let mut node = self.parse_stmt();

        while self.index < self.tokens.len() {
            node = Node {
                kind: NodeKind::Stmt,
                left: Some(Box::new(node)),
                right: Some(Box::new(self.parse_stmt())),
            }
        }

        Some(node)
    }

    fn parse_stmt(&mut self) -> Node {
        let node = self.parse_expr();
        self.expect(&TokenKind::SemiColon);
        node
    }

    fn parse_expr(&mut self) -> Node {
        self.parse_assign()
    }

    fn parse_assign(&mut self) -> Node {
        let mut node = self.parse_equality();

        if self.tokens[self.index].kind == TokenKind::Equal {
            self.index += 1;
            node = Node {
                kind: NodeKind::Assign,
                left: Some(Box::new(node)),
                right: Some(Box::new(self.parse_assign())),
            };
        }

        node
    }

    fn parse_equality(&mut self) -> Node {
        let mut node = self.parse_relational();

        loop {
            match &self.tokens[self.index].kind {
                TokenKind::DoubleEqual => {
                    self.index += 1;
                    node = Node {
                        kind: NodeKind::DoubleEqual,
                        left: Some(Box::new(node)),
                        right: Some(Box::new(self.parse_relational())),
                    };
                }
                TokenKind::NotEqual => {
                    self.index += 1;
                    node = Node {
                        kind: NodeKind::NotEqual,
                        left: Some(Box::new(node)),
                        right: Some(Box::new(self.parse_relational())),
                    };
                }
                _ => return node,
            }
        }
    }

    fn parse_relational(&mut self) -> Node {
        let mut node = self.parse_add();

        loop {
            match &self.tokens[self.index].kind {
                TokenKind::LessThan => {
                    self.index += 1;
                    node = Node {
                        kind: NodeKind::LessThan,
                        left: Some(Box::new(node)),
                        right: Some(Box::new(self.parse_add())),
                    };
                }
                TokenKind::LessEqual => {
                    self.index += 1;
                    node = Node {
                        kind: NodeKind::LessEqual,
                        left: Some(Box::new(node)),
                        right: Some(Box::new(self.parse_add())),
                    };
                }
                TokenKind::GreaterThan => {
                    self.index += 1;
                    node = Node {
                        kind: NodeKind::LessThan,
                        left: Some(Box::new(self.parse_add())),
                        right: Some(Box::new(node)),
                    };
                }
                TokenKind::GreaterEqual => {
                    self.index += 1;
                    node = Node {
                        kind: NodeKind::LessEqual,
                        left: Some(Box::new(self.parse_add())),
                        right: Some(Box::new(node)),
                    };
                }
                _ => return node,
            }
        }
    }

    fn parse_add(&mut self) -> Node {
        let mut node = self.parse_mul();

        loop {
            match &self.tokens[self.index].kind {
                TokenKind::Plus => {
                    self.index += 1;
                    node = Node {
                        kind: NodeKind::Add,
                        left: Some(Box::new(node)),
                        right: Some(Box::new(self.parse_mul())),
                    };
                }
                TokenKind::Minus => {
                    self.index += 1;
                    node = Node {
                        kind: NodeKind::Sub,
                        left: Some(Box::new(node)),
                        right: Some(Box::new(self.parse_mul())),
                    };
                }
                _ => return node,
            }
        }
    }

    fn parse_mul(&mut self) -> Node {
        let mut node = self.parse_unary();

        loop {
            match &self.tokens[self.index].kind {
                TokenKind::Star => {
                    self.index += 1;
                    node = Node {
                        kind: NodeKind::Mul,
                        left: Some(Box::new(node)),
                        right: Some(Box::new(self.parse_unary())),
                    }
                }
                TokenKind::Slash => {
                    self.index += 1;
                    node = Node {
                        kind: NodeKind::Div,
                        left: Some(Box::new(node)),
                        right: Some(Box::new(self.parse_unary())),
                    }
                }
                _ => return node,
            }
        }
    }

    fn parse_unary(&mut self) -> Node {
        match self.tokens[self.index].kind {
            TokenKind::Plus => {
                self.index += 1;
                self.parse_primary()
            }
            TokenKind::Minus => {
                self.index += 1;
                Node {
                    kind: NodeKind::Sub,
                    left: Some(Box::new(Node::new_number(0))),
                    right: Some(Box::new(self.parse_primary())),
                }
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Node {
        match &self.tokens[self.index].kind {
            TokenKind::LeftParen => {
                self.index += 1;
                let node = self.parse_expr();
                self.expect(&TokenKind::RightParen);
                node
            }
            TokenKind::Num(num) => {
                self.index += 1;
                Node::new_number(*num)
            }
            TokenKind::Ident(ident) => {
                self.index += 1;
                Node::new_ident(ident.clone())
            }

            t => panic!("Unexpected token: {:?}", t),
        }
    }
}
