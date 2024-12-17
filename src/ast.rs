use crate::token::TokenKind;

#[derive(Debug)]
pub struct Node {
    pub kind: TokenKind,
    pub left: Option<Box<Node>>,
    pub right: Option<Box<Node>>,
}

#[derive(Debug)]
pub struct Ast {
    tokens: Vec<TokenKind>,
    index: usize,
}

impl Node {
    pub fn new_number(num: i32) -> Self {
        Self {
            kind: TokenKind::Num(num),
            left: None,
            right: None,
        }
    }

    pub fn new_ident(ident: String) -> Self {
        Self {
            kind: TokenKind::Ident(ident),
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
    pub fn new(tokens: Vec<TokenKind>) -> Self {
        Self { tokens, index: 0 }
    }

    pub fn parse(&mut self) -> Node {
        self.parse_program()
    }

    fn expect(&mut self, kind: &TokenKind) {
        let t = &self.tokens[self.index];
        if t != kind {
            panic!("Unexpected token: {:?}", t);
        }
        self.index += 1;
    }

    fn parse_program(&mut self) -> Node {
        let mut node = Node {
            kind: TokenKind::Eof,
            left: None,
            right: None,
        };

        while self.index < self.tokens.len() {
            node = Node {
                kind: TokenKind::Stmt,
                left: Some(Box::new(self.parse_stmt())),
                right: Some(Box::new(node)),
            }
        }

        node
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

        if self.tokens[self.index] == TokenKind::Assign {
            self.index += 1;
            node = Node {
                kind: TokenKind::Assign,
                left: Some(Box::new(node)),
                right: Some(Box::new(self.parse_assign())),
            };
        }

        node
    }

    fn parse_equality(&mut self) -> Node {
        let mut node = self.parse_relational();

        loop {
            match &self.tokens[self.index] {
                kind if *kind == TokenKind::Equal || *kind == TokenKind::NotEqual => {
                    self.index += 1;
                    node = Node {
                        kind: kind.clone(),
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
            match &self.tokens[self.index] {
                kind if *kind == TokenKind::LessThan || *kind == TokenKind::LessEqual => {
                    self.index += 1;
                    node = Node {
                        kind: kind.clone(),
                        left: Some(Box::new(node)),
                        right: Some(Box::new(self.parse_add())),
                    };
                }
                kind if *kind == TokenKind::GreaterThan => {
                    self.index += 1;
                    node = Node {
                        kind: TokenKind::LessThan,
                        left: Some(Box::new(self.parse_add())),
                        right: Some(Box::new(node)),
                    };
                }
                kind if *kind == TokenKind::GreaterEqual => {
                    self.index += 1;
                    node = Node {
                        kind: TokenKind::LessEqual,
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
            match &self.tokens[self.index] {
                kind if *kind == TokenKind::Add || *kind == TokenKind::Sub => {
                    self.index += 1;
                    node = Node {
                        kind: kind.clone(),
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
            match &self.tokens[self.index] {
                kind if *kind == TokenKind::Mul || *kind == TokenKind::Div => {
                    self.index += 1;
                    node = Node {
                        kind: kind.clone(),
                        left: Some(Box::new(node)),
                        right: Some(Box::new(self.parse_unary())),
                    }
                }
                _ => return node,
            }
        }
    }

    fn parse_unary(&mut self) -> Node {
        match self.tokens[self.index] {
            TokenKind::Add => {
                self.index += 1;
                self.parse_primary()
            }
            TokenKind::Sub => {
                self.index += 1;
                Node {
                    kind: TokenKind::Sub,
                    left: Some(Box::new(Node::new_number(0))),
                    right: Some(Box::new(self.parse_primary())),
                }
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Node {
        match &self.tokens[self.index] {
            TokenKind::LeftParen => {
                self.index += 1;
                let node = self.parse_expr();
                let t = &self.tokens[self.index];
                self.expect(&TokenKind::RightParen);
                node
            }
            TokenKind::Num(num) => {
                self.index += 1;
                Node::new_number(*num)
            }
            TokenKind::Ident(ident) => {
                self.index += ident.len();
                Node::new_ident(ident.clone())
            }

            t => panic!("Unexpected token: {:?}", t),
        }
    }
}
