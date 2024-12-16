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
}

/// expr    = mul ("+" mul | "-" mul)*
/// mul     = unary ("*" unary | "/" unary)*
/// unary   = ("+" | "-")? primary
/// primary = num | "(" expr ")"
impl Ast {
    pub fn new(tokens: Vec<TokenKind>) -> Self {
        Self { tokens, index: 0 }
    }

    pub fn parse(&mut self) -> Node {
        self.parse_expr()
    }

    fn parse_expr(&mut self) -> Node {
        let mut node = self.parse_mul();

        loop {
            match self.tokens[self.index] {
                kind if kind == TokenKind::Add || kind == TokenKind::Sub => {
                    self.index += 1;
                    node = Node {
                        kind,
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
            match self.tokens[self.index] {
                kind if kind == TokenKind::Mul || kind == TokenKind::Div => {
                    self.index += 1;
                    node = Node {
                        kind,
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
        match self.tokens[self.index] {
            TokenKind::LeftParen => {
                self.index += 1;
                let node = self.parse_expr();
                let t = self.tokens[self.index];
                if t != TokenKind::RightParen {
                    panic!("Unexpected token: {:?}", t);
                }
                self.index += 1;
                node
            }
            TokenKind::Num(num) => {
                self.index += 1;
                Node::new_number(num)
            }

            t => panic!("Unexpected token: {:?}", t),
        }
    }
}
