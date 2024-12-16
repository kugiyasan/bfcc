#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    Add,
    Sub,
    Mul,
    Div,
    Num(i32),
    LeftParen,
    RightParen,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equal,
    NotEqual,
    Eof,
}

#[derive(Debug)]
pub struct Tokens {
    tokens: Vec<TokenKind>,
    index: usize,
}

impl Tokens {
    fn new() -> Self {
        Self {
            tokens: vec![],
            index: 0,
        }
    }

    fn new_token(&mut self, kind: TokenKind, s: &str) {
        self.tokens.push(kind);
        self.index += s.len();
    }

    fn parse_number(&mut self, chars: &[char]) -> (i32, usize) {
        let n: String = chars.iter().take_while(|c| c.is_numeric()).collect();
        (n.parse().unwrap(), self.index + n.len())
    }

    fn _tokenize(&mut self, s: String) {
        let chars: Vec<_> = s.chars().collect();

        while self.index < chars.len() {
            match chars[self.index] {
                ' ' | '\n' => self.index += 1,
                '+' => self.new_token(TokenKind::Add, "+"),
                '-' => self.new_token(TokenKind::Sub, "-"),
                '*' => self.new_token(TokenKind::Mul, "*"),
                '/' => self.new_token(TokenKind::Div, "/"),
                c if c.is_numeric() => {
                    let start = self.index;
                    let (value, end) = self.parse_number(&chars[start..]);
                    let s = chars[start..end].iter().collect::<String>();
                    self.new_token(TokenKind::Num(value), &s);
                }
                '(' => self.new_token(TokenKind::LeftParen, "("),
                ')' => self.new_token(TokenKind::RightParen, ")"),
                '<' => {
                    if chars[self.index + 1] == '=' {
                        self.new_token(TokenKind::LessEqual, "<=")
                    } else {
                        self.new_token(TokenKind::LessThan, "<")
                    }
                }
                '>' => {
                    if chars[self.index + 1] == '=' {
                        self.new_token(TokenKind::GreaterEqual, ">=")
                    } else {
                        self.new_token(TokenKind::GreaterThan, ">")
                    }
                }
                '=' => {
                    if chars[self.index + 1] == '=' {
                        self.new_token(TokenKind::Equal, "!=")
                    } else {
                        panic!("can't tokenize");
                    }
                }
                '!' => {
                    if chars[self.index + 1] == '=' {
                        self.new_token(TokenKind::NotEqual, "!=")
                    } else {
                        panic!("can't tokenize");
                    }
                }
                _ => panic!("can't tokenize"),
            }
        }

        self.new_token(TokenKind::Eof, "");
    }

    pub fn tokenize(s: String) -> Vec<TokenKind> {
        let mut tokens = Tokens::new();
        tokens._tokenize(s);

        tokens.tokens
    }
}
