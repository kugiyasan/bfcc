use phf::phf_map;

static KEYWORDS: phf::Map<&str, TokenKind> = phf_map! {
    "return" => TokenKind::Return,
    "if" => TokenKind::If,
    "else" => TokenKind::Else,
    "while" => TokenKind::While,
    "for" => TokenKind::For,
};

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Plus,
    Minus,
    Star,
    Slash,
    Num(i32),
    LeftParen,
    RightParen,

    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    DoubleEqual,
    NotEqual,

    Ident(String),
    Equal,
    SemiColon,
    Return,

    If,
    Else,
    While,
    For,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
}

#[derive(Debug)]
pub struct Tokens {
    tokens: Vec<Token>,
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
        self.tokens.push(Token { kind });
        self.index += s.len();
    }

    fn parse_number(&mut self, chars: &[char]) {
        let s: String = chars.iter().take_while(|c| c.is_numeric()).collect();
        let value = s.parse().unwrap();
        self.new_token(TokenKind::Num(value), &s);
    }

    fn parse_identifier(&mut self, chars: &[char]) {
        let s: String = chars
            .iter()
            .take_while(|&&c| c.is_ascii_alphanumeric() || c == '_')
            .collect();

        if let Some(kind) = KEYWORDS.get(&s) {
            self.new_token(kind.clone(), &s);
        } else {
            self.new_token(TokenKind::Ident(s.clone()), &s);
        }
    }

    fn _tokenize(&mut self, s: String) {
        let chars: Vec<_> = s.chars().collect();

        while self.index < chars.len() {
            match chars[self.index] {
                ' ' | '\n' => self.index += 1,
                '+' => self.new_token(TokenKind::Plus, "+"),
                '-' => self.new_token(TokenKind::Minus, "-"),
                '*' => self.new_token(TokenKind::Star, "*"),
                '/' => self.new_token(TokenKind::Slash, "/"),
                c if c.is_numeric() => self.parse_number(&chars[self.index..]),
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
                        self.new_token(TokenKind::DoubleEqual, "==")
                    } else {
                        self.new_token(TokenKind::Equal, "=")
                    }
                }
                '!' => {
                    if chars[self.index + 1] == '=' {
                        self.new_token(TokenKind::NotEqual, "!=")
                    } else {
                        panic!("can't tokenize");
                    }
                }
                c if c.is_ascii_alphabetic() => self.parse_identifier(&chars[self.index..]),
                ';' => self.new_token(TokenKind::SemiColon, ";"),
                _ => panic!("can't tokenize"),
            }
        }
    }

    pub fn tokenize(s: String) -> Vec<Token> {
        let mut tokens = Tokens::new();
        tokens._tokenize(s);

        tokens.tokens
    }
}
