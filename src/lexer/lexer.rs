use super::{
    token::{KEYWORDS, ONE_SYMBOL_TOKENS, THREE_SYMBOLS_TOKENS, TWO_SYMBOLS_TOKENS},
    TokenKind,
};

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
}

#[derive(Debug)]
pub struct Lexer {
    tokens: Vec<Token>,
    index: usize,
}

impl Lexer {
    fn new() -> Self {
        Self {
            tokens: vec![],
            index: 0,
        }
    }

    fn new_token(&mut self, kind: TokenKind, len: usize) {
        self.tokens.push(Token { kind });
        self.index += len;
    }

    fn parse_number(&mut self, chars: &[char]) {
        let s: String = chars.iter().take_while(|c| c.is_numeric()).collect();
        let value = s.parse().unwrap();
        self.new_token(TokenKind::Num(value), s.len());
    }

    fn parse_identifier(&mut self, chars: &[char]) {
        let s: String = chars
            .iter()
            .take_while(|&&c| c.is_ascii_alphanumeric() || c == '_')
            .collect();

        if let Some(kind) = KEYWORDS.get(&s) {
            self.new_token(kind.clone(), s.len());
        } else {
            self.new_token(TokenKind::Ident(s.clone()), s.len());
        }
    }

    fn parse_string(&mut self, chars: &[char]) {
        let mut i = 0;
        while chars[i] != '"' {
            i += 1;
        }
        let s = chars[..i].iter().map(|&c| c as u8).collect::<Vec<u8>>();
        let len = s.len();
        self.new_token(TokenKind::String(s), len);
    }

    fn _tokenize(&mut self, s: &str) {
        let chars: Vec<_> = s.chars().collect();

        while self.index < chars.len() {
            let c = chars[self.index];
            let c2 = chars[self.index..].iter().take(2).collect::<String>();
            let c3 = chars[self.index..].iter().take(3).collect::<String>();

            if c == ' ' || c == '\n' || c == '\t' {
                self.index += 1;
            } else if c2 == "//" {
                self.index += 2;
                while self.index < chars.len() && chars[self.index] != '\n' {
                    self.index += 1;
                }
            } else if c2 == "/*" {
                self.index += 2;
                while self.index < chars.len()
                    && chars[self.index] != '*'
                    && chars[self.index + 1] != '/'
                {
                    self.index += 1;
                }
            } else if c.is_numeric() {
                self.parse_number(&chars[self.index..]);
            } else if c.is_ascii_alphabetic() {
                self.parse_identifier(&chars[self.index..]);
            } else if let Some(kind) = THREE_SYMBOLS_TOKENS.get(&c3) {
                self.new_token(kind.clone(), 3);
            } else if let Some(kind) = TWO_SYMBOLS_TOKENS.get(&c2) {
                self.new_token(kind.clone(), 2);
            } else if let Some(kind) = ONE_SYMBOL_TOKENS.get(&c) {
                self.new_token(kind.clone(), 1);
            } else if c == '"' {
                self.index += 1;
                self.parse_string(&chars[self.index..]);
                self.index += 1;
            } else {
                panic!("can't tokenize {:?}", c);
            }
        }
    }

    pub fn tokenize(s: &str) -> Vec<Token> {
        let mut lexer = Lexer::new();
        lexer._tokenize(s);

        lexer.tokens
    }
}
