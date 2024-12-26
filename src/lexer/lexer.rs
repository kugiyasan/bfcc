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
            let c = chars[self.index];
            let c2 = chars[self.index..].iter().take(2).collect::<String>();
            let c3 = chars[self.index..].iter().take(3).collect::<String>();

            if c == ' ' || c == '\n' {
                self.index += 1;
            } else if c.is_numeric() {
                self.parse_number(&chars[self.index..]);
            } else if c.is_ascii_alphabetic() {
                self.parse_identifier(&chars[self.index..]);
            } else if let Some(kind) = THREE_SYMBOLS_TOKENS.get(&c3) {
                self.new_token(kind.clone(), &c3);
            } else if let Some(kind) = TWO_SYMBOLS_TOKENS.get(&c2) {
                self.new_token(kind.clone(), &c2);
            } else if let Some(kind) = ONE_SYMBOL_TOKENS.get(&c) {
                self.new_token(kind.clone(), &c.to_string());
            } else {
                panic!("can't tokenize");
            }
        }
    }

    pub fn tokenize(s: String) -> Vec<Token> {
        let mut lexer = Lexer::new();
        lexer._tokenize(s);

        lexer.tokens
    }
}
