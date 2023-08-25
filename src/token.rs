#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Reserved,
    Number(i32),
    EndOfFile,
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    str: String,
}

impl Token {
    pub fn new(kind: TokenKind, str: String) -> Self {
        Self { kind, str }
    }
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

    pub fn consume(&mut self, op: char) -> bool {
        let token = &self.tokens[self.index];
        let c = token.str.chars().next().unwrap();
        if token.kind != TokenKind::Reserved || c != op {
            return false;
        }
        self.index += 1;
        true
    }

    pub fn expect(&mut self, op: char) {
        let token = &self.tokens[self.index];
        let c = token.str.chars().next().unwrap();
        if token.kind != TokenKind::Reserved || c != op {
            eprintln!("Was not {} as expected", op);
        }
        self.index += 1;
    }

    pub fn expect_number(&mut self) -> i32 {
        let token = &self.tokens[self.index];
        self.index += 1;
        match token.kind {
            TokenKind::Number(n) => n,
            _ => panic!("Not a number"),
        }
    }

    pub fn at_eof(&self) -> bool {
        let token = &self.tokens[self.index];
        token.kind == TokenKind::EndOfFile
    }

    fn new_token(&mut self, kind: TokenKind, str: String) {
        self.index += str.len();
        let token = Token::new(kind, str);
        self.tokens.push(token);
    }

    fn parse_number(&mut self, chars: &[char]) -> (i32, usize) {
        let n: String = chars.iter().take_while(|c| c.is_numeric()).collect();
        (n.parse().unwrap(), self.index + n.len())
    }

    pub fn get_current_token_str(&self) -> String {
        self.tokens[self.index].str.clone()
    }

    fn _tokenize(&mut self, s: String) {
        let chars: Vec<_> = s.chars().collect();
        self.index = 0;

        while self.index < chars.len() {
            match chars[self.index] {
                ' ' => self.index += 1,
                '+' => self.new_token(TokenKind::Reserved, "+".to_string()),
                '-' => self.new_token(TokenKind::Reserved, "-".to_string()),
                c if c.is_numeric() => {
                    let start = self.index;
                    let (n, end) = self.parse_number(&chars[start..]);
                    self.new_token(TokenKind::Number(n), chars[start..end].iter().collect());
                }
                _ => panic!("can't tokenize"),
            }
        }

        self.new_token(TokenKind::EndOfFile, "".to_string());
    }

    pub fn tokenize(s: String) -> Self {
        let mut tokens = Tokens::new();
        tokens._tokenize(s);
        tokens.index = 0;
        tokens
    }
}
