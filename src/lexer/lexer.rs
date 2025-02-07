use std::iter;

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
        if let Some(t) = self.tokens.last_mut() {
            if let TokenKind::String(s1) = &mut t.kind {
                if let TokenKind::String(s2) = kind {
                    s1.extend_from_slice(&s2);
                    self.index += len;
                    return;
                }
            }
        }

        self.tokens.push(Token { kind });
        self.index += len;
    }

    fn parse_integer_suffix(&mut self, chars: &[char]) {
        let suffixes = &[
            "ull", "uLL", "Ull", "ULL", "llu", "llU", "LLu", "LLU", "ul", "uL", "Ul", "UL", "lu",
            "lU", "Lu", "LU", "ll", "LL", "u", "U", "l", "L",
        ];
        for suffix in suffixes {
            if chars.starts_with(&suffix.chars().collect::<Vec<_>>()) {
                self.index += suffix.len();
                return;
            }
        }
    }

    fn parse_number(&mut self, chars: &[char]) -> Result<(), Box<dyn std::error::Error>> {
        let (value, len) = match (chars[0], chars[1]) {
            ('0', 'x') | ('0', 'X') => {
                let s = chars
                    .iter()
                    .skip(2)
                    .take_while(|c| c.is_ascii_hexdigit())
                    .collect::<String>();
                (u64::from_str_radix(&s, 16)?, 2 + s.len())
            }
            ('0', n) if n.is_digit(8) => {
                let s = chars
                    .iter()
                    .skip(1)
                    .take_while(|c| c.is_digit(8))
                    .collect::<String>();
                (u64::from_str_radix(&s, 8)?, 1 + s.len())
            }
            _ => {
                let s: String = chars.iter().take_while(|c| c.is_ascii_digit()).collect();
                (s.parse()?, s.len())
            }
        };
        self.new_token(TokenKind::Num(value as i64), len);
        self.parse_integer_suffix(&chars[len..]);
        Ok(())
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

    fn parse_char(&mut self, chars: &[char]) -> (usize, u8) {
        let c = chars[0];
        if c != '\\' {
            return (1, c as u8);
        }

        let b = match chars[1] {
            '\'' => 39,
            '"' => 34,
            '?' => 63,
            '\\' => 92,
            'a' => 7,
            'b' => 8,
            'f' => 12,
            'n' => 10,
            'r' => 13,
            't' => 9,
            'v' => 1,
            '0' => 0, // remove this and tokenize octal numbers
            _ => todo!("Cant tokenize escape sequence {:?}", &chars[0..4]),
        };

        (2, b)
    }

    fn parse_multichar(&mut self, chars: &[char]) {
        let mut bytes = vec![];
        let mut index = 0;
        while chars[index] != '\'' {
            if chars[index] == '\n' {
                panic!("Illegal newline inside char literal");
            }
            let (i, b) = self.parse_char(&chars[index..]);
            index += i;
            bytes.push(b);
        }

        let len = bytes.len().min(4);
        let bytes = iter::repeat_n(0, 4 - len)
            .chain(bytes.into_iter().take(len))
            .collect::<Vec<_>>();
        let n = u32::from_be_bytes(bytes.try_into().unwrap());
        self.new_token(TokenKind::Num(n as i64), index);
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
            } else if c.is_ascii_digit() {
                self.parse_number(&chars[self.index..])
                    .unwrap_or_else(|err| {
                        panic!(
                            "can't parse {:?}: {}",
                            chars[self.index..].iter().take(10).collect::<String>(),
                            err
                        )
                    })
            } else if c.is_ascii_alphabetic() || c == '_' {
                self.parse_identifier(&chars[self.index..]);
            } else if let Some(kind) = THREE_SYMBOLS_TOKENS.get(&c3) {
                self.new_token(kind.clone(), 3);
            } else if let Some(kind) = TWO_SYMBOLS_TOKENS.get(&c2) {
                self.new_token(kind.clone(), 2);
            } else if let Some(kind) = ONE_SYMBOL_TOKENS.get(&c) {
                self.new_token(kind.clone(), 1);
            } else if c == '\'' {
                self.index += 1;
                self.parse_multichar(&chars[self.index..]);
                self.index += 1;
            } else if c == '"' {
                self.index += 1;
                self.parse_string(&chars[self.index..]);
                self.index += 1;
            } else {
                panic!("can't tokenize {:?} at index {}", c, self.index);
            }
        }
    }

    pub fn tokenize(s: &str) -> Vec<Token> {
        let mut lexer = Lexer::new();
        lexer._tokenize(s);

        lexer.tokens
    }
}
