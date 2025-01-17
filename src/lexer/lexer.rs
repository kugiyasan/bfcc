use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use super::{
    token::{KEYWORDS, ONE_SYMBOL_TOKENS, THREE_SYMBOLS_TOKENS, TWO_SYMBOLS_TOKENS},
    TokenKind,
};

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    tokens: Vec<Token>,
    index: usize,
    defines: &'a mut HashMap<String, Vec<Token>>,
}

fn find_include_file(filename: &str) -> Option<PathBuf> {
    let paths = [
        // "/usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/include/",
        // "/usr/local/include/",
        // "/usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/include-fixed/",
        "./",
        "/usr/include/",
    ];

    let filename = filename.trim_matches(['<', '>', '"']);

    for path in paths {
        let mut p = PathBuf::from(path);
        p.push(filename);

        if let Ok(true) = fs::exists(&p) {
            return Some(p);
        }
    }
    None
}

impl<'a> Lexer<'a> {
    fn new(defines: &'a mut HashMap<String, Vec<Token>>) -> Self {
        Self {
            tokens: vec![],
            index: 0,
            defines,
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

        if let Some(tokens) = self.defines.get(&s) {
            self.tokens.extend_from_slice(tokens);
            self.index += s.len();
        } else if let Some(kind) = KEYWORDS.get(&s) {
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

    fn parse_macro(&mut self, chars: &[char]) {
        let line: String = chars[self.index..]
            .iter()
            .take_while(|&&c| c != '\n')
            .collect();
        self.index += line.len();
        let mut tokens = line.split_whitespace();
        let cmd = tokens.next().unwrap();

        match cmd {
            "define" => {
                let ident = tokens.next().unwrap();
                let t = Self::tokenize(&tokens.collect::<String>(), self.defines);
                self.defines.insert(ident.to_string(), t);
            }
            "undef" => {
                let ident = tokens.next().unwrap();
                self.defines.remove(ident);
            }
            "include" => {
                let filename = tokens.next().unwrap();
                let Some(content) = find_include_file(filename) else {
                    panic!("Couldn't find the include file: {}", filename);
                };
                self.tokens
                    .extend(Self::tokenize_from_file(&content, self.defines));
            }
            "if" => todo!(),
            "ifdef" => {
                let ident = tokens.next().unwrap();
                if !self.defines.contains_key(ident) {
                    let endif = "\n#endif".chars().collect::<Vec<_>>();
                    while chars[self.index..self.index + endif.len()] != endif {
                        self.index += 1;
                    }
                    self.index += endif.len();
                }
            }
            "ifndef" => {
                let ident = tokens.next().unwrap();
                if self.defines.contains_key(ident) {
                    let endif = "\n#endif".chars().collect::<Vec<_>>();
                    while chars[self.index..self.index + endif.len()] != endif {
                        self.index += 1;
                    }
                    self.index += endif.len();
                }
            }
            "elif" => todo!(),
            "else" => todo!(),
            "endif" => (),

            "line" => todo!(),
            "error" => panic!("{}", line),
            "pragma" => todo!(),
            _ => panic!("Unknown control line: {}", line),
        }
    }

    fn tokenize_next(&mut self, chars: &[char]) {
        let c = chars[self.index];
        let c2 = chars[self.index..].iter().take(2).collect::<String>();
        let c3 = chars[self.index..].iter().take(3).collect::<String>();

        if self.index == 0 && c == '#' {
            self.index += 1;
            self.parse_macro(chars);
        } else if c2 == "\n#" {
            self.index += 2;
            self.parse_macro(chars);
        } else if c == ' ' || c == '\n' || c == '\t' {
            self.index += 1;
        } else if c2 == "//" {
            self.index += 2;
            while chars[self.index] != '\n' {
                self.index += 1;
            }
        } else if c2 == "/*" {
            self.index += 2;
            while chars[self.index] != '*' || chars[self.index + 1] != '/' {
                self.index += 1;
            }
            self.index += 2;
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
            let ctx_size = 50;
            let before = chars[self.index - ctx_size..self.index]
                .iter()
                .collect::<String>();
            let after = chars[self.index..self.index + ctx_size]
                .iter()
                .collect::<String>();
            panic!(
                "can't tokenize {:?} at index {}: \nbefore: {:?}\nafter: {:?}",
                c, self.index, before, after
            );
        }
    }

    pub fn tokenize<'b>(s: &str, defines: &'b mut HashMap<String, Vec<Token>>) -> Vec<Token>
    where
        'a: 'b,
    {
        let s = s.replace("\\\n", "");
        let chars = s.chars().collect::<Vec<_>>();

        let mut lexer = Lexer::new(defines);
        while lexer.index < chars.len() {
            lexer.tokenize_next(&chars);
        }

        lexer.tokens
    }

    pub fn tokenize_from_file<'b, P>(
        filename: P,
        defines: &'b mut HashMap<String, Vec<Token>>,
    ) -> Vec<Token>
    where
        P: AsRef<Path>,
        'a: 'b,
    {
        dbg!(filename.as_ref());
        let s = fs::read_to_string(filename).unwrap();
        Self::tokenize(&s, defines)
    }
}
