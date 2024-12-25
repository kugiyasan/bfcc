use phf::phf_map;

static KEYWORDS: phf::Map<&str, TokenKind> = phf_map! {
    "auto" => TokenKind::Auto,
    "break" => TokenKind::Break,
    "case" => TokenKind::Case,
    "char" => TokenKind::Char,
    "const" => TokenKind::Const,
    "continue" => TokenKind::Continue,
    "default" => TokenKind::Default,
    "do" => TokenKind::Do,
    "double" => TokenKind::Double,
    "else" => TokenKind::Else,
    "enum" => TokenKind::Enum,

    "extern" => TokenKind::Extern,
    "float" => TokenKind::Float,
    "for" => TokenKind::For,
    "goto" => TokenKind::Goto,
    "if" => TokenKind::If,
    "inline" => TokenKind::Inline,
    "int" => TokenKind::Int,
    "long" => TokenKind::Long,
    "register" => TokenKind::Register,
    "restrict" => TokenKind::Restrict,
    "return" => TokenKind::Return,
    "short" => TokenKind::Short,
    "signed" => TokenKind::Signed,

    "sizeof" => TokenKind::Sizeof,
    "static" => TokenKind::Static,
    "struct" => TokenKind::Struct,
    "switch" => TokenKind::Switch,
    "typedef" => TokenKind::Typedef,
    "union" => TokenKind::Union,
    "unsigned" => TokenKind::Unsigned,
    "void" => TokenKind::Void,
    "volatile" => TokenKind::Volatile,
    "while" => TokenKind::While,
    "_Complex" => TokenKind::Complex,
    "_Imaginary" => TokenKind::Imaginary,
};

static ONE_SYMBOL_TOKENS: phf::Map<char, TokenKind> = phf_map! {
    '?' => TokenKind::Question,
    '=' => TokenKind::Equal,
    '.' => TokenKind::Dot,
    ',' => TokenKind::Comma,
    ':' => TokenKind::Colon,
    ';' => TokenKind::SemiColon,

    '(' => TokenKind::OpenParen,
    ')' => TokenKind::CloseParen,
    '{' => TokenKind::OpenCurlyBrace,
    '}' => TokenKind::CloseCurlyBrace,
    '[' => TokenKind::OpenSquareBrace,
    ']' => TokenKind::CloseSquareBrace,

    '<' => TokenKind::LessThan,
    '>' => TokenKind::GreaterThan,

    '&' => TokenKind::Ampersand,
    '|' => TokenKind::Pipe,
    '^' => TokenKind::Hat,
    '~' => TokenKind::Tilde,
    '!' => TokenKind::Not,

    '+' => TokenKind::Plus,
    '-' => TokenKind::Minus,
    '*' => TokenKind::Star,
    '/' => TokenKind::Slash,
    '%' => TokenKind::Percent,
};

static TWO_SYMBOLS_TOKENS: phf::Map<&str, TokenKind> = phf_map! {
    "*=" => TokenKind::StarEqual,
    "/=" => TokenKind::SlashEqual,
    "%=" => TokenKind::PercentEqual,
    "+=" => TokenKind::PlusEqual,
    "-=" => TokenKind::MinusEqual,
    "&=" => TokenKind::AmpersandEqual,
    "^=" => TokenKind::HatEqual,
    "|=" => TokenKind::PipeEqual,

    "<=" => TokenKind::LessEqual,
    ">=" => TokenKind::GreaterEqual,
    "==" => TokenKind::DoubleEqual,
    "!=" => TokenKind::NotEqual,

    "||" => TokenKind::PipePipe,
    "&&" => TokenKind::AmpersandAmpersand,
    "<<" => TokenKind::LeftShift,
    ">>" => TokenKind::RightShift,
    "++" => TokenKind::PlusPlus,
    "--" => TokenKind::MinusMinus,
    "->" => TokenKind::Arrow,
};

static THREE_SYMBOLS_TOKENS: phf::Map<&str, TokenKind> = phf_map! {
    ">>=" => TokenKind::RightShiftAssign,
    "<<=" => TokenKind::LeftShiftAssign,
    "..." => TokenKind::ThreeDots,
};

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,

    Extern,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Restrict,
    Return,
    Short,
    Signed,

    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
    Complex,
    Imaginary,

    Question,
    Equal,
    Dot,
    Comma,
    Colon,
    SemiColon,

    OpenParen,
    CloseParen,
    OpenCurlyBrace,
    CloseCurlyBrace,
    OpenSquareBrace,
    CloseSquareBrace,

    LessThan,
    GreaterThan,

    Ampersand,
    Pipe,
    Hat,
    Tilde,
    Not,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    StarEqual,
    SlashEqual,
    PercentEqual,
    PlusEqual,
    MinusEqual,
    AmpersandEqual,
    HatEqual,
    PipeEqual,

    LessEqual,
    GreaterEqual,
    DoubleEqual,
    NotEqual,

    PipePipe,
    AmpersandAmpersand,
    LeftShift,
    RightShift,
    PlusPlus,
    MinusMinus,
    Arrow,

    RightShiftAssign,
    LeftShiftAssign,
    ThreeDots,

    Num(i32),
    Ident(String),
}

#[derive(Clone, Debug)]
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
        let mut tokens = Tokens::new();
        tokens._tokenize(s);

        tokens.tokens
    }
}
