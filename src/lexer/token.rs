use phf::phf_map;

pub(super) static KEYWORDS: phf::Map<&str, TokenKind> = phf_map! {
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

pub(super) static ONE_SYMBOL_TOKENS: phf::Map<char, TokenKind> = phf_map! {
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

pub(super) static TWO_SYMBOLS_TOKENS: phf::Map<&str, TokenKind> = phf_map! {
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

pub(super) static THREE_SYMBOLS_TOKENS: phf::Map<&str, TokenKind> = phf_map! {
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