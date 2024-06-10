use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: Kind,
    pub line: usize,
    pub literal: String,
}

impl Token {
    pub fn new(kind: Kind, line: usize) -> Self {
        let literal = kind.literal();
        Self {
            kind,
            line,
            literal,
        }
    }

    pub fn is_binary_op(&self) -> bool {
        match self.kind {
            Kind::Minus
            | Kind::Plus
            | Kind::Slash
            | Kind::Asterisk
            | Kind::Lsh
            | Kind::Rsh
            | Kind::And
            | Kind::Or
            | Kind::Eq
            | Kind::NotEq
            | Kind::Lt
            | Kind::Lte
            | Kind::Gt
            | Kind::Gte
            | Kind::Assign
            | Kind::MinusAssign
            | Kind::DivAssign
            | Kind::MulAssign
            | Kind::PlusAssign => true,
            _ => false,
        }
    }

    pub fn is_assign_op(&self) -> bool {
        match self.kind {
            Kind::Assign
            | Kind::MinusAssign
            | Kind::DivAssign
            | Kind::MulAssign
            | Kind::PlusAssign => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool,
    Fn,
    Int,
    Str,
    UnKnown,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    NotEq,

    Plus,
    Minus,
    Asterisk,
    Slash,
    Mod,
    And,
    Or,
    Lsh,
    Rsh,

    Inc,
    Dec,

    Assign,
    PlusAssign,
    MinusAssign,
    MulAssign,
    DivAssign,

    Semi,
    Colon,
    Lbrace,
    Rbrace,
    Lparen,
    Rparen,
    Lbrack,
    Rbrack,

    Comma,
    Dot,
    Bang,

    Fn,
    Let,
    If,
    Else,
    For,
    While,

    True,
    False,

    Ident(String),
    Number(f64),
    String(String),
    Hex(String),
    Char(char),
    Eof,
    Invalid,
    Import,
    Return,
}

impl Kind {
    pub fn literal(&self) -> String {
        match self {
            Kind::Number(s) => s.to_string(),
            Kind::Let => "let".to_string(),
            Kind::For => "for".to_string(),
            Kind::If => "if".to_string(),
            Kind::Else => "else".to_string(),
            Kind::Fn => "fn".to_string(),
            Kind::Lbrace => '{'.to_string(),
            Kind::Rbrace => '}'.to_string(),
            Kind::Lbrack => '['.to_string(),
            Kind::Rbrack => ']'.to_string(),
            Kind::Lparen => '('.to_string(),
            Kind::Rparen => ')'.to_string(),
            Kind::Semi => ';'.to_string(),
            Kind::Colon => ':'.to_string(),
            Kind::Comma => ','.to_string(),
            Kind::Bang => '!'.to_string(),
            Kind::Eof => "Eof".to_string(),
            Kind::Mod => '%'.to_string(),
            Kind::Dot => '.'.to_string(),
            Kind::Asterisk => '*'.to_string(),
            Kind::MulAssign => "*=".to_string(),
            Kind::Slash => '/'.to_string(),
            Kind::DivAssign => "/=".to_string(),
            Kind::Plus => '+'.to_string(),
            Kind::PlusAssign => "+=".to_string(),
            Kind::Minus => '-'.to_string(),
            Kind::MinusAssign => "-=".to_string(),
            Kind::Assign => '='.to_string(),
            Kind::Or => '|'.to_string(),
            Kind::And => '&'.to_string(),
            Kind::Lsh => "<<".to_string(),
            Kind::Rsh => ">>".to_string(),
            Kind::Lt => '<'.to_string(),
            Kind::Lte => "<=".to_string(),
            Kind::Gt => '>'.to_string(),
            Kind::Gte => ">=".to_string(),
            Kind::Eq => "==".to_string(),
            Kind::NotEq => "!=".to_string(),
            Kind::String(s) => s.into(),
            Kind::Hex(s) => s.into(),
            Kind::Char(ch) => ch.to_string(),
            Kind::Ident(l) => l.into(),
            Kind::True => "true".to_string(),
            Kind::False => "false".to_string(),
            Kind::Return => "return".to_string(),
            s => format!("Weird - {:?}", s),
        }
    }
}

pub fn get_ident_or_keyword(literal: String) -> Kind {
    match literal.as_str() {
        "let" => Kind::Let,
        "for" => Kind::For,
        "while" => Kind::While,
        "fn" => Kind::Fn,
        "true" => Kind::True,
        "false" => Kind::False,
        "return" => Kind::Return,
        "import" => Kind::Import,
        "if" => Kind::If,
        "else" => Kind::Else,
        _ => Kind::Ident(literal),
    }
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{{Literal: {}}}", self.literal())
    }
}
