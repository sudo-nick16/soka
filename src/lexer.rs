use core::f64;
use std::{fs, io::Error};

use crate::token::{get_ident_or_keyword, Kind, Token};

pub struct Lexer {
    src: Vec<char>,
    index: usize,
    line: usize,
}

pub fn strip_slashes(s: String) -> String {
    let mut n = String::new();

    let mut chars = s.chars();

    while let Some(c) = chars.next() {
        n.push(match c {
            '\\' => chars.next().expect("Please provide a valid string"),
            c => c,
        });
    }
    n
}

impl Lexer {
    pub fn new(filepath: String) -> Result<Self, Error> {
        let buf = fs::read_to_string(filepath)?;
        let src = buf.chars().collect();
        Ok(Self {
            src,
            line: 1,
            index: 0,
        })
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        while let Some(token) = self.next_token() {
            tokens.push(token)
        }
        tokens
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.read_whitespace();
        self.read_comment();
        if let Some(ch) = self.current() {
            // println!("char: {}", ch);
            let kind: Kind = match ch {
                '%' => Kind::Mod,
                '-' => {
                    if self.peek_is('=') {
                        self.adv();
                        Kind::MinusAssign
                    } else if self.peek_is('-') {
                        self.adv();
                        Kind::Dec
                    } else if self
                        .peek()
                        .expect("Something's very wrong, I can feel it")
                        .is_digit(10)
                    {
                        let num = self.read_number();
                        if let Some(n) = num {
                            Kind::Number(n)
                        } else {
                            assert!(false);
                            Kind::Invalid
                        }
                    } else {
                        Kind::Minus
                    }
                }
                '+' => {
                    if self.peek_is('=') {
                        self.adv();
                        Kind::PlusAssign
                    } else if self.peek_is('+') {
                        self.adv();
                        Kind::Inc
                    } else {
                        Kind::Plus
                    }
                }
                '*' => {
                    if self.peek_is('=') {
                        self.adv();
                        Kind::MulAssign
                    } else {
                        Kind::Asterisk
                    }
                }
                '/' => {
                    if self.peek_is('=') {
                        self.adv();
                        Kind::DivAssign
                    } else {
                        Kind::Slash
                    }
                }
                '<' => {
                    if self.peek_is('=') {
                        self.adv();
                        Kind::Lte
                    } else if self.peek_is('<') {
                        self.adv();
                        Kind::Lsh
                    } else {
                        Kind::Lt
                    }
                }
                '>' => {
                    if self.peek_is('=') {
                        self.adv();
                        Kind::Gte
                    } else if self.peek_is('>') {
                        self.adv();
                        Kind::Rsh
                    } else {
                        Kind::Gt
                    }
                }
                '!' => {
                    if self.peek_is('=') {
                        self.adv();
                        Kind::NotEq
                    } else {
                        Kind::Bang
                    }
                }
                '&' => Kind::And,
                '|' => Kind::Or,
                ';' => Kind::Semi,
                ':' => Kind::Colon,
                ',' => Kind::Comma,
                '.' => Kind::Dot,
                '(' => Kind::Lparen,
                ')' => Kind::Rparen,
                '{' => Kind::Lbrace,
                '}' => Kind::Rbrace,
                '[' => Kind::Lbrack,
                ']' => Kind::Rbrack,
                '=' => {
                    if self.peek_is('=') {
                        self.adv();
                        Kind::Eq
                    } else {
                        Kind::Assign
                    }
                }
                '\'' => match self.peek() {
                    Some(c) => {
                        if c.is_ascii() {
                            let lit = self.read_char();
                            if self.current()?.eq(&'\'') {
                                Kind::Char(lit)
                            } else {
                                Kind::Invalid
                            }
                        } else {
                            Kind::Invalid
                        }
                    }
                    None => Kind::Invalid,
                },
                '"' => match self.peek() {
                    Some(c) => {
                        let literal = self.read_string();
                        if self.current()?.eq(&'"') {
                            Kind::String(literal)
                        } else {
                            Kind::Invalid
                        }
                    }
                    None => Kind::Invalid,
                },
                c => {
                    if c.eq(&'0') && self.peek().expect("Bad code!").eq(&'x') {
                        self.adv();
                        let lit = self.read_hex();
                        println!("c: {}, {}", c, lit);
                        Kind::Hex(lit)
                    } else if c.is_alphabetic() {
                        let literal = self.read_ident();
                        get_ident_or_keyword(literal)
                    } else if c.is_numeric() {
                        let num = self.read_number();
                        if let Some(n) = num {
                            Kind::Number(n)
                        } else {
                            assert!(false);
                            Kind::Invalid
                        }
                    } else {
                        println!("what is this?");
                        Kind::Invalid
                    }
                }
            };
            self.adv();
            Some(Token::new(kind, self.line))
        } else {
            None
        }
    }

    pub fn peek_is(&mut self, c: char) -> bool {
        if let Some(ch) = self.peek() {
            return ch == c;
        }
        false
    }

    pub fn current(&mut self) -> Option<char> {
        if self.index >= self.src.len() {
            return None;
        }
        Some(self.src[self.index])
    }

    pub fn curr_is(&mut self, c: char) -> bool {
        if self.index >= self.src.len() {
            return false;
        }
        self.src[self.index].eq(&c)
    }

    pub fn peek(&self) -> Option<char> {
        if self.index + 1 >= self.src.len() {
            return None;
        }
        Some(self.src[self.index + 1])
    }

    pub fn next(&mut self) -> Option<char> {
        if self.index + 1 >= self.src.len() {
            return None;
        }
        self.index += 1;
        Some(self.src[self.index])
    }

    pub fn adv(&mut self) {
        self.index += 1;
    }

    pub fn step_back(&mut self) {
        self.index -= 1;
    }

    pub fn read_whitespace(&mut self) {
        while let Some(ch) = self.current() {
            if ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r' {
                if ch == '\n' {
                    self.line += 1;
                }
                self.adv();
            } else {
                break;
            }
        }
    }

    pub fn read_comment(&mut self) {
        if self.curr_is('/') && self.peek_is('/') {
            self.adv();
            self.adv();
            while !self.curr_is('\n') {
                self.adv();
            }
            self.adv();
        }
        self.read_whitespace();
        if self.curr_is('/') {
            self.read_comment();
        }
    }

    pub fn read_string(&mut self) -> String {
        let mut s = String::new();
        self.adv();
        while let Some(ch) = self.current() {
            if ch.ne(&'"') {
                s.push(ch);
                self.adv();
            } else {
                break;
            }
        }
        s
    }

    pub fn read_hex(&mut self) -> String {
        let mut s = String::new();
        self.adv();
        while let Some(mut ch) = self.current() {
            ch = ch.to_lowercase().next().expect("Invalid hex literal.");
            if ch.is_digit(10) || (ch <= 'f' && ch >= 'a') {
                s.push(ch);
                self.adv();
            } else {
                break;
            }
        }
        self.step_back();
        s
    }

    pub fn read_char(&mut self) -> char {
        let mut s = String::new();
        self.adv();
        while let Some(ch) = self.current() {
            if ch.is_ascii() && ch.ne(&'\'') {
                s.push(ch);
                self.adv();
            } else {
                break;
            }
        }
        println!("char analysis  = {:?}", s.chars());
        if s.eq("\\n") {
            return '\n';
        }
        if s.eq(" ") {
            return ' ';
        }
        if s.eq("\\r") {
            return '\r';
        }
        if s.eq("\\t") {
            return '\t';
        }
        s.chars().next().expect("Invalid char literal.")
    }

    pub fn read_ident(&mut self) -> String {
        let mut s = String::new();
        let ch = self.current().expect("should not reach");
        s.push(ch);
        while let Some(ch) = self.peek() {
            if ch.is_alphabetic() || ch.eq(&'_') {
                s.push(ch);
                self.adv();
            } else {
                break;
            }
        }
        s
    }

    pub fn read_number(&mut self) -> Option<f64> {
        let mut s = String::new();
        let mut dot_count = 0;
        let mut neg_count = 0;
        while let Some(ch) = self.current() {
            if ch.is_digit(10) || ch == '.' || ch.eq(&'-') {
                s.push(ch);
                self.adv();
                if ch == '.' {
                    dot_count += 1;
                }
                if ch == '-' {
                    neg_count += 1;
                }
            } else {
                break;
            }
        }
        if dot_count > 1 {
            return None;
        }
        if neg_count > 1 {
            return None;
        }
        let f: f64 = match s.parse() {
            Ok(num) => num,
            _ => return None,
        };
        self.step_back();
        Some(f)
    }
}
