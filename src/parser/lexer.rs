use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone)]
pub struct Token {
    //pub start: u32,
    //pub end: u32,
    pub ty: TokenType,
    /// is this token on a different line than the previous one
    pub new_line: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    Ident(String),
    Equals,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Number(u64),
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Arrow,
    Eof,
}

pub struct Lexer<'a> {
    pub chars: Peekable<Chars<'a>>,
    pub peeked: Option<Token>,
}
impl<'a> Lexer<'a> {
    pub fn next(&mut self) -> Token {
        if let Some(tok) = self.peeked.take() {
            return tok;
        }
        let mut new_line = false;
        loop {
            let ty = match self.chars.next() {
                Some(' ' | '\t' | '\r') => continue,
                Some('\n') => {
                    new_line = true;
                    continue;
                }
                Some('=') => TokenType::Equals,
                Some('+') => TokenType::Plus,
                Some('-') => {
                    if self.chars.next_if_eq(&'>').is_some() {
                        TokenType::Arrow
                    } else {
                        TokenType::Minus
                    }
                }
                Some('*') => TokenType::Star,
                Some('/') => TokenType::Slash,
                Some('%') => TokenType::Percent,
                Some('(') => TokenType::LParen,
                Some(')') => TokenType::RParen,
                Some('{') => TokenType::LBrace,
                Some('}') => TokenType::RBrace,
                Some('[') => TokenType::LBracket,
                Some(']') => TokenType::RBracket,
                Some(c @ ('a'..='z' | 'A'..='Z')) => {
                    let mut s = String::new();
                    s.push(c);
                    while self
                        .chars
                        .peek()
                        .is_some_and(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9'))
                    {
                        s.push(self.chars.next().unwrap());
                    }
                    TokenType::Ident(s)
                }
                Some(c @ '0'..='9') => {
                    let mut s = String::new();
                    s.push(c);
                    while self.chars.peek().is_some_and(|c| matches!(c, '0'..='9')) {
                        s.push(self.chars.next().unwrap());
                    }
                    TokenType::Number(s.parse().unwrap())
                }
                Some(c) => panic!("unexpected token {c}"),
                None => TokenType::Eof,
            };
            return Token { ty, new_line };
        }
    }

    pub fn peek(&mut self) -> &Token {
        if self.peeked.is_none() {
            let tok = self.next();
            self.peeked = Some(tok);
        }
        self.peeked.as_ref().unwrap()
    }
}
