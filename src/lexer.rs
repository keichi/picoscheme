use std::iter;
use std::str;

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Boolean(bool),
    Integer(i64),
    String(String),
    OpenParen,
    CloseParen,
    Quote,
    BackQuote,
    Comma,
    Dot
}

pub struct Lexer<'a> {
    iter: iter::Peekable<str::Chars<'a>>
}

impl<'a> iter::Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.skip_space();

        match self.iter.peek().cloned() {
            Some('(') => {
                self.iter.next();
                Some(Token::OpenParen)
            },
            Some(')') => {
                self.iter.next();
                Some(Token::CloseParen)
            },
            Some('\'') => {
                self.iter.next();
                Some(Token::Quote)
            }
            Some('`') => {
                self.iter.next();
                Some(Token::BackQuote)
            },
            Some(',') => {
                self.iter.next();
                Some(Token::Comma)
            },
            Some('.') => {
                self.iter.next();
                Some(Token::Dot)
            },
            Some('"') => self.lex_string(),
            Some('#') => self.lex_boolean(),
            Some(c) if c.is_ascii_digit() => self.lex_integer(),
            Some(c) if self.is_initial(c) => self.lex_identifier(),
            Some(c) if self.is_peculiar_identifier(c) =>
                self.lex_peculiar_identifier(),
            _ => None
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Self {
        Lexer {
            iter: s.chars().peekable()
        }
    }

    fn is_initial(&self, c: char) -> bool {
        c.is_ascii_alphabetic() | self.is_special_initial(c)
    }

    fn is_subsequent(&self, c: char) -> bool {
        self.is_initial(c) | c.is_ascii_digit() | self.is_special_subsequent(c)
    }

    fn is_special_initial(&self, c: char) -> bool {
        "!$%&*/:<=>?^_~".contains(c)
    }

    fn is_special_subsequent(&self, c: char) -> bool {
        "+-.@".contains(c)
    }

    fn is_peculiar_identifier(&self, c: char) -> bool {
        "+-".contains(c)
    }

    fn skip_space(&mut self) {
        loop {
            match self.iter.peek().cloned() {
                Some(c) if c.is_ascii_whitespace() => {
                    self.iter.next();
                },
                _ => break
            }
        }
    }

    fn lex_string(&mut self) -> Option<Token> {
        let mut s = String::new();

        self.iter.next();

        loop {
            match self.iter.peek().cloned() {
                Some('"') => {
                    self.iter.next();
                    break
                },
                Some(c) => {
                    self.iter.next();
                    s.push(c)
                },
                _ => break
            }
        }

        Some(Token::String(s))
    }

    fn lex_boolean(&mut self) -> Option<Token> {
        self.iter.next();

        match self.iter.next() {
            Some('t') => Some(Token::Boolean(true)),
            Some('f') => Some(Token::Boolean(false)),
            _ => None
        }
    }

    fn lex_integer(&mut self) -> Option<Token> {
        let mut s = String::new();

        loop {
            match self.iter.peek().cloned() {
                Some(c) if c.is_ascii_digit() => {
                    s.push(c);
                    self.iter.next();
                },
                _ => break
            }
        }

        s.parse().ok().map(|i| Token::Integer(i))
    }

    fn lex_identifier(&mut self) -> Option<Token> {
        let mut s = String::new();

        match self.iter.next() {
            Some(c) => s.push(c),
            None => return None
        }

        loop {
            match self.iter.peek().cloned() {
                Some(c) if self.is_subsequent(c) => {
                    s.push(c);
                    self.iter.next();
                },
                _ => break
            }
        }

        Some(Token::Identifier(s))
    }

    fn lex_peculiar_identifier(&mut self) -> Option<Token> {
        let c = self.iter.next();

        c.map(|c| Token::Identifier(c.to_string()))
    }
}

#[test]
fn test_lex_identifier() {
    let mut lexer = Lexer::new("foo bar-baz qux?");
    assert_eq!(Token::Identifier("foo".to_string()), lexer.next().unwrap());
    assert_eq!(Token::Identifier("bar-baz".to_string()), lexer.next().unwrap());
    assert_eq!(Token::Identifier("qux?".to_string()), lexer.next().unwrap());
}

#[test]
fn test_lex_boolean() {
    let mut lexer = Lexer::new("#t #f");

    assert_eq!(Token::Boolean(true), lexer.next().unwrap());
    assert_eq!(Token::Boolean(false), lexer.next().unwrap());
}

#[test]
fn test_lex_integer() {
    let mut lexer = Lexer::new("123");
    assert_eq!(Token::Integer(123), lexer.next().unwrap());
}

#[test]
fn test_lex_string() {
    let mut lexer = Lexer::new("\"test\"");
    assert_eq!(Token::String("test".to_string()), lexer.next().unwrap());
}

#[test]
fn test_lex_miscs() {
    let mut lexer = Lexer::new("()'`,.");

    assert_eq!(Token::OpenParen, lexer.next().unwrap());
    assert_eq!(Token::CloseParen, lexer.next().unwrap());
    assert_eq!(Token::Quote, lexer.next().unwrap());
    assert_eq!(Token::BackQuote, lexer.next().unwrap());
    assert_eq!(Token::Comma, lexer.next().unwrap());
    assert_eq!(Token::Dot, lexer.next().unwrap());
}
