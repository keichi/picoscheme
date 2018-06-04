use std::fmt;
use std::iter::{Iterator,Peekable};
use std::vec::Vec;

use lexer::Token;

#[derive(Debug,PartialEq)]
pub enum Value {
    List(Vec<Value>),
    DottedList(Vec<Value>),
    Boolean(bool),
    Symbol(String),
    Integer(i64),
    String(String)
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::List(vs) => {
                try!(write!(f, "("));
                for (i, v) in vs.iter().enumerate() {
                    if i >= 1 {
                        try!(write!(f, " "));
                    }
                    try!(write!(f, "{}", v));
                }
                try!(write!(f, ")"));
                return Ok(());
            },
            Value::DottedList(vs) => {
                try!(write!(f, "("));
                for (i, v) in vs.iter().enumerate() {
                    if i == vs.len() - 1 {
                        try!(write!(f, " . "));
                    } else if i >= 1 {
                        try!(write!(f, " "));
                    }
                    try!(write!(f, "{}", v));
                }
                try!(write!(f, ")"));
                return Ok(());
            },
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::Integer(i) => write!(f, "{}", i),
            Value::String(s) => write!(f, "\"{}\"", s)
        }
    }
}

// <datum> -> <boolean>
//          | <number>
//          | <string>
//          | <symbol>
//          | ( <datum>* )
//          | ( <datum>+ . <datum> )
//          | ' <datum>
//          | ` <datum>
//          | , <datum>
//          | ,@ <datum>

pub struct Parser<T: Iterator<Item=Token>> {
    tokens: Peekable<T>
}

impl<T: Iterator<Item=Token>> Parser<T> {
    pub fn new(lexer: T) -> Self {
        Parser {
            tokens: lexer.peekable()
        }
    }

    fn parse_list(&mut self) -> Result<Value, String> {
        self.tokens.next();

        let mut items = Vec::new();

        loop {
            match self.tokens.peek().cloned() {
                Some(Token::CloseParen) => {
                    self.tokens.next();
                    return Ok(Value::List(items));
                },
                Some(Token::Dot) => {
                    self.tokens.next();
                    items.push(self.parse()?);
                    return Ok(Value::DottedList(items));
                },
                Some(_) => {
                    items.push(self.parse()?);
                },
                None => return Err("Unexepcted end of list".to_owned())
            }
        }
    }

    pub fn parse(&mut self) -> Result<Value, String> {
        let token = self.tokens.peek().cloned()
                        .ok_or("Unexected end of input")?;

        match token {
            Token::Boolean(b) => {
                self.tokens.next();
                Ok(Value::Boolean(b))
            },
            Token::Integer(i) => {
                self.tokens.next();
                Ok(Value::Integer(i))
            },
            Token::String(s) => {
                self.tokens.next();
                Ok(Value::String(s))
            },
            Token::Identifier(s) => {
                self.tokens.next();
                Ok(Value::Symbol(s))
            },
            Token::OpenParen => self.parse_list(),
            Token::Quote => {
                self.tokens.next();
                self.parse().map(|v|
                    Value::List(
                        vec![Value::Symbol("quote".to_owned()), v]
                    )
                )
            },
            Token::BackQuote => {
                self.tokens.next();
                self.parse().map(|v|
                    Value::List(
                        vec![Value::Symbol("quasiquote".to_owned()), v]
                    )
                )
            },
            Token::Comma => {
                self.tokens.next();
                self.parse().map(|v|
                    Value::List(
                        vec![Value::Symbol("unquote".to_owned()), v]
                    )
                )
            },
            Token::CommaAt => {
                self.tokens.next();
                self.parse().map(|v|
                    Value::List(
                        vec![Value::Symbol("unquote-splicing".to_owned()), v]
                    )
                )
            }
            _ => Err("Unexepcted token".to_owned())
        }
    }
}


#[test]
fn test_parse_literal() {
    let tokens = vec![Token::Boolean(true)];
    let mut parser = Parser::new(tokens.into_iter());

    assert_eq!(Value::Boolean(true), parser.parse().unwrap());

    let tokens = vec![Token::Identifier("a".to_owned())];
    let mut parser = Parser::new(tokens.into_iter());

    assert_eq!(Value::Symbol("a".to_owned()), parser.parse().unwrap());

    let tokens = vec![Token::Integer(123)];
    let mut parser = Parser::new(tokens.into_iter());

    assert_eq!(Value::Integer(123), parser.parse().unwrap());

    let tokens = vec![Token::String("b".to_owned())];
    let mut parser = Parser::new(tokens.into_iter());

    assert_eq!(Value::String("b".to_owned()), parser.parse().unwrap());
}

#[test]
fn test_list() {
    let tokens = vec![
        Token::OpenParen,
        Token::String("a".to_owned()),
        Token::Integer(123),
        Token::Identifier("b".to_owned()),
        Token::CloseParen
    ];
    let mut parser = Parser::new(tokens.into_iter());

    assert_eq!(
        Value::List(vec![
            Value::String("a".to_owned()),
            Value::Integer(123),
            Value::Symbol("b".to_owned())
        ]),
        parser.parse().unwrap()
    );
}

#[test]
fn test_nested_list() {
    let tokens = vec![
        Token::OpenParen,
        Token::String("a".to_owned()),
        Token::Integer(123),
        Token::OpenParen,
        Token::Identifier("b".to_owned()),
        Token::Integer(456),
        Token::CloseParen,
        Token::CloseParen
    ];
    let mut parser = Parser::new(tokens.into_iter());

    assert_eq!(
        Value::List(vec![
            Value::String("a".to_owned()),
            Value::Integer(123),
            Value::List(vec![
                Value::Symbol("b".to_owned()),
                Value::Integer(456),
            ]),
        ]),
        parser.parse().unwrap()
    );
}

#[test]
fn test_dotted_list() {
    let tokens = vec![
        Token::OpenParen,
        Token::Integer(123),
        Token::Dot,
        Token::Integer(456),
        Token::CloseParen
    ];
    let mut parser = Parser::new(tokens.into_iter());

    assert_eq!(
        Value::DottedList(vec![
            Value::Integer(123),
            Value::Integer(456)
        ]),
        parser.parse().unwrap()
    );
}
