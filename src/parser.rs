use std::iter::{Iterator,Peekable};

use lexer::Token;

#[derive(Debug,PartialEq)]
pub enum Value {
    Pair(Box<Value>, Box<Value>),
    Nil,
    Boolean(bool),
    Symbol(String),
    Integer(i64),
    String(String)
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
        match self.tokens.peek().cloned() {
            Some(Token::CloseParen) => {
                self.tokens.next();
                Ok(Value::Nil)
            },
            Some(Token::Dot) => {
                self.tokens.next();
                Ok(self.parse()?)
            },
            Some(_) => {
                let car = Box::new(self.parse()?);
                let cdr = Box::new(self.parse_list()?);
                Ok(Value::Pair(car, cdr))
            },
            None => Err("Unexepcted end of list".to_owned())
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
            Token::OpenParen => {
                self.tokens.next();
                self.parse_list()
            },
            Token::Quote => {
                self.tokens.next();
                self.parse().map(|v|
                    Value::Pair(
                        Box::new(Value::Symbol("quote".to_owned())),
                        Box::new(v)
                    )
                )
            },
            Token::BackQuote => {
                self.tokens.next();
                self.parse().map(|v|
                    Value::Pair(
                        Box::new(Value::Symbol("quasiquote".to_owned())),
                        Box::new(v)
                    )
                )
            },
            Token::Comma => {
                self.tokens.next();
                self.parse().map(|v|
                    Value::Pair(
                        Box::new(Value::Symbol("unquote".to_owned())),
                        Box::new(v)
                    )
                )
            },
            Token::CommaAt => {
                self.tokens.next();
                self.parse().map(|v|
                    Value::Pair(
                        Box::new(Value::Symbol("unqote-splicing".to_owned())),
                        Box::new(v)
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
fn test_proper_list() {
    let tokens = vec![
        Token::OpenParen,
        Token::String("a".to_owned()),
        Token::Integer(123),
        Token::Identifier("b".to_owned()),
        Token::CloseParen
    ];
    let mut parser = Parser::new(tokens.into_iter());

    assert_eq!(
        Value::Pair(
            Box::new(Value::String("a".to_owned())),
            Box::new(Value::Pair(
                Box::new(Value::Integer(123)),
                Box::new(Value::Pair(
                    Box::new(Value::Symbol("b".to_owned())),
                    Box::new(Value::Nil)
                )),
            ))
        ),
        parser.parse().unwrap()
    );
}

#[test]
fn test_improper_list() {
    let tokens = vec![
        Token::OpenParen,
        Token::Integer(123),
        Token::Dot,
        Token::Integer(456),
        Token::CloseParen
    ];
    let mut parser = Parser::new(tokens.into_iter());

    assert_eq!(
        Value::Pair(Box::new(Value::Integer(123)),
                    Box::new(Value::Integer(456))),
        parser.parse().unwrap()
    );
}
