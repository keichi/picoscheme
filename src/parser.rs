use std::iter;

use lexer;

#[derive(Clone, Debug, PartialEq)]
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

pub struct Parser<T: iter::Iterator<Item=lexer::Token>> {
    tokens: iter::Peekable<T>
}

impl<T: iter::Iterator<Item=lexer::Token>> Parser<T> {
    pub fn new(lexer: T) -> Self {
        Parser {
            tokens: lexer.peekable()
        }
    }

    fn parse_list(&mut self) -> Result<Value, &str> {
        match self.tokens.peek().cloned() {
            Some(lexer::Token::CloseParen) => {
                self.tokens.next();
                Ok(Value::Nil)
            },
            _ => {
                let car = Box::new(self.parse().unwrap());
                let cdr = Box::new(self.parse_list().unwrap());

                Ok(Value::Pair(car, cdr))
            }
        }
    }

    pub fn parse(&mut self) -> Result<Value, &str> {
        let token = self.tokens.peek().cloned();

        if token.is_none() {
            return Err("Unexpected end of input")
        }

        match token.unwrap() {
            lexer::Token::Boolean(b) => {
                self.tokens.next();
                Ok(Value::Boolean(b))
            },
            lexer::Token::Integer(i) => {
                self.tokens.next();
                Ok(Value::Integer(i))
            },
            lexer::Token::String(s) => {
                self.tokens.next();
                Ok(Value::String(s))
            },
            lexer::Token::Identifier(s) => {
                self.tokens.next();
                Ok(Value::Symbol(s))
            },
            lexer::Token::OpenParen => {
                self.tokens.next();
                self.parse_list()
            },
            lexer::Token::Quote => {
                self.tokens.next();
                self.parse().map(|v|
                    Value::Pair(
                        Box::new(Value::Symbol("quote".to_owned())),
                        Box::new(v)
                    )
                )
            },
            lexer::Token::BackQuote => {
                self.tokens.next();
                self.parse().map(|v|
                    Value::Pair(
                        Box::new(Value::Symbol("quasiquote".to_owned())),
                        Box::new(v)
                    )
                )
            },
            lexer::Token::Comma => {
                self.tokens.next();
                self.parse().map(|v|
                    Value::Pair(
                        Box::new(Value::Symbol("unquote".to_owned())),
                        Box::new(v)
                    )
                )
            },
            lexer::Token::CommaAt => {
                self.tokens.next();
                self.parse().map(|v|
                    Value::Pair(
                        Box::new(Value::Symbol("unqote-splicing".to_owned())),
                        Box::new(v)
                    )
                )
            }
            _ => Err("Unexepcted token")
        }
    }
}


#[test]
fn test_parse_literal() {
    let tokens = vec![lexer::Token::Integer(123)];
    let mut parser = Parser::new(tokens.into_iter());
    assert_eq!(Value::Integer(123), parser.parse().unwrap());
}
