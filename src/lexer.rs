use std::iter;
use std::str;

#[derive(Clone, Debug, PartialEq)]
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
    CommaAt,
    Dot
}

pub struct Lexer<'a> {
    iter: iter::Peekable<str::Chars<'a>>
}

impl<'a> iter::Iterator for Lexer<'a> {
    type Item = Result<Token, String>;

    fn next(&mut self) -> Option<Result<Token, String>> {
        self.skip_space();

        match self.iter.peek().cloned() {
            Some('(') => {
                self.iter.next();
                Some(Ok(Token::OpenParen))
            },
            Some(')') => {
                self.iter.next();
                Some(Ok(Token::CloseParen))
            },
            Some('\'') => {
                self.iter.next();
                Some(Ok(Token::Quote))
            }
            Some('`') => {
                self.iter.next();
                Some(Ok(Token::BackQuote))
            },
            Some(',') => {
                self.iter.next();

                match self.iter.peek().cloned() {
                    Some('@') => {
                        self.iter.next();
                        Some(Ok(Token::CommaAt))
                    },
                    _ => Some(Ok(Token::Comma))
                }
            },
            Some('.') => {
                self.iter.next();
                Some(Ok(Token::Dot))
            },
            Some('"') => self.lex_string(),
            Some('#') => self.lex_boolean(),
            Some(c) if c.is_ascii_digit() => self.lex_integer(),
            Some(c) if self.is_initial(c) => self.lex_identifier(),
            Some(c) if self.is_peculiar_identifier(c) =>
                self.lex_peculiar_identifier(),
            Some(c)=> Some(Err(format!("Unexpected character {}", c))),
            None => None
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

    fn lex_string(&mut self) -> Option<Result<Token, String>> {
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

        Some(Ok(Token::String(s)))
    }

    fn lex_boolean(&mut self) -> Option<Result<Token, String>> {
        self.iter.next();

        match self.iter.next() {
            Some('t') => Some(Ok(Token::Boolean(true))),
            Some('f') => Some(Ok(Token::Boolean(false))),
            Some(c) => Some(Err(format!("Unexpected character {}", c))),
            None => None
        }
    }

    fn lex_integer(&mut self) -> Option<Result<Token, String>> {
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

        Some(s.parse().map(|i| Token::Integer(i))
                      .map_err(|e| e.to_string()))
    }

    fn lex_identifier(&mut self) -> Option<Result<Token, String>> {
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

        Some(Ok(Token::Identifier(s)))
    }

    fn lex_peculiar_identifier(&mut self) -> Option<Result<Token, String>> {
        let c = self.iter.next();

        c.map(|c| Ok(Token::Identifier(c.to_string())))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_identifier() {
        let lexer = Lexer::new("foo bar-baz qux?");
        let expected = vec![
            Token::Identifier("foo".to_owned()),
            Token::Identifier("bar-baz".to_owned()),
            Token::Identifier("qux?".to_owned())
        ];

        for (actual, expected) in lexer.zip(expected) {
            assert_eq!(actual.unwrap(), expected);
        }
    }

    #[test]
    fn test_lex_boolean() {
        let lexer = Lexer::new("#t #f");
        let expected = vec![
            Token::Boolean(true),
            Token::Boolean(false)
        ];

        for (actual, expected) in lexer.zip(expected) {
            assert_eq!(actual.unwrap(), expected);
        }
    }

    #[test]
    fn test_lex_integer() {
        let lexer = Lexer::new("123");
        let expected = vec![Token::Integer(123)];

        for (actual, expected) in lexer.zip(expected) {
            assert_eq!(actual.unwrap(), expected);
        }
    }

    #[test]
    fn test_lex_string() {
        let lexer = Lexer::new("\"test\"");
        let expected = vec![Token::String("test".to_owned())];

        for (actual, expected) in lexer.zip(expected) {
            assert_eq!(actual.unwrap(), expected);
        }
    }

    #[test]
    fn test_lex_miscs() {
        let lexer = Lexer::new("()'`,,@.");
        let expected = vec![
            Token::OpenParen,
            Token::CloseParen,
            Token::Quote,
            Token::BackQuote,
            Token::Comma,
            Token::CommaAt,
            Token::Dot
        ];

        for (actual, expected) in lexer.zip(expected) {
            assert_eq!(actual.unwrap(), expected);
        }
    }
}
