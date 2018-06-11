use std::io::BufRead;
use std::iter::{Iterator, Peekable};
use std::vec;

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

struct ReadChar<T: BufRead> {
    reader: T,
    chars: vec::IntoIter<char>
}

impl<T: BufRead> Iterator for ReadChar<T> {
    type Item = Result<char, String>;

    fn next(&mut self) -> Option<Result<char, String>> {
        if let Some(c) = self.chars.next() {
            return Some(Ok(c))
        }

        let mut line = String::new();

        match self.reader.read_line(&mut line) {
            Ok(0) => None,
            Ok(_) => {
                self.chars = line.chars().collect::<Vec<char>>()
                                         .into_iter();
                self.chars.next().map(|c| Ok(c))
            },
            Err(e) => Some(Err(e.to_string()))
        }
    }
}

impl<T: BufRead> ReadChar<T> {
    fn new(r: T) -> Self {
        ReadChar {
            reader: r,
            chars: Vec::new().into_iter()
        }
    }
}


pub struct Lexer<T: BufRead> {
    iter: Peekable<ReadChar<T>>
}

impl<T: BufRead> Iterator for Lexer<T> {
    type Item = Result<Token, String>;

    fn next(&mut self) -> Option<Result<Token, String>> {
        match self.skip_space() {
            Err(e) => return Some(Err(e)),
            _ => {}
        }

        self.peek_char().map(|r| r.and_then(|c| match c {
            '(' => {
                self.advance_char();
                Ok(Token::OpenParen)
            },
            ')' => {
                self.advance_char();
                Ok(Token::CloseParen)
            },
            '\'' => {
                self.advance_char();
                Ok(Token::Quote)
            },
            '`' => {
                self.advance_char();
                Ok(Token::BackQuote)
            },
            ',' => {
                self.advance_char();

                match self.peek_char() {
                    Some(Ok('@')) => {
                        self.advance_char();
                        Ok(Token::CommaAt)
                    },
                    _ => Ok(Token::Comma)
                }
            },
            '.' => {
                self.advance_char();
                Ok(Token::Dot)
            },
            '"' => self.lex_string(),
            '#' => self.lex_boolean(),
            c if c.is_ascii_digit() => self.lex_integer(),
            c if self.is_initial(c) => self.lex_identifier(),
            c if self.is_peculiar_identifier(c) =>
                self.lex_peculiar_identifier(),
            c => Err(format!("Unexpected character {}", c))
        }))
    }
}

impl<T: BufRead> Lexer<T> {
    pub fn new(r: T) -> Self {
        let reader = ReadChar::new(r);

        Lexer {
            iter: reader.peekable()
        }
    }

    fn peek_char(&mut self) -> Option<Result<char, String>> {
        self.iter.peek().cloned()
    }

    fn advance_char(&mut self) -> Option<Result<char, String>> {
        self.iter.next()
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

    fn skip_space(&mut self) -> Result<(), String> {
        loop {
            match self.peek_char() {
                Some(Ok(c)) if c.is_ascii_whitespace() => {
                    self.advance_char();
                },
                Some(Err(e)) => return Err(e),
                _ => return Ok(())
            }
        }
    }

    fn lex_string(&mut self) -> Result<Token, String> {
        let mut s = String::new();

        self.advance_char();

        loop {
            match self.peek_char() {
                Some(Ok('"')) => {
                    self.advance_char();
                    return Ok(Token::String(s))
                },
                Some(Ok(c)) => {
                    self.advance_char();
                    s.push(c)
                },
                Some(Err(e)) => return Err(e),
                None => return Err("Unclosed string literal".to_owned())
            }
        }
    }

    fn lex_boolean(&mut self) -> Result<Token, String> {
        self.advance_char();

        self.advance_char()
            .unwrap_or(Err("Unexpected end of input".to_owned()))
            .and_then(|c| match c {
                't' => Ok(Token::Boolean(true)),
                'f' => Ok(Token::Boolean(false)),
                c => Err(format!("Unexpected character {}", c))
            })
    }

    fn lex_integer(&mut self) -> Result<Token, String> {
        let mut s = String::new();

        loop {
            match self.peek_char() {
                Some(Ok(c)) if c.is_ascii_digit() => {
                    s.push(c);
                    self.advance_char();
                },
                Some(Err(e)) => return Err(e),
                _ => return s.parse().map(|i| Token::Integer(i))
                                     .map_err(|e| e.to_string())
            }
        }
    }

    fn lex_identifier(&mut self) -> Result<Token, String> {
        let mut s = String::new();

        if let Some(c) = self.advance_char() {
            s.push(c?);
        }

        loop {
            match self.peek_char() {
                Some(Ok(c)) if self.is_subsequent(c) => {
                    s.push(c);
                    self.advance_char();
                },
                Some(Err(e)) => return Err(e),
                _ => return Ok(Token::Identifier(s))
            }
        }
    }

    fn lex_peculiar_identifier(&mut self) -> Result<Token, String> {
        let c = self.advance_char();

        Ok(Token::Identifier(c.unwrap()?.to_string()))
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
