enum Token {
    Idenfitifer(String),
    Boolean(bool),
    Integer(i64),
    String(String),
    OpenParen,
    CloseParen,
    Quote,
    BackQuote,
    Comma
}

struct Lexer<'a> {
    iter: std::iter::Peekable<std::str::Chars<'a>>
}

impl<'a> Lexer<'a> {
    fn new(s: &'a str) -> Lexer<'a> {
        Lexer {
            iter: s.chars().peekable()
        }
    }

    fn lex_string(&mut self) -> Option<Token> {
        None
    }

    fn lex_boolean(&mut self) -> Option<Token> {
        None
    }

    fn lex_integer(&mut self) -> Option<Token> {
        None
    }

    fn lex_identifier(&mut self) -> Option<Token> {
        None
    }

    fn get_token(&mut self) -> Option<Token> {
        let c = self.iter.peek();

        match c {
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
            Some('"') => self.lex_string(),
            Some('#') => self.lex_boolean(),
            Some(c) if c.is_ascii_digit() => self.lex_integer(),
            Some(c) if c.is_ascii_alphanumeric() => self.lex_identifier(),
            _ => None
        }
    }
}

fn main() {
    let mut lexer = Lexer::new("123");
    lexer.get_token();
}
