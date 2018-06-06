mod builtin;
mod lexer;
mod parser;
mod evaluator;

use lexer::Lexer;
use parser::Parser;
use evaluator::{eval, Environment};

fn main() {
    let lexer = Lexer::new("(cons 1 '(2 . 3))");
    let mut parser = Parser::new(lexer);
    let parsed = parser.parse();
    let env = Environment::new();

    match parsed {
        Ok(v) => match eval(&v, &env) {
            Ok(v) => println!("{}", v),
            Err(e) => println!("Error: {}", e)
        },
        Err(e) => println!("Error {}", e)
    }
}
