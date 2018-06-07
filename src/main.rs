mod builtin;
mod lexer;
mod parser;
mod evaluator;

use lexer::Lexer;
use parser::Parser;
use evaluator::{eval, Environment};

fn main() {
    let lexer = Lexer::new("((lambda (x) (+ (* x x) x 1)) 2)");
    let mut parser = Parser::new(lexer);
    let parsed = parser.parse();
    let mut env = Environment::new();

    match parsed {
        Ok(v) => match eval(&v, &mut env) {
            Ok(v) => println!("{}", v),
            Err(e) => println!("Error: {}", e)
        },
        Err(e) => println!("Error {}", e)
    }
}
