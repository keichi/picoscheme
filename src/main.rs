mod builtin;
mod lexer;
mod parser;
mod evaluator;

use lexer::Lexer;
use parser::Parser;
use evaluator::{eval, Environment};

fn main() {
    let mut env = Environment::new();

    let exps = vec![
        "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))",
        "(fact 5)"
    ];

    for exp in exps {
        let lexer = Lexer::new(exp);
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse();

        println!("{} =>", exp);

        match parsed {
            Ok(v) => match eval(&v, &mut env) {
                Ok(v) => println!("{}", v),
                Err(e) => println!("Error: {}", e)
            },
            Err(e) => println!("Error {}", e)
        }
    }
}
