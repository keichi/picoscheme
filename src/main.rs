mod value;
mod builtin;
mod lexer;
mod parser;
mod evaluator;
mod util;
mod environment;

use std::io;
use std::io::Write;
use std::rc::Rc;

use lexer::Lexer;
use parser::Parser;
use evaluator::eval;
use environment::Environment;

pub struct Interpreter {
    env: Rc<Environment>
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Rc::new(Environment::new_global())
        }
    }

    pub fn start_repl(&self) {
        loop {
            print!("picoscheme> ");
            io::stdout().flush().expect("Flush error");

            let stdin = io::stdin();

            let lexer = Lexer::new(stdin.lock());
            let mut parser = Parser::new(lexer);
            let parsed = parser.parse();

            match parsed.and_then(|e| eval(&e, self.env.clone())) {
                Ok(v) =>  println!("=> {}", v),
                Err(e) => println!("Error: {}", e)
            }
        }
    }
}

fn main() {
    let interp = Interpreter::new();
    interp.start_repl();
}
