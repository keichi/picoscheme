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

    #[cfg(test)]
    pub fn rep_str(&self, s: &str) -> String {
        let lexer = Lexer::new(s.as_bytes());
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse().unwrap();

        let result = eval(&parsed, self.env.clone()).unwrap();

        format!("{}", result)
    }
}
