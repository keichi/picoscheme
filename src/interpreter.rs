use std::env;
use std::io::{self, BufReader, Write};
use std::fs::File;
use std::path::Path;
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

    pub fn rep_file(&self, path: &Path) -> Result<(), String> {
        let file = File::open(path).map_err(|e| e.to_string())?;
        let reader = BufReader::new(file);

        let lexer = Lexer::new(reader);
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse();

        return parsed.and_then(|e| eval(&e, self.env.clone())).and(Ok(()))
    }

    pub fn start_repl(&self) {
        if let Err(e) = self.rep_file(Path::new("stdlib.scm")) {
            println!("Error: {}", e)
        }

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
