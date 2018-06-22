use std::fs::File;
use std::io::{self, BufReader, Write};
use std::path::Path;
use std::rc::Rc;

use environment::Environment;
use evaluator::eval;
use lexer::Lexer;
use parser::Parser;

pub struct Interpreter {
    env: Rc<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Rc::new(Environment::new_global()),
        }
    }

    pub fn eval_file(&self, path: &Path) -> Result<(), String> {
        let file = File::open(path).map_err(|e| e.to_string())?;
        let reader = BufReader::new(file);
        let lexer = Lexer::new(reader);
        let mut parser = Parser::new(lexer);

        while parser.tokens.peek().is_some() {
            let parsed = parser.parse();

            if let Err(e) = parsed.and_then(|e| eval(&e, self.env.clone())) {
                return Err(e);
            }
        }

        return Ok(());
    }

    pub fn start_repl(&self) {
        if let Err(e) = self.eval_file(Path::new("stdlib.scm")) {
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
                Ok(v) => println!("=> {}", v),
                Err(e) => println!("Error: {}", e),
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
