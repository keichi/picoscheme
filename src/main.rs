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

    pub fn rep(&self, input: &str) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse();

        match parsed.and_then(|e| eval(&e, self.env.clone())) {
            Ok(v) =>  println!("=> {}", v),
            Err(e) => println!("Error: {}", e)
        }
    }

    pub fn start_repl(&self) {
        loop {
            let mut input = String::new();

            print!("picoscheme> ");
            io::stdout().flush().expect("Flush error");

            io::stdin().read_line(&mut input).expect("Input error");
            self.rep(&input);
        }
    }
}

fn main() {
    let interp = Interpreter::new();
    interp.start_repl();

    // let env = Rc::new(Environment::new_global());

    // let exps = vec![
    //     "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))",
    //     "(fact 5)",
    //     r#"(define new-counter
    //       (lambda ()
    //        (define count 0)
    //        (lambda () (set! count (+ count 1)) count)
    //       )
    //     )"#,
    //     "(define c (new-counter))",
    //     "(c)",
    //     "(c)",
    //     "(c)",
    //     "count",
    //     "n",
    //     "(define c2 (new-counter))",
    //     "(c2)",
    //     "(c2)"
    // ];

    // for exp in exps {
    //     let lexer = Lexer::new(exp);
    //     let mut parser = Parser::new(lexer);
    //     let parsed = parser.parse();

    //     println!("{} =>", exp);

    //     match parsed.and_then(|e| eval(&e, env.clone())) {
    //         Ok(v) =>  println!("{}", v),
    //         Err(e) => println!("Error: {}", e)
    //     }
    // }
}
