mod value;
mod builtin;
mod lexer;
mod parser;
mod evaluator;
mod util;

use lexer::Lexer;
use parser::Parser;
use evaluator::{eval, Environment};

fn main() {
    let mut env = Environment::new();

    let exps = vec![
        "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))",
        "(fact 5)",
        r#"(define new-counter
          (lambda ()
           (define count 0)
           (lambda () (set! count (+ count 1)) count)
          )
        )"#,
        "(define c (new-counter))",
        "(c)",
        "(c)",
        "(c)"
    ];

    for exp in exps {
        let lexer = Lexer::new(exp);
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse();

        println!("{} =>", exp);

        match parsed.and_then(|e| eval(&e, &mut env)) {
            Ok(v) => println!("{}", v),
            Err(e) => println!("Error: {}", e)
        }
    }
}
