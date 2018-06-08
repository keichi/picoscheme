mod value;
mod builtin;
mod lexer;
mod parser;
mod evaluator;
mod util;

use std::rc::Rc;

use lexer::Lexer;
use parser::Parser;
use evaluator::{eval, Environment};

fn main() {
    let env = Rc::new(Environment::new_global());

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
        "(c)",
        "count",
        "n",
        "(define c2 (new-counter))",
        "(c2)",
        "(c2)"
    ];

    for exp in exps {
        let lexer = Lexer::new(exp);
        let mut parser = Parser::new(lexer);
        let parsed = parser.parse();

        println!("{} =>", exp);

        match parsed.and_then(|e| eval(&e, env.clone())) {
            Ok(v) =>  println!("{}", v),
            Err(e) => println!("Error: {}", e)
        }
    }
}
