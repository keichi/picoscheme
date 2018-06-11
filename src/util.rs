use std::io::Cursor;
use std::rc::Rc;

use lexer::Lexer;
use parser::Parser;
use evaluator::eval;
use environment::Environment;

#[allow(dead_code)]
pub fn rep(sexp: &str) -> String {
    let r = Cursor::new(sexp.clone().as_bytes());
    let lexer = Lexer::new(r);
    let mut parser = Parser::new(lexer);
    let parsed = parser.parse().expect("Failed to parse");
    let env = Environment::new_global();
    let result = eval(&parsed, Rc::new(env)).expect("Failed to evaluate");

    format!("{}", result)
}
