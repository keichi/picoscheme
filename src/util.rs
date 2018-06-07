use lexer::Lexer;
use parser::{Parser};
use evaluator::{eval, Environment};

#[allow(dead_code)]
pub fn rep(sexp: &str) -> String {
    let lexer = Lexer::new(sexp);
    let mut parser = Parser::new(lexer);
    let parsed = parser.parse().expect("Failed to parse");
    let mut env = Environment::new_global();
    let result = eval(&parsed, &mut env).expect("Failed to evaluate");

    format!("{}", result)
}
