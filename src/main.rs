mod value;
mod builtin;
mod lexer;
mod parser;
mod evaluator;
mod environment;
mod interpreter;

use interpreter::Interpreter;

fn main() {
    let interp = Interpreter::new();
    interp.start_repl();
}
