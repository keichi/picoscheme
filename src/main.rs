mod builtin;
mod environment;
mod evaluator;
mod interpreter;
mod lexer;
mod parser;
mod value;

use interpreter::Interpreter;

fn main() {
    let interp = Interpreter::new();
    interp.start_repl();
}
