mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;

fn main() {
    let lexer = Lexer::new("((lambda (x) x) 1)");
    let mut parser = Parser::new(lexer);

    println!("{:?}", parser.parse());
}
