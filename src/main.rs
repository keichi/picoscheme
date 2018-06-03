mod lexer;
mod parser;

fn main() {
    let lexer = lexer::Lexer::new("((lambda (x) x) 1)");
    let mut parser = parser::Parser::new(lexer);

    println!("{:?}", parser.parse());
}
