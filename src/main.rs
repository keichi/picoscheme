mod lexer;
mod parser;

fn main() {
    // let lexer = Lexer::new("((123 456) '(#t #f) \"foo\" \"\" test)");
    let lexer = lexer::Lexer::new("(list->hoge 123 456 'foo (1 2 . 3))");

    for tok in lexer {
        println!("tok: {:?}", tok);
    }
}
