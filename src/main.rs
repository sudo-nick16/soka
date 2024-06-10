mod ast;
mod codegen;
mod lexer;
mod parser;
mod token;

use codegen::x64::Codegen;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let filename = String::from("first.n");
    let mut lexer = Lexer::new(filename.clone()).unwrap();
    let tokens = Box::new(lexer.lex());
    println!("tokens: {:#?}", tokens);
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    println!("ast: {:#?}", ast);
    let mut codegen = Codegen::new();
    codegen.gen_bin(filename, ast);
}
