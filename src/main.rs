mod ast;
mod codegen;
mod lexer;
mod parser;
mod token;

use std::env;

use codegen::x64::Codegen;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = args.get(1).unwrap().to_owned();
    let mut lexer = Lexer::new(filename.clone()).unwrap();
    let tokens = Box::new(lexer.lex());
    println!("tokens: {:#?}", tokens);
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    println!("ast: {:#?}", ast);
    let mut codegen = Codegen::new();
    codegen.gen_bin(filename, ast);
}
