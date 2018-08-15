extern crate rnix;

use rnix::parser::nometa::AST as ASTNoMeta;
use std::{env, fs};

fn main() {
    let file = match env::args().skip(1).next() {
        Some(file) => file,
        None => {
            eprintln!("Usage: dump-ast <file>");
            return;
        }
    };
    let content = match fs::read_to_string(file) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("error reading file: {}", err);
            return;
        }
    };
    match rnix::parse(&content) {
        Ok(ast) => println!("{:#?}", ASTNoMeta::from(ast)),
        Err(err) => eprintln!("error: {}", err)
    }
}
