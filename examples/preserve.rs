#![feature(rust_2018_preview)]

use std::{env, fs};

fn main() {
    let mut iter = env::args().skip(1).peekable();
    if iter.peek().is_none() {
        eprintln!("Usage: preserve <file>");
        return;
    }
    for file in iter {
        let content = match fs::read_to_string(file) {
            Ok(content) => content,
            Err(err) => {
                eprintln!("error reading file: {}", err);
                return;
            }
        };
        match rnix::parse(&content) {
            Ok(ast) => print!("{}", ast),
            Err(err) => eprintln!("error: {:?}", err)
        }
    }
}
