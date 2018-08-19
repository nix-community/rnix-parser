use rnix::parser::intoactualslowtree::{AST as ASTActual, IntoTree};
use std::{env, fs};

fn main() {
    let mut iter = env::args().skip(1).peekable();
    if iter.peek().is_none() {
        eprintln!("Usage: dump-ast <file>");
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
            Ok(mut ast) => {
                println!("{:#?}", ASTActual::into_tree(ast.root, &mut ast.arena))
            },
            Err(err) => eprintln!("error: {:?}", err)
        }
    }
}
