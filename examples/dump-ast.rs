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
        let parse = rnix::Root::parse(&content);

        for error in parse.errors() {
            println!("error: {}", error);
        }
        println!("{:#?}", parse.tree());
    }
}
