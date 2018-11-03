extern crate rnix;
extern crate rowan;

use rowan::WalkEvent;
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
        let ast = rnix::parse(&content);
        let node = ast.node().borrowed();

        for error in node.root_data() {
            println!("error: {}", error);
        }

        let mut indent = 0;
        for event in node.preorder() {
            match event {
                WalkEvent::Enter(node) => {
                    println!("{:indent$}{:?} {:?}", "", node, node.leaf_text(), indent = indent);
                    indent += 2;
                },
                WalkEvent::Leave(_) =>
                    indent -= 2
            }
        }
    }
}
