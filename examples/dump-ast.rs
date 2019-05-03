use rowan::{SyntaxElement, WalkEvent};
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

        for error in ast.root_errors() {
            println!("error: {}", error);
        }

        let mut indent = 0;
        for event in ast.node().preorder_with_tokens() {
            match event {
                WalkEvent::Enter(node) => {
                    match node {
                        SyntaxElement::Node(node) => println!("{:indent$}{:?}", "", node, indent = indent),
                        SyntaxElement::Token(token) => println!("{:indent$}{:?} {:?}", "", token, token.text(), indent = indent)
                    }
                    indent += 2;
                },
                WalkEvent::Leave(_) =>
                    indent -= 2
            }
        }
    }
}
