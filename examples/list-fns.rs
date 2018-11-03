#[macro_use]
extern crate failure;
extern crate rnix;
extern crate rowan;

use failure::Error;
use rnix::{
    parser::{Node, NodeType, Types},
    tokenizer::Token,
    types::*,
};
use rowan::SmolStr;
use std::{env, fs};

fn main() -> Result<(), Error> {
    let file = match env::args().skip(1).next() {
        Some(file) => file,
        None => {
            eprintln!("Usage: list-fns <file>");
            return Ok(());
        }
    };
    let content = fs::read_to_string(&file)?;
    let ast = rnix::parse(&content).as_result()?;
    let set = Set::cast(ast.root().inner()).ok_or(format_err!("root isn't a set"))?;

    for entry in set.entries() {
        if let Some(lambda) = Lambda::cast(entry.value()) {
            let attr = entry.key();
            let ident = attr.path().last().and_then(Ident::cast);
            let s = ident.as_ref().map(Ident::as_str).unwrap_or("error");
            println!("Function name: {}", s);

            let mut value = Some(lambda);
            while let Some(lambda) = value {
                let ident = Ident::cast(lambda.arg());
                let s = ident.as_ref().map(Ident::as_str).unwrap_or("error");
                println!("Arg: {}", s);
                if let Some(comment) = find_comment(lambda.arg()) {
                    println!("Doc: {}", comment);
                }
                value = Lambda::cast(lambda.body());
            }
        }
    }

    Ok(())
}
fn find_comment<R: rowan::TreeRoot<Types>>(mut node: Node<R>) -> Option<String> {
    loop {
        loop {
            let new = node.prev_sibling();
            if let Some(new) = new {
                node = new;
                break;
            } else {
                node = node.parent()?;
            }
        }

        match node.kind() {
            NodeType::Token(Token::Comment) =>
                return node.borrowed().leaf_text().map(SmolStr::as_str).map(String::from),
            NodeType::Token(t) if t.is_trivia() => (),
            _ => return None
        }
    }
}
