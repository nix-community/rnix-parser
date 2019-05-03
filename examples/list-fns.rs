#[macro_use]
extern crate failure;

use failure::Error;
use rnix::{parser::nodes::*, types::*};
use rowan::{SyntaxElement, SyntaxNode};
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
            let s = ident.map(Ident::as_str).unwrap_or("error");
            println!("Function name: {}", s);
            if let Some(comment) = find_comment(entry.key().node()) {
                println!("-> Doc: {}", comment);
            }

            let mut value = Some(lambda);
            while let Some(lambda) = value {
                let ident = Ident::cast(lambda.arg());
                let s = ident.map(Ident::as_str).unwrap_or("error");
                println!("-> Arg: {}", s);
                if let Some(comment) = find_comment(lambda.arg()) {
                    println!("--> Doc: {}", comment);
                }
                value = Lambda::cast(lambda.body());
            }
            println!();
        }
    }

    Ok(())
}
fn find_comment(node: &SyntaxNode) -> Option<String> {
    let mut node = SyntaxElement::Node(node);
    let mut doc = Vec::new();
    loop {
        loop {
            let new = node.prev_sibling_or_token();
            if let Some(new) = new {
                node = new;
                break;
            } else {
                node = SyntaxElement::Node(node.parent()?);
            }
        }

        match node.kind() {
            TOKEN_COMMENT => match node {
                SyntaxElement::Token(token) => doc.push(
                    token.text().as_str().trim_start_matches('#').trim()
                ),
                SyntaxElement::Node(_) => unreachable!()
            },
            t if token_helpers::is_trivia(t) => (),
            _ => break
        }
    }
    doc.reverse();
    return Some(doc)
        .filter(|lines| !lines.is_empty())
        .map(|lines| lines.join("\n        "));
}
