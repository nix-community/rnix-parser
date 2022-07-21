use std::{env, error::Error, fs};

use rnix::ast::AstToken;
use rnix::{
    ast::{self, EntryHolder},
    match_ast, NodeOrToken, SyntaxNode,
};
use rowan::ast::AstNode;

macro_rules! single_match {
    ($expression:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? => $captured:expr) => {
        match $expression {
            $( $pattern )|+ $( if $guard )? => Some($captured),
            _ => None
        }
    };
}

fn main() -> Result<(), Box<dyn Error>> {
    let file = match env::args().nth(1) {
        Some(file) => file,
        None => {
            eprintln!("Usage: list-fns <file>");
            return Ok(());
        }
    };
    let content = fs::read_to_string(&file)?;
    let ast = rnix::Root::parse(&content).ok()?;
    let expr = ast.expr().unwrap();
    let set = match expr {
        ast::Expr::AttrSet(set) => set,
        _ => return Err("root isn't a set".into()),
    };
    for entry in set.entries() {
        if let ast::Entry::KeyValue(key_value) = entry {
            if let Some(ast::Expr::Lambda(lambda)) = key_value.value() {
                let key = key_value.key().unwrap();
                let ident = key.attrs().last().and_then(|attr| match attr {
                    ast::Attr::Ident(ident) => Some(ident),
                    _ => None,
                });
                let s = ident.as_ref().map_or_else(
                    || "error".to_string(),
                    |ident| ident.ident_token().unwrap().text().to_string(),
                );
                println!("Function name: {}", s);
                {
                    let comments = comments_before(key_value.syntax());
                    if !comments.is_empty() {
                        println!("--> Doc: {comments}");
                    }
                }

                let mut value = Some(lambda);
                while let Some(lambda) = value {
                    let s = lambda
                        .param()
                        .as_ref()
                        .map_or_else(|| "error".to_string(), |param| param.to_string());
                    println!("-> Param: {}", s);
                    {
                        let comments = comments_before(lambda.syntax());
                        if !comments.is_empty() {
                            println!("--> Doc: {comments}");
                        }
                    }
                    value =
                        single_match!(lambda.body().unwrap(), ast::Expr::Lambda(lambda) => lambda);
                }
                println!();
            }
        }
    }

    Ok(())
}

fn comments_before(node: &SyntaxNode) -> String {
    node.siblings_with_tokens(rowan::Direction::Prev)
        // rowan always returns the first node for some reason
        .skip(1)
        .map_while(|element| match element {
            NodeOrToken::Token(token) => match_ast! {
                match token {
                    ast::Comment(it) => Some(Some(it)),
                    ast::Whitespace(_) => Some(None),
                    _ => None,
                }
            },
            _ => None,
        })
        .flatten()
        .map(|s| s.text().trim().to_string())
        .collect::<Vec<_>>()
        .join(" ")
}
