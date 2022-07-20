use std::{env, error::Error, fs};

use rnix::{
    ast::{self, EntryHolder},
    NodeOrToken, SyntaxNode,
};
use rowan::ast::AstNode;

#[macro_export]
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
            if let ast::Expr::Lambda(lambda) = key_value.value().unwrap() {
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
                // if let Some(comment) = find_comment(key.syntax().clone()) {
                //     println!("-> Doc: {}", comment);
                // }

                let mut value = Some(lambda);
                while let Some(lambda) = value {
                    let s = ident.as_ref().map_or_else(
                        || "error".to_string(),
                        |ident| ident.ident_token().unwrap().to_string(),
                    );
                    println!("-> Arg: {}", s);
                    // if let Some(comment) =
                    //     lambda.param().map(|param| param.syntax()).and_then(find_comment)
                    // {
                    //     println!("--> Doc: {}", comment);
                    // }
                    value =
                        single_match!(lambda.body().unwrap(), ast::Expr::Lambda(lambda) => lambda);
                }
                println!();
            }
        }
    }

    Ok(())
}

// fn find_comment(node: &SyntaxNode) -> Option<String> {
//     let mut node = NodeOrToken::Node(node);
//     let mut comments = Vec::new();
//     loop {
//         loop {
//             if let Some(new) = node.prev_sibling_or_token() {
//                 node = new;
//                 break;
//             } else {
//                 node = NodeOrToken::Node(node.parent()?);
//             }
//         }

//         match node.kind() {
//             TOKEN_COMMENT => match &node {
//                 NodeOrToken::Token(token) => comments.push(token.text().to_string()),
//                 NodeOrToken::Node(_) => unreachable!(),
//             },
//             t if t.is_trivia() => (),
//             _ => break,
//         }
//     }
//     let doc = comments
//         .iter()
//         .map(|it| it.trim_start_matches('#').trim())
//         .collect::<Vec<_>>()
//         .join("\n        ");
//     Some(doc).filter(|it| !it.is_empty())
// }
