use std::{env, error::Error, fs};


fn main() -> Result<(), Box<dyn Error>> {
    todo!()
//     let file = match env::args().nth(1) {
//         Some(file) => file,
//         None => {
//             eprintln!("Usage: list-fns <file>");
//             return Ok(());
//         }
//     };
//     let content = fs::read_to_string(&file)?;
//     let ast = rnix::Root::parse(&content).as_result()?;
//     let set = ast.root().inner().and_then(AttrSet::cast).ok_or("root isn't a set")?;

//     for entry in set.entries() {
//         if let Some(lambda) = entry.value().and_then(Lambda::cast) {
//             if let Some(attr) = entry.key() {
//                 let ident = attr.path().last().and_then(Ident::cast);
//                 let s = ident.as_ref().map_or_else(|| "error".to_string(), Ident::to_inner_string);
//                 println!("Function name: {}", s);
//                 if let Some(comment) = find_comment(attr.node().clone()) {
//                     println!("-> Doc: {}", comment);
//                 }

//                 let mut value = Some(lambda);
//                 while let Some(lambda) = value {
//                     let ident = lambda.arg().and_then(Ident::cast);
//                     let s =
//                         ident.as_ref().map_or_else(|| "error".to_string(), Ident::to_inner_string);
//                     println!("-> Arg: {}", s);
//                     if let Some(comment) = lambda.arg().and_then(find_comment) {
//                         println!("--> Doc: {}", comment);
//                     }
//                     value = lambda.body().and_then(Lambda::cast);
//                 }
//                 println!();
//             }
//         }
//     }

//     Ok(())
// }
// fn find_comment(node: SyntaxNode) -> Option<String> {
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
}
