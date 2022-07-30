use crate::kinds::SyntaxKind::*;
use rowan::{ast::AstNode as OtherAstNode, NodeOrToken};

use crate::ast;

use super::InterpolPart;

impl ast::nodes::Path {
    pub fn parts(&self) -> impl Iterator<Item = InterpolPart> {
        self.syntax().children_with_tokens().map(|child| match child {
            NodeOrToken::Token(token) => {
                assert_eq!(token.kind(), TOKEN_PATH);
                InterpolPart::Literal(token.text().to_string())
            }
            NodeOrToken::Node(node) => {
                InterpolPart::Interpolation(ast::Interpol::cast(node.clone()).unwrap())
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use rowan::ast::AstNode;

    use crate::{
        ast::{self, InterpolPart},
        Root,
    };

    #[test]
    fn parts() {
        fn assert_eq_ast_ctn(it: &mut dyn Iterator<Item = InterpolPart>, x: &str) {
            let tmp = it.next().expect("unexpected EOF");
            if let InterpolPart::Interpolation(astn) = tmp {
                assert_eq!(astn.expr().unwrap().syntax().to_string(), x);
            } else {
                unreachable!("unexpected literal {:?}", tmp);
            }
        }

        let inp = r#"./a/b/${"c"}/${d}/e/f"#;
        let expr = Root::parse(inp).ok().unwrap().expr().unwrap();
        match expr {
            ast::Expr::Path(p) => {
                let mut it = p.parts().into_iter();
                assert_eq!(it.next().unwrap(), InterpolPart::Literal("./a/b/".to_string()));
                assert_eq_ast_ctn(&mut it, "\"c\"");
                assert_eq!(it.next().unwrap(), InterpolPart::Literal("/".to_string()));
                assert_eq_ast_ctn(&mut it, "d");
                assert_eq!(it.next().unwrap(), InterpolPart::Literal("/e/f".to_string()));
            }
            _ => unreachable!(),
        }
    }
}
