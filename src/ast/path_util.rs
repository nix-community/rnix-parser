use crate::kinds::SyntaxKind::*;
use rowan::{ast::AstNode as OtherAstNode, NodeOrToken};

use crate::ast;

impl ast::PathWithInterpol {
    pub fn parts(&self) -> Vec<PathPart> {
        let mut parts = Vec::new();

        for child in self.syntax().children_with_tokens() {
            match child {
                NodeOrToken::Token(token) => {
                    assert_eq!(token.kind(), TOKEN_PATH);
                    parts.push(PathPart::Literal(token.text().to_string()));
                }
                NodeOrToken::Node(node) => {
                    assert_eq!(node.kind(), NODE_STRING_INTERPOL);
                    parts.push(PathPart::Interpolation(
                        ast::StrInterpol::cast(node.clone()).unwrap(),
                    ));
                }
            }
        }

        parts
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PathPart {
    Literal(String),
    Interpolation(ast::StrInterpol),
}

#[cfg(test)]
mod tests {
    use rowan::ast::AstNode;

    use crate::{
        ast::{self, PathPart},
        Root,
    };

    #[test]
    fn parts() {
        fn assert_eq_ast_ctn(it: &mut dyn Iterator<Item = PathPart>, x: &str) {
            let tmp = it.next().expect("unexpected EOF");
            if let PathPart::Interpolation(astn) = tmp {
                assert_eq!(astn.expr().unwrap().syntax().to_string(), x);
            } else {
                unreachable!("unexpected literal {:?}", tmp);
            }
        }

        let inp = r#"./a/b/${"c"}/${d}/e/f"#;
        let expr = Root::parse(inp).ok().unwrap().expr().unwrap();
        match expr {
            ast::Expr::PathWithInterpol(p) => {
                let mut it = p.parts().into_iter();
                assert_eq!(it.next().unwrap(), PathPart::Literal("./a/b/".to_string()));
                assert_eq_ast_ctn(&mut it, "\"c\"");
                assert_eq!(it.next().unwrap(), PathPart::Literal("/".to_string()));
                assert_eq_ast_ctn(&mut it, "d");
                assert_eq!(it.next().unwrap(), PathPart::Literal("/e/f".to_string()));
            }
            _ => unreachable!(),
        }
    }
}
