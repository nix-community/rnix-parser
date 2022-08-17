use crate::{ast::AstToken, kinds::SyntaxKind::*};
use rowan::{ast::AstNode as OtherAstNode, NodeOrToken};

use crate::ast;

use super::{InterpolPart, PathContent};

impl ast::nodes::Path {
    pub fn parts(&self) -> impl Iterator<Item = InterpolPart<PathContent>> {
        self.syntax().children_with_tokens().map(|child| match child {
            NodeOrToken::Token(token) => {
                assert_eq!(token.kind(), TOKEN_PATH);
                InterpolPart::Literal(PathContent::cast(token).unwrap())
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
        ast::{self, AstToken, InterpolPart, PathContent},
        Root,
    };

    #[test]
    fn parts() {
        fn assert_eq_ast_ctn(it: &mut dyn Iterator<Item = InterpolPart<PathContent>>, x: &str) {
            let tmp = it.next().expect("unexpected EOF");
            if let InterpolPart::Interpolation(astn) = tmp {
                assert_eq!(astn.expr().unwrap().syntax().to_string(), x);
            } else {
                unreachable!("unexpected literal {:?}", tmp);
            }
        }

        fn assert_eq_lit(it: &mut dyn Iterator<Item = InterpolPart<PathContent>>, x: &str) {
            let tmp = it.next().expect("unexpected EOF");
            if let InterpolPart::Literal(astn) = tmp {
                assert_eq!(astn.syntax().text(), x);
            } else {
                unreachable!("unexpected interpol {:?}", tmp);
            }
        }

        let inp = r#"./a/b/${"c"}/${d}/e/f"#;
        let expr = Root::parse(inp).ok().unwrap().expr().unwrap();
        match expr {
            ast::Expr::Path(p) => {
                let mut it = p.parts();
                assert_eq_lit(&mut it, "./a/b/");
                assert_eq_ast_ctn(&mut it, "\"c\"");
                assert_eq_lit(&mut it, "/");
                assert_eq_ast_ctn(&mut it, "d");
                assert_eq_lit(&mut it, "/e/f");
            }
            _ => unreachable!(),
        }
    }
}
