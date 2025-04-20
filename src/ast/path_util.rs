use crate::kinds::SyntaxKind::{TOKEN_PATH_ABS, TOKEN_PATH_HOME, TOKEN_PATH_REL};

use rowan::{ast::AstNode as OtherAstNode, NodeOrToken};

pub use super::nodes::Path;
use super::{
    nodes::{PathAbs, PathHome, PathRel, PathSearch},
    AstToken, InterpolPart, PathContent,
};
use crate::ast;

fn extract_path_parts<T: ast::AstNode>(node: &T) -> Vec<InterpolPart<PathContent>> {
    node.syntax()
        .children_with_tokens()
        .map(|child| match child {
            NodeOrToken::Token(token) => {
                debug_assert!(matches!(
                    token.kind(),
                    TOKEN_PATH_ABS | TOKEN_PATH_REL | TOKEN_PATH_HOME
                ));
                InterpolPart::Literal(PathContent::cast(token).unwrap())
            }
            NodeOrToken::Node(node) => {
                InterpolPart::Interpolation(ast::Interpol::cast(node.clone()).unwrap())
            }
        })
        .collect()
}

// Direct methods for interpolatable path types
impl PathAbs {
    pub fn parts(&self) -> Vec<InterpolPart<PathContent>> {
        extract_path_parts(self)
    }
}

impl PathRel {
    pub fn parts(&self) -> Vec<InterpolPart<PathContent>> {
        extract_path_parts(self)
    }
}

impl PathHome {
    pub fn parts(&self) -> Vec<InterpolPart<PathContent>> {
        extract_path_parts(self)
    }
}

// Direct methods for search path
impl PathSearch {
    /// Get the content of a search path
    pub fn content(&self) -> Option<PathContent> {
        self.syntax()
            .children_with_tokens()
            .filter_map(|child| child.into_token().and_then(PathContent::cast))
            .next()
    }
}

/// Extension methods for the Path enum
impl Path {
    /// Get parts from any path type in a unified way
    pub fn parts(&self) -> Vec<InterpolPart<PathContent>> {
        match self {
            // For interpolatable paths, get their parts
            Path::PathAbs(p) => p.parts(),
            Path::PathRel(p) => p.parts(),
            Path::PathHome(p) => p.parts(),
            // For search paths, return a single literal component if content exists
            Path::PathSearch(p) => {
                if let Some(content) = p.content() {
                    vec![InterpolPart::Literal(content)]
                } else {
                    vec![]
                }
            }
        }
    }

    pub fn is_search(&self) -> bool {
        matches!(self, Path::PathSearch(_))
    }

    pub fn is_interpolatable(&self) -> bool {
        !self.is_search()
    }
}

#[cfg(test)]
mod tests {
    use rowan::ast::AstNode;

    use super::InterpolPart;
    use crate::{
        ast::{self, Path},
        Root,
    };

    #[test]
    fn test_path_types() {
        // Absolute path
        let inp = "/foo/bar";
        let expr = Root::parse(inp).ok().unwrap().expr().unwrap();
        if let ast::Expr::PathAbs(p) = expr {
            let path = Path::cast(p.syntax().clone()).unwrap();
            assert!(path.is_interpolatable());
            assert!(!path.is_search());
        }

        // Search path
        let inp = "<nixpkgs>";
        let expr = Root::parse(inp).ok().unwrap().expr().unwrap();
        if let ast::Expr::PathSearch(p) = expr {
            let path = Path::cast(p.syntax().clone()).unwrap();
            assert!(!path.is_interpolatable());
            assert!(path.is_search());
        }
    }

    #[test]
    fn test_parts() {
        // Test parts with absolute path
        let inp = "/foo/bar";
        let expr = Root::parse(inp).ok().unwrap().expr().unwrap();
        if let ast::Expr::PathAbs(p) = expr {
            let path = Path::cast(p.syntax().clone()).unwrap();

            let parts = path.parts();
            assert_eq!(parts.len(), 1);

            match &parts[0] {
                InterpolPart::Literal(content) => {
                    assert_eq!(content.text(), "/foo/bar");
                }
                _ => panic!("Expected literal part"),
            }
        }

        // Test parts with interpolated path
        let inp = r#"./a/${"hello"}"#;
        let expr = Root::parse(inp).ok().unwrap().expr().unwrap();
        if let ast::Expr::PathRel(p) = expr {
            let path = Path::cast(p.syntax().clone()).unwrap();

            let parts = path.parts();
            assert_eq!(parts.len(), 2);

            match &parts[0] {
                InterpolPart::Literal(content) => {
                    assert_eq!(content.text(), "./a/");
                }
                _ => panic!("Expected literal part"),
            }

            match &parts[1] {
                InterpolPart::Interpolation(_) => {} // Success
                _ => panic!("Expected interpolation part"),
            }
        }

        // Test parts with search path
        let inp = "<nixpkgs>";
        let expr = Root::parse(inp).ok().unwrap().expr().unwrap();
        if let ast::Expr::PathSearch(p) = expr {
            let path = Path::cast(p.syntax().clone()).unwrap();

            let parts = path.parts();
            assert_eq!(parts.len(), 1);

            match &parts[0] {
                InterpolPart::Literal(content) => {
                    assert_eq!(content.text(), "<nixpkgs>");
                }
                _ => panic!("Expected literal part"),
            }
        }
    }

    #[test]
    fn direct_method_usage() {
        // Test direct parts() method on PathAbs
        let inp = "/foo/bar";
        let expr = Root::parse(inp).ok().unwrap().expr().unwrap();
        if let ast::Expr::PathAbs(p) = expr {
            let parts = p.parts();
            assert_eq!(parts.len(), 1);

            match &parts[0] {
                InterpolPart::Literal(content) => {
                    assert_eq!(content.text(), "/foo/bar");
                }
                _ => panic!("Expected literal part"),
            }
        }

        // Test direct content() method on PathSearch
        let inp = "<nixpkgs>";
        let expr = Root::parse(inp).ok().unwrap().expr().unwrap();
        if let ast::Expr::PathSearch(p) = expr {
            let content = p.content().expect("Expected content");
            assert_eq!(content.text(), "<nixpkgs>");
        }
    }
}
