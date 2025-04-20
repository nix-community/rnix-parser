use core::num;

use crate::{
    ast::AstToken,
    SyntaxKind::{self, *},
    SyntaxToken,
};

macro_rules! token {
    (
        #[from($kind:ident)]
        $(#[$meta:meta])*
        struct $name:ident;
    ) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        $(#[$meta])*
        pub struct $name(pub(super) SyntaxToken);

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(self.syntax(), f)
            }
        }

        impl AstToken for $name {
            fn can_cast(kind: SyntaxKind) -> bool {
                $kind == kind
            }

            fn cast(from: SyntaxToken) -> Option<Self> {
                if from.kind() == $kind {
                    Some(Self(from))
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxToken {
                &self.0
            }
        }
    };
}

token! { #[from(TOKEN_WHITESPACE)] struct Whitespace; }

token! { #[from(TOKEN_COMMENT)] struct Comment; }

impl Comment {
    pub fn text(&self) -> &str {
        let text = self.syntax().text();
        // Handle both "#..." and "/*...*/" comments.
        match text.strip_prefix("#") {
            Some(s) => s,
            None => text.strip_prefix(r#"/*"#).unwrap().strip_suffix(r#"*/"#).unwrap(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast;
    use crate::match_ast;
    use crate::Root;

    use super::*;
    use rowan::ast::AstNode;

    #[test]
    fn comment() {
        let s = "# comment bruh
/* this is a multiline comment wow
asdfasdf
asdfasdf */
1 + 1
/* last one */
";
        let comments: Vec<String> = Root::parse(s)
            .ok()
            .unwrap()
            .syntax()
            .children_with_tokens()
            .filter_map(|e| match e {
                rowan::NodeOrToken::Token(token) => match_ast! { match token {
                    ast::Comment(it) => Some(it.text().to_string()),
                    _ => None,
                }},
                rowan::NodeOrToken::Node(_) => None,
            })
            .collect();
        let expected = vec![
            " comment bruh",
            " this is a multiline comment wow\nasdfasdf\nasdfasdf ",
            " last one ",
        ];

        assert_eq!(comments, expected);
    }
}

token! { #[from(TOKEN_FLOAT)] struct Float; }

impl Float {
    pub fn value(&self) -> Result<f64, num::ParseFloatError> {
        self.syntax().text().parse()
    }
}

token! { #[from(TOKEN_INTEGER)] struct Integer; }

impl Integer {
    pub fn value(&self) -> Result<i64, num::ParseIntError> {
        self.syntax().text().parse()
    }
}

// A literal part of a path (absolute / relative / home / search) without
// distinguishing which concrete flavour it was taken from.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathContent(pub(super) SyntaxToken);

impl PathContent {
    /// Returns the path as a string
    pub fn text(&self) -> &str {
        self.syntax().text()
    }

    /// Returns the kind of path token
    pub fn path_kind(&self) -> SyntaxKind {
        self.syntax().kind()
    }

    /// Returns true if this is an absolute path
    pub fn is_absolute(&self) -> bool {
        self.path_kind() == TOKEN_PATH_ABS
    }

    /// Returns true if this is a relative path
    pub fn is_relative(&self) -> bool {
        self.path_kind() == TOKEN_PATH_REL
    }

    /// Returns true if this is a home-relative path
    pub fn is_home(&self) -> bool {
        self.path_kind() == TOKEN_PATH_HOME
    }

    /// Returns true if this is a search path
    pub fn is_search(&self) -> bool {
        self.path_kind() == TOKEN_PATH_SEARCH
    }
}

impl AstToken for PathContent {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, TOKEN_PATH_ABS | TOKEN_PATH_REL | TOKEN_PATH_HOME | TOKEN_PATH_SEARCH)
    }

    fn cast(from: SyntaxToken) -> Option<Self> {
        if Self::can_cast(from.kind()) {
            Some(Self(from))
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.0
    }
}

token! { #[from(TOKEN_STRING_CONTENT)] struct StrContent; }

token! { #[from(TOKEN_URI)] struct Uri; }
