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
        self.syntax().text().strip_prefix('#').unwrap()
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

token! { #[from(TOKEN_PATH)] struct PathContent; }

token! { #[from(TOKEN_STRING_CONTENT)] struct StrContent; }

token! { #[from(TOKEN_URI)] struct Uri; }
