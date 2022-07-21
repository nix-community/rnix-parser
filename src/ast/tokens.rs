use core::num;

use crate::{
    ast::AstToken,
    SyntaxKind::{self, *},
    SyntaxToken,
};

macro_rules! ast_tokens {
    ($($kind:ident => $name:ident $(: $trait:ident)* $(: { $($block:tt)* })?),* $(,)?) => {
        $(
            #[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

            $(impl $trait for $name {})*

            $(impl $name { $($block)* })?
        )*
    }
}

ast_tokens! {
    TOKEN_WHITESPACE => Whitespace,
    TOKEN_COMMENT => Comment: {
        pub fn text(&self) -> &str {
            self.syntax().text().strip_prefix('#').unwrap()
        }
    },
    TOKEN_FLOAT => Float: {
        pub fn value(&self) -> Result<f64, num::ParseFloatError> {
            self.syntax().text().parse()
        }
    },
    TOKEN_INTEGER => Integer: {
        pub fn value(&self) -> Result<i64, num::ParseIntError> {
            self.syntax().text().parse()
        }
    },
    TOKEN_PATH => Path,
    TOKEN_URI => Uri,
}
