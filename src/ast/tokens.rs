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
                    std::fmt::Display::fmt(self.token(), f)
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

                fn token(&self) -> &SyntaxToken {
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
    TOKEN_COMMENT => Comment,
    TOKEN_FLOAT => Float,
    TOKEN_INTEGER => Integer,
    TOKEN_PATH => Path,
    TOKEN_URI => Uri,
}
