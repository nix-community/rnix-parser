//! Provides a type system for the AST, in some sense

mod expr_ext;
mod node_ext;
mod nodes;
mod operators;
mod tokens;

use crate::{NixLanguage, SyntaxKind, SyntaxToken};

pub use nodes::*;
pub use operators::{BinOpKind, UnaryOpKind};
pub use tokens::*;

pub trait AstNode: rowan::ast::AstNode<Language = NixLanguage> {}

impl<T> AstNode for T where T: rowan::ast::AstNode<Language = NixLanguage> {}

mod support {
    use rowan::ast::AstChildren;

    use super::{AstNode, AstToken};
    use crate::{SyntaxElement, SyntaxKind, SyntaxToken};

    pub(super) fn nth<N: AstNode, NN: AstNode>(parent: &N, n: usize) -> Option<NN> {
        parent.syntax().children().flat_map(NN::cast).nth(n)
    }

    pub(super) fn children<N: AstNode, NN: AstNode>(parent: &N) -> AstChildren<NN> {
        rowan::ast::support::children(parent.syntax())
    }

    pub(super) fn token<N: AstNode, T: AstToken>(parent: &N) -> Option<T> {
        children_tokens(parent).find_map(T::cast)
    }

    /// Token untyped
    pub(super) fn token_u<N: AstNode>(parent: &N, kind: SyntaxKind) -> Option<SyntaxToken> {
        children_tokens(parent).find(|it| it.kind() == kind)
    }

    pub(super) fn children_tokens<N: AstNode>(parent: &N) -> impl Iterator<Item = SyntaxToken> {
        parent.syntax().children_with_tokens().filter_map(SyntaxElement::into_token)
    }

    pub(super) fn children_tokens_u<N: AstNode>(parent: &N) -> impl Iterator<Item = SyntaxToken> {
        parent.syntax().children_with_tokens().filter_map(SyntaxElement::into_token)
    }
}

pub trait AstToken {
    fn can_cast(from: SyntaxKind) -> bool
    where
        Self: Sized;

    /// Cast an untyped token into this strongly-typed token. This will return
    /// None if the type was not correct.
    fn cast(from: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;
}

pub struct Another;
pub struct My;

impl From<Another> for Option<My> {
    fn from(_: Another) -> Self {
        None
    }
}

fn test() {
    let test = <Option<My>>::from(Another).unwrap();
}
