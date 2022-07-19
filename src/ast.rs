//! Provides a type system for the AST, in some sense

mod expr_ext;
mod node_ext;
mod nodes;
mod operators;
mod token_ext;
mod tokens;

use std::marker::PhantomData;

use crate::{
    NixLanguage,
    SyntaxKind::{self, *},
    SyntaxNode, SyntaxNodeChildren, SyntaxToken,
};

pub use nodes::*;
pub use operators::{BinOpKind, UnaryOpKind};
pub use tokens::*;

pub trait AstNode: rowan::ast::AstNode<Language = NixLanguage> {}

impl<T> AstNode for T where T: rowan::ast::AstNode<Language = NixLanguage> {}

#[derive(Debug, Clone)]
pub struct AstNodeChildren<N> {
    inner: SyntaxNodeChildren,
    _p: PhantomData<N>,
}

impl<N> AstNodeChildren<N> {
    fn new(parent: &SyntaxNode) -> Self {
        AstNodeChildren { inner: parent.children(), _p: PhantomData }
    }
}

impl<N: AstNode> Iterator for AstNodeChildren<N> {
    type Item = N;

    fn next(&mut self) -> Option<N> {
        self.inner.find_map(N::cast)
    }
}

mod support {
    use super::{AstNode, AstNodeChildren, AstToken};
    use crate::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

    pub(super) fn first<N: AstNode, NN: AstNode>(parent: &N) -> Option<NN> {
        parent.syntax().children().find_map(|n| NN::cast(n))
    }

    pub(super) fn nth<N: AstNode, NN: AstNode>(parent: &N, n: usize) -> Option<NN> {
        parent.syntax().children().flat_map(NN::cast).nth(n)
    }

    pub(super) fn children<N: AstNode, NN: AstNode>(parent: &N) -> AstNodeChildren<NN> {
        AstNodeChildren::new(parent.syntax())
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

    fn token(&self) -> &SyntaxToken;
}
