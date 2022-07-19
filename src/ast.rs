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
        parent
            .syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find_map(T::cast)
    }

    /// Token untyped
    pub(super) fn token_u<N: AstNode>(parent: &N, kind: SyntaxKind) -> Option<SyntaxToken> {
        parent
            .syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|it| it.kind() == kind)
    }

    pub(super) fn children_tokens_u<N: AstNode>(parent: &N) -> impl Iterator<Item = SyntaxToken> {
        parent.syntax().children_with_tokens().filter_map(SyntaxElement::into_token)
    }
}

// /// A TypedNode is simply a wrapper around an untyped node to provide a type
// /// system in some sense.
// pub trait AstNode {
//     fn can_cast(from: &SyntaxNode) -> bool
//     where
//         Self: Sized;

//     /// Cast an untyped node into this strongly-typed node. This will return
//     /// None if the type was not correct.
//     fn cast(from: SyntaxNode) -> Option<Self>
//     where
//         Self: Sized;

//     /// Return a reference to the inner untyped node
//     fn node(&self) -> &SyntaxNode;

//     /// Return all errors of all children, recursively
//     fn errors(&self) -> Vec<SyntaxElement> {
//         self.node()
//             .descendants_with_tokens()
//             // Empty errors can happen if it encounteres EOF while
//             // creating them, which in case a root error is added.
//             .filter(|node| !node.text_range().is_empty())
//             .filter(|node| node.kind() == NODE_ERROR || node.kind() == TOKEN_ERROR)
//             .collect()
//     }

//     // fn dump(&self) -> TextDump {
//     //     TextDump(self.node().clone())
//     // }
// }

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
