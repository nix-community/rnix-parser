#[macro_use]
mod macros;
pub mod ast;
mod kinds;
pub mod parser;
#[cfg(test)]
mod tests;
mod token_set;
pub mod tokenizer;

use std::marker::PhantomData;

use self::kinds::SYNTAX_KIND_MAX;
pub use self::{kinds::ParseSyntaxKindError, kinds::SyntaxKind, tokenizer::tokenize};

use ast::AstNode;
use parser::ParseError;
use rowan::GreenNode;
pub use rowan::{NodeOrToken, TextRange, TextSize, TokenAtOffset, WalkEvent};
pub(crate) use token_set::TokenSet;

use self::tokenizer::Tokenizer;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NixLanguage {}

impl rowan::Language for NixLanguage {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        let discriminant: u16 = raw.0;
        assert!(discriminant <= SYNTAX_KIND_MAX);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(discriminant) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

pub type SyntaxNode = rowan::SyntaxNode<NixLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<NixLanguage>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<NixLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<NixLanguage>;

pub use ast::Root;

impl Root {
    pub fn parse(s: &str) -> Parse<Root> {
        let (green, errors) = parser::parse(Tokenizer::new(s));
        Parse { green, errors, _ty: PhantomData }
    }
}

/// The result of a parse
#[derive(Clone)]
pub struct Parse<T> {
    green: GreenNode,
    errors: Vec<ParseError>,
    _ty: PhantomData<fn() -> T>,
}

impl<T> Parse<T> {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }
}

impl<T: AstNode> Parse<T> {
    pub fn tree(&self) -> T {
        T::cast(self.syntax()).unwrap()
    }

    /// Return all errors in the tree, if any
    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    /// Either return the first error in the tree, or if there are none return self
    pub fn ok(self) -> Result<T, ParseError> {
        if let Some(err) = self.errors().first() {
            return Err(err.clone());
        }
        Ok(self.tree())
    }
}

/// Matches a `SyntaxNode` against an `ast` type.
///
/// # Example:
///
/// ```ignore
/// match_ast! {
///     match node {
///         ast::Apply(it) => { ... },
///         ast::Assert(it) => { ... },
///         ast::IfElse(it) => { ... },
///         _ => None,
///     }
/// }
/// ```
#[macro_export]
macro_rules! match_ast {
    (match $node:ident { $($tt:tt)* }) => { match_ast!(match ($node) { $($tt)* }) };

    (match ($node:expr) {
        $( ast::$ast:ident($it:pat) => $res:expr, )*
        _ => $catch_all:expr $(,)?
    }) => {{
        $( if let Some($it) = ast::$ast::cast($node.clone()) { $res } else )*
        { $catch_all }
    }};
}
