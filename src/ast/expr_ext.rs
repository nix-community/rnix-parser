use crate::{
    ast::{self, support::*, AstNode, AstToken},
    kinds::SyntaxKind::*,
    match_ast, SyntaxElement,
};

use super::support::first;

// this is a separate type because it mixes tokens and nodes
// for example, a Str is a node because it can contain nested subexpressions but an Integer is a token.
// This means that we have to write it out manually instead of using the macro to create the type for us.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LiteralKind {
    Str(ast::Str),
    Float(ast::Float),
    Integer(ast::Integer),
    Path(ast::Path),
    Uri(ast::Uri),
}

impl ast::Literal {
    pub fn kind(&self) -> LiteralKind {
        if let Some(it) = first(self) {
            return LiteralKind::Str(it);
        }

        if let Some(it) = token(self) {
            return LiteralKind::Float(it);
        }

        if let Some(it) = token(self) {
            return LiteralKind::Integer(it);
        }

        if let Some(it) = token(self) {
            return LiteralKind::Path(it);
        }

        if let Some(it) = token(self) {
            return LiteralKind::Uri(it);
        }

        unreachable!()
    }
}
