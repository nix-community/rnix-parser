use crate::{
    ast::{self, support::*, AstNode, AstToken},
    kinds::SyntaxKind::*,
    match_ast, SyntaxElement,
};

use super::support::first;

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