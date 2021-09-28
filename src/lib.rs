#[macro_use]
mod macros;
mod kinds;
pub mod parser;
mod token_set;
pub mod tokenizer;
pub mod types;
pub mod value;

pub use self::{
    kinds::SyntaxKind,
    parser::AST,
    token_set::TokenSet,
    value::{StrPart, Value as NixValue},
};

pub use rowan::{
    NodeOrToken, SyntaxElementChildren, SyntaxNodeChildren, TextRange, TextSize, TokenAtOffset,
    WalkEvent,
};

use self::tokenizer::Tokenizer;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NixLanguage {}

impl rowan::Language for NixLanguage {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        let discriminant: u16 = raw.0;
        assert!(discriminant <= (SyntaxKind::__LAST as u16));
        unsafe { std::mem::transmute::<u16, SyntaxKind>(discriminant) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

pub type SyntaxNode = rowan::SyntaxNode<NixLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<NixLanguage>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

/// A convenience function for first tokenizing and then parsing given input
pub fn parse(input: &str) -> AST {
    parser::parse(Tokenizer::new(input))
}

#[cfg(test)]
mod tests {
    use super::{parse, types::*, value::StrPart, NixValue, SyntaxKind};

    #[test]
    fn interpolation() {
        let ast = parse(include_str!("../test_data/general/interpolation.nix"));

        let let_in = ast.root().inner().and_then(LetIn::cast).unwrap();
        let set = let_in.body().and_then(AttrSet::cast).unwrap();
        let entry = set.entries().nth(1).unwrap();
        let value = entry.value().and_then(Str::cast).unwrap();

        match &*value.parts() {
            &[
                StrPart::Literal(ref s1),
                StrPart::Ast(_),
                StrPart::Literal(ref s2),
                StrPart::Ast(_),
                StrPart::Literal(ref s3)
            ]
            if s1 == "The set\'s x value is: "
                && s2 == "\n\nThis line shall have no indention\n  This line shall be indented by 2\n\n\n"
                && s3 == "\n" => (),
            parts => panic!("did not match: {:#?}", parts)
        }
    }
    #[test]
    fn inherit() {
        let ast = parse(include_str!("../test_data/general/inherit.nix"));

        let let_in = ast.root().inner().and_then(LetIn::cast).unwrap();
        let set = let_in.body().and_then(AttrSet::cast).unwrap();
        let inherit = set.inherits().nth(1).unwrap();

        let from = inherit.from().unwrap().inner().and_then(Ident::cast).unwrap();
        assert_eq!(from.as_str(), "set");
        let mut children = inherit.idents();
        assert_eq!(children.next().unwrap().as_str(), "z");
        assert_eq!(children.next().unwrap().as_str(), "a");
        assert!(children.next().is_none());
    }
    #[test]
    fn math() {
        let ast = parse(include_str!("../test_data/general/math.nix"));
        let root = ast.root().inner().and_then(BinOp::cast).unwrap();
        let operation = root.lhs().and_then(BinOp::cast).unwrap();

        assert_eq!(root.operator(), BinOpKind::Add);
        assert_eq!(operation.operator(), BinOpKind::Add);

        let lhs = operation.lhs().and_then(Value::cast).unwrap();
        assert_eq!(lhs.to_value(), Ok(NixValue::Integer(1)));

        let rhs = operation.rhs().and_then(BinOp::cast).unwrap();
        assert_eq!(rhs.operator(), BinOpKind::Mul);
    }
    #[test]
    fn t_macro() {
        assert_eq!(T![@], SyntaxKind::TOKEN_AT);
        assert!(match SyntaxKind::TOKEN_PAREN_OPEN {
            T!["("] => true,
            _ => false,
        });
    }
}
