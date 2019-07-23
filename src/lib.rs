#[macro_use]
mod macros;
mod kinds;
pub mod parser;
pub mod tokenizer;
pub mod types;
pub mod value;

pub use self::{
    kinds::SyntaxKind,
    parser::AST,
    value::{StrPart, Value as NixValue},
};

pub use rowan::{
    SmolStr, SyntaxElementChildren, SyntaxNodeChildren,
    TextRange, TextUnit, TokenAtOffset, WalkEvent, NodeOrToken,
};

use self::tokenizer::Tokenizer;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NixLanguage {}

impl rowan::Language for NixLanguage {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::cursor::SyntaxKind) -> Self::Kind {
        let discriminant: u16 = raw.0;
        assert!(discriminant <= (SyntaxKind::__LAST as u16));
        unsafe { std::mem::transmute::<u16, SyntaxKind>(discriminant) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::cursor::SyntaxKind {
        rowan::cursor::SyntaxKind(kind as u16)
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
    use super::{parse, types::*, value::StrPart, NixValue};

    fn test(code: &str) {
        parse(code).as_result().expect("parsing error");
    }

    #[test]
    fn all() {
        test(include_str!("../test_data/dynamic-attrs.nix"));
        //test(include_str!("../test_data/error.nix"));
        test(include_str!("../test_data/ifs.nix"));
        test(include_str!("../test_data/inherit.nix"));
        test(include_str!("../test_data/interpolation.nix"));
        test(include_str!("../test_data/isset.nix"));
        test(include_str!("../test_data/lambdas.nix"));
        test(include_str!("../test_data/lists.nix"));
        test(include_str!("../test_data/math.nix"));
        test(include_str!("../test_data/meta.nix"));
        test(include_str!("../test_data/pattern.nix"));
        test(include_str!("../test_data/simple-set.nix"));
        test(include_str!("../test_data/with-import-let-in.nix"));
    }
    #[test]
    fn interpolation() {
        let ast = parse(include_str!("../test_data/interpolation.nix"));

        let let_in = ast.root().inner().and_then(LetIn::cast).unwrap();
        let set = let_in.body().and_then(Set::cast).unwrap();
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
        let ast = parse(include_str!("../test_data/inherit.nix"));

        let let_in = ast.root().inner().and_then(LetIn::cast).unwrap();
        let set = let_in.body().and_then(Set::cast).unwrap();
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
        let ast = parse(include_str!("../test_data/math.nix"));
        let root = ast.root().inner().and_then(Operation::cast).unwrap();
        let operation = root.value1().and_then(Operation::cast).unwrap();

        assert_eq!(root.operator(), OpKind::Add);
        assert_eq!(operation.operator(), OpKind::Add);

        let value = operation.value1().and_then(Value::cast).unwrap();
        assert_eq!(value.to_value(), Ok(NixValue::Integer(1)));
    }
    // #[test]
    // fn remove_pattern_entry() {
    //     let ast = parse("{\n  /* 1 */ a,\n  /* 2 */ b,\n  /* 3 */ c\n}:\na").as_result().unwrap();
    //     let lambda = Lambda::cast(ast.root().inner()).unwrap();
    //     let pattern = Pattern::cast(lambda.arg()).unwrap();

    //     let only_b = pattern.filter_entries(|entry| entry.name().as_str() == "b");
    //     assert_eq!(only_b.node().to_string(), "{\n  /* 2 */ b\n  }:\na");

    //     let without_b = pattern.filter_entries(|entry| entry.name().as_str() != "b");
    //     assert_eq!(without_b.node().to_string(), "{\n  /* 1 */ a,\n  /* 3 */ c\n}:\na");
    // }
    // #[test]
    // fn remove_set_entry() {
    //     let ast = parse("{\n  /* 1 */ a = 3;\n  /* 2 */ b = 2;\n  /* 3 */ c = 3;\n}").as_result().unwrap();
    //     let set = Set::cast(ast.root().inner()).unwrap();

    //     let only_b = set.filter_entries(|entry| {
    //         let key = entry.key();
    //         let mut path = key.path();
    //         Ident::cast(path.next().unwrap()).unwrap().as_str() == "b"
    //     });
    //     assert_eq!(only_b.node().to_string(), "{\n  /* 2 */ b = 2;\n  }");

    //     let without_b = set.filter_entries(|entry| {
    //         let key = entry.key();
    //         let mut path = key.path();
    //         Ident::cast(path.next().unwrap()).unwrap().as_str() != "b"
    //     });
    //     assert_eq!(without_b.node().to_string(), "{\n  /* 1 */ a = 3;\n  /* 3 */ c = 3;\n}");
    // }
}
