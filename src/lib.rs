#[macro_use]
extern crate failure;
extern crate rowan;

pub mod parser;
pub mod tokenizer;
pub mod types;
pub mod value;

use self::{
    parser::AST,
    tokenizer::Tokenizer
};

/// A convenience function for first tokenizing and then parsing given input
pub fn parse(input: &str) -> AST {
    parser::parse(Tokenizer::new(input))
}

#[cfg(test)]
mod tests {
    use super::{
        types::*,
        parse
    };

    fn test(code: &str) {
        parse(code).as_result().expect("parsing error");
    }

    #[test]
    fn all() {
        test(include_str!("../tests/dynamic-attrs.nix"));
        //test(include_str!("../tests/error.nix"));
        test(include_str!("../tests/ifs.nix"));
        test(include_str!("../tests/inherit.nix"));
        test(include_str!("../tests/interpolation.nix"));
        test(include_str!("../tests/isset.nix"));
        test(include_str!("../tests/lambdas.nix"));
        test(include_str!("../tests/lists.nix"));
        test(include_str!("../tests/math.nix"));
        test(include_str!("../tests/meta.nix"));
        test(include_str!("../tests/pattern.nix"));
        test(include_str!("../tests/simple-set.nix"));
        test(include_str!("../tests/with-import-let-in.nix"));
    }
    #[test]
    fn interpolation() {
        let ast = parse(include_str!("../tests/interpolation.nix"));

        let let_in = LetIn::cast(ast.root().inner()).unwrap();
        let set = Set::cast(let_in.body()).unwrap();
        let entry = set.entries().nth(1).unwrap();
        let value = Interpol::cast(entry.value()).unwrap();

        match &*value.parts() {
            &[
                InterpolPart::Literal(ref s1),
                InterpolPart::Ast(_),
                InterpolPart::Literal(ref s2),
                InterpolPart::Ast(_),
                InterpolPart::Literal(ref s3)
            ]
            if s1 == "The set\'s x value is: "
                && s2 == "\n\nThis line shall have no indention\n  This line shall be indented by 2\n\n\n"
                && s3 == "\n" => (),
            parts => panic!("did not match: {:#?}", parts)
        }
    }
    #[test]
    fn remove_pattern_entry() {
        let ast = parse("{\n  /* 1 */ a,\n  /* 2 */ b,\n  /* 3 */ c\n}:\na").as_result().unwrap();
        let lambda = Lambda::cast(ast.root().inner()).unwrap();
        let pattern = Pattern::cast(lambda.arg()).unwrap();

        let only_b = pattern.filter_entries(|entry| entry.name().as_str() == "b");
        assert_eq!(only_b.node().to_string(), "{\n  /* 2 */ b\n  }:\na");

        let without_b = pattern.filter_entries(|entry| entry.name().as_str() != "b");
        assert_eq!(without_b.node().to_string(), "{\n  /* 1 */ a,\n  /* 3 */ c\n}:\na");
    }
    #[test]
    fn remove_set_entry() {
        let ast = parse("{\n  /* 1 */ a = 3;\n  /* 2 */ b = 2;\n  /* 3 */ c = 3;\n}").as_result().unwrap();
        let set = Set::cast(ast.root().inner()).unwrap();

        let only_b = set.filter_entries(|entry| {
            let key = entry.key();
            let mut path = key.path();
            Ident::cast(path.next().unwrap()).unwrap().as_str() == "b"
        });
        assert_eq!(only_b.node().to_string(), "{\n  /* 2 */ b = 2;\n  }");

        let without_b = set.filter_entries(|entry| {
            let key = entry.key();
            let mut path = key.path();
            Ident::cast(path.next().unwrap()).unwrap().as_str() != "b"
        });
        assert_eq!(without_b.node().to_string(), "{\n  /* 1 */ a = 3;\n  /* 3 */ c = 3;\n}");
    }
}
