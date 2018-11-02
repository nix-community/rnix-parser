#[macro_use]
extern crate failure;

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
            if s1 == "\n\n    The set\'s x value is: "
                && s2 == "\n\n    This line shall have no indention\n      This line shall be indented by 2\n    \n\n    "
                && s3 == "\n" => (),
            parts => panic!("did not match: {:#?}", parts)
        }
    }
}
