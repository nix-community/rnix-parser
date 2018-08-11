#![feature(rust_2018_preview)]

#[macro_use]
extern crate failure;

#[macro_use]
pub mod macros;
pub mod parser;
pub mod tokenizer;
pub mod value;

#[cfg(test)]
mod tests {
    use super::{
        parser::{parse as inner_parse, AST},
        tokenizer::tokenize
    };

    fn parse(string: &str) -> AST {
        inner_parse(
            tokenize(string.chars())
                .map(|(span, result)| (span, result.expect("error while tokenizing")))
        ).expect("error while parsing")
    }

    #[test]
    fn test_all() {
        assert_eq!(
            parse(include_str!("../tests/simple-set.nix")),
            nix!({
                int = (3);
                float = (2.1);
                string = ("Hello World");
                multiline = (r#"This is a
multiline
string :D
\'\'\'\'\"#);
            })
        );
        assert_eq!(
            parse(include_str!("../tests/math.nix")),
            nix!(((1) + ((2) * (3))) + ((4) / ((5) - (6))))
        );
    }
}
