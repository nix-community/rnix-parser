#![feature(rust_2018_preview)]

#[macro_use]
extern crate failure;

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
            AST::Set(vec![
                ("int".into(), AST::Value(3.into())),
                ("float".into(), AST::Value(2.1.into())),
                ("string".into(), AST::Value("Hello World".into())),
                (
                    "multiline".into(),
                    AST::Value(r#"This is a
multiline
string :D
\'\'\'\'\"#.into())
                )
            ])
        );
    }
}
