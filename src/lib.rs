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
        parser::{parse as inner_parse, ASTNoSpan as AST, InterpolNoSpan as Interpol},
        tokenizer::tokenize
    };

    fn parse(string: &str) -> AST {
        AST::from(inner_parse(
            tokenize(string)
                .map(|(span, result)| (span, result.expect("error while tokenizing")))
        ).expect("error while parsing"))
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
\'\'\'\'\
"#);
            })
        );
        assert_eq!(
            parse(include_str!("../tests/math.nix")),
            nix!(((1) + ((2) * (3))) + ((4) / ((5) - (6))))
        );
        assert_eq!(
            parse(include_str!("../tests/with-import-let-in.nix")),
            nix!(
                with (import (./"simple-set.nix"));

                let {
                    a = ((4) + (2));
                } in {
                    b = ((a) + (2));
                }
            )
        );
        assert_eq!(
            parse(include_str!("../tests/interpolation.nix")),
            nix!(
                let {
                    world = ("World");
                } in {
                    string = (raw (AST::Interpol(vec![
                        Interpol::Literal("Hello ".into()),
                        Interpol::AST(AST::Var("world".into())),
                        Interpol::Literal("!".into()),
                    ])));
                    multiline = (raw (AST::Interpol(vec![
                        Interpol::Literal("The set's x value is: ".into()),
                        Interpol::AST(nix!(
                            ({
                                x = ("1");
                                y = ("2");
                            }).x
                        ))
                    ])));
                }
            )
        )
    }
}
