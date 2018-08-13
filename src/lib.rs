#![feature(rust_2018_preview)]

#[macro_use]
extern crate failure;

#[cfg(test)]
macro_rules! meta {
    (start: $start:expr, end: None) => {{
        Meta { span: Span { start: $start, end: None }, comments: Vec::new() }
    }};
    (start: $start:expr, end: $end:expr) => {{
        Meta { span: Span { start: $start, end: Some($end) }, comments: Vec::new() }
    }};
}

pub mod nometa;
pub mod parser;
pub mod tokenizer;
#[macro_use]
pub mod utils;
pub mod value;

#[cfg(test)]
mod tests {
    use super::{nometa::*, parser::parse as inner_parse, tokenizer::tokenize};

    fn parse(string: &str) -> AST {
        AST::from(inner_parse(
            tokenize(string).map(|result| result.expect("error while tokenizing"))
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

                    legacy = (let {
                        this_syntax_sucks = (true);
                        body = (this_syntax_sucks);
                    });
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
        );
        assert_eq!(
            parse(include_str!("../tests/comments.nix")),
            nix!({
                string = ("42");
                password = ("hunter2");
            })
        );
        assert_eq!(
            parse(include_str!("../tests/lists.nix")),
            nix!({
                thing = ([(1) (2) (3) ((2) + (2))]);
                concat = ((([(1)]) ++ ([(2) (3)])) ++ ([(4)]));
            })
        );
        assert_eq!(
            parse(include_str!("../tests/lambdas.nix")),
            nix!(let {
                add = (x: y: (x) + (y));
            } in {
                result = (((add) (2)) (5));
            })
        );
        assert_eq!(
            parse(include_str!("../tests/pattern.nix")),
            nix!(
                { a ? ({ b ? ("test") }: b) }:
                bind @ { (exact = false) value }:
                1
            )
        );
    }
}
