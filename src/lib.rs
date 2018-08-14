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

use self::{
    parser::ParseError,
    tokenizer::{Meta, Span, Token, TokenizeError}
};

#[derive(Clone, Debug, Fail, PartialEq)]
pub enum Error {
    #[fail(display = "parse error: {}", _1)]
    ParseError(Option<Span>, #[cause] ParseError),
    #[fail(display = "parse error: {}", _1)]
    TokenizeError(Span, #[cause] TokenizeError)
}
impl From<(Span, TokenizeError)> for Error {
    fn from(err: (Span, TokenizeError)) -> Self {
        let (span, err) = err;
        Error::TokenizeError(span, err)
    }
}
impl From<(Option<Span>, ParseError)> for Error {
    fn from(err: (Option<Span>, ParseError)) -> Self {
        let (span, err) = err;
        Error::ParseError(span, err)
    }
}

pub fn parse(input: &str) -> Result<parser::AST, Error> {
    let tokens: Result<Vec<(Meta, Token)>, (Span, TokenizeError)> = tokenizer::tokenize(input).collect();
    if input.contains("define") {
        println!("{:#?}", tokens);
    }
    parser::parse(tokens?).map_err(Error::from)
}

#[cfg(test)]
mod tests {
    use super::{nometa::*, parse as inner_parse};

    fn parse(input: &str) -> AST {
        inner_parse(input).expect("error while parsing").into()
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
        assert_eq!(
            parse(include_str!("../tests/ifs.nix")),
            nix!(
                { value ? (null), life }:
                {
                  x = (if ((value) != (null))
                        then (if ((value) <= (5))
                          then (assert ((value) >= (0)); value)
                          else 5)
                        else if (!life)
                          then (1337)
                          else 42);
                }
            )
        );
        assert_eq!(
            parse(include_str!("../tests/inherit.nix")),
            nix!(let {
              y = (2);
              set = ({ z = (3); a = (4); b = (5); });
            } in {
              x = (1);
              inherit y;
              inherit (set) z a;
            })
        );
        assert_eq!(
            parse(include_str!("../tests/dynamic-attrs.nix")),
            nix!(let {
              define = (name: val: { (name) = (val); });
              key = ("hello");
            } in (((define) ("key")) ("value")) merge ({
                (raw AST::Interpol(vec![Interpol::AST(AST::Var("key".into()))])) (world) = (":D");
            }))
        );
    }
}
