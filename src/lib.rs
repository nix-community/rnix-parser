#![feature(catch_expr)]

#[macro_use]
extern crate failure;

#[cfg(test)]
macro_rules! meta {
    (start: $start:expr, end: None) => {{
        Meta { span: Span { start: $start, end: None }, ..Default::default() }
    }};
    (start: $start:expr, end: $end:expr) => {{
        Meta { span: Span { start: $start, end: Some($end) }, ..Default::default() }
    }};
    (start: $start:expr, end: $end:expr, trailing: $amount:expr) => {{
        Meta { span: Span { start: $start, end: Some($end) }, trailing: vec![Trivia::Spaces($amount)], ..Default::default() }
    }};
}

#[macro_use] crate mod utils;
pub mod parser;
pub mod tokenizer;
pub mod value;

// only `impl`s, no need to expose the module
mod display;

use self::{
    parser::ParseError,
    tokenizer::{Meta, Span, Token, TokenizeError}
};

/// An error during either tokenizing or parsing
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

/// A convenience function for first tokenizing and then parsing.
///
/// Note: This is not lazy. It tokenizes first and parses later.
/// It is perhaps more efficient to tokenize one at a time before parsing, but
/// by how much is unclear. If you need every ounce of speed you can get, rnix'
/// other functions allow you not only to tokenize/parse lazily, but also do it
/// in paralell.
pub fn parse(input: &str) -> Result<parser::AST<'static>, Error> {
    let tokens: Result<Vec<(Meta, Token)>, (Span, TokenizeError)> = tokenizer::tokenize(input).collect();
    parser::parse(tokens?).map_err(Error::from)
}

#[cfg(test)]
mod tests {
    use super::{
        parse as inner_parse,
        parser::ParseError,
        parser::intoactualslowtree::*,
        tokenizer::{Span, TokenKind}
    };

    fn parse(input: &str) -> AST {
        let mut ast = inner_parse(input).expect("error while parsing");
        AST::into_tree(ast.root, &mut ast.arena)
    }

    #[test]
    fn test_preserve() {
        assert_eq!(
            inner_parse(include_str!("../tests/meta.nix")).expect("error while parsing").to_string(),
            include_str!("../tests/meta.nix")
        );
    }
    #[test]
    fn test_all() {
        assert_eq!(
            parse(include_str!("../tests/simple-set.nix")),
            nix!({
                int = (3);
                float = (2.1);
                string = ("Hello World");
                multiline = (multiline
original = r#"
    This is a multiline string :D
    \'\'\'\'\
    multiline = '''
      Two single quotes: '''',
      Interpolation: ''${test},
      Escape interpolation: '''''${test}
    ''';
    special escape: $${test}
  "#,
r#"This is a multiline string :D
\'\'\'\'\
multiline = ''
  Two single quotes: ''',
  Interpolation: ${test},
  Escape interpolation: ''${test}
'';
special escape: $${test}
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
                    string = (raw (AST::Interpol {
                        multiline: false,
                        parts: vec![
                            Interpol::Literal("Hello ".into()),
                            Interpol::AST(AST::Var("world".into())),
                            Interpol::Literal("!".into()),
                        ]
                    }));
                    multiline = (raw (AST::Interpol {
                        multiline: true,
                        parts: vec![
                            Interpol::Literal("The set's x value is: ".into()),
                            Interpol::AST(nix!(
                                ({
                                    x = ("1");
                                    y = ("2");
                                }).x
                            )),
                            Interpol::Literal("\n".into())
                        ]
                    }));
                }
            )
        );
        assert_eq!(
            parse(include_str!("../tests/meta.nix")),
            nix!({
                string = ("42");
                hi = (raw AST::Value(Value::Float("3.0000000".into())));
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
                bind @ { (ellipsis = true) value }:
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

                  enabled = (false);
                  value = (null);
                  valid = (assert ((enabled) -> ((value) != (null))); true);
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
                define = (name: val: { (dyn {name}) = (val); });
                key = ("hello");
                set = ({ a = (1); b = (2); });
                dynamic_key = ("c");
            } in (((define) ("key")) ("value")) merge ({
                    (raw AST::Interpol {
                        multiline: false,
                        parts: vec![Interpol::AST(AST::Var("key".into()))]
                    }).(world) = (":D");
                    dynamic_key_set = ((set) ? (dyn {dynamic_key}));
            }))
        );
        assert_eq!(
            parse(include_str!("../tests/isset.nix")),
            nix!(rec {
                x = (({ a = (1); }) ? (b));
                y = (({ b = (2); }).(c) or (5));
            })
        );
        assert_eq!(
            parse(include_str!("../tests/error.nix")),
            nix!({
                test = ({
                    valid = ("entry");
                    error (
                        Some(Span { start: 46, end: Some(53) }),
                        ParseError::Expected(TokenKind::Assign, Some(TokenKind::Value))
                    );
                    another = ("valid entry");
                });
                test2 = ({
                    hello = ("hi");
                    error (
                        Some(Span { start: 130, end: Some(131) }),
                        ParseError::Expected(TokenKind::Assign, Some(TokenKind::CurlyBClose))
                    );
                });
            })
        );
    }
}
