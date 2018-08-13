use crate::{
    tokenizer::{Interpol as TokenInterpol, Meta, Span, Token},
    value::Value
};
use std::iter::Peekable;

#[derive(Clone, Debug, Fail, PartialEq)]
pub enum ParseError {
    #[fail(display = "unexpected eof")]
    UnexpectedEOF,
    #[fail(display = "expected {:?}, found {:?}", _0, _1)]
    Expected(Token, Option<Token>),
    #[fail(display = "unexpected token {:?} not applicable in this context", _0)]
    Unexpected(Token)
}

pub type Set = Vec<(String, AST)>;
pub type SetNoSpan = Vec<(String, ASTNoSpan)>;

#[derive(Clone, Debug, PartialEq)]
pub struct AST(Meta, ASTType);

#[derive(Clone, Debug, PartialEq)]
pub enum Interpol {
    Literal(String),
    AST(AST)
}

#[derive(Clone, Debug, PartialEq)]
pub enum ASTType {
    Set {
        recursive: bool,
        values: Set
    },
    List(Vec<AST>),
    LetIn(Set, Box<AST>),
    With(Box<(AST, AST)>),
    Import(Box<AST>),
    Var(String),
    Interpol(Vec<Interpol>),
    IndexSet(Box<AST>, String),
    Concat(Box<(AST, AST)>),

    // Could also do Add(Box<AST>, Box<AST>), but I believe this is more
    // efficient.
    Negate(Box<AST>),
    Add(Box<(AST, AST)>),
    Sub(Box<(AST, AST)>),
    Mul(Box<(AST, AST)>),
    Div(Box<(AST, AST)>),
    Value(Value)
}

#[derive(Clone, Debug, PartialEq)]
pub enum InterpolNoSpan {
    Literal(String),
    AST(ASTNoSpan)
}

impl From<Interpol> for InterpolNoSpan {
    fn from(interpol: Interpol) -> Self {
        match interpol {
            Interpol::Literal(text) => InterpolNoSpan::Literal(text),
            Interpol::AST(ast) => InterpolNoSpan::AST(ASTNoSpan::from(ast))
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ASTNoSpan {
    Set {
        recursive: bool,
        values: SetNoSpan
    },
    List(Vec<ASTNoSpan>),
    LetIn(SetNoSpan, Box<ASTNoSpan>),
    With(Box<(ASTNoSpan, ASTNoSpan)>),
    Import(Box<ASTNoSpan>),
    Var(String),
    Interpol(Vec<InterpolNoSpan>),
    IndexSet(Box<ASTNoSpan>, String),
    Concat(Box<(ASTNoSpan, ASTNoSpan)>),

    Negate(Box<ASTNoSpan>),
    Add(Box<(ASTNoSpan, ASTNoSpan)>),
    Sub(Box<(ASTNoSpan, ASTNoSpan)>),
    Mul(Box<(ASTNoSpan, ASTNoSpan)>),
    Div(Box<(ASTNoSpan, ASTNoSpan)>),
    Value(Value)
}

fn set_discard_span(set: Set) -> SetNoSpan {
    set.into_iter()
        .map(|(name, ast)| (name, ASTNoSpan::from(ast)))
        .collect()
}
fn vec_into<F, T: From<F>>(vec: Vec<F>) -> Vec<T> {
    vec.into_iter()
        .map(|item| T::from(item))
        .collect()
}
fn discard_span(ast: Box<AST>) -> Box<ASTNoSpan> {
    Box::new((*ast).into())
}
fn tuple_discard_span(ast: Box<(AST, AST)>) -> Box<(ASTNoSpan, ASTNoSpan)> {
    Box::new((ast.0.into(), ast.1.into()))
}

impl From<AST> for ASTNoSpan {
    fn from(ast: AST) -> ASTNoSpan {
        match ast.1 {
            ASTType::Set { recursive, values } => ASTNoSpan::Set { recursive, values: set_discard_span(values) },
            ASTType::List(inner) => ASTNoSpan::List(vec_into(inner)),
            ASTType::LetIn(set, ast) => ASTNoSpan::LetIn(set_discard_span(set), discard_span(ast)),
            ASTType::With(inner) => ASTNoSpan::With(tuple_discard_span(inner)),
            ASTType::Import(inner) => ASTNoSpan::Import(discard_span(inner)),
            ASTType::Var(inner) => ASTNoSpan::Var(inner),
            ASTType::Interpol(inner) => ASTNoSpan::Interpol(vec_into(inner)),
            ASTType::IndexSet(set, key) => ASTNoSpan::IndexSet(discard_span(set), key),
            ASTType::Concat(inner) => ASTNoSpan::Concat(tuple_discard_span(inner)),

            ASTType::Negate(inner) => ASTNoSpan::Negate(discard_span(inner)),
            ASTType::Add(inner) => ASTNoSpan::Add(tuple_discard_span(inner)),
            ASTType::Sub(inner) => ASTNoSpan::Sub(tuple_discard_span(inner)),
            ASTType::Mul(inner) => ASTNoSpan::Mul(tuple_discard_span(inner)),
            ASTType::Div(inner) => ASTNoSpan::Div(tuple_discard_span(inner)),
            ASTType::Value(inner) => ASTNoSpan::Value(inner)
        }
    }
}

type Error = (Option<Span>, ParseError);
type Result<T> = std::result::Result<T, Error>;

macro_rules! math {
    ($self:expr, $next:block, $($token:pat => $ast:expr),*) => {{
        let mut val = { $next };
        loop {
            match $self.iter.peek() {
                $(Some(&(_, $token)) => {
                    $self.next()?;
                    let AST(end, expr) = { $next };
                    val = AST(val.0.span.until(end.span).into(), $ast(Box::new((val, AST(end, expr)))));
                },)*
                _ => break
            }
        }
        Ok(val)
    }}
}

pub struct Parser<I>
    where I: Iterator<Item = (Meta, Token)>
{
    iter: Peekable<I>
}
impl<I> Parser<I>
    where I: Iterator<Item = (Meta, Token)>
{
    pub fn new(iter: Peekable<I>) -> Self {
        Self { iter }
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.iter.peek().map(|(_, token)| token)
    }
    pub fn next(&mut self) -> Result<(Meta, Token)> {
        self.iter.next().ok_or((None, ParseError::UnexpectedEOF))
    }
    pub fn expect(&mut self, expected: Token) -> Result<Meta> {
        if let Some((meta, actual)) = self.iter.next() {
            if actual == expected {
                Ok(meta)
            } else {
                Err((Some(meta.span), ParseError::Expected(expected, Some(actual))))
            }
        } else {
            Err((None, ParseError::Expected(expected, None)))
        }
    }
    pub fn expect_peek(&mut self, expected: Token) -> Result<()> {
        if let Some((meta, actual)) = self.iter.peek() {
            if *actual == expected {
                Ok(())
            } else {
                Err((Some(meta.span), ParseError::Expected(expected, Some(actual.clone()))))
            }
        } else {
            Err((None, ParseError::Expected(expected, None)))
        }
    }

    pub fn parse_val(&mut self) -> Result<AST> {
        let mut next = match self.next()? {
            (_, Token::ParenOpen) => {
                let expr = self.parse_expr()?;
                self.expect(Token::ParenClose)?;
                expr
            },
            (start, Token::Rec) => {
                self.expect_peek(Token::CurlyBOpen)?;
                let AST(end, mut set) = self.parse_val()?;
                if let ASTType::Set { ref mut recursive, .. } = set {
                    *recursive = true;
                }
                AST(start.until(&end), set)
            },
            (start, Token::CurlyBOpen) => {
                let values = self.parse_set()?;
                let end = self.expect(Token::CurlyBClose)?;
                AST(start.until(&end), ASTType::Set {
                    recursive: false,
                    values
                })
            },
            (start, Token::SquareBOpen) => {
                let mut values = Vec::new();
                loop {
                    let peek = self.peek();
                    match peek {
                        None | Some(Token::SquareBClose) => break,
                        _ => values.push(self.parse_val()?)
                    }
                }
                let end = self.expect(Token::SquareBClose)?;
                AST(start.until(&end), ASTType::List(values))
            },
            (start, Token::Sub) => {
                let AST(end, expr) = self.parse_val()?;
                AST(start.until(&end), ASTType::Negate(Box::new(AST(end, expr))))
            },
            (meta, Token::Value(val)) => AST(meta, ASTType::Value(val)),
            (meta, Token::Ident(name)) => AST(meta, ASTType::Var(name)),
            (meta, Token::Interpol(values)) => {
                let mut parsed = Vec::new();
                for value in values {
                    parsed.push(match value {
                        TokenInterpol::Literal(text) => Interpol::Literal(text),
                        TokenInterpol::Tokens(tokens) => Interpol::AST(parse(tokens.into_iter())?)
                    });
                }
                AST(meta, ASTType::Interpol(parsed))
            },
            (meta, token) => return Err((Some(meta.span), ParseError::Unexpected(token)))
        };

        while self.peek() == Some(&Token::Dot) {
            self.next()?;
            if let (end, Token::Ident(ident)) = self.next()? {
                next = AST(next.0.span.until(end.span).into(), ASTType::IndexSet(Box::new(next), ident));
            }
        }

        Ok(next)
    }

    pub fn parse_mul(&mut self) -> Result<AST> {
        math!(
            self, { self.parse_val()? },
            Token::Mul => ASTType::Mul,
            Token::Div => ASTType::Div
        )
    }

    pub fn parse_add(&mut self) -> Result<AST> {
        math!(
            self, { self.parse_mul()? },
            Token::Add => ASTType::Add,
            Token::Sub => ASTType::Sub,
            Token::Concat => ASTType::Concat
        )
    }

    pub fn parse_set(&mut self) -> Result<Set> {
        let mut values = Vec::new();
        while let Some(&Token::Ident(_)) = self.peek() {
            let key = match self.next()? {
                (_, Token::Ident(name)) => name,
                _ => unreachable!()
            };
            self.expect(Token::Equal)?;
            let value = self.parse_expr()?;
            self.expect(Token::Semicolon)?;

            values.push((key, value));
        }
        Ok(values)
    }

    pub fn parse_expr(&mut self) -> Result<AST> {
        Ok(match self.peek() {
            Some(Token::Let) => {
                let (start, _) = self.next()?;
                let vars = self.parse_set()?;
                self.expect(Token::In)?;
                let AST(end, expr) = self.parse_expr()?;
                AST(start.until(&end), ASTType::LetIn(vars, Box::new(AST(end, expr))))
            },
            Some(Token::With) => {
                let (start, _) = self.next()?;
                let vars = self.parse_expr()?;
                self.expect(Token::Semicolon)?;
                let AST(end, expr) = self.parse_expr()?;
                AST(start.until(&end), ASTType::With(Box::new((vars, AST(end, expr)))))
            },
            Some(Token::Import) => {
                let (start, _) = self.next()?;
                let AST(end, expr) = self.parse_expr()?;
                AST(start.until(&end), ASTType::Import(Box::new(AST(end, expr))))
            },
            _ => self.parse_add()?
        })
    }
}

pub fn parse<I>(iter: I) -> Result<AST>
    where I: IntoIterator<Item = (Meta, Token)>
{
    Parser::new(iter.into_iter().peekable()).parse_expr()
}

#[cfg(test)]
mod tests {
    use crate::{
        tokenizer::{Interpol as TokenInterpol, Meta, Span, Token},
        value::{Anchor, Value}
    };
    use super::{AST as ASTSpan, ASTNoSpan as AST, ASTType, InterpolNoSpan as Interpol, ParseError};

    macro_rules! parse {
        ($($token:expr),*) => {
            super::parse(vec![$((Meta::default(), $token)),*].into_iter())
                .map(AST::from)
        }
    }

    #[test]
    fn set() {
        assert_eq!(
            parse![
                Token::CurlyBOpen,

                Token::Ident("meaning_of_life".into()), Token::Equal, Token::Value(42.into()), Token::Semicolon,
                Token::Ident("H4X0RNUM83R".into()), Token::Equal, Token::Value(1.337.into()), Token::Semicolon,

                Token::CurlyBClose
            ],
            Ok(AST::Set {
                recursive: false,
                values: vec![
                    ("meaning_of_life".into(), AST::Value(42.into())),
                    ("H4X0RNUM83R".into(), AST::Value(1.337.into()))
                ]
            })
        );
        assert_eq!(
            parse![
                Token::Rec, Token::CurlyBOpen,
                Token::Ident("test".into()), Token::Equal, Token::Value(1.into()), Token::Semicolon,
                Token::CurlyBClose
            ],
            Ok(AST::Set {
                recursive: true,
                values: vec![("test".into(), AST::Value(1.into()))]
            })
        );
    }
    #[test]
    fn meta() {
        assert_eq!(
            super::parse(vec![
                (Meta::default(), Token::CurlyBOpen),
                (meta! { start: (4, 2), end: None }, Token::Semicolon),
            ].into_iter()),
            Err((
                Some(Span { start: (4, 2), end: None }),
                ParseError::Expected(Token::CurlyBClose, Some(Token::Semicolon))
            ))
        );
        assert_eq!(
            super::parse(vec![
                (meta! { start: (0, 0), end: (0, 1) }, Token::Value(1.into())),
                (meta! { start: (0, 2), end: (0, 3) }, Token::Add),
                (
                    Meta {
                        comments: vec!["Hello World!".into()],
                        span: Span { start: (0, 4), end: Some((0, 5)) }
                    },
                    Token::Value(2.into())
                ),
                (meta! { start: (0, 6), end: (0, 7) }, Token::Mul),
                (meta! { start: (0, 8), end: (0, 9) }, Token::Value(3.into())),
            ].into_iter()),
            Ok(ASTSpan(
                meta! { start: (0, 0), end: (0, 9) },
                ASTType::Add(Box::new((
                    ASTSpan(
                        meta! { start: (0, 0), end: (0, 1) },
                        ASTType::Value(1.into())
                    ),
                    ASTSpan(
                        meta! { start: (0, 4), end: (0, 9) },
                        ASTType::Mul(Box::new((
                            ASTSpan(
                                Meta {
                                    comments: vec!["Hello World!".into()],
                                    span: Span { start: (0, 4), end: Some((0, 5)) }
                                },
                                ASTType::Value(2.into())
                            ),
                            ASTSpan(
                                meta! { start: (0, 8), end: (0, 9) },
                                ASTType::Value(3.into())
                            )
                        )))
                    )
                )))
            ))
        );
    }
    #[test]
    fn math() {
        assert_eq!(
            parse![
                Token::Value(1.into()), Token::Add, Token::Value(2.into()), Token::Mul, Token::Value(3.into())
            ],
            Ok(AST::Add(Box::new((
                AST::Value(1.into()),
                AST::Mul(Box::new((
                    AST::Value(2.into()),
                    AST::Value(3.into()),
                )))
            ))))
        );
        assert_eq!(
            parse![
                Token::Value(5.into()), Token::Mul,
                Token::Sub, Token::ParenOpen,
                    Token::Value(3.into()), Token::Sub, Token::Value(2.into()),
                Token::ParenClose
            ],
            Ok(AST::Mul(Box::new((
                AST::Value(5.into()),
                AST::Negate(Box::new(AST::Sub(Box::new((
                    AST::Value(3.into()),
                    AST::Value(2.into()),
                )))))
            ))))
        );
    }
    #[test]
    fn let_in() {
        assert_eq!(
            parse![
                Token::Let,
                    Token::Ident("a".into()), Token::Equal, Token::Value(42.into()), Token::Semicolon,
                Token::In,
                    Token::Ident("a".into())
            ],
            Ok(AST::LetIn(
                vec![("a".into(), AST::Value(42.into()))],
                Box::new(AST::Var("a".into()))
            ))
        );
    }
    #[test]
    fn with() {
        assert_eq!(
            parse![
                Token::With, Token::Ident("namespace".into()), Token::Semicolon,
                Token::Ident("expr".into())
            ],
            Ok(AST::With(Box::new((
                AST::Var("namespace".into()),
                AST::Var("expr".into())
            ))))
        );
    }
    #[test]
    fn import() {
        assert_eq!(
            parse![
                Token::Import,
                Token::Value(Value::Path(Anchor::Store, "nixpkgs".into()))
            ],
            Ok(AST::Import(Box::new(
                AST::Value(Value::Path(Anchor::Store, "nixpkgs".into()))
            )))
        );
    }
    #[test]
    fn index_set() {
        assert_eq!(
            parse![Token::Ident("hello".into()), Token::Dot, Token::Ident("world".into())],
            Ok(AST::IndexSet(
                Box::new(AST::Var("hello".into())),
                "world".into()
            ))
        );
    }
    #[test]
    fn interpolation() {
        assert_eq!(
            parse![
                Token::Interpol(vec![
                    TokenInterpol::Literal("Hello, ".into()),
                    TokenInterpol::Tokens(vec![
                        (meta! { start: (0, 12), end: (0, 13) }, Token::CurlyBOpen),
                        (meta! { start: (0, 14), end: (0, 19) }, Token::Ident("world".into())),
                        (meta! { start: (0, 20), end: (0, 21) }, Token::Equal),
                        (meta! { start: (0, 22), end: (0, 29) }, Token::Value("World".into())),
                        (meta! { start: (0, 29), end: (0, 30) }, Token::Semicolon),
                        (meta! { start: (0, 31), end: (0, 32) }, Token::CurlyBClose),
                        (meta! { start: (0, 32), end: (0, 33) }, Token::Dot),
                        (meta! { start: (0, 33), end: (0, 38) }, Token::Ident("world".into()))
                    ]),
                    TokenInterpol::Literal("!".into())
                ])
            ],
            Ok(AST::Interpol(vec![
                Interpol::Literal("Hello, ".into()),
                Interpol::AST(AST::IndexSet(
                    Box::new(AST::Set {
                        recursive: false,
                        values: vec![("world".into(), AST::Value("World".into()))]
                    }),
                    "world".into()
                )),
                Interpol::Literal("!".into())
            ]))
        );
    }
    #[test]
    fn list() {
        assert_eq!(
            parse![
               Token::SquareBOpen,
               Token::Ident("a".into()), Token::Value(2.into()), Token::Value(3.into()),
               Token::Value("lol".into()),
               Token::SquareBClose
            ],
            Ok(AST::List(vec![
                AST::Var("a".into()), AST::Value(2.into()), AST::Value(3.into()),
                AST::Value("lol".into())
            ]))
        );
        assert_eq!(
            parse![
               Token::SquareBOpen, Token::Value(1.into()), Token::SquareBClose, Token::Concat,
               Token::SquareBOpen, Token::Value(2.into()), Token::SquareBClose, Token::Concat,
               Token::SquareBOpen, Token::Value(3.into()), Token::SquareBClose
            ],
            Ok(AST::Concat(Box::new((
                AST::Concat(Box::new((
                    AST::List(vec![AST::Value(1.into())]),
                    AST::List(vec![AST::Value(2.into())]),
                ))),
                AST::List(vec![AST::Value(3.into())])
            ))))
        );
    }
}
