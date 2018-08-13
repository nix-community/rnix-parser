use crate::{
    tokenizer::{Interpol as TokenInterpol, Meta, Span, Token},
    utils::stack::Stack,
    value::Value
};

#[derive(Clone, Debug, Fail, PartialEq)]
pub enum ParseError {
    #[fail(display = "expected {:?}, found {:?}", _0, _1)]
    Expected(Token, Option<Token>),
    #[fail(display = "expected {}, found {:?}", _0, _1)]
    ExpectedType(&'static str, Token),
    #[fail(display = "invalid type! expected {}", _0)]
    InvalidType(&'static str),
    #[fail(display = "unexpected eof")]
    UnexpectedEOF,
    #[fail(display = "unexpected token {:?} not applicable in this context", _0)]
    Unexpected(Token)
}

#[derive(Clone, Debug, PartialEq)]
pub struct AST(pub Meta, pub ASTType);
#[derive(Clone, Debug, PartialEq)]
pub enum ASTType {
    // Types
    EmptySet,
    Interpol(Vec<Interpol>),
    Lambda(FnArg, Box<AST>),
    List(Vec<AST>),
    Set {
        recursive: bool,
        values: Vec<SetEntry>
    },
    Value(Value),
    Var(String),

    // Expressions
    Import(Box<AST>),
    Let(Vec<SetEntry>),
    LetIn(Vec<SetEntry>, Box<AST>),
    With(Box<(AST, AST)>),

    // Operators
    Apply(Box<(AST, AST)>),
    Concat(Box<(AST, AST)>),
    IndexSet(Box<AST>, String),
    Negate(Box<AST>),

    Add(Box<(AST, AST)>),
    Sub(Box<(AST, AST)>),
    Mul(Box<(AST, AST)>),
    Div(Box<(AST, AST)>)
}
#[derive(Clone, Debug, PartialEq)]
pub enum FnArg {
    Ident(String),
    Pattern {
        args: Vec<PatEntry>,
        bind: Option<String>,
        exact: bool
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum Interpol {
    Literal(String),
    AST(AST)
}
#[derive(Clone, Debug, PartialEq)]
pub struct PatEntry(pub String, pub Option<AST>);
#[derive(Clone, Debug, PartialEq)]
pub struct SetEntry(pub Vec<String>, pub AST);

type Error = (Option<Span>, ParseError);
type Result<T> = std::result::Result<T, Error>;

macro_rules! math {
    ($self:expr, $next:block, $($token:pat => $ast:expr),*) => {{
        let mut val = { $next };
        loop {
            match $self.peek() {
                $(Some(&$token) => {
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
    iter: I,
    buffer: Stack<I::Item>,
}
impl<I> Parser<I>
    where I: Iterator<Item = (Meta, Token)>
{
    pub fn new(iter: I) -> Self {
        Self {
            iter,
            // Can't use [None; 2] because I::Item isn't Copy
            buffer: Stack::new([None, None])
        }
    }

    fn peek_meta(&mut self) -> Option<&(Meta, Token)> {
        if self.buffer.is_empty() {
            *self.buffer.first_free() = self.iter.next();
        }
        self.buffer.peek()
    }
    fn peek(&mut self) -> Option<&Token> {
        self.peek_meta().map(|(_, token)| token)
    }
    fn next(&mut self) -> Result<I::Item> {
        self.buffer.pop()
            .or_else(|| self.iter.next())
            .ok_or((None, ParseError::UnexpectedEOF))
    }
    fn expect(&mut self, expected: Token) -> Result<Meta> {
        if let Ok((meta, actual)) = self.next() {
            if actual == expected {
                Ok(meta)
            } else {
                Err((Some(meta.span), ParseError::Expected(expected, Some(actual))))
            }
        } else {
            Err((None, ParseError::Expected(expected, None)))
        }
    }
    fn expect_peek(&mut self, expected: Token) -> Result<()> {
        if let Some((meta, actual)) = self.peek_meta() {
            if *actual == expected {
                Ok(())
            } else {
                Err((Some(meta.span), ParseError::Expected(
                    expected,
                    Some(self.next().unwrap().1)
                )))
            }
        } else {
            Err((None, ParseError::Expected(expected, None)))
        }
    }

    fn next_ident(&mut self) -> Result<(Meta, String)> {
        match self.next()? {
            (meta, Token::Ident(name)) => Ok((meta, name)),
            (meta, token) => Err((Some(meta.span), ParseError::ExpectedType("ident", token)))
        }
    }
    fn parse_ident(&mut self) -> Result<Vec<String>> {
        let mut ident = Vec::with_capacity(1);
        loop {
            ident.push(self.next_ident()?.1);

            if self.peek() != Some(&Token::Dot) {
                break;
            }
            self.next().unwrap();
        }
        Ok(ident)
    }
    fn parse_val(&mut self) -> Result<AST> {
        let mut val = match self.next()? {
            (_, Token::ParenOpen) => {
                let expr = self.parse_expr()?;
                self.expect(Token::ParenClose)?;
                expr
            },
            (start, Token::Rec) => {
                self.expect_peek(Token::CurlyBOpen)?;
                let AST(end, mut set) = self.parse_val()?;
                match set {
                    ASTType::Set { ref mut recursive, .. } => *recursive = true,
                    ASTType::EmptySet => (),
                    _ => return Err((Some(end.span), ParseError::InvalidType("set")))
                }
                AST(start.until(&end), set)
            },
            (start, Token::CurlyBOpen) => {
                if self.peek() == Some(&Token::CurlyBClose) {
                    let (end, _) = self.next().unwrap();
                    return Ok(AST(start.until(&end), ASTType::EmptySet));
                }
                let (ident_span, mut ident) = self.next_ident()?;
                match self.peek() {
                    Some(Token::Comma) | Some(Token::Question) => {
                        let mut args = Vec::with_capacity(1);
                        let mut exact = true;
                        loop {
                            match self.next()? {
                                (_, Token::Comma) => args.push(PatEntry(ident, None)),
                                (_, Token::Question) => {
                                    args.push(PatEntry(ident, Some(self.parse_expr()?)));
                                    match self.peek_meta() {
                                        Some((_, Token::CurlyBClose)) => (),
                                        Some((_, Token::Comma)) => { self.next().unwrap(); },
                                        Some((meta, _)) => return Err((
                                            Some(meta.span),
                                            ParseError::ExpectedType("comma or end", self.next().unwrap().1)
                                        )),
                                        None => return Err((Some(start.span), ParseError::UnexpectedEOF))
                                    }
                                },
                                (meta, token) => return Err((Some(meta.span), ParseError::Unexpected(token)))
                            }
                            ident = match self.peek() {
                                Some(&Token::Ident(_)) => self.next_ident()?.1,
                                Some(&Token::Ellipsis) => {
                                    self.next().unwrap();
                                    exact = false;
                                    break;
                                },
                                _ => break
                            }
                        }
                        self.expect(Token::CurlyBClose)?;

                        let bind = if self.peek() == Some(&Token::At) {
                            self.next().unwrap();
                            Some(self.next_ident()?.1)
                        } else {
                            None
                        };

                        self.expect(Token::Colon)?;

                        let AST(end, expr) = self.parse_expr()?;

                        AST(start.until(&end), ASTType::Lambda(
                            FnArg::Pattern { args, bind, exact },
                            Box::new(AST(end, expr))
                        ))
                    },
                    _ => {
                        // We did a lookahead, put it back
                        *self.buffer.first_free() = Some((ident_span, Token::Ident(ident)));

                        let values = self.parse_set()?;
                        let end = self.expect(Token::CurlyBClose)?;
                        AST(start.until(&end), ASTType::Set {
                            recursive: false,
                            values
                        })
                    }
                }
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
                val = AST(
                    val.0.span.until(end.span).into(),
                    ASTType::IndexSet(Box::new(val), ident)
                );
            }
        }

        Ok(val)
    }
    fn parse_fn(&mut self) -> Result<AST> {
        let mut val = self.parse_val()?;

        while self.peek().map(|t| t.is_fn_arg()).unwrap_or(false) {
            let arg = self.parse_val()?;
            val = AST(
                val.0.span.until(arg.0.span).into(),
                ASTType::Apply(Box::new((val, arg)))
            );
        }

        Ok(val)
    }
    fn parse_mul(&mut self) -> Result<AST> {
        math!(
            self, { self.parse_fn()? },
            Token::Mul => ASTType::Mul,
            Token::Div => ASTType::Div
        )
    }
    fn parse_add(&mut self) -> Result<AST> {
        math!(
            self, { self.parse_mul()? },
            Token::Add => ASTType::Add,
            Token::Sub => ASTType::Sub,
            Token::Concat => ASTType::Concat
        )
    }
    fn parse_set(&mut self) -> Result<Vec<SetEntry>> {
        let mut values = Vec::new();
        while let Some(&Token::Ident(_)) = self.peek() {
            let key = self.parse_ident()?;
            self.expect(Token::Equal)?;
            let value = self.parse_expr()?;
            self.expect(Token::Semicolon)?;

            values.push(SetEntry(key, value));
        }
        Ok(values)
    }
    pub fn parse_expr(&mut self) -> Result<AST> {
        Ok(match self.peek() {
            Some(Token::Let) => {
                let (start, _) = self.next()?;
                if self.peek() == Some(&Token::CurlyBOpen) {
                    self.next()?;
                    let vars = self.parse_set()?;
                    let end = self.expect(Token::CurlyBClose)?;
                    AST(start.until(&end), ASTType::Let(vars))
                } else {
                    let vars = self.parse_set()?;
                    self.expect(Token::In)?;
                    let AST(end, expr) = self.parse_expr()?;
                    AST(start.until(&end), ASTType::LetIn(vars, Box::new(AST(end, expr))))
                }
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
            _ => match self.parse_add()? {
                AST(start, ASTType::Var(name)) => if self.peek() == Some(&Token::Colon) {
                    self.next()?;
                    let AST(end, expr) = self.parse_expr()?;
                    AST(start.until(&end), ASTType::Lambda(FnArg::Ident(name), Box::new(AST(end, expr))))
                } else {
                    AST(start, ASTType::Var(name))
                },
                ast => ast
            }
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
        nometa::*,
        tokenizer::{Interpol as TokenInterpol, Meta, Span, Token},
        value::{Anchor, Value}
    };
    use super::{AST as ASTSpan, ASTType, ParseError};

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
                    SetEntry(vec!["meaning_of_life".into()], AST::Value(42.into())),
                    SetEntry(vec!["H4X0RNUM83R".into()], AST::Value(1.337.into()))
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
                values: vec![SetEntry(vec!["test".into()], AST::Value(1.into()))]
            })
        );
        assert_eq!(
            parse![Token::CurlyBOpen, Token::CurlyBClose],
            Ok(AST::EmptySet)
        );
        assert_eq!(
            parse![Token::Rec, Token::CurlyBOpen, Token::CurlyBClose],
            Ok(AST::EmptySet)
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
                ParseError::ExpectedType("ident", Token::Semicolon)
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
                vec![SetEntry(vec!["a".into()], AST::Value(42.into()))],
                Box::new(AST::Var("a".into()))
            ))
        );
    }
    #[test]
    fn let_legacy_syntax() {
        assert_eq!(
            parse![
                Token::Let, Token::CurlyBOpen,
                    Token::Ident("a".into()), Token::Equal, Token::Value(42.into()), Token::Semicolon,
                    Token::Ident("body".into()), Token::Equal, Token::Ident("a".into()), Token::Semicolon,
                Token::CurlyBClose
            ],
            Ok(AST::Let(vec![
                SetEntry(vec!["a".into()], AST::Value(42.into())),
                SetEntry(vec!["body".into()], AST::Var("a".into()))
            ]))
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
            parse![
                Token::Ident("a".into()),
                Token::Dot, Token::Ident("b".into()),
                Token::Dot, Token::Ident("c".into())
            ],
            Ok(AST::IndexSet(
                Box::new(AST::IndexSet(
                    Box::new(AST::Var("a".into())),
                    "b".into()
                )),
                "c".into()
            ))
        );
        assert_eq!(
            parse![
                Token::CurlyBOpen,
                    Token::Ident("a".into()),
                        Token::Dot, Token::Ident("b".into()),
                        Token::Dot, Token::Ident("c".into()),
                    Token::Equal, Token::Value(1.into()), Token::Semicolon,
                Token::CurlyBClose
            ],
            Ok(AST::Set {
                recursive: false,
                values: vec![
                    SetEntry(vec!["a".into(), "b".into(), "c".into()], AST::Value(1.into()))
                ]
            })
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
                        values: vec![SetEntry(vec!["world".into()], AST::Value("World".into()))]
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
    #[test]
    fn functions() {
        assert_eq!(
            parse![
               Token::Ident("a".into()), Token::Colon, Token::Ident("b".into()), Token::Colon,
               Token::Ident("a".into()), Token::Add, Token::Ident("b".into())
            ],
            Ok(AST::Lambda(
                FnArg::Ident("a".into()),
                Box::new(AST::Lambda(
                    FnArg::Ident("b".into()),
                    Box::new(AST::Add(Box::new((
                        AST::Var("a".into()),
                        AST::Var("b".into())
                    ))))
                ))
            ))
        );
        assert_eq!(
            parse![
                Token::Ident("a".into()), Token::Value(1.into()), Token::Value(2.into()),
                Token::Add,
                Token::Value(3.into())
            ],
            Ok(AST::Add(Box::new((
                AST::Apply(Box::new((
                    AST::Apply(Box::new((
                        AST::Var("a".into()),
                        AST::Value(1.into())
                    ))),
                    AST::Value(2.into()),
                ))),
                AST::Value(3.into())
            ))))
        );
    }
    #[test]
    fn patterns() {
        assert_eq!(
            parse![
               Token::CurlyBOpen,
                   Token::Ident("a".into()), Token::Comma,
                   Token::Ident("b".into()), Token::Question, Token::Value("default".into()), Token::Comma,
                   Token::Ellipsis,
               Token::CurlyBClose,
               Token::At,
               Token::Ident("outer".into()),
               Token::Colon,
               Token::Ident("outer".into())
            ],
            Ok(AST::Lambda(
                FnArg::Pattern {
                    args: vec![
                        PatEntry("a".into(), None),
                        PatEntry("b".into(), Some(AST::Value("default".into()))),
                    ],
                    bind: Some("outer".into()),
                    exact: false
                },
                Box::new(AST::Var("outer".into()))
            ))
        )
    }
}
