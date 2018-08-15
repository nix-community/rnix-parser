//! The parser: turns a series of tokens into an AST

pub mod nometa;
pub mod recurse;

use crate::{
    tokenizer::{Interpol as TokenInterpol, Meta, Span, Token},
    utils::stack::Stack,
    value::Value
};

const OR: &'static str = "or";

/// An error that occured during parsing
#[derive(Clone, Debug, Fail, PartialEq)]
pub enum ParseError {
    #[fail(display = "can't bind pattern here, already bound before")]
    AlreadyBound,
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

/// An AST node, with metadata
#[derive(Clone, Debug, PartialEq)]
pub struct AST(pub Meta, pub ASTType);
/// An AST node type
#[derive(Clone, Debug, PartialEq)]
pub enum ASTType {
    // Types
    Interpol {
        multiline: bool,
        parts: Vec<Interpol>
    },
    Lambda(LambdaArg, Box<AST>),
    List(Vec<AST>),
    Set {
        recursive: bool,
        values: Vec<SetEntry>
    },
    Value(Value),
    Var(String),

    // Expressions
    Assert(Box<(AST, AST)>),
    IfElse(Box<(AST, AST, AST)>),
    Import(Box<AST>),
    Let(Vec<SetEntry>),
    LetIn(Vec<SetEntry>, Box<AST>),
    With(Box<(AST, AST)>),

    // Operators
    Apply(Box<(AST, AST)>),
    Concat(Box<(AST, AST)>),
    Dynamic(Box<AST>),
    IndexSet(Box<(AST, AST)>),
    Invert(Box<AST>),
    IsSet(Box<(AST, AST)>),
    Merge(Box<(AST, AST)>),
    Negate(Box<AST>),
    OrDefault(Box<(AST, AST, AST)>),

    Add(Box<(AST, AST)>),
    Sub(Box<(AST, AST)>),
    Mul(Box<(AST, AST)>),
    Div(Box<(AST, AST)>),

    And(Box<(AST, AST)>),
    Equal(Box<(AST, AST)>),
    Implication(Box<(AST, AST)>),
    Less(Box<(AST, AST)>),
    LessOrEq(Box<(AST, AST)>),
    More(Box<(AST, AST)>),
    MoreOrEq(Box<(AST, AST)>),
    NotEqual(Box<(AST, AST)>),
    Or(Box<(AST, AST)>)
}
/// A lambda argument type
#[derive(Clone, Debug, PartialEq)]
pub enum LambdaArg {
    Ident(String),
    Pattern {
        args: Vec<PatEntry>,
        bind: Option<String>,
        exact: bool
    }
}
/// An interpolation part
#[derive(Clone, Debug, PartialEq)]
pub enum Interpol {
    Literal(String),
    AST(AST)
}
/// An entry in a pattern
#[derive(Clone, Debug, PartialEq)]
pub struct PatEntry(pub String, pub Option<AST>);
/// An entry in a set
#[derive(Clone, Debug, PartialEq)]
pub enum SetEntry {
    Assign(Vec<AST>, AST),
    Inherit(Option<AST>, Vec<String>)
}

type Error = (Option<Span>, ParseError);
type Result<T> = std::result::Result<T, Error>;

macro_rules! math {
    (only_once, $self:expr, $next:block, $($token:pat $(if $cond:expr)* => $ast:expr),*) => {{
        let val = { $next };
        Ok(match $self.peek() {
            $(Some(&$token) $(if $cond)* => {
                $self.next()?;
                let expr = { $next };
                AST(val.0.span.until(expr.0.span).into(), $ast(Box::new((val, expr))))
            },)*
            _ => val
        })
    }};
    ($self:expr, $next:block, $($token:pat $(if $cond:expr)* => $ast:expr),*) => {{
        let mut val = { $next };
        loop {
            match $self.peek() {
                $(Some(&$token) $(if $cond)* => {
                    $self.next()?;
                    let AST(end, expr) = { $next };
                    val = AST(val.0.span.until(end.span).into(), $ast(Box::new((val, AST(end, expr)))));
                },)*
                _ => break
            }
        }
        Ok(val)
    }};
}

fn parse_interpol(multiline: bool, values: Vec<TokenInterpol>) -> Result<ASTType> {
    let mut parsed = Vec::new();
    for value in values {
        parsed.push(match value {
            TokenInterpol::Literal(text) => Interpol::Literal(text),
            TokenInterpol::Tokens(tokens) => Interpol::AST(parse(tokens)?)
        });
    }
    Ok(ASTType::Interpol {
        multiline,
        parts: parsed
    })
}

/// The parser. You may want to use the `parse` convenience function from this module instead.
pub struct Parser<I>
    where I: Iterator<Item = (Meta, Token)>
{
    iter: I,
    buffer: Stack<I::Item>,
}
impl<I> Parser<I>
    where I: Iterator<Item = (Meta, Token)>
{
    /// Create a new instance
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

    fn next_attr(&mut self) -> Result<AST> {
        match self.next()? {
            (meta, Token::Ident(ident)) => Ok(AST(meta, ASTType::Var(ident))),
            (meta, Token::Value(value)) => Ok(AST(meta, ASTType::Value(value))),
            (meta, Token::Dynamic(values)) => Ok(AST(meta, ASTType::Dynamic(Box::new(parse(values)?)))),
            (meta, Token::Interpol { multiline, parts }) => Ok(AST(meta, parse_interpol(multiline, parts)?)),
            (meta, token) => Err((Some(meta.span), ParseError::ExpectedType("attribute", token)))
        }
    }
    fn parse_attr(&mut self) -> Result<Vec<AST>> {
        let mut path = Vec::with_capacity(1);
        loop {
            path.push(self.next_attr()?);

            if self.peek() != Some(&Token::Dot) {
                break;
            }
            self.next().unwrap();
        }
        Ok(path)
    }
    fn next_ident(&mut self) -> Result<(Meta, String)> {
        match self.next()? {
            (meta, Token::Ident(name)) => Ok((meta, name)),
            (meta, token) => Err((Some(meta.span), ParseError::ExpectedType("ident", token)))
        }
    }
    fn parse_pattern(&mut self, start: Meta, mut bind: Option<String>) -> Result<AST> {
        let mut args = Vec::with_capacity(1);
        let mut exact = true;
        loop {
            let ident = match self.peek_meta() {
                Some((_, Token::Ellipsis)) => {
                    self.next().unwrap();
                    exact = false;
                    break;
                },
                Some((_, Token::CurlyBClose)) => break,
                _ => self.next_ident()?.1,
            };
            let default = if self.peek() == Some(&Token::Question) {
                self.next().unwrap();
                Some(self.parse_expr()?)
            } else {
                None
            };
            args.push(PatEntry(ident, default));
            match self.peek() {
                Some(Token::Comma) => {
                    self.next().unwrap();
                },
                _ => break
            }
        }

        self.expect(Token::CurlyBClose)?;

        if let Some((meta, Token::At)) = self.peek_meta() {
            if bind.is_some() {
                return Err((Some(meta.span), ParseError::AlreadyBound));
            }
            self.next().unwrap();
            bind = Some(self.next_ident()?.1);
        }

        self.expect(Token::Colon)?;
        let AST(end, expr) = self.parse_expr()?;

        Ok(AST(start.until(&end), ASTType::Lambda(
            LambdaArg::Pattern { args, bind, exact },
            Box::new(AST(end, expr))
        )))
    }
    fn parse_set(&mut self, until: &Token) -> Result<(Meta, Vec<SetEntry>)> {
        let mut values = Vec::new();
        loop {
            match self.peek() {
                token if token == Some(until) => break,
                Some(Token::Inherit) => {
                    self.next().unwrap();

                    let from = if self.peek() == Some(&Token::ParenOpen) {
                        self.next().unwrap();
                        let from = self.parse_expr()?;
                        self.expect(Token::ParenClose)?;
                        Some(from)
                    } else {
                        None
                    };

                    let mut vars = Vec::new();
                    while let Some(Token::Ident(_)) = self.peek() {
                        vars.push(self.next_ident().unwrap().1);
                    }

                    values.push(SetEntry::Inherit(from, vars));
                },
                _ => {
                    let key = self.parse_attr()?;
                    self.expect(Token::Assign)?;
                    let value = self.parse_expr()?;

                    values.push(SetEntry::Assign(key, value));
                }
            }
            self.expect(Token::Semicolon)?;
        }
        let (end, _) = self.next().unwrap(); // Won't break until reached
        Ok((end, values))
    }
    fn parse_val(&mut self) -> Result<AST> {
        let mut val = match self.next()? {
            (_, Token::ParenOpen) => {
                let expr = self.parse_expr()?;
                self.expect(Token::ParenClose)?;
                expr
            },
            (start, Token::Import) => {
                let value = self.parse_val()?;
                AST(start.until(&value.0), ASTType::Import(Box::new(value)))
            },
            (start, Token::Rec) => {
                self.expect(Token::CurlyBOpen)?;
                let (end, values) = self.parse_set(&Token::CurlyBClose)?;
                AST(start.until(&end), ASTType::Set {
                    recursive: true,
                    values
                })
            },
            (start, Token::CurlyBOpen) => {
                let temporary = self.next()?;
                match (&temporary.1, self.peek()) {
                    (Token::Ident(_), Some(Token::Comma))
                            | (Token::Ident(_), Some(Token::Question))
                            | (Token::Ellipsis, Some(Token::CurlyBClose))
                            | (Token::Ident(_), Some(Token::CurlyBClose))
                            | (Token::CurlyBClose, Some(Token::Colon))
                            | (Token::CurlyBClose, Some(Token::At)) => {
                        // We did a lookahead, put it back
                        self.buffer.push(temporary);
                        self.parse_pattern(start, None)?
                    },
                    _ => {
                        // We did a lookahead, put it back
                        self.buffer.push(temporary);

                        let (end, values) = self.parse_set(&Token::CurlyBClose)?;
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
            (meta, Token::Dynamic(values)) => AST(meta, ASTType::Dynamic(Box::new(parse(values)?))),
            (meta, Token::Value(val)) => AST(meta, ASTType::Value(val)),
            (start, Token::Ident(name)) => if self.peek() == Some(&Token::At) {
                self.next().unwrap();
                self.expect(Token::CurlyBOpen)?;
                self.parse_pattern(start, Some(name))?
            } else {
                AST(start, ASTType::Var(name))
            },
            (meta, Token::Interpol { multiline, parts }) => AST(meta, parse_interpol(multiline, parts)?),
            (meta, token) => return Err((Some(meta.span), ParseError::Unexpected(token)))
        };

        while self.peek() == Some(&Token::Dot) {
            self.next().unwrap();
            let attr = self.next_attr()?;
            match self.peek() {
                Some(Token::Ident(s)) if s == OR => {
                    self.next().unwrap();
                    let default = self.parse_val()?;
                    val = AST(
                        val.0.span.until(attr.0.span).into(),
                        ASTType::OrDefault(Box::new((val, attr, default)))
                    );
                },
                _ => val = AST(
                    val.0.span.until(attr.0.span).into(),
                    ASTType::IndexSet(Box::new((val, attr)))
                )
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
    fn parse_negate(&mut self) -> Result<AST> {
        if self.peek() == Some(&Token::Sub) {
            let (start, _) = self.next().unwrap();
            let expr = self.parse_negate()?;
            Ok(AST(start.until(&expr.0), ASTType::Negate(Box::new(expr))))
        } else {
            self.parse_fn()
        }
    }
    fn parse_isset(&mut self) -> Result<AST> {
        math!(
            self, { self.parse_negate()? },
            Token::Question => ASTType::IsSet
        )
    }
    fn parse_concat(&mut self) -> Result<AST> {
        math!(self, { self.parse_isset()? }, Token::Concat => ASTType::Concat)
    }
    fn parse_mul(&mut self) -> Result<AST> {
        math!(
            self, { self.parse_concat()? },
            Token::Mul => ASTType::Mul,
            Token::Div => ASTType::Div
        )
    }
    fn parse_add(&mut self) -> Result<AST> {
        math!(
            self, { self.parse_mul()? },
            Token::Add => ASTType::Add,
            Token::Sub => ASTType::Sub
        )
    }
    fn parse_invert(&mut self) -> Result<AST> {
        if self.peek() == Some(&Token::Invert) {
            let (start, _) = self.next().unwrap();
            let expr = self.parse_invert()?;
            Ok(AST(start.until(&expr.0), ASTType::Invert(Box::new(expr))))
        } else {
            self.parse_add()
        }
    }
    fn parse_merge(&mut self) -> Result<AST> {
        math!(self, { self.parse_invert()? }, Token::Merge => ASTType::Merge)
    }
    fn parse_compare(&mut self) -> Result<AST> {
        math!(
            only_once, self, { self.parse_merge()? },
            Token::Less => ASTType::Less,
            Token::LessOrEq => ASTType::LessOrEq,
            Token::More => ASTType::More,
            Token::MoreOrEq => ASTType::MoreOrEq
        )
    }
    fn parse_equal(&mut self) -> Result<AST> {
        math!(
            only_once, self, { self.parse_compare()? },
            Token::Equal => ASTType::Equal,
            Token::NotEqual => ASTType::NotEqual
        )
    }
    fn parse_and(&mut self) -> Result<AST> {
        math!(
            self, { self.parse_equal()? },
            Token::And => ASTType::And
        )
    }
    fn parse_or(&mut self) -> Result<AST> {
        math!(
            self, { self.parse_and()? },
            Token::Or => ASTType::Or
        )
    }
    fn parse_implication(&mut self) -> Result<AST> {
        math!(
            self, { self.parse_or()? },
            Token::Implication => ASTType::Implication
        )
    }
    #[inline(always)]
    fn parse_math(&mut self) -> Result<AST> {
        // Always point this to the lowest-level math function there is
        self.parse_implication()
    }
    /// Parse Nix code into an AST
    pub fn parse_expr(&mut self) -> Result<AST> {
        Ok(match self.peek() {
            Some(Token::Let) => {
                let (start, _) = self.next().unwrap();
                if self.peek() == Some(&Token::CurlyBOpen) {
                    self.next().unwrap();
                    let (end, vars) = self.parse_set(&Token::CurlyBClose)?;
                    AST(start.until(&end), ASTType::Let(vars))
                } else {
                    let (_, vars) = self.parse_set(&Token::In)?;
                    let AST(end, expr) = self.parse_expr()?;
                    AST(start.until(&end), ASTType::LetIn(vars, Box::new(AST(end, expr))))
                }
            },
            Some(Token::With) => {
                let (start, _) = self.next().unwrap();
                let vars = self.parse_expr()?;
                self.expect(Token::Semicolon)?;
                let rest = self.parse_expr()?;
                AST(start.until(&rest.0), ASTType::With(Box::new((vars, rest))))
            },
            Some(Token::If) => {
                let (start, _) = self.next().unwrap();
                let condition = self.parse_expr()?;
                self.expect(Token::Then)?;
                let body = self.parse_expr()?;
                self.expect(Token::Else)?;
                let otherwise = self.parse_expr()?;
                AST(
                    start.span.until(otherwise.0.span).into(),
                    ASTType::IfElse(Box::new((condition, body, otherwise)))
                )
            },
            Some(Token::Assert) => {
                let (start, _) = self.next().unwrap();
                let condition = self.parse_expr()?;
                self.expect(Token::Semicolon)?;
                let rest = self.parse_expr()?;
                AST(start.until(&rest.0), ASTType::Assert(Box::new((condition, rest))))
            },
            _ => match self.parse_math()? {
                AST(start, ASTType::Var(name)) => if self.peek() == Some(&Token::Colon) {
                    self.next()?;
                    let AST(end, expr) = self.parse_expr()?;
                    AST(start.until(&end), ASTType::Lambda(LambdaArg::Ident(name), Box::new(AST(end, expr))))
                } else {
                    AST(start, ASTType::Var(name))
                },
                ast => ast
            }
        })
    }
}

/// Convenience function for turning an iterator of tokens into an AST
pub fn parse<I>(iter: I) -> Result<AST>
    where I: IntoIterator<Item = (Meta, Token)>
{
    Parser::new(iter.into_iter()).parse_expr()
}

#[cfg(test)]
mod tests {
    use crate::{
        tokenizer::{Interpol as TokenInterpol, Meta, Span, Token},
        value::{Anchor, Value}
    };
    use super::{nometa::*, AST as ASTSpan, ASTType, OR, ParseError};

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

                Token::Ident("meaning_of_life".into()), Token::Assign, Token::Value(42.into()), Token::Semicolon,
                Token::Ident("H4X0RNUM83R".into()), Token::Assign, Token::Value(1.337.into()), Token::Semicolon,

                Token::CurlyBClose
            ],
            Ok(AST::Set {
                recursive: false,
                values: vec![
                    SetEntry::Assign(vec![AST::Var("meaning_of_life".into())], AST::Value(42.into())),
                    SetEntry::Assign(vec![AST::Var("H4X0RNUM83R".into())], AST::Value(1.337.into()))
                ]
            })
        );
        assert_eq!(
            parse![
                Token::Rec, Token::CurlyBOpen,
                Token::Ident("test".into()), Token::Assign, Token::Value(1.into()), Token::Semicolon,
                Token::CurlyBClose
            ],
            Ok(AST::Set {
                recursive: true,
                values: vec![SetEntry::Assign(vec![AST::Var("test".into())], AST::Value(1.into()))]
            })
        );
        assert_eq!(
            parse![Token::CurlyBOpen, Token::CurlyBClose],
            Ok(AST::Set {
                recursive: false,
                values: Vec::new()
            })
        );
        assert_eq!(
            parse![
                Token::CurlyBOpen,

                Token::Ident("a".into()),
                    Token::Dot, Token::Value("b".into()),
                Token::Assign, Token::Value(1.into()), Token::Semicolon,

                Token::Interpol { multiline: false, parts: vec![TokenInterpol::Literal("c".into())] },
                    Token::Dot, Token::Dynamic(vec![(Meta::default(), Token::Ident("d".into()))]),
                Token::Assign, Token::Value(2.into()), Token::Semicolon,

                Token::CurlyBClose
            ],
            Ok(AST::Set {
                recursive: false,
                values: vec![
                    SetEntry::Assign(vec![
                        AST::Var("a".into()),
                        AST::Value("b".into()),
                    ], AST::Value(1.into())),
                    SetEntry::Assign(vec![
                        AST::Interpol { multiline: false, parts: vec![Interpol::Literal("c".into())] },
                        AST::Dynamic(Box::new(AST::Var("d".into())))
                    ], AST::Value(2.into()))
                ]
            })
        );
    }
    #[test]
    fn meta() {
        assert_eq!(
            super::parse(vec![
                (Meta::default(), Token::CurlyBOpen),
                (meta! { start: 1, end: 2 }, Token::Semicolon),
            ].into_iter()),
            Err((
                Some(Span { start: 1, end: Some(2) }),
                ParseError::ExpectedType("attribute", Token::Semicolon)
            ))
        );
        assert_eq!(
            super::parse(vec![
                (meta! { start: 0, end: 1 }, Token::Value(1.into())),
                (meta! { start: 2, end: 3 }, Token::Add),
                (
                    Meta {
                        comments: vec!["Hello World!".into()],
                        span: Span { start: 4, end: Some(5) }
                    },
                    Token::Value(2.into())
                ),
                (meta! { start: 6, end: 7 }, Token::Mul),
                (meta! { start: 8, end: 9 }, Token::Value(3.into())),
            ].into_iter()),
            Ok(ASTSpan(
                meta! { start: 0, end: 9 },
                ASTType::Add(Box::new((
                    ASTSpan(
                        meta! { start: 0, end: 1 },
                        ASTType::Value(1.into())
                    ),
                    ASTSpan(
                        meta! { start: 4, end: 9 },
                        ASTType::Mul(Box::new((
                            ASTSpan(
                                Meta {
                                    comments: vec!["Hello World!".into()],
                                    span: Span { start: 4, end: Some(5) }
                                },
                                ASTType::Value(2.into())
                            ),
                            ASTSpan(
                                meta! { start: 8, end: 9 },
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
                    Token::Ident("a".into()), Token::Assign, Token::Value(42.into()), Token::Semicolon,
                Token::In,
                    Token::Ident("a".into())
            ],
            Ok(AST::LetIn(
                vec![SetEntry::Assign(vec![AST::Var("a".into())], AST::Value(42.into()))],
                Box::new(AST::Var("a".into()))
            ))
        );
    }
    #[test]
    fn let_legacy_syntax() {
        assert_eq!(
            parse![
                Token::Let, Token::CurlyBOpen,
                    Token::Ident("a".into()), Token::Assign, Token::Value(42.into()), Token::Semicolon,
                    Token::Ident("body".into()), Token::Assign, Token::Ident("a".into()), Token::Semicolon,
                Token::CurlyBClose
            ],
            Ok(AST::Let(vec![
                SetEntry::Assign(vec![AST::Var("a".into())], AST::Value(42.into())),
                SetEntry::Assign(vec![AST::Var("body".into())], AST::Var("a".into()))
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
                Token::Value(Value::Path(Anchor::Store, "nixpkgs".into())),
                Token::CurlyBOpen, Token::CurlyBClose
            ],
            Ok(AST::Apply(Box::new((
                AST::Import(Box::new(
                    AST::Value(Value::Path(Anchor::Store, "nixpkgs".into()))
                )),
                AST::Set {
                    recursive: false,
                    values: Vec::new()
                }
            ))))
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
            Ok(AST::IndexSet(Box::new((
                AST::IndexSet(Box::new((
                    AST::Var("a".into()),
                    AST::Var("b".into())
                ))),
                AST::Var("c".into())
            ))))
        );
        assert_eq!(
            parse![
                Token::CurlyBOpen,
                    Token::Ident("a".into()),
                        Token::Dot, Token::Ident("b".into()),
                        Token::Dot, Token::Ident("c".into()),
                    Token::Assign, Token::Value(1.into()), Token::Semicolon,
                Token::CurlyBClose
            ],
            Ok(AST::Set {
                recursive: false,
                values: vec![
                    SetEntry::Assign(vec![
                        AST::Var("a".into()),
                        AST::Var("b".into()),
                        AST::Var("c".into())
                    ], AST::Value(1.into()))
                ]
            })
        );
        assert_eq!(
            parse![
                Token::Ident("test".into()),
                    Token::Dot, Token::Value("invalid ident".into()),
                    Token::Dot, Token::Interpol { multiline: false, parts: vec![TokenInterpol::Literal("hi".into())] },
                    Token::Dot, Token::Dynamic(vec![
                        (Meta::default(), Token::Ident("a".into()))
                    ])
            ],
            Ok(AST::IndexSet(Box::new((
                AST::IndexSet(Box::new((
                    AST::IndexSet(Box::new((
                        AST::Var("test".into()),
                        AST::Value("invalid ident".into())
                    ))),
                    AST::Interpol { multiline: false, parts: vec![Interpol::Literal("hi".into())] }
                ))),
                AST::Dynamic(Box::new(AST::Var("a".into())))
            ))))
        );
    }
    #[test]
    fn interpolation() {
        assert_eq!(
            parse![
                Token::Interpol {
                    multiline: false,
                    parts: vec![
                        TokenInterpol::Literal("Hello, ".into()),
                        TokenInterpol::Tokens(vec![
                            (Meta::default(), Token::CurlyBOpen),
                            (Meta::default(), Token::Ident("world".into())),
                            (Meta::default(), Token::Assign),
                            (Meta::default(), Token::Value("World".into())),
                            (Meta::default(), Token::Semicolon),
                            (Meta::default(), Token::CurlyBClose),
                            (Meta::default(), Token::Dot),
                            (Meta::default(), Token::Ident("world".into()))
                        ]),
                        TokenInterpol::Literal("!".into())
                    ]
                }
            ],
            Ok(AST::Interpol {
                multiline: false,
                parts: vec![
                    Interpol::Literal("Hello, ".into()),
                    Interpol::AST(AST::IndexSet(Box::new((
                        AST::Set {
                            recursive: false,
                            values: vec![SetEntry::Assign(vec![AST::Var("world".into())], AST::Value("World".into()))]
                        },
                        AST::Var("world".into())
                    )))),
                    Interpol::Literal("!".into())
                ]
            })
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
                LambdaArg::Ident("a".into()),
                Box::new(AST::Lambda(
                    LambdaArg::Ident("b".into()),
                    Box::new(AST::Add(Box::new((
                        AST::Var("a".into()),
                        AST::Var("b".into())
                    ))))
                ))
            ))
        );
        assert_eq!(
            parse![Token::CurlyBOpen, Token::CurlyBClose, Token::Colon, Token::Value(1.into())],
            Ok(AST::Lambda(
                LambdaArg::Pattern {
                    args: Vec::new(),
                    bind: None,
                    exact: true
                },
                Box::new(AST::Value(1.into()))
            ))
        );
        assert_eq!(
            parse![
                Token::CurlyBOpen, Token::CurlyBClose, Token::At, Token::Ident("outer".into()),
                Token::Colon, Token::Value(1.into())
            ],
            Ok(AST::Lambda(
                LambdaArg::Pattern {
                    args: Vec::new(),
                    bind: Some("outer".into()),
                    exact: true
                },
                Box::new(AST::Value(1.into()))
            ))
        );
        assert_eq!(
            parse![Token::CurlyBOpen, Token::Ellipsis, Token::CurlyBClose, Token::Colon, Token::Value(1.into())],
            Ok(AST::Lambda(
                LambdaArg::Pattern {
                    args: Vec::new(),
                    bind: None,
                    exact: false
                },
                Box::new(AST::Value(1.into()))
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
                    Token::Ident("b".into()), Token::Question, Token::Value("default".into()),
                Token::CurlyBClose,
                Token::Colon,
                Token::Ident("a".into())
            ],
            Ok(AST::Lambda(
                LambdaArg::Pattern {
                    args: vec![
                        PatEntry("a".into(), None),
                        PatEntry("b".into(), Some(AST::Value("default".into()))),
                    ],
                    bind: None,
                    exact: true
                },
                Box::new(AST::Var("a".into()))
            ))
        );
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
                LambdaArg::Pattern {
                    args: vec![
                        PatEntry("a".into(), None),
                        PatEntry("b".into(), Some(AST::Value("default".into()))),
                    ],
                    bind: Some("outer".into()),
                    exact: false
                },
                Box::new(AST::Var("outer".into()))
            ))
        );
        assert_eq!(
            parse![
                Token::Ident("outer".into()), Token::At,
                Token::CurlyBOpen, Token::Ident("a".into()), Token::CurlyBClose,
                Token::Colon,
                Token::Ident("outer".into())
            ],
            Ok(AST::Lambda(
                LambdaArg::Pattern {
                    args: vec![PatEntry("a".into(), None)],
                    bind: Some("outer".into()),
                    exact: true
                },
                Box::new(AST::Var("outer".into()))
            ))
        );
        assert_eq!(
            parse![
                Token::CurlyBOpen,
                    Token::Ident("a".into()), Token::Question, Token::CurlyBOpen, Token::CurlyBClose,
                Token::CurlyBClose, Token::Colon, Token::Ident("a".into())
            ],
            Ok(AST::Lambda(
                LambdaArg::Pattern {
                    args: vec![PatEntry("a".into(), Some(AST::Set { recursive: false, values: Vec::new() }))],
                    bind: None,
                    exact: true
                },
                Box::new(AST::Var("a".into()))
            ))
        );
    }
    #[test]
    fn merge() {
        assert_eq!(
            parse![
                Token::CurlyBOpen,
                    Token::Ident("a".into()), Token::Assign, Token::Value(1.into()), Token::Semicolon,
                Token::CurlyBClose,
                Token::Merge,
                Token::CurlyBOpen,
                    Token::Ident("b".into()), Token::Assign, Token::Value(2.into()), Token::Semicolon,
                Token::CurlyBClose
            ],
            Ok(AST::Merge(Box::new((
                AST::Set {
                    recursive: false,
                    values: vec![SetEntry::Assign(vec![AST::Var("a".into())], AST::Value(1.into()))]
                },
                AST::Set {
                    recursive: false,
                    values: vec![SetEntry::Assign(vec![AST::Var("b".into())], AST::Value(2.into()))]
                }
            ))))
        )
    }
    #[test]
    fn ifs() {
        assert_eq!(
            parse![
                Token::Value(false.into()), Token::Implication,
                Token::Invert, Token::Value(false.into()),
                Token::And,
                Token::Value(false.into()), Token::Equal, Token::Value(true.into()),
                Token::Or,
                Token::Value(true.into())
            ],
            Ok(AST::Implication(Box::new((
                AST::Value(false.into()),
                AST::Or(Box::new((
                    AST::And(Box::new((
                        AST::Invert(Box::new(AST::Value(false.into()))),
                        AST::Equal(Box::new((AST::Value(false.into()), AST::Value(true.into()))))
                    ))),
                    AST::Value(true.into())
                )))
            ))))
        );
        assert_eq!(
            parse![
                Token::Value(1.into()), Token::Less, Token::Value(2.into()),
                Token::Or,
                Token::Value(2.into()), Token::LessOrEq, Token::Value(2.into()),
                Token::And,
                Token::Value(2.into()), Token::More, Token::Value(1.into()),
                Token::And,
                Token::Value(2.into()), Token::MoreOrEq, Token::Value(2.into())
            ],
            Ok(AST::Or(Box::new((
                AST::Less(Box::new((AST::Value(1.into()), AST::Value(2.into())))),
                AST::And(Box::new((
                    AST::And(Box::new((
                        AST::LessOrEq(Box::new((AST::Value(2.into()), AST::Value(2.into())))),
                        AST::More(Box::new((AST::Value(2.into()), AST::Value(1.into())))),
                    ))),
                    AST::MoreOrEq(Box::new((AST::Value(2.into()), AST::Value(2.into()))))
                )))
            ))))
        );
        assert_eq!(
            parse![
                Token::Value(1.into()), Token::Equal, Token::Value(1.into()),
                Token::And,
                Token::Value(2.into()), Token::NotEqual, Token::Value(3.into())
            ],
            Ok(AST::And(Box::new((
                AST::Equal(Box::new((AST::Value(1.into()), AST::Value(1.into())))),
                AST::NotEqual(Box::new((AST::Value(2.into()), AST::Value(3.into()))))
            ))))
        );
        assert_eq!(
            parse![
                Token::If, Token::Value(false.into()), Token::Then,
                    Token::Value(1.into()),
                Token::Else,
                    Token::If, Token::Value(true.into()), Token::Then,
                        Token::Value(2.into()),
                    Token::Else,
                        Token::Value(3.into())
            ],
            Ok(AST::IfElse(Box::new((
                AST::Value(false.into()),
                AST::Value(1.into()),
                AST::IfElse(Box::new((
                    AST::Value(true.into()),
                    AST::Value(2.into()),
                    AST::Value(3.into())
                )))
            ))))
        )
    }
    #[test]
    fn assert() {
        assert_eq!(
            parse![
                Token::Assert, Token::Ident("a".into()), Token::Equal, Token::Ident("b".into()), Token::Semicolon,
                Token::Value("a == b".into())
            ],
            Ok(AST::Assert(Box::new((
                AST::Equal(Box::new((AST::Var("a".into()), AST::Var("b".into())))),
                AST::Value("a == b".into())
            ))))
        );
    }
    #[test]
    fn inherit() {
        assert_eq!(
            parse![
                Token::CurlyBOpen,
                    Token::Ident("a".into()), Token::Assign, Token::Value(1.into()), Token::Semicolon,
                    Token::Inherit, Token::Ident("b".into()), Token::Semicolon,

                    Token::Inherit, Token::ParenOpen, Token::Ident("set".into()), Token::ParenClose,
                    Token::Ident("c".into()), Token::Semicolon,
                Token::CurlyBClose
            ],
            Ok(AST::Set {
                recursive: false,
                values: vec![
                    SetEntry::Assign(vec![AST::Var("a".into())], AST::Value(1.into())),
                    SetEntry::Inherit(None, vec!["b".into()]),
                    SetEntry::Inherit(Some(AST::Var("set".into())), vec!["c".into()]),
                ]
            })
        );
    }
    #[test]
    fn isset() {
        assert_eq!(
            parse![
                Token::Ident("a".into()), Token::Question, Token::Value("b".into()),
                Token::And, Token::Value(true.into())
            ],
            Ok(AST::And(Box::new((
                AST::IsSet(Box::new((
                    AST::Var("a".into()),
                    AST::Value("b".into())
                ))),
                AST::Value(true.into())
            ))))
        );
        assert_eq!(
            parse![
                Token::Ident("a".into()),
                    Token::Dot, Token::Ident("b".into()),
                    Token::Dot, Token::Ident("c".into()),
                Token::Ident(OR.into()), Token::Value(1.into()),
                Token::Add, Token::Value(1.into())
            ],
            Ok(AST::Add(Box::new((
                AST::OrDefault(Box::new((
                    AST::IndexSet(Box::new((
                        AST::Var("a".into()),
                        AST::Var("b".into())
                    ))),
                    AST::Var("c".into()),
                    AST::Value(1.into())
                ))),
                AST::Value(1.into())
            ))))
        );
    }
}
