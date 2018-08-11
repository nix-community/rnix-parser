use crate::{
    tokenizer::{Span, Token},
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

type Set = Vec<(String, AST)>;

#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    Set(Set),
    LetIn(Set, Box<AST>),
    With(Box<(AST, AST)>),
    Import(Box<AST>),
    Var(String),
    Negate(Box<AST>),
    // Could also do Add(Box<AST>, Box<AST>), but I believe this is more
    // efficient.
    Add(Box<(AST, AST)>),
    Sub(Box<(AST, AST)>),
    Mul(Box<(AST, AST)>),
    Div(Box<(AST, AST)>),
    Value(Value)
}

type Error = (Option<Span>, ParseError);
type Result<T> = std::result::Result<T, Error>;

pub struct Parser<I>
    where I: Iterator<Item = (Span, Token)>
{
    iter: Peekable<I>
}
impl<I> Parser<I>
    where I: Iterator<Item = (Span, Token)>
{
    pub fn new(iter: Peekable<I>) -> Self {
        Self { iter }
    }

    pub fn peek(&mut self) -> Result<&Token> {
        self.iter.peek()
            .map(|(_, token)| token)
            .ok_or((None, ParseError::UnexpectedEOF))
    }
    pub fn next(&mut self) -> Result<(Span, Token)> {
        self.iter.next()
            .map(|entry| entry)
            .ok_or((None, ParseError::UnexpectedEOF))
    }
    pub fn expect(&mut self, expected: Token) -> Result<()> {
        if let Some((span, actual)) = self.iter.next() {
            if actual == expected {
                Ok(())
            } else {
                Err((Some(span), ParseError::Expected(expected, Some(actual))))
            }
        } else {
            Err((None, ParseError::Expected(expected, None)))
        }
    }

    pub fn parse_val(&mut self) -> Result<AST> {
        Ok(match self.next()? {
            (_, Token::ParenOpen) => {
                let expr = self.parse_expr()?;
                self.expect(Token::ParenClose)?;
                expr
            },
            (_, Token::Sub) => AST::Negate(Box::new(self.parse_val()?)),
            (_, Token::Value(val)) => AST::Value(val),
            (_, Token::Ident(name)) => AST::Var(name),
            (span, token) => return Err((Some(span), ParseError::Unexpected(token)))
        })
    }

    pub fn parse_mul(&mut self) -> Result<AST> {
        let mut val = self.parse_val()?;
        loop {
            match self.iter.peek() {
                Some(&(_, Token::Mul)) => {
                    self.next()?;
                    val = AST::Mul(Box::new((val, self.parse_val()?)));
                },
                Some(&(_, Token::Div)) => {
                    self.next()?;
                    val = AST::Div(Box::new((val, self.parse_val()?)));
                },
                _ => break
            }
        }
        Ok(val)
    }

    pub fn parse_add(&mut self) -> Result<AST> {
        let mut val = self.parse_mul()?;
        loop {
            match self.iter.peek() {
                Some(&(_, Token::Add)) => {
                    self.next()?;
                    val = AST::Add(Box::new((val, self.parse_mul()?)));
                },
                Some(&(_, Token::Sub)) => {
                    self.next()?;
                    val = AST::Sub(Box::new((val, self.parse_mul()?)));
                },
                _ => break
            }
        }
        Ok(val)
    }

    pub fn parse_set(&mut self) -> Result<Set> {
        let mut values = Vec::new();
        while let &Token::Ident(_) = self.peek()? {
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
        Ok(match self.peek()? {
            Token::BracketOpen => {
                self.next()?;
                let values = self.parse_set()?;
                self.expect(Token::BracketClose)?;
                AST::Set(values)
            },
            Token::Let => {
                self.next()?;
                let vars = self.parse_set()?;
                self.expect(Token::In)?;
                let expr = self.parse_expr()?;
                AST::LetIn(vars, Box::new(expr))
            },
            Token::With => {
                self.next()?;
                let vars = self.parse_expr()?;
                self.expect(Token::Semicolon)?;
                let expr = self.parse_expr()?;
                AST::With(Box::new((vars, expr)))
            },
            Token::Import => {
                self.next()?;
                AST::Import(Box::new(self.parse_expr()?))
            },
            _ => self.parse_add()?
        })
    }
}

pub fn parse<I>(iter: I) -> Result<AST>
    where I: IntoIterator<Item = (Span, Token)>
{
    Parser::new(iter.into_iter().peekable()).parse_expr()
}

#[cfg(test)]
mod tests {
    use crate::{
        tokenizer::{Span, Token},
        value::{Anchor, Value}
    };
    use super::{parse, AST, ParseError};

    macro_rules! test {
        ($($token:expr),*) => {
            vec![
                $((Span::default(), $token)),*
            ].into_iter()
        }
    }

    #[test]
    fn set() {
        assert_eq!(
            parse(test![
                Token::BracketOpen,

                Token::Ident("meaning_of_life".into()), Token::Equal, Token::Value(42.into()), Token::Semicolon,
                Token::Ident("H4X0RNUM83R".into()), Token::Equal, Token::Value(1.337.into()), Token::Semicolon,

                Token::BracketClose
            ]),
            Ok(AST::Set(vec![
                ("meaning_of_life".into(), AST::Value(42.into())),
                ("H4X0RNUM83R".into(), AST::Value(1.337.into()))
            ]))
        );
    }
    #[test]
    fn spans() {
        assert_eq!(
            parse(vec![
                (Span::default(), Token::BracketOpen),
                (Span { start: (4, 2), end: None }, Token::Semicolon),
            ].into_iter()),
            Err((
                Some(Span { start: (4, 2), end: None }),
                ParseError::Expected(Token::BracketClose, Some(Token::Semicolon))
            ))
        )
    }
    #[test]
    fn math() {
        assert_eq!(
            parse(test![Token::Value(1.into()), Token::Add, Token::Value(2.into()), Token::Mul, Token::Value(3.into())]),
            Ok(AST::Add(Box::new((
                AST::Value(1.into()),
                AST::Mul(Box::new((
                    AST::Value(2.into()),
                    AST::Value(3.into()),
                )))
            ))))
        );
        assert_eq!(
            parse(test![
                Token::Value(5.into()), Token::Mul,
                Token::Sub, Token::ParenOpen,
                    Token::Value(3.into()), Token::Sub, Token::Value(2.into()),
                Token::ParenClose
            ]),
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
            parse(test![
                Token::Let,
                    Token::Ident("a".into()), Token::Equal, Token::Value(42.into()), Token::Semicolon,
                Token::In,
                    Token::Ident("a".into())
            ]),
            Ok(AST::LetIn(
                vec![("a".into(), AST::Value(42.into()))],
                Box::new(AST::Var("a".into()))
            ))
        );
    }
    #[test]
    fn with() {
        assert_eq!(
            parse(test![
                Token::With, Token::Ident("namespace".into()), Token::Semicolon,
                Token::Ident("expr".into())
            ]),
            Ok(AST::With(Box::new((
                AST::Var("namespace".into()),
                AST::Var("expr".into())
            ))))
        );
    }
    #[test]
    fn import() {
        assert_eq!(
            parse(test![
                Token::Import,
                Token::Value(Value::Path(Anchor::Store, "nixpkgs".into()))
            ]),
            Ok(AST::Import(Box::new(
                AST::Value(Value::Path(Anchor::Store, "nixpkgs".into()))
            )))
        );
    }
}
