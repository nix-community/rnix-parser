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

#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    Set(Vec<(String, AST)>),
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

    pub fn parse_expr(&mut self) -> Result<AST> {
        Ok(match self.next()? {
            (_, Token::BracketOpen) => {
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
                self.expect(Token::BracketClose)?;
                AST::Set(values)
            },
            (_, Token::Value(value)) => AST::Value(value),
            (span, token) => return Err((Some(span), ParseError::Unexpected(token)))
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
    use crate::tokenizer::{Span, Token};
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
}
