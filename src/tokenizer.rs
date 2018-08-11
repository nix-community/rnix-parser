use crate::value::Value;
use std::iter::Peekable;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    BracketOpen,
    BracketClose,
    Equal,
    Semicolon,
    Ident(String),
    Value(Value),
    ParenOpen,
    ParenClose,
    Add,
    Sub,
    Mul,
    Div
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Span {
    pub start: (u64, u64),
    pub end: Option<(u64, u64)>
}

#[derive(Clone, Copy, Debug, Fail, PartialEq)]
pub enum TokenizeError {
    #[fail(display = "error parsing integer: overflow")]
    IntegerOverflow,
    #[fail(display = "dot after number, but no decimals")]
    TrailingDecimal,
    #[fail(display = "unexpected eof")]
    UnexpectedEOF,
    #[fail(display = "undefined token")]
    UndefinedToken
}

pub struct Tokenizer<I>
    where I: Iterator<Item = char>
{
    iter: Peekable<I>,
    row: u64,
    col: u64,
    span_start: (u64, u64)
}
impl<I> Tokenizer<I>
    where I: Iterator<Item = char>
{
    pub fn new(iter: Peekable<I>) -> Self {
        Self {
            iter,
            row: 0,
            col: 0,
            span_start: (0, 0)
        }
    }
    fn next(&mut self) -> Option<char> {
        let c = self.iter.next();
        match c {
            Some('\n') => {
                self.col = 0;
                self.row += 1;
            },
            Some(_) => self.col += 1,
            None => ()
        }
        c
    }
    fn span_start(&mut self) {
        self.span_start = (self.row, self.col);
    }
    fn span_err(&self, error: TokenizeError) -> Option<(Span, Result<Token, TokenizeError>)> {
        Some((
            Span {
                start: self.span_start,
                end: None
            },
            Err(error)
        ))
    }
    fn span_end(&self, token: Token) -> Option<(Span, Result<Token, TokenizeError>)> {
        Some((
            Span {
                start: self.span_start,
                end: Some((self.row, self.col))
            },
            Ok(token)
        ))
    }
}
impl<I> Iterator for Tokenizer<I>
    where I: Iterator<Item = char>
{
    type Item = (Span, Result<Token, TokenizeError>);

    fn next(&mut self) -> Option<Self::Item> {
        self.span_start();
        let mut c = self.next()?;
        while c.is_whitespace() {
            self.span_start();
            c = self.next()?;
        }
        match c {
            '{' => self.span_end(Token::BracketOpen),
            '}' => self.span_end(Token::BracketClose),
            '=' => self.span_end(Token::Equal),
            ';' => self.span_end(Token::Semicolon),
            '(' => self.span_end(Token::ParenOpen),
            ')' => self.span_end(Token::ParenClose),
            '+' => self.span_end(Token::Add),
            '-' => self.span_end(Token::Sub),
            '*' => self.span_end(Token::Mul),
            '/' => self.span_end(Token::Div),
            'a'..='z' | 'A'..='Z' => {
                let mut ident = String::new();
                ident.push(c);
                loop {
                    match self.iter.peek() {
                        None => break,
                        // TODO: Some(&('a'..='z')) | Some(&('A'..='Z')) | Some(&('0'..='9'))
                        Some(c) => match *c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' => {
                                ident.push(self.next()?);
                            },
                            _ => break
                        }
                    }
                }
                self.span_end(Token::Ident(ident))
            },
            '"' => {
                let mut literal = String::new();
                loop {
                    match self.iter.peek() {
                        None => return self.span_err(TokenizeError::UnexpectedEOF),
                        Some(&'"') => { self.next(); break },
                        Some(&'\\') => {
                            self.next()?;
                            literal.push(self.next()?);
                        },
                        Some(_) => {
                            literal.push(self.next()?);
                        }
                    }
                }
                self.span_end(Token::Value(Value::Str(literal)))
            },
            '\'' => {
                if self.iter.next() != Some('\'') {
                    return self.span_err(TokenizeError::UndefinedToken);
                }
                let mut multiline = String::new();
                loop {
                    match self.iter.peek() {
                        None => return self.span_err(TokenizeError::UnexpectedEOF),
                        Some(&'\'') => match { self.next()?; self.iter.peek() } {
                            None => return self.span_err(TokenizeError::UnexpectedEOF),
                            Some(&'\'') => { self.next()?; break; },
                            Some(_) => multiline.push('\''),
                        },
                        Some(&'\n') => {
                            // Don't push initial newline
                            self.next()?;
                            if !multiline.is_empty() {
                                multiline.push('\n');
                            }
                            while self.iter.peek() == Some(&' ')
                                    || self.iter.peek() == Some(&'\t') {
                                self.next();
                            }
                        },
                        Some(_) => multiline.push(self.next()?),
                    }
                }
                // Remove all trailing newlines
                while multiline.chars().last() == Some('\n') {
                    multiline.pop();
                }
                self.span_end(Token::Value(Value::Str(multiline)))
            },
            '0'..='9' => {
                // Could use built-in rust parse function here, however that
                // requires collecting stuff to a string first, which is very
                // expensive.

                // TODO: Multiple radixes?
                const RADIX: u32 = 10;

                // We already know it's a digit
                let mut num = c.to_digit(RADIX).unwrap() as i64;

                while let Some(digit) = self.iter.peek().and_then(|c| c.to_digit(RADIX)) {
                    self.next();
                    num = match num.checked_mul(RADIX as i64).and_then(|num| num.checked_add(digit as i64)) {
                        Some(num) => num,
                        None => return self.span_err(TokenizeError::IntegerOverflow)
                    };
                }

                if self.iter.peek() == Some(&'.') {
                    self.next();

                    let mut i = 1;
                    let mut num = num as f64;

                    while let Some(digit) = self.iter.peek().and_then(|c| c.to_digit(RADIX)) {
                        self.next();
                        i *= RADIX;
                        num += digit as f64 / i as f64;
                    }

                    if i == 1 {
                        return self.span_err(TokenizeError::TrailingDecimal)
                    }

                    self.span_end(Token::Value(Value::Float(num)))
                } else {
                    self.span_end(Token::Value(Value::Integer(num)))
                }
            },
            _ => self.span_err(TokenizeError::UndefinedToken)
        }
    }
}

pub fn tokenize<I>(input: I) -> impl Iterator<Item = (Span, Result<Token, TokenizeError>)>
    where I: IntoIterator<Item = char>
{
    Tokenizer::new(input.into_iter().peekable())
}

#[cfg(test)]
mod tests {
    use super::{Span, Token, TokenizeError};

    fn tokenize(input: &str) -> Result<Vec<Token>, TokenizeError> {
        super::tokenize(input.chars())
            .map(|(_, result)| result)
            .collect()
    }

    #[test]
    fn basic_int_set() {
        assert_eq!(
            tokenize("{ int = 42; }"),
            Ok(vec![Token::BracketOpen, Token::Ident("int".into()), Token::Equal,
            Token::Value(42.into()), Token::Semicolon, Token::BracketClose])
        );
    }
    #[test]
    fn basic_float_set() {
        assert_eq!(
            tokenize("{ float = 1.234; }"),
            Ok(vec![Token::BracketOpen, Token::Ident("float".into()), Token::Equal,
            Token::Value(1.234.into()), Token::Semicolon, Token::BracketClose])
        );
    }
    #[test]
    fn basic_string_set() {
        assert_eq!(
            tokenize(r#"{ string = "Hello \"World\""; }"#),
            Ok(vec![Token::BracketOpen, Token::Ident("string".into()), Token::Equal,
            Token::Value("Hello \"World\"".into()), Token::Semicolon, Token::BracketClose])
        );
    }
    #[test]
    fn spans() {
        assert_eq!(
            super::tokenize("{\n    int = 1;\n}".chars())
                .map(|(span, result)| result.map(|token| (span, token)))
                .collect::<Result<Vec<(Span, Token)>, TokenizeError>>(),
            Ok(vec![
               (Span { start: (0,  0), end: Some((0,  1)) }, Token::BracketOpen),
               (Span { start: (1,  4), end: Some((1,  7)) }, Token::Ident("int".to_string())),
               (Span { start: (1,  8), end: Some((1,  9)) }, Token::Equal),
               (Span { start: (1, 10), end: Some((1, 11)) }, Token::Value(1.into())),
               (Span { start: (1, 11), end: Some((1, 12)) }, Token::Semicolon),
               (Span { start: (2,  0), end: Some((2,  1)) }, Token::BracketClose),
            ])
        );
        assert_eq!(
            super::tokenize("{\n    overflow = 9999999999999999999999999999".chars())
                .map(|(span, result)| result.map_err(|err| (span, err)))
                .collect::<Result<Vec<Token>, (Span, TokenizeError)>>(),
            Err((Span { start: (1, 15), end: None }, TokenizeError::IntegerOverflow))
        );
    }
    #[test]
    fn multiline() {
        assert_eq!(
            tokenize(r#"{
    multiline = ''
        This is a
        multiline
        string :D
        \'\'\'\'\
    '';
}"#),
            Ok(vec![
                Token::BracketOpen,
                    Token::Ident("multiline".into()), Token::Equal,
                    Token::Value(r#"This is a
multiline
string :D
\'\'\'\'\"#.into()),
                Token::Semicolon, Token::BracketClose
            ])
        );
    }
    #[test]
    fn math() {
        assert_eq!(
            tokenize("1 + 2 * 3"),
            Ok(vec![Token::Value(1.into()), Token::Add, Token::Value(2.into()), Token::Mul, Token::Value(3.into())])
        );
        assert_eq!(
            tokenize("5 * -(3 - 2)"),
            Ok(vec![
                Token::Value(5.into()), Token::Mul,
                Token::Sub, Token::ParenOpen,
                    Token::Value(3.into()), Token::Sub, Token::Value(2.into()),
                Token::ParenClose
            ])
        );
    }
}
