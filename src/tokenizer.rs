use crate::value::{Anchor, Value};

#[derive(PartialEq, Eq)]
enum IdentType {
    Uri,
    Path,
    Ident
}

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
    Div,
    Let,
    In,
    With,
    Import
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
    UndefinedToken,
    #[fail(display = "paths cannot have a trailing slash")]
    TrailingSlash
}

fn is_valid_path_char(c: char) -> bool {
    match c {
        'a'..='z' | 'A'..='Z' | '0'..='9' | '/' | '_' | '.' | '+' | '-' => true,
        _ => false
    }
}

pub struct Tokenizer<'a> {
    input: &'a str,
    row: u64,
    col: u64,
    span_start: (u64, u64)
}
impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            row: 0,
            col: 0,
            span_start: (0, 0)
        }
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

    fn next(&mut self) -> Option<char> {
        let c = self.peek();
        if let Some(c) = c {
            self.input = &self.input[c.len_utf8()..];
            if c == '\n' {
                self.col = 0;
                self.row += 1;
            } else {
                self.col += 1;
            }
        }
        c
    }
    fn peek(&mut self) -> Option<char> {
        self.input.chars().next()
    }
    fn next_ident<F>(&mut self, prefix: Option<char>, mut include: F) -> String
        where F: FnMut(char) -> bool
    {
        let mut ident = String::new();
        if let Some(c) = prefix {
            ident.push(c);
        }
        loop {
            match self.peek() {
                Some(c) => if include(c) {
                    ident.push(self.next().unwrap())
                } else {
                    break;
                },
                _ => break,
            }
        }
        ident
    }
}
impl<'a> Iterator for Tokenizer<'a> {
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
            '/' => {
                match self.peek() {
                    Some(c) if c.is_whitespace() => self.span_end(Token::Div),
                    None => self.span_end(Token::Div),
                    Some(_) => {
                        let ident = self.next_ident(Some(c), is_valid_path_char);
                        self.span_end(Token::Value(Value::Path(Anchor::Absolute, ident)))
                    }
                }
            },
            '.' | '~' => {
                let anchor = match c {
                    '.' => Anchor::Relative,
                    '~' => Anchor::Home,
                    _ => unreachable!()
                };
                if self.next() != Some('/') {
                    return self.span_err(TokenizeError::UndefinedToken);
                }
                let ident = self.next_ident(None, is_valid_path_char);
                if ident.ends_with('/') {
                    return self.span_err(TokenizeError::TrailingSlash);
                }
                self.span_end(Token::Value(Value::Path(anchor, ident)))
            },
            '<' => {
                let ident = self.next_ident(None, is_valid_path_char);
                if self.next() != Some('>') {
                    return self.span_err(TokenizeError::UndefinedToken);
                }
                self.span_end(Token::Value(Value::Path(Anchor::Store, ident)))
            },
            'a'..='z' | 'A'..='Z' => {
                let mut lookahead = self.input.chars().skip_while(|c| match c {
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '.' | '+' | '-' => true,
                    _ => false
                });
                // Check what character is after all these characters to see what
                // type it is.
                let kind = match (lookahead.next(), lookahead.next()) {
                    (Some(':'), Some(c)) if !c.is_whitespace() => IdentType::Uri,
                    (Some('/'), Some(c)) if !c.is_whitespace() => IdentType::Path,
                    _ => IdentType::Ident
                };
                let ident = self.next_ident(Some(c), |c| match c {
                    '/' | '+' | '-' => kind == IdentType::Path || kind == IdentType::Uri,
                    ':' | '?' | '@' | '&' | '=' | '$' | ',' | '!'
                        | '~' | '*' | '\'' | '%' => kind == IdentType::Uri,
                    c => is_valid_path_char(c)
                });
                if kind == IdentType::Ident {
                    self.span_end(match &*ident {
                        "let" => Token::Let,
                        "in" => Token::In,
                        "with" => Token::With,
                        "import" => Token::Import,
                        _ => Token::Ident(ident),
                    })
                } else {
                    self.span_end(match kind {
                        IdentType::Ident => Token::Ident(ident),
                        IdentType::Path => Token::Value(Value::Path(Anchor::Relative, ident)),
                        IdentType::Uri => Token::Value(Value::Path(Anchor::Uri, ident)),
                    })
                }
            },
            '"' => {
                let mut literal = String::new();
                loop {
                    match self.peek() {
                        None => return self.span_err(TokenizeError::UnexpectedEOF),
                        Some('"') => { self.next(); break },
                        Some('\\') => {
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
                if self.next() != Some('\'') {
                    return self.span_err(TokenizeError::UndefinedToken);
                }
                let mut multiline = String::new();
                loop {
                    match self.peek() {
                        None => return self.span_err(TokenizeError::UnexpectedEOF),
                        Some('\'') => match { self.next()?; self.peek() } {
                            None => return self.span_err(TokenizeError::UnexpectedEOF),
                            Some('\'') => { self.next()?; break; },
                            Some(_) => multiline.push('\''),
                        },
                        Some('\n') => {
                            // Don't push initial newline
                            self.next()?;
                            if !multiline.is_empty() {
                                multiline.push('\n');
                            }
                            while self.peek() == Some(' ')
                                    || self.peek() == Some('\t') {
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

                while let Some(digit) = self.peek().and_then(|c| c.to_digit(RADIX)) {
                    self.next();
                    num = match num.checked_mul(RADIX as i64).and_then(|num| num.checked_add(digit as i64)) {
                        Some(num) => num,
                        None => return self.span_err(TokenizeError::IntegerOverflow)
                    };
                }

                if self.peek() == Some('.') {
                    self.next();

                    let mut i = 1;
                    let mut num = num as f64;

                    while let Some(digit) = self.peek().and_then(|c| c.to_digit(RADIX)) {
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

pub fn tokenize<'a>(input: &'a str) -> impl Iterator<Item = (Span, Result<Token, TokenizeError>)> + 'a {
    Tokenizer::new(input)
}

#[cfg(test)]
mod tests {
    use crate::value::{Anchor, Value};
    use super::{Span, Token, TokenizeError};

    fn tokenize(input: &str) -> Result<Vec<Token>, TokenizeError> {
        super::tokenize(input)
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
            super::tokenize("{\n    int = 1;\n}")
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
            super::tokenize("{\n    overflow = 9999999999999999999999999999")
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
        assert_eq!(
            tokenize("a/ 3"), // <- could get mistaken for a path
            Ok(vec![Token::Ident("a".into()), Token::Div, Token::Value(3.into())])
        );
    }
    #[test]
    fn let_in() {
        assert_eq!(
            tokenize("let a = 3; in a"),
            Ok(vec![
                Token::Let,
                    Token::Ident("a".into()), Token::Equal, Token::Value(3.into()), Token::Semicolon,
                Token::In,
                    Token::Ident("a".into())
            ])
        );
    }
    #[test]
    fn with() {
        assert_eq!(
            tokenize("with namespace; expr"),
            Ok(vec![
                Token::With, Token::Ident("namespace".into()), Token::Semicolon,
                Token::Ident("expr".into())
            ])
        );
    }
    #[test]
    fn paths() {
        fn path(anchor: Anchor, path: &str) -> Result<Vec<Token>, TokenizeError> {
            Ok(vec![Token::Value(Value::Path(anchor, path.into()))])
        }
        assert_eq!(tokenize("/hello/world"), path(Anchor::Absolute, "/hello/world"));
        assert_eq!(tokenize("hello/world"), path(Anchor::Relative, "hello/world"));
        assert_eq!(tokenize("a+3/5+b"), path(Anchor::Relative, "a+3/5+b"));
        assert_eq!(tokenize("./hello/world"), path(Anchor::Relative, "hello/world"));
        assert_eq!(tokenize("~/hello/world"), path(Anchor::Home, "hello/world"));
        assert_eq!(tokenize("<hello/world>"), path(Anchor::Store, "hello/world"));
        assert_eq!(
            tokenize("https://google.com/?q=Hello+World"),
            path(Anchor::Uri, "https://google.com/?q=Hello+World")
        );
    }
    #[test]
    fn import() {
        assert_eq!(
            tokenize("import <nixpkgs>"),
            Ok(vec![
                Token::Import,
                Token::Value(Value::Path(Anchor::Store, "nixpkgs".into()))
            ])
        );
    }
}
