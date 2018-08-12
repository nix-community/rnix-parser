use crate::value::{Anchor, Value};
use std::mem;

#[derive(PartialEq, Eq)]
enum IdentType {
    Uri,
    Path,
    Ident
}

#[derive(Clone, Debug, PartialEq)]
pub enum Interpol {
    Literal(String),
    Tokens(Vec<(Span, Token)>)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    BracketOpen,
    BracketClose,
    Equal,
    Semicolon,
    Dot,
    Ident(String),
    Value(Value),
    Interpol(Vec<Interpol>),
    Let,
    In,
    With,
    Import,
    Rec,

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

impl Span {
    pub fn until(self, other: Span) -> Span {
        Span {
            start: self.start,
            end: other.end
        }
    }
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
    fn peek(&self) -> Option<char> {
        self.input.chars().next()
    }
    fn next_ident<F>(&mut self, prefix: Option<char>, include: F) -> String
        where F: Fn(char) -> bool
    {
        let capacity = self.input.chars().take_while(|&c| include(c)).count()
            + if prefix.is_some() { 1 } else { 0 };
        let mut ident = String::with_capacity(capacity);
        let initial_pointer = ident.as_ptr();
        if let Some(c) = prefix {
            ident.push(c);
        }
        loop {
            match self.peek() {
                Some(c) if include(c) => ident.push(self.next().unwrap()),
                _ => break,
            }
        }
        assert_eq!(ident.as_ptr(), initial_pointer, "String reallocated, wasn't given enough capacity");
        ident
    }

    fn next_string(&mut self, multiline: bool) -> Option<(Span, Result<Token, TokenizeError>)> {
        let mut interpol = Vec::new();
        let mut literal = String::new();
        loop {
            match self.peek() {
                None => return self.span_err(TokenizeError::UnexpectedEOF),
                Some('"') if !multiline => { self.next(); break },
                Some('\'') if multiline => match { self.next()?; self.peek() } {
                    None => return self.span_err(TokenizeError::UnexpectedEOF),
                    Some('\'') => { self.next()?; break; },
                    Some(_) => literal.push('\''),
                },
                Some('\n') if multiline => {
                    // Don't push initial newline
                    self.next()?;
                    if !literal.is_empty() {
                        literal.push('\n');
                    }
                    while self.peek() == Some(' ')
                            || self.peek() == Some('\t') {
                        self.next();
                    }
                },
                Some('\\') if !multiline => {
                    self.next()?;
                    literal.push(self.next()?);
                },
                Some('$') => match { self.next(); self.peek() } {
                    Some('{') => {
                        self.next()?;
                        interpol.push(Interpol::Literal(mem::replace(&mut literal, String::new())));

                        let mut tokens = Vec::new();
                        let mut count = 0;
                        loop {
                            let start_backup = self.span_start;
                            let token = Iterator::next(self);
                            self.span_start = start_backup;

                            match token {
                                None => return self.span_err(TokenizeError::UnexpectedEOF),
                                Some(token) => {
                                    let token = match token {
                                        result @ (_, Err(_)) => return Some(result),
                                        (span, Ok(token)) => (span, token)
                                    };
                                    match token.1 {
                                        Token::BracketOpen => count += 1,
                                        Token::BracketClose if count == 0 => break,
                                        Token::BracketClose => count -= 1,
                                        _ => ()
                                    }
                                    tokens.push(token);
                                }
                            }
                        }

                        interpol.push(Interpol::Tokens(tokens));
                    },
                    _ => literal.push('$')
                }
                Some(_) => {
                    literal.push(self.next()?);
                }
            }
        }

        if interpol.is_empty() {
            self.span_end(Token::Value(Value::Str(literal)))
        } else {
            if !literal.is_empty() {
                interpol.push(Interpol::Literal(literal));
            }
            self.span_end(Token::Interpol(interpol))
        }
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
            '.' | '~' if self.peek() == Some('/') => {
                self.next()?; // the slash
                let anchor = match c {
                    '.' => Anchor::Relative,
                    '~' => Anchor::Home,
                    _ => unreachable!()
                };
                let ident = self.next_ident(None, is_valid_path_char);
                if ident.ends_with('/') {
                    return self.span_err(TokenizeError::TrailingSlash);
                }
                self.span_end(Token::Value(Value::Path(anchor, ident)))
            },
            '.' => self.span_end(Token::Dot),
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
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
                    ':' | '?' | '@' | '&' | '=' | '$' | ',' | '!'
                        | '~' | '*' | '\'' | '%' => kind == IdentType::Uri,
                    c => (kind == IdentType::Path || kind == IdentType::Uri) && is_valid_path_char(c),
                });
                if kind == IdentType::Ident {
                    self.span_end(match &*ident {
                        "let" => Token::Let,
                        "in" => Token::In,
                        "with" => Token::With,
                        "import" => Token::Import,
                        "rec" => Token::Rec,
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
            '"' => self.next_string(false),
            '\'' if self.peek() == Some('\'') => {
                self.next()?;
                self.next_string(true)
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
    use super::{Interpol, Span, Token, TokenizeError};

    fn tokenize(input: &str) -> Result<Vec<Token>, TokenizeError> {
        super::tokenize(input)
            .map(|(_, result)| result)
            .collect()
    }
    fn tokenize_span(input: &str) -> Result<Vec<(Span, Token)>, (Span, TokenizeError)> {
        super::tokenize(input)
            .map(|(span, result)| result
                .map(|token| (span, token))
                .map_err(|err| (span, err)))
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
            tokenize_span("{\n    int = 1;\n}"),
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
            tokenize_span("{\n    overflow = 9999999999999999999999999999"),
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
\'\'\'\'\
"#.into()),
                Token::Semicolon, Token::BracketClose
            ])
        );
    }
    #[test]
    fn interpolation() {
        assert_eq!(
            tokenize_span(r#" "Hello, ${ { world = "World"; }.world }!" "#),
            Ok(vec![(
                Span { start: (0, 1), end: Some((0, 42)) },
                Token::Interpol(vec![
                    Interpol::Literal("Hello, ".into()),
                    Interpol::Tokens(vec![
                        (Span { start: (0, 12), end: Some((0, 13)) }, Token::BracketOpen),
                        (Span { start: (0, 14), end: Some((0, 19)) }, Token::Ident("world".into())),
                        (Span { start: (0, 20), end: Some((0, 21)) }, Token::Equal),
                        (Span { start: (0, 22), end: Some((0, 29)) }, Token::Value("World".into())),
                        (Span { start: (0, 29), end: Some((0, 30)) }, Token::Semicolon),
                        (Span { start: (0, 31), end: Some((0, 32)) }, Token::BracketClose),
                        (Span { start: (0, 32), end: Some((0, 33)) }, Token::Dot),
                        (Span { start: (0, 33), end: Some((0, 38)) }, Token::Ident("world".into()))
                    ]),
                    Interpol::Literal("!".into())
                ])
            )])
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
