use crate::value::{Anchor, Value};
use std::mem;

#[derive(Debug, PartialEq, Eq)]
enum IdentType {
    Ident,
    Path,
    Store,
    Uri,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Interpol {
    Literal(String),
    Tokens(Vec<(Meta, Token)>)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Keywords
    Assert,
    Else,
    If,
    Import,
    In,
    Inherit,
    Let,
    // or, for some reason, is also a valid identifier
    // OrDefault,
    Rec,
    Then,
    With,

    // Symbols
    CurlyBOpen,
    CurlyBClose,
    SquareBOpen,
    SquareBClose,
    Assign,
    At,
    Colon,
    Comma,
    Dot,
    Ellipsis,
    Question,
    Semicolon,

    // Operators
    ParenClose,
    ParenOpen,
    Concat,
    Invert,
    Merge,

    Add,
    Sub,
    Mul,
    Div,

    And,
    Equal,
    Implication,
    Less,
    LessOrEq,
    More,
    MoreOrEq,
    NotEqual,
    Or,

    // Identifiers and values
    Dynamic(Vec<(Meta, Token)>),
    Ident(String),
    Interpol {
        multiline: bool,
        parts: Vec<Interpol>
    },
    Value(Value),
}
impl Token {
    /// Returns true if this token should be used as a function argument.
    /// Example:
    /// add 1 2 + 3
    /// ^   ^ ^ ^
    /// |   | | | false
    /// |   | | true
    /// |   | true
    /// | true
    pub fn is_fn_arg(&self) -> bool {
        match self {
            Token::Rec | Token::CurlyBOpen | Token::SquareBOpen | Token::ParenOpen
                | Token::Ident(_) | Token::Value(_) | Token::Interpol { .. } => true,
            _ => false
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Meta {
    pub span: Span,
    pub comments: Vec<String>
}
impl Meta {
    pub fn until(mut self, other: &Meta) -> Meta {
        self.span = self.span.until(other.span);
        self
    }
}
impl From<Span> for Meta {
    fn from(span: Span) -> Meta {
        Meta { span, ..Default::default() }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: Option<usize>
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
    TrailingSlash,
    #[fail(display = "unclosed multiline comment")]
    UnclosedComment,
}

fn is_valid_path_char(c: char) -> bool {
    match c {
        'a'..='z' | 'A'..='Z' | '0'..='9' | '/' | '_' | '.' | '+' | '-' => true,
        _ => false
    }
}

type Result<T> = std::result::Result<T, (Span, TokenizeError)>;
type Item = Result<(Meta, Token)>;

pub struct Tokenizer<'a> {
    input: &'a str,
    cursor: usize
}
impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            cursor: 0
        }
    }

    fn span_start(&mut self) -> Span {
        Span {
            start: self.cursor,
            end: None
        }
    }
    fn span_err(&self, meta: Meta, error: TokenizeError) -> Option<Item> {
        Some(Err((meta.span, error)))
    }
    fn span_end(&self, mut meta: Meta, token: Token) -> Option<Item> {
        meta.span.end = Some(self.cursor);
        Some(Ok((meta, token)))
    }

    fn next(&mut self) -> Option<char> {
        let c = self.peek();
        if let Some(c) = c {
            self.input = &self.input[c.len_utf8()..];
            self.cursor += 1;
        }
        c
    }
    fn peek(&self) -> Option<char> {
        self.input.chars().next()
    }

    fn next_ident<P, F>(&mut self, prefix: P, include: F) -> String
        where
            P: IntoIterator<Item = char>,
            F: Fn(char) -> bool
    {
        let mut ident = String::with_capacity(self.input.chars().take_while(|&c| include(c)).count());

        for c in prefix.into_iter() {
            ident.push(c);
        }

        loop {
            match self.peek() {
                Some(c) if include(c) => ident.push(self.next().unwrap()),
                _ => break,
            }
        }
        ident
    }
    fn next_interpol(&mut self, start: Span) -> Result<Vec<(Meta, Token)>> {
        self.next().expect("next_interpol was called in an invalid context");

        let mut tokens = Vec::new();
        let mut count = 0;
        loop {
            match Iterator::next(self) {
                None => return Err((start, TokenizeError::UnexpectedEOF)),
                Some(token) => {
                    let token = match token {
                        Ok(inner) => inner,
                        Err(err) => return Err(err)
                    };
                    match token.1 {
                        Token::CurlyBOpen => count += 1,
                        Token::CurlyBClose if count == 0 => break,
                        Token::CurlyBClose => count -= 1,
                        _ => ()
                    }
                    tokens.push(token);
                }
            }
        }

        Ok(tokens)
    }
    fn next_string(&mut self, meta: Meta, multiline: bool) -> Option<Item> {
        fn remove_shared_indent(indent: usize, string: &mut String) {
            let mut pos = 0;
            while let Some(newline) = string[pos..].find('\n') {
                pos += newline + 1;
                let end = string.len().min(pos + indent);
                string.drain(pos..end);
            }
        }

        let mut interpol = Vec::new();
        let mut literal = String::new();

        let mut min_indent = None;
        let mut last_indent = 0;
        let mut first_newline = true;
        while multiline && self.peek().map(char::is_whitespace).unwrap_or(false) {
            if self.peek() == Some('\n') {
                last_indent = 0;
                if !first_newline {
                    literal.push(self.next().unwrap());
                } else {
                    literal.clear();
                    self.next().unwrap();
                    first_newline = false;
                }
                continue;
            }

            // Will later remove common indention
            last_indent += 1;
            literal.push(self.next().unwrap());
        }

        loop {
            let whitespace = self.peek().map(char::is_whitespace).unwrap_or(false);
            match self.peek() {
                None => return self.span_err(meta, TokenizeError::UnexpectedEOF),
                Some('"') if !multiline => { self.next().unwrap(); break },
                Some('\'') if multiline => match { self.next().unwrap(); self.peek() } {
                    None => return self.span_err(meta, TokenizeError::UnexpectedEOF),
                    Some('\'') => match { self.next().unwrap(); self.peek() } {
                        Some('n') => { self.next().unwrap(); literal.push('\n'); },
                        Some('r') => { self.next().unwrap(); literal.push('\r'); },
                        Some('t') => { self.next().unwrap(); literal.push('\t'); },
                        Some('\'') => { self.next().unwrap(); literal.push_str("''"); },
                        Some('$') => { self.next().unwrap(); literal.push('$'); },
                        Some('\\') => { self.next().unwrap(); literal.push(self.next()?); },
                        _ => break
                    }
                    Some(_) => literal.push('\''),
                },
                Some('\n') if multiline => {
                    // Don't push initial newline
                    self.next()?;

                    literal.push('\n');

                    last_indent = 0;
                    while self.peek().map(|c| c.is_whitespace() && c != '\n').unwrap_or(false) {
                        last_indent += 1;
                        // Will later remove common indention
                        literal.push(self.next().unwrap());
                    }
                },
                Some('\\') if !multiline => {
                    self.next()?;
                    literal.push(match self.next()? {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        c => c
                    });
                },
                Some('$') => match { self.next().unwrap(); self.peek() } {
                    Some('{') => {
                        if !literal.is_empty() {
                            interpol.push(Interpol::Literal(mem::replace(&mut literal, String::new())));
                        }
                        interpol.push(Interpol::Tokens(match self.next_interpol(meta.span) {
                            Ok(inner) => inner,
                            Err(err) => return Some(Err(err))
                        }));
                    },
                    Some('$') => { self.next().unwrap(); literal.push_str("$$"); }
                    _ => literal.push('$')
                }
                Some(_) => {
                    literal.push(self.next()?);
                }
            }
            // Set indent after because then if it's the end of the string we
            // won't get here
            if multiline && !whitespace {
                min_indent = Some(min_indent.unwrap_or(std::usize::MAX).min(last_indent));
            }
        }

        if let Some(indent) = min_indent {
            if indent > 0 {
                // Trim from first line first
                if interpol.is_empty() {
                    literal.drain(..indent);
                } else {
                    for entry in &mut interpol {
                        if let Interpol::Literal(ref mut inner) = entry {
                            inner.drain(..indent);
                            break;
                        }
                    }
                }

                // Trim rest of the lines
                remove_shared_indent(indent, &mut literal);
                for entry in &mut interpol {
                    if let Interpol::Literal(ref mut inner) = entry {
                        remove_shared_indent(indent, inner);
                    }
                }
            }
        } else if multiline {
            // Must be all whitespace
            literal.retain(|c| c == '\n');
            for entry in &mut interpol {
                if let Interpol::Literal(ref mut inner) = entry {
                    inner.retain(|c| c == '\n');
                }
            }
        }

        if interpol.is_empty() {
            self.span_end(meta, Token::Value(Value::Str {
                multiline,
                content: literal
            }))
        } else {
            if !literal.is_empty() {
                interpol.push(Interpol::Literal(literal));
            }
            self.span_end(meta, Token::Interpol {
                multiline,
                parts: interpol
            })
        }
    }
}
impl<'a> Iterator for Tokenizer<'a> {
    type Item = Item;

    fn next(&mut self) -> Option<Self::Item> {
        let mut meta = Meta::default();

        // This temporary span is only used if there's an error reading the comment
        meta.span = self.span_start();

        loop {
            while self.peek().map(char::is_whitespace).unwrap_or(false) {
                self.next()?;
            }

            match self.peek() {
                Some('#') => {
                    let end = self.input.find('\n')
                        .map(|i| i + 1)
                        .unwrap_or(self.input.len());

                    meta.comments.push(self.input[1..end].to_string());
                    self.input = &self.input[end..];
                    self.cursor += end;
                },
                Some('/') => {
                    if self.input[1..].chars().next() != Some('*') {
                        break;
                    }

                    let end = match self.input.find("*/") {
                        Some(end) => end,
                        None => return self.span_err(meta, TokenizeError::UnclosedComment)
                    };

                    let comment = self.input[2..end].to_string();

                    let target = &self.input[end+2..];
                    while self.input.as_ptr() < target.as_ptr() {
                        self.next()?;
                    }

                    meta.comments.push(comment);
                },
                _ => break
            }
        }

        meta.span = self.span_start();

        if self.input.starts_with("...") {
            self.cursor += 3;
            self.input = &self.input[3..];
            return self.span_end(meta, Token::Ellipsis);
        }

        // Check if it's a path
        let store_path = self.peek() == Some('<');
        let mut lookahead = self.input.chars().skip_while(|c| match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '.' | '+' | '-' => true,
            '<' | '/' => store_path,
            _ => false
        });
        let kind = match (lookahead.next(), lookahead.next()) {
            // a//b parses as Merge(a, b)
            (Some('/'), Some('/')) => None,
            (Some('/'), Some(c)) if !c.is_whitespace() => Some(IdentType::Path),
            (Some('>'), _) => Some(IdentType::Store),
            (Some(':'), Some(c)) if !c.is_whitespace() => Some(IdentType::Uri),
            _ => None
        };

        let c = self.next()?;

        if c == '~' || kind == Some(IdentType::Path) {
            let (anchor, prefix) = match c {
                '~' => if self.next() != Some('/') {
                    return self.span_err(meta, TokenizeError::UndefinedToken);
                } else {
                    (Anchor::Home, None)
                },
                '/' => (Anchor::Absolute, Some('/')),
                c => (Anchor::Relative, Some(c))
            };
            let ident = self.next_ident(prefix, is_valid_path_char);
            if ident.ends_with('/') {
                return self.span_err(meta, TokenizeError::TrailingSlash);
            }
            return self.span_end(meta, Token::Value(Value::Path(anchor, ident)));
        }

        match c {
            '=' if self.peek() == Some('=') => { self.next()?; self.span_end(meta, Token::Equal) },
            '!' if self.peek() == Some('=') => { self.next()?; self.span_end(meta, Token::NotEqual) },
            '!' => self.span_end(meta, Token::Invert),
            '{' => self.span_end(meta, Token::CurlyBOpen),
            '}' => self.span_end(meta, Token::CurlyBClose),
            '[' => self.span_end(meta, Token::SquareBOpen),
            ']' => self.span_end(meta, Token::SquareBClose),
            '@' => self.span_end(meta, Token::At),
            ':' => self.span_end(meta, Token::Colon),
            ',' => self.span_end(meta, Token::Comma),
            '.' => self.span_end(meta, Token::Dot),
            '=' => self.span_end(meta, Token::Assign),
            '?' => self.span_end(meta, Token::Question),
            ';' => self.span_end(meta, Token::Semicolon),
            '(' => self.span_end(meta, Token::ParenOpen),
            ')' => self.span_end(meta, Token::ParenClose),
            '+' if self.peek() == Some('+') => { self.next()?; self.span_end(meta, Token::Concat) },
            '-' if self.peek() == Some('>') => { self.next()?; self.span_end(meta, Token::Implication) },
            '/' if self.peek() == Some('/') => { self.next()?; self.span_end(meta, Token::Merge) },
            '+' => self.span_end(meta, Token::Add),
            '-' => self.span_end(meta, Token::Sub),
            '*' => self.span_end(meta, Token::Mul),
            '/' => self.span_end(meta, Token::Div),
            '<' if kind == Some(IdentType::Store) => {
                let ident = self.next_ident(None, is_valid_path_char);
                if self.next() != Some('>') {
                    return self.span_err(meta, TokenizeError::UndefinedToken);
                }
                self.span_end(meta, Token::Value(Value::Path(Anchor::Store, ident)))
            },
            '&' if self.peek() == Some('&') => { self.next()?; self.span_end(meta, Token::And) },
            '|' if self.peek() == Some('|') => { self.next()?; self.span_end(meta, Token::Or) },
            '<' if self.peek() == Some('=') => { self.next()?; self.span_end(meta, Token::LessOrEq) },
            '<' => self.span_end(meta, Token::Less),
            '>' if self.peek() == Some('=') => { self.next()?; self.span_end(meta, Token::MoreOrEq) },
            '>' => self.span_end(meta, Token::More),
            '$' if self.peek() == Some('{') => match self.next_interpol(meta.span) {
                Ok(tokens) => self.span_end(meta, Token::Dynamic(tokens)),
                Err(err) => Some(Err(err))
            },
            'a'..='z' | 'A'..='Z' | '_' if kind != Some(IdentType::Store) => {
                let kind = kind.unwrap_or(IdentType::Ident);
                assert_ne!(kind, IdentType::Path, "paths are checked earlier");
                let ident = self.next_ident(Some(c), |c| match c {
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '\'' => true,
                    ':' | '?' | '@' | '&' | '=' | '$' | ',' | '!'
                        | '~' | '*' | '%' => kind == IdentType::Uri,
                    c => kind == IdentType::Uri && is_valid_path_char(c),
                });
                if kind == IdentType::Ident {
                    self.span_end(meta, match &*ident {
                        "assert" => Token::Assert,
                        "else" => Token::Else,
                        "if" => Token::If,
                        "import" => Token::Import,
                        "in" => Token::In,
                        "inherit" => Token::Inherit,
                        "let" => Token::Let,
                        // "or" => Token::OrDefault,
                        "rec" => Token::Rec,
                        "then" => Token::Then,
                        "with" => Token::With,

                        "true" => Token::Value(Value::Bool(true)),
                        "false" => Token::Value(Value::Bool(false)),
                        "null" => Token::Value(Value::Null),
                        _ => Token::Ident(ident),
                    })
                } else {
                    self.span_end(meta, match kind {
                        IdentType::Ident => Token::Ident(ident),
                        IdentType::Path => Token::Value(Value::Path(Anchor::Relative, ident)),
                        IdentType::Store => unreachable!(),
                        IdentType::Uri => Token::Value(Value::Path(Anchor::Uri, ident)),
                    })
                }
            },
            '"' => self.next_string(meta, false),
            '\'' if self.peek() == Some('\'') => {
                self.next()?;
                self.next_string(meta, true)
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
                        None => return self.span_err(meta, TokenizeError::IntegerOverflow)
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
                        return self.span_err(meta, TokenizeError::TrailingDecimal)
                    }

                    self.span_end(meta, Token::Value(Value::Float(num)))
                } else {
                    self.span_end(meta, Token::Value(Value::Integer(num)))
                }
            },
            _ => self.span_err(meta, TokenizeError::UndefinedToken)
        }
    }
}

pub fn tokenize<'a>(input: &'a str) -> impl Iterator<Item = Item> + 'a {
    Tokenizer::new(input)
}

#[cfg(test)]
mod tests {
    use crate::value::{Anchor, Value};
    use super::{Interpol, Meta, Span, Token, TokenizeError};

    fn tokenize(input: &str) -> Result<Vec<Token>, TokenizeError> {
        super::tokenize(input)
            .map(|result| result
                .map(|(_, token)| token)
                .map_err(|(_, err)| err))
            .collect()
    }
    fn tokenize_span(input: &str) -> Result<Vec<(Meta, Token)>, (Span, TokenizeError)> {
        super::tokenize(input).collect()
    }

    #[test]
    fn basic_int_set() {
        assert_eq!(
            tokenize("{ int = 42; }"),
            Ok(vec![Token::CurlyBOpen, Token::Ident("int".into()), Token::Assign,
            Token::Value(42.into()), Token::Semicolon, Token::CurlyBClose])
        );
    }
    #[test]
    fn basic_float_set() {
        assert_eq!(
            tokenize("{ float = 1.234; }"),
            Ok(vec![Token::CurlyBOpen, Token::Ident("float".into()), Token::Assign,
            Token::Value(1.234.into()), Token::Semicolon, Token::CurlyBClose])
        );
    }
    #[test]
    fn basic_string_set() {
        assert_eq!(
            tokenize(r#"{ string = "Hello \"World\""; }"#),
            Ok(vec![Token::CurlyBOpen, Token::Ident("string".into()), Token::Assign,
            Token::Value("Hello \"World\"".into()), Token::Semicolon, Token::CurlyBClose])
        );
    }
    #[test]
    fn meta() {
        assert_eq!(
            tokenize_span("{\n    int /* hi */ = 1; # testing comments!\n}"),
            Ok(vec![
                (meta! { start: 0, end: 1 }, Token::CurlyBOpen),
                (meta! { start: 6, end: 9 }, Token::Ident("int".to_string())),
                (
                    Meta {
                        comments: vec![" hi ".into()],
                        span: Span { start: 19, end: Some(20) },
                    },
                    Token::Assign
                ),
                (meta! { start: 21, end: 22 }, Token::Value(1.into())),
                (meta! { start: 22, end: 23 }, Token::Semicolon),
                (
                    Meta {
                        comments: vec![" testing comments!\n".into()],
                        span: Span { start: 44, end: Some(45) }
                    },
                    Token::CurlyBClose
                )
            ])
        );
        assert_eq!(
            tokenize_span("{\n    overflow = 9999999999999999999999999999"),
            Err((Span { start: 17, end: None }, TokenizeError::IntegerOverflow))
        );
    }
    #[test]
    fn multiline() {
        assert_eq!(
            tokenize(r#"{
    multiline = ''
            
                  
        This is a multiline string :D
          indented by two
        \'\'\'\'\
        ''${ interpolation was escaped }
        two single quotes: '''
        three single quotes: ''''
    '';
}"#),
            Ok(vec![
                Token::CurlyBOpen,
                    Token::Ident("multiline".into()), Token::Assign,
                    Token::Value(Value::Str {
                        multiline: true,
                        // Generate the below with nix-shell
                        content: "    \n          \nThis is a multiline string :D\n  \
                            indented by two\n\\'\\'\\'\\'\\\n${ interpolation was escaped }\n\
                            two single quotes: ''\nthree single quotes: '''\n".into()
                    }),
                Token::Semicolon, Token::CurlyBClose
            ])
        );
        assert_eq!(
            tokenize("''\n  \n    \n \n ''"),
            Ok(vec![Token::Value(Value::Str {
                multiline: true,
                content: "\n\n\n".into()
            })])
        );
        assert_eq!(
            tokenize("''\n  \n    \n a\n''"),
            Ok(vec![Token::Value(Value::Str {
                multiline: true,
                content: " \n   \na\n".into()
            })])
        );
        assert_eq!(
            tokenize("''  \n    \n a\n''"),
            Ok(vec![Token::Value(Value::Str {
                multiline: true,
                content: "   \na\n".into()
            })])
        );
    }
    #[test]
    fn special_escape() {
        assert_eq!(
            tokenize(r#" "$${test}" "#),
            Ok(vec![Token::Value("$${test}".into())])
        );
        assert_eq!(
            tokenize(r#" ''$${test}'' "#),
            Ok(vec![Token::Value(Value::Str {
                multiline: true,
                content: "$${test}".into()
            })])
        );
    }
    #[test]
    fn interpolation() {
        assert_eq!(
            tokenize_span(r#" "Hello, ${ { world = "World"; }.world }!" "#),
            Ok(vec![(
                meta! { start: 1, end: 42 },
                Token::Interpol {
                    multiline: false,
                    parts: vec![
                        Interpol::Literal("Hello, ".into()),
                        Interpol::Tokens(vec![
                            (meta! { start: 12, end: 13 }, Token::CurlyBOpen),
                            (meta! { start: 14, end: 19 }, Token::Ident("world".into())),
                            (meta! { start: 20, end: 21 }, Token::Assign),
                            (meta! { start: 22, end: 29 }, Token::Value("World".into())),
                            (meta! { start: 29, end: 30 }, Token::Semicolon),
                            (meta! { start: 31, end: 32 }, Token::CurlyBClose),
                            (meta! { start: 32, end: 33 }, Token::Dot),
                            (meta! { start: 33, end: 38 }, Token::Ident("world".into()))
                        ]),
                        Interpol::Literal("!".into())
                    ]
                }
            )])
        );
        assert_eq!(
            tokenize_span(r#" "\$${test}" "#),
            Ok(vec![(
                meta! { start: 1, end: 12 },
                Token::Interpol {
                    multiline: false,
                    parts: vec![
                        Interpol::Literal("$".into()),
                        Interpol::Tokens(vec![
                            (meta! { start: 6, end: 10 }, Token::Ident("test".into()))
                        ])
                    ]
                }
            )])
        );
        assert_eq!(
            tokenize_span(r#" ''''$${test}'' "#),
            Ok(vec![(
                meta! { start: 1, end: 15 },
                Token::Interpol {
                    multiline: true,
                    parts: vec![
                        Interpol::Literal("$".into()),
                        Interpol::Tokens(vec![
                            (meta! { start: 8, end: 12 }, Token::Ident("test".into()))
                        ])
                    ]
                }
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
                    Token::Ident("a".into()), Token::Assign, Token::Value(3.into()), Token::Semicolon,
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
        assert_eq!(tokenize("1-2/3"), path(Anchor::Relative, "1-2/3"));
        assert_eq!(tokenize("./hello/world"), path(Anchor::Relative, "./hello/world"));
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
    #[test]
    fn list() {
        assert_eq!(
            tokenize(r#"[a 2 3 "lol"]"#),
            Ok(vec![
               Token::SquareBOpen,
               Token::Ident("a".into()), Token::Value(2.into()), Token::Value(3.into()),
               Token::Value("lol".into()),
               Token::SquareBClose
            ])
        );
        assert_eq!(
            tokenize("[1] ++ [2] ++ [3]"),
            Ok(vec![
               Token::SquareBOpen, Token::Value(1.into()), Token::SquareBClose, Token::Concat,
               Token::SquareBOpen, Token::Value(2.into()), Token::SquareBClose, Token::Concat,
               Token::SquareBOpen, Token::Value(3.into()), Token::SquareBClose
            ])
        );
    }
    #[test]
    fn functions() {
        assert_eq!(
            tokenize("a: b: a + b"),
            Ok(vec![
               Token::Ident("a".into()), Token::Colon, Token::Ident("b".into()), Token::Colon,
               Token::Ident("a".into()), Token::Add, Token::Ident("b".into())
            ])
        );
    }
    #[test]
    fn patterns() {
        assert_eq!(
            tokenize(r#"{ a, b ? "default", ... } @ outer"#),
            Ok(vec![
               Token::CurlyBOpen,
                   Token::Ident("a".into()), Token::Comma,
                   Token::Ident("b".into()), Token::Question, Token::Value("default".into()), Token::Comma,
                   Token::Ellipsis,
               Token::CurlyBClose,
               Token::At,
               Token::Ident("outer".into())
            ])
        );
    }
    #[test]
    fn combine() {
        assert_eq!(
            tokenize("a//b"),
            Ok(vec![Token::Ident("a".into()), Token::Merge, Token::Ident("b".into())])
        );
    }
    #[test]
    fn ifs() {
        assert_eq!(
            tokenize("false -> !false && false == true || true"),
            Ok(vec![
                Token::Value(false.into()), Token::Implication,
                Token::Invert, Token::Value(false.into()),
                Token::And,
                Token::Value(false.into()), Token::Equal, Token::Value(true.into()),
                Token::Or,
                Token::Value(true.into())
            ])
        );
        assert_eq!(
            tokenize("1 < 2 && 2 <= 2 && 2 > 1 && 2 >= 2"),
            Ok(vec![
                Token::Value(1.into()), Token::Less, Token::Value(2.into()),
                Token::And,
                Token::Value(2.into()), Token::LessOrEq, Token::Value(2.into()),
                Token::And,
                Token::Value(2.into()), Token::More, Token::Value(1.into()),
                Token::And,
                Token::Value(2.into()), Token::MoreOrEq, Token::Value(2.into())
            ])
        );
        assert_eq!(
            tokenize("1 == 1 && 2 != 3"),
            Ok(vec![
                Token::Value(1.into()), Token::Equal, Token::Value(1.into()),
                Token::And,
                Token::Value(2.into()), Token::NotEqual, Token::Value(3.into())
            ])
        );
        assert_eq!(
            tokenize("if false then 1 else if true then 2 else 3"),
            Ok(vec![
                Token::If, Token::Value(false.into()), Token::Then,
                    Token::Value(1.into()),
                Token::Else,
                    Token::If, Token::Value(true.into()), Token::Then,
                        Token::Value(2.into()),
                    Token::Else,
                        Token::Value(3.into())
            ])
        );
    }
    #[test]
    fn dynamic_attrs() {
        assert_eq!(
            tokenize("a.${b}.c"),
            Ok(vec![
               Token::Ident("a".into()),
               Token::Dot,
               Token::Dynamic(vec![
                    (meta! { start: 4, end: 5 }, Token::Ident("b".into()))
               ]),
               Token::Dot,
               Token::Ident("c".into())
            ])
        );
    }
}
