//! The tokenizer: turns a string into tokens, such as numbers, strings, and keywords

use crate::value::{Anchor, Value};
use std::{fmt::{self, Write}, mem};

#[cfg(feature = "smol_str")]
use smol_str::SmolStr;
#[cfg(not(feature = "smol_str"))]
type SmolStr = String;

/// A span, information about where in the original string a token started and ended
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Span {
    /// Offset (in bytes) in the original string where the token started
    pub start: u32,
    /// Offset (in bytes) in the original string where the token ended, including the next char
    pub end: Option<u32>
}
impl Span {
    /// Set the end of `self` to `other`
    pub fn until(self, other: Span) -> Span {
        Span {
            start: self.start,
            end: other.end
        }
    }
}

/// Information about token spacing
#[derive(Clone, Debug, PartialEq)]
pub enum Trivia {
    Newline(u32),
    Spaces(u32),
    Tabs(u32),
    Comment {
        span: Span,
        multiline: bool,
        content: SmolStr
    }
}
impl fmt::Display for Trivia {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Trivia::Newline(amount) => for _ in 0..amount {
                f.write_char('\n')?;
            },
            Trivia::Spaces(amount) => for _ in 0..amount {
                f.write_char(' ')?;
            },
            Trivia::Tabs(amount) => for _ in 0..amount {
                f.write_char('\t')?;
            },
            Trivia::Comment { span: _, multiline, ref content } => {
                if multiline {
                    f.write_str("/*")?;
                } else {
                    f.write_char('#')?;
                }
                f.write_str(content)?;
                if multiline {
                    f.write_str("*/")?;
                }
            },
        }
        Ok(())
    }
}

/// Metadata for a token, such as span information and trivia
#[derive(Clone, Debug, Default, PartialEq)]
#[must_use = "all metadata must be preserved"]
pub struct Meta {
    pub span: Span,
    pub leading: Vec<Trivia>,
    pub trailing: Vec<Trivia>
}

#[derive(Debug, PartialEq, Eq)]
enum IdentType {
    Ident,
    Path,
    Store,
    Uri
}

/// An interpolation part
#[derive(Clone, Debug, PartialEq)]
pub enum Interpol {
    Literal {
        original: String,
        content: String
    },
    Tokens(Vec<(Meta, Token)>, Meta)
}

/// A token, such as a string literal, a number, or a keyword
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // So we get trailing trivia
    EOF,

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
    Dynamic(Vec<(Meta, Token)>, Meta),
    Ident(String),
    Interpol {
        multiline: bool,
        parts: Vec<Interpol>
    },
    Value(Value),
}
impl Token {
    /// Returns true if this token should be used as a function argument.
    /// ```ignore
    /// Example:
    /// add 1 2 + 3
    /// ^   ^ ^ ^
    /// |   | | +- false
    /// |   | +- true
    /// |   +- true
    /// +- true
    /// ```
    pub fn is_fn_arg(&self) -> bool {
        match self {
            Token::Rec | Token::CurlyBOpen | Token::SquareBOpen | Token::ParenOpen
                | Token::Ident(_) | Token::Value(_) | Token::Interpol { .. } => true,
            _ => false
        }
    }
}

/// An error that occured during tokenizing
#[derive(Clone, Copy, Debug, Fail, PartialEq)]
pub enum TokenizeError {
    #[fail(display = "dot after number, but no decimals")]
    TrailingDecimal,
    #[fail(display = "unexpected eof")]
    UnexpectedEOF,
    #[fail(display = "undefined token")]
    UndefinedToken,
    #[fail(display = "paths cannot have a trailing slash")]
    TrailingSlash,
    #[fail(display = "unclosed multiline comment")]
    UnclosedComment
}

fn is_valid_path_char(c: char) -> bool {
    match c {
        'a'..='z' | 'A'..='Z' | '0'..='9' | '/' | '_' | '.' | '+' | '-' => true,
        _ => false
    }
}

type Result<T> = std::result::Result<T, (Span, TokenizeError)>;
type Item = Result<(Meta, Token)>;

enum Context {
    Default,
    Interpol {
        brackets: u32,
        ended: bool,
        string: bool
    }
}

/// The tokenizer. You may want to use the `tokenize` convenience function from this module instead.
#[derive(Clone, Copy)]
pub struct Tokenizer<'a> {
    input: &'a str,
    cursor: u32
}
impl<'a> Tokenizer<'a> {
    /// Create a new instance
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
    fn span_err(&self, meta: Meta, error: TokenizeError) -> Item {
        Err((meta.span, error))
    }
    fn span_end(&mut self, mut meta: Meta, ctx: &mut Context, token: Token) -> Item {
        meta.span.end = Some(self.cursor);

        let trivia = match *ctx {
            Context::Interpol { ended, string, .. } => !string || !ended,
            _ => true
        };
        if trivia {
            while let Some(trivia) = self.next_trivia(false) {
                meta.trailing.push(trivia?);
            }
        }

        Ok((meta, token))
    }

    fn next(&mut self) -> Option<char> {
        let c = self.peek();
        if let Some(c) = c {
            let len = c.len_utf8();
            self.input = &self.input[len..];
            self.cursor += len as u32;
        }
        c
    }
    fn peek(&self) -> Option<char> {
        self.input.chars().next()
    }

    fn next_trivia(&mut self, multiline: bool) -> Option<Result<Trivia>> {
        let mut span = self.span_start();

        match self.peek() {
            Some(' ') => {
                let mut amount = 0;
                while self.peek() == Some(' ') {
                    self.next().unwrap();
                    amount += 1;
                }
                Some(Ok(Trivia::Spaces(amount)))
            },
            Some('\t') => {
                let mut amount = 0;
                while self.peek() == Some('\t') {
                    self.next().unwrap();
                    amount += 1;
                }
                Some(Ok(Trivia::Tabs(amount)))
            },
            Some('\n') if multiline => {
                let mut amount = 0;
                while self.peek() == Some('\n') {
                    self.next().unwrap();
                    amount += 1;
                }
                Some(Ok(Trivia::Newline(amount)))
            },
            Some('#') => {
                let end = self.input.find('\n').unwrap_or(self.input.len());

                let content = self.input[1..end].into();
                self.input = &self.input[end..];
                self.cursor += end as u32;

                span.end = Some(self.cursor);
                Some(Ok(Trivia::Comment {
                    span,
                    multiline: false,
                    content
                }))
            },
            Some('/') => {
                if self.input[1..].chars().next() != Some('*') {
                    return None;
                }

                let end = match self.input.find("*/") {
                    Some(end) => end,
                    None => return Some(Err((span, TokenizeError::UnclosedComment)))
                };

                let content = self.input[2..end].into();

                self.input = &self.input[end+2..];
                self.cursor += end as u32 + 2;

                span.end = Some(self.cursor);
                Some(Ok(Trivia::Comment {
                    span,
                    multiline: true,
                    content
                }))
            },
            _ => None
        }
    }
    fn next_ident<P, F>(&mut self, prefix: P, include: F) -> String
        where
            P: IntoIterator<Item = char>,
            F: Fn(char) -> bool
    {
        let len = self.input.chars()
            .take_while(|&c| include(c))
            .map(|c| c.len_utf8())
            .sum();
        let mut ident = String::with_capacity(len);
        for c in prefix {
            ident.push(c);
        }
        ident.push_str(&self.input[..len]);
        self.input = &self.input[len..];
        self.cursor += len as u32;
        ident
    }
    fn next_interpol(&mut self, start: Span, string: bool) -> Result<(Vec<(Meta, Token)>, Meta)> {
        self.next().expect("next_interpol was called in an invalid context");

        let mut tokens = Vec::new();
        let mut ctx = Context::Interpol {
            brackets: 0,
            ended: false,
            string
        };
        loop {
            let token = self.next_token(&mut ctx)?;
            if token.1 == Token::EOF {
                return Err((start, TokenizeError::UnexpectedEOF));
            }
            if let Context::Interpol { ended, .. } = ctx {
                if ended {
                    break Ok((tokens, token.0));
                }
            } else {
                unreachable!("context should never switch, wtf");
            }
            tokens.push(token);
        }
    }
    fn next_string(&mut self, meta: Meta, ctx: &mut Context, multiline: bool) -> Item {
        fn remove_shared_indent(indent: usize, string: &mut String) {
            let mut pos = 0;
            while let Some(newline) = string[pos..].find('\n') {
                pos += newline + 1;
                let end = string.len().min(pos + indent);
                string.drain(pos..end);
            }
        }

        let mut original_start = *self;

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

        let mut original_len = self.cursor - original_start.cursor;

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
                        Some('\\') => { self.next().unwrap(); literal.push(match self.next() {
                            None => return self.span_err(meta, TokenizeError::UnexpectedEOF),
                            Some(c) => c
                        }); },
                        _ => break
                    }
                    Some(_) => literal.push('\''),
                },
                Some('\n') if multiline => {
                    literal.push(self.next().unwrap());

                    last_indent = 0;
                    while self.peek().map(|c| c == ' ' || c == '\n').unwrap_or(false) {
                        last_indent += 1;
                        // Will later remove common indention
                        literal.push(self.next().unwrap());
                    }
                },
                Some('\\') if !multiline => {
                    self.next().unwrap();
                    literal.push(match self.next() {
                        None => return self.span_err(meta, TokenizeError::UnexpectedEOF),
                        Some('n') => '\n',
                        Some('r') => '\r',
                        Some('t') => '\t',
                        Some(c) => c
                    });
                },
                Some('$') => match { self.next().unwrap(); self.peek() } {
                    Some('{') => {
                        if original_len > 0 {
                            interpol.push(Interpol::Literal {
                                original: original_start.input[..original_len as usize].into(),
                                content: mem::replace(&mut literal, String::new())
                            });
                        }
                        let (tokens, close) = self.next_interpol(meta.span, true)?;
                        original_start = *self;
                        interpol.push(Interpol::Tokens(tokens, close));
                    },
                    Some('$') => { self.next().unwrap(); literal.push_str("$$"); }
                    _ => literal.push('$')
                }
                Some(_) => {
                    literal.push(self.next().unwrap());
                }
            }

            // When the string is closing we won't get to this point:

            original_len = self.cursor - original_start.cursor;

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
                        if let Interpol::Literal { ref mut content, .. } = entry {
                            content.drain(..indent);
                            break;
                        }
                    }
                }

                // Trim rest of the lines
                remove_shared_indent(indent, &mut literal);
                for entry in &mut interpol {
                    if let Interpol::Literal { ref mut content, .. } = entry {
                        remove_shared_indent(indent, content);
                    }
                }
            }
        } else if multiline {
            // Must be all whitespace
            literal.retain(|c| c == '\n');
            for entry in &mut interpol {
                if let Interpol::Literal { ref mut content, .. } = entry {
                    content.retain(|c| c == '\n');
                }
            }
        }

        if interpol.is_empty() {
            self.span_end(meta, ctx, Token::Value(Value::Str {
                multiline,
                original: original_start.input[..original_len as usize].into(),
                content: literal.into()
            }))
        } else {
            if original_len > 0 {
                interpol.push(Interpol::Literal {
                    original: original_start.input[..original_len as usize].into(),
                    content: literal
                });
            }
            self.span_end(meta, ctx, Token::Interpol {
                multiline,
                parts: interpol
            })
        }
    }
    fn next_token(&mut self, ctx: &mut Context) -> Item {
        let mut meta = Meta::default();

        while let Some(trivia) = self.next_trivia(true) {
            meta.leading.push(trivia?);
        }

        meta.span = self.span_start();

        if self.input.starts_with("...") {
            self.cursor += 3;
            self.input = &self.input[3..];
            return self.span_end(meta, ctx, Token::Ellipsis);
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
            (Some('/'), Some('*')) => None,
            (Some('/'), Some(c)) if !c.is_whitespace() => Some(IdentType::Path),
            (Some('>'), _) => Some(IdentType::Store),
            (Some(':'), Some(c)) if !c.is_whitespace() => Some(IdentType::Uri),
            _ => None
        };

        let c = match self.next() {
            Some(c) => c,
            None => return self.span_end(meta, ctx, Token::EOF)
        };

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
            return self.span_end(meta, ctx, Token::Value(Value::Path(anchor, ident)));
        }

        match c {
            '=' if self.peek() == Some('=') => { self.next().unwrap(); self.span_end(meta, ctx, Token::Equal) },
            '!' if self.peek() == Some('=') => { self.next().unwrap(); self.span_end(meta, ctx, Token::NotEqual) },
            '!' => self.span_end(meta, ctx, Token::Invert),
            '{' => {
                if let Context::Interpol { brackets, .. } = ctx {
                    *brackets += 1;
                }
                self.span_end(meta, ctx, Token::CurlyBOpen)
            },
            '}' => {
                if let Context::Interpol { brackets, ended, .. } = ctx {
                    match brackets.checked_sub(1) {
                        Some(new) => *brackets = new,
                        None => *ended = true
                    }
                }
                self.span_end(meta, ctx, Token::CurlyBClose)
            },
            '[' => self.span_end(meta, ctx, Token::SquareBOpen),
            ']' => self.span_end(meta, ctx, Token::SquareBClose),
            '@' => self.span_end(meta, ctx, Token::At),
            ':' => self.span_end(meta, ctx, Token::Colon),
            ',' => self.span_end(meta, ctx, Token::Comma),
            '.' => self.span_end(meta, ctx, Token::Dot),
            '=' => self.span_end(meta, ctx, Token::Assign),
            '?' => self.span_end(meta, ctx, Token::Question),
            ';' => self.span_end(meta, ctx, Token::Semicolon),
            '(' => self.span_end(meta, ctx, Token::ParenOpen),
            ')' => self.span_end(meta, ctx, Token::ParenClose),
            '+' if self.peek() == Some('+') => { self.next().unwrap(); self.span_end(meta, ctx, Token::Concat) },
            '-' if self.peek() == Some('>') => { self.next().unwrap(); self.span_end(meta, ctx, Token::Implication) },
            '/' if self.peek() == Some('/') => { self.next().unwrap(); self.span_end(meta, ctx, Token::Merge) },
            '+' => self.span_end(meta, ctx, Token::Add),
            '-' => self.span_end(meta, ctx, Token::Sub),
            '*' => self.span_end(meta, ctx, Token::Mul),
            '/' => self.span_end(meta, ctx, Token::Div),
            '<' if kind == Some(IdentType::Store) => {
                let ident = self.next_ident(None, is_valid_path_char);
                if self.next() != Some('>') {
                    return self.span_err(meta, TokenizeError::UndefinedToken);
                }
                self.span_end(meta, ctx, Token::Value(Value::Path(Anchor::Store, ident)))
            },
            '&' if self.peek() == Some('&') => { self.next().unwrap(); self.span_end(meta, ctx, Token::And) },
            '|' if self.peek() == Some('|') => { self.next().unwrap(); self.span_end(meta, ctx, Token::Or) },
            '<' if self.peek() == Some('=') => { self.next().unwrap(); self.span_end(meta, ctx, Token::LessOrEq) },
            '<' => self.span_end(meta, ctx, Token::Less),
            '>' if self.peek() == Some('=') => { self.next().unwrap(); self.span_end(meta, ctx, Token::MoreOrEq) },
            '>' => self.span_end(meta, ctx, Token::More),
            '$' if self.peek() == Some('{') => {
                let (tokens, close) = self.next_interpol(meta.span, false)?;
                self.span_end(meta, ctx, Token::Dynamic(tokens, close))
            },
            'a'..='z' | 'A'..='Z' | '_' => {
                let kind = match kind {
                    // It's detected as store if it ends with >, but if it
                    // didn't start with <, that's wrong
                    Some(IdentType::Store) | None => IdentType::Ident,
                    Some(kind) => kind,
                };
                assert_ne!(kind, IdentType::Path, "paths are checked earlier");
                let ident = self.next_ident(Some(c), |c| match c {
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '\'' => true,
                    ':' | '?' | '@' | '&' | '=' | '$' | ',' | '!'
                        | '~' | '*' | '%' => kind == IdentType::Uri,
                    c => kind == IdentType::Uri && is_valid_path_char(c),
                });
                if kind == IdentType::Ident {
                    self.span_end(meta, ctx, match &*ident {
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
                    self.span_end(meta, ctx, match kind {
                        IdentType::Ident => Token::Ident(ident),
                        IdentType::Path => Token::Value(Value::Path(Anchor::Relative, ident)),
                        IdentType::Store => unreachable!(),
                        IdentType::Uri => Token::Value(Value::Path(Anchor::Uri, ident)),
                    })
                }
            },
            '"' => self.next_string(meta, ctx, false),
            '\'' if self.peek() == Some('\'') => {
                self.next().unwrap();
                self.next_string(meta, ctx, true)
            },
            '0'..='9' => {
                let mut len = self.input.chars()
                    .take_while(|&c| c >= '0' && c <= '9')
                    .map(|c| c.len_utf8())
                    .sum();
                let float = if self.input.bytes().nth(len) == Some(b'.') {
                    let trailing: usize = self.input[len+1..].chars()
                        .take_while(|&c| c >= '0' && c <= '9')
                        .map(|c| c.len_utf8())
                        .sum();
                    if trailing == 0 {
                        return self.span_err(meta, TokenizeError::TrailingDecimal)
                    }
                    len += 1 + trailing;
                    true
                } else {
                    false
                };

                let mut num = String::with_capacity(1 + len);
                num.push(c);
                num.push_str(&self.input[..len]);
                self.input = &self.input[len..];
                self.cursor += len as u32;

                self.span_end(meta, ctx, Token::Value(if float {
                    Value::Float(num)
                } else {
                    Value::Integer(num)
                }))
            },
            _ => self.span_err(meta, TokenizeError::UndefinedToken)
        }
    }
}
impl<'a> Iterator for Tokenizer<'a> {
    type Item = Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.input.is_empty() {
            None
        } else {
            Some(self.next_token(&mut Context::Default))
        }
    }
}

/// Convenience function for turning a string into an iterator of tokens
pub fn tokenize<'a>(input: &'a str) -> impl Iterator<Item = Item> + 'a {
    Tokenizer::new(input)
}

#[cfg(test)]
mod tests {
    use crate::value::{Anchor, Value};
    use super::{Interpol, Meta, Span, Token, TokenizeError, Trivia};

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

    fn interpol(content: &str) -> Interpol {
        interpol_original(content, content)
    }
    fn interpol_original(original: &str, content: &str) -> Interpol {
        Interpol::Literal {
            original: original.into(),
            content: content.into()
        }
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
            Token::Value(Value::Str {
                multiline: false,
                original: r#"Hello \"World\""#.into(),
                content: "Hello \"World\"".into(),
            }), Token::Semicolon, Token::CurlyBClose])
        );
    }
    #[test]
    fn meta() {
        assert_eq!(
            tokenize_span("{\n    int /* hi */ = 1; # testing comments!\n}\n# trailing"),
            Ok(vec![
                (
                    Meta {
                        span: Span { start: 0, end: Some(1) },
                        leading: Vec::new(),
                        trailing: Vec::new()
                    },
                    Token::CurlyBOpen
                ),
                (
                    Meta {
                        span: Span { start: 6, end: Some(9) },
                        leading: vec![Trivia::Newline(1), Trivia::Spaces(4)],
                        trailing: vec![
                            Trivia::Spaces(1),
                            Trivia::Comment {
                                span: Span { start: 10, end: Some(18) },
                                multiline: true,
                                content: " hi ".into()
                            },
                            Trivia::Spaces(1)
                        ]
                    },
                    Token::Ident("int".into())
                ),
                (
                    Meta {
                        span: Span { start: 19, end: Some(20) },
                        leading: Vec::new(),
                        trailing: vec![Trivia::Spaces(1)]
                    },
                    Token::Assign
                ),
                (
                    Meta {
                        span: Span { start: 21, end: Some(22) },
                        leading: Vec::new(),
                        trailing: Vec::new()
                    },
                    Token::Value(1.into())
                ),
                (
                    Meta {
                        span: Span { start: 22, end: Some(23) },
                        leading: Vec::new(),
                        trailing: vec![
                            Trivia::Spaces(1),
                            Trivia::Comment {
                                span: Span { start: 24, end: Some(43) },
                                multiline: false,
                                content: " testing comments!".into()
                            }
                        ]
                    },
                    Token::Semicolon
                ),
                (
                    Meta {
                        span: Span { start: 44, end: Some(45) },
                        leading: vec![Trivia::Newline(1)],
                        trailing: Vec::new(),
                    },
                    Token::CurlyBClose
                ),
                (
                    Meta {
                        span: Span { start: 56, end: Some(56) },
                        leading: vec![
                            Trivia::Newline(1),
                            Trivia::Comment {
                                span: Span { start: 46, end: Some(56) },
                                multiline: false,
                                content: " trailing".into()
                            }
                        ],
                        trailing: Vec::new()
                    },
                    Token::EOF
                )
            ])
        );
        assert_eq!(
            tokenize_span("{\n    overflow = 1."),
            Err((Span { start: 17, end: None }, TokenizeError::TrailingDecimal))
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
                        original: r#"
            
                  
        This is a multiline string :D
          indented by two
        \'\'\'\'\
        ''${ interpolation was escaped }
        two single quotes: '''
        three single quotes: ''''
    "#.into(),
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
                original: "\n  \n    \n \n ".into(),
                content: "\n\n\n".into()
            })])
        );
        assert_eq!(
            tokenize("''\n  \n    \n a\n''"),
            Ok(vec![Token::Value(Value::Str {
                multiline: true,
                original: "\n  \n    \n a\n".into(),
                content: " \n   \na\n".into()
            })])
        );
        assert_eq!(
            tokenize("''  \n    \n a\n''"),
            Ok(vec![Token::Value(Value::Str {
                multiline: true,
                original: "  \n    \n a\n".into(),
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
                original: "$${test}".into(),
                content: "$${test}".into()
            })])
        );
    }
    #[test]
    fn interpolation() {
        assert_eq!(
            tokenize_span(r#" "Hello, ${ { world = "World"; }.world }!" "#),
            Ok(vec![(
                Meta {
                    span: Span { start: 1, end: Some(42) },
                    leading: vec![Trivia::Spaces(1)],
                    trailing: vec![Trivia::Spaces(1)]
                },
                Token::Interpol {
                    multiline: false,
                    parts: vec![
                        interpol("Hello, ".into()),
                        Interpol::Tokens(
                            vec![
                                (
                                    Meta {
                                        span: Span { start: 12, end: Some(13) },
                                        leading: vec![Trivia::Spaces(1)],
                                        trailing: vec![Trivia::Spaces(1)]
                                    },
                                    Token::CurlyBOpen
                                ),
                                (meta! { start: 14, end: 19, trailing: 1 }, Token::Ident("world".into())),
                                (meta! { start: 20, end: 21, trailing: 1 }, Token::Assign),
                                (meta! { start: 22, end: 29              }, Token::Value("World".into())),
                                (meta! { start: 29, end: 30, trailing: 1 }, Token::Semicolon),
                                (meta! { start: 31, end: 32              }, Token::CurlyBClose),
                                (meta! { start: 32, end: 33              }, Token::Dot),
                                (meta! { start: 33, end: 38, trailing: 1 }, Token::Ident("world".into()))
                            ],
                            meta! { start: 39, end: 40 }
                        ),
                        interpol("!".into())
                    ]
                }
            )])
        );
        assert_eq!(
            tokenize_span(r#" "\$${test}" "#),
            Ok(vec![(
                Meta {
                    span: Span { start: 1, end: Some(12) },
                    leading: vec![Trivia::Spaces(1)],
                    trailing: vec![Trivia::Spaces(1)]
                },
                Token::Interpol {
                    multiline: false,
                    parts: vec![
                        interpol_original("\\$".into(), "$".into()),
                        Interpol::Tokens(
                            vec![(meta! { start: 6, end: 10 }, Token::Ident("test".into()))],
                            meta! { start: 10, end: 11 }
                        )
                    ]
                }
            )])
        );
        assert_eq!(
            tokenize_span(r#" ''''$${test}'' "#),
            Ok(vec![(
                Meta {
                    span: Span { start: 1, end: Some(15) },
                    leading: vec![Trivia::Spaces(1)],
                    trailing: vec![Trivia::Spaces(1)]
                },
                Token::Interpol {
                    multiline: true,
                    parts: vec![
                        interpol_original("''$".into(), "$".into()),
                        Interpol::Tokens(
                            vec![(meta! { start: 8, end: 12 }, Token::Ident("test".into()))],
                            meta! { start: 12, end: 13 }
                        )
                    ]
                }
            )])
        );
        assert_eq!(
            tokenize_span(r#" "${test}#123" "#),
            Ok(vec![(
                Meta {
                    span: Span { start: 1, end: Some(14) },
                    leading: vec![Trivia::Spaces(1)],
                    trailing: vec![Trivia::Spaces(1)]
                },
                Token::Interpol {
                    multiline: false,
                    parts: vec![
                        Interpol::Tokens(
                            vec![(meta! { start: 4, end: 8 }, Token::Ident("test".into()))],
                            meta! { start: 8, end: 9 }
                        ),
                        interpol("#123".into()),
                    ]
                }
            )])
        );
        assert_eq!(
            tokenize_span(" ''\n${test}'' "),
            Ok(vec![(
                Meta {
                    span: Span { start: 1, end: Some(13) },
                    leading: vec![Trivia::Spaces(1)],
                    trailing: vec![Trivia::Spaces(1)]
                },
                Token::Interpol {
                    multiline: true,
                    parts: vec![
                        interpol_original("\n".into(), "".into()),
                        Interpol::Tokens(
                            vec![(meta! { start: 6, end: 10 }, Token::Ident("test".into()))],
                            meta! { start: 10, end: 11 }
                        ),
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
        assert_eq!(
            tokenize("x>=y"), // <- could be confused with store path because of the '>'
            Ok(vec![Token::Ident("x".into()), Token::MoreOrEq, Token::Ident("y".into())])
        );
    }
    #[test]
    fn dynamic_attrs() {
        assert_eq!(
            tokenize("a.${b}.c"),
            Ok(vec![
                Token::Ident("a".into()),
                Token::Dot,
                Token::Dynamic(
                   vec![(meta! { start: 4, end: 5 }, Token::Ident("b".into()))],
                   meta! { start: 5, end: 6 }
                ),
                Token::Dot,
                Token::Ident("c".into())
            ])
        );
    }
}
