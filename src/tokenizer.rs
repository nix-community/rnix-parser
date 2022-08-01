//! The tokenizer: turns a string into tokens, such as numbers, strings, and keywords

use crate::SyntaxKind::{self, *};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum IdentType {
    Ident,
    Path,
    Store,
    Uri,
}

fn is_valid_path_char(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '/' | '_' | '.' | '+' | '-')
}

fn is_valid_uri_char(c: char) -> bool {
    match c {
        '%' | '?' | ':' | '@' | '&' | '=' | '$' | ',' | '!' | '~' | '*' | '\'' => true,
        _ => is_valid_path_char(c),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Context {
    StringBody { multiline: bool },
    StringEnd,
    Interpol { brackets: u32 },
    InterpolStart,
    Path,
}

#[derive(Clone, Copy)]
struct State<'a> {
    input: &'a str,
    offset: usize,
}

impl PartialEq for State<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.input, other.input) && self.offset == other.offset
    }
}

impl Eq for State<'_> {}

pub type Token<'a> = (SyntaxKind, &'a str);

/// A convenience function for tokenizing the given input
pub fn tokenize(input: &str) -> Vec<Token<'_>> {
    Tokenizer::new(input).collect()
}

/// The tokenizer. You may want to use the `tokenize` convenience function from this module instead.
pub struct Tokenizer<'a> {
    ctx: Vec<Context>,
    state: State<'a>,
}
impl<'a> Tokenizer<'a> {
    /// Create a new instance
    pub fn new(input: &'a str) -> Self {
        Self { ctx: Vec::new(), state: State { input, offset: 0 } }
    }

    fn remaining(&self) -> &str {
        &self.state.input[self.state.offset..]
    }
    fn peek(&self) -> Option<char> {
        self.remaining().chars().next()
    }
    fn next(&mut self) -> Option<char> {
        let c = self.peek();
        if let Some(c) = c {
            self.state.offset += c.len_utf8();
        }
        c
    }
    fn starts_with_bump(&mut self, s: &str) -> bool {
        let starts_with = self.remaining().starts_with(s);
        if starts_with {
            self.state.offset += s.len();
        }
        starts_with
    }
    fn str_since<'p>(&self, past: State<'p>) -> &'p str {
        &past.input[past.offset..self.state.offset]
    }

    fn push_ctx(&mut self, ctx: Context) {
        self.ctx.push(ctx)
    }

    fn pop_ctx(&mut self, ctx: Context) {
        debug_assert_eq!(self.ctx.last(), Some(&ctx));
        self.ctx.pop();
    }

    fn consume<F>(&mut self, mut f: F) -> usize
    where
        F: FnMut(char) -> bool,
    {
        let len: usize =
            self.remaining().chars().take_while(|&c| f(c)).map(|c| c.len_utf8()).sum::<usize>();
        self.state.offset += len;
        len
    }
    fn next_string(&mut self, multiline: bool) -> SyntaxKind {
        loop {
            let start = self.state;
            match self.next() {
                None => {
                    self.pop_ctx(Context::StringBody { multiline });
                    return TOKEN_ERROR;
                }
                Some('"') if !multiline => {
                    self.state = start;
                    self.pop_ctx(Context::StringBody { multiline: false });
                    self.push_ctx(Context::StringEnd);
                    return TOKEN_STRING_CONTENT;
                }
                Some('\\') if !multiline => match self.next() {
                    None => return TOKEN_ERROR,
                    Some(_) => (),
                },

                Some('\'') if multiline => match self.peek() {
                    None => return TOKEN_ERROR,
                    Some('\'') => match {
                        self.next();
                        self.peek()
                    } {
                        Some('\'') | Some('$') => {
                            self.next().unwrap();
                        }
                        Some('\\') => {
                            self.next().unwrap();
                            if let None = self.next() {
                                return TOKEN_ERROR;
                            }
                        }
                        _ => {
                            self.state = start;
                            self.pop_ctx(Context::StringBody { multiline: true });
                            self.push_ctx(Context::StringEnd);
                            return TOKEN_STRING_CONTENT;
                        }
                    },
                    Some(_) => (),
                },

                Some('$') => match self.peek() {
                    Some('$') => {
                        self.next().unwrap();
                    }
                    Some('{') => {
                        self.state = start;
                        self.push_ctx(Context::InterpolStart);
                        return TOKEN_STRING_CONTENT;
                    }
                    _ => (),
                },
                Some(_) => (),
            }
        }
    }

    fn check_path_since(&mut self, past: State) -> SyntaxKind {
        self.consume(is_valid_path_char);
        if self.remaining().starts_with("${") {
            self.ctx.push(Context::InterpolStart);
        } else if self.str_since(past).ends_with('/') {
            return TOKEN_ERROR;
        } else {
            self.pop_ctx(Context::Path);
        }
        TOKEN_PATH
    }

    fn next_inner(&mut self) -> Option<SyntaxKind> {
        let start = self.state;

        // Handle already started multi-token
        loop {
            match self.ctx.last() {
                Some(Context::InterpolStart) => {
                    self.pop_ctx(Context::InterpolStart);
                    self.ctx.push(Context::Interpol { brackets: 0 });
                    if self.starts_with_bump("${") {
                        return Some(TOKEN_INTERPOL_START);
                    } else {
                        unreachable!()
                    }
                }
                Some(Context::Path) => {
                    if self.starts_with_bump("${") {
                        self.ctx.push(Context::Interpol { brackets: 0 });
                        return Some(TOKEN_INTERPOL_START);
                    } else if self.peek().map_or(false, is_valid_path_char) {
                        return Some(self.check_path_since(start));
                    } else {
                        self.pop_ctx(Context::Path);
                    }
                }
                Some(Context::StringBody { multiline }) => {
                    let token = self.next_string(*multiline);
                    // skip empty stuff
                    if self.state == start {
                        continue;
                    }
                    return Some(token);
                }
                Some(Context::StringEnd) => {
                    self.pop_ctx(Context::StringEnd);
                    let status = match self.peek() {
                        Some('"') => {
                            self.next().unwrap();
                            true
                        }
                        Some('\'') => match {
                            self.next().unwrap();
                            self.peek()
                        } {
                            Some('\'') => {
                                self.next().unwrap();
                                true
                            }
                            _ => false,
                        },
                        _ => false,
                    };
                    return Some(if status { TOKEN_STRING_END } else { TOKEN_ERROR });
                }
                _ => (),
            }
            break;
        }

        if self.consume(char::is_whitespace) > 0 {
            return Some(TOKEN_WHITESPACE);
        }

        if self.peek() == Some('#') {
            self.consume(|c| c != '\n');
            return Some(TOKEN_COMMENT);
        }
        if self.starts_with_bump("/*") {
            loop {
                self.consume(|c| c != '*');
                self.next(); // consume the '*', if any
                match self.peek() {
                    None => return Some(TOKEN_ERROR),
                    Some('/') => {
                        self.next().unwrap();
                        return Some(TOKEN_COMMENT);
                    }
                    _ => (),
                }
            }
        }

        if self.starts_with_bump("...") {
            return Some(TOKEN_ELLIPSIS);
        }

        // Check if it's a path
        let store_path = self.peek() == Some('<');
        let kind = {
            let skipped = self
                .remaining()
                .chars()
                .take_while(|&c| match c {
                    '<' | '/' => store_path,
                    _ => is_valid_path_char(c),
                })
                .collect::<String>();

            let mut lookahead = self.remaining().chars().skip(skipped.chars().count());

            match (lookahead.next(), lookahead.next()) {
                // a//b parses as Update(a, b)
                (Some('/'), Some('/')) => None,
                (Some('/'), Some('*')) => None,
                (Some('/'), Some(c)) if !c.is_whitespace() => Some(IdentType::Path),
                (Some('>'), _) => Some(IdentType::Store),
                (Some(':'), Some(c)) if is_valid_uri_char(c) && !skipped.contains('_') => {
                    Some(IdentType::Uri)
                }
                _ => None,
            }
        };

        let c = self.next()?;

        if c == '~' || kind == Some(IdentType::Path) {
            return Some(if c == '~' && self.next() != Some('/') {
                TOKEN_ERROR
            } else {
                self.push_ctx(Context::Path);
                self.check_path_since(start)
            });
        }

        Some(match c {
            '=' if self.peek() == Some('=') => {
                self.next().unwrap();
                TOKEN_EQUAL
            }
            '!' if self.peek() == Some('=') => {
                self.next().unwrap();
                TOKEN_NOT_EQUAL
            }
            '!' => TOKEN_INVERT,
            '{' => {
                if let Some(Context::Interpol { brackets }) = self.ctx.last_mut() {
                    *brackets += 1;
                }
                TOKEN_CURLY_B_OPEN
            }
            '}' => {
                if let Some(Context::Interpol { brackets }) = self.ctx.last_mut() {
                    match brackets.checked_sub(1) {
                        Some(new) => *brackets = new,
                        None => {
                            self.pop_ctx(Context::Interpol { brackets: 0 });
                            return Some(TOKEN_INTERPOL_END);
                        }
                    }
                }
                TOKEN_CURLY_B_CLOSE
            }
            '[' => TOKEN_SQUARE_B_OPEN,
            ']' => TOKEN_SQUARE_B_CLOSE,
            '@' => TOKEN_AT,
            ':' => TOKEN_COLON,
            ',' => TOKEN_COMMA,
            '.' => {
                if self.peek().map_or(false, |x| ('0'..='9').contains(&x)) {
                    self.consume(|c| ('0'..='9').contains(&c));
                    TOKEN_FLOAT
                } else {
                    TOKEN_DOT
                }
            }
            '=' => TOKEN_ASSIGN,
            '?' => TOKEN_QUESTION,
            ';' => TOKEN_SEMICOLON,
            '(' => TOKEN_PAREN_OPEN,
            ')' => TOKEN_PAREN_CLOSE,
            '+' if self.peek() == Some('+') => {
                self.next().unwrap();
                TOKEN_CONCAT
            }
            '-' if self.peek() == Some('>') => {
                self.next().unwrap();
                TOKEN_IMPLICATION
            }
            '/' if self.peek() == Some('/') => {
                self.next().unwrap();
                TOKEN_UPDATE
            }
            '+' => TOKEN_ADD,
            '-' => TOKEN_SUB,
            '*' => TOKEN_MUL,
            '/' => TOKEN_DIV,
            '<' if kind == Some(IdentType::Store) => {
                self.consume(is_valid_path_char);
                if self.next() != Some('>') {
                    TOKEN_ERROR
                } else {
                    TOKEN_PATH
                }
            }
            '&' if self.peek() == Some('&') => {
                self.next().unwrap();
                TOKEN_AND_AND
            }
            '|' if self.peek() == Some('|') => {
                self.next().unwrap();
                TOKEN_OR_OR
            }
            '<' if self.peek() == Some('=') => {
                self.next().unwrap();
                TOKEN_LESS_OR_EQ
            }
            '<' => TOKEN_LESS,
            '>' if self.peek() == Some('=') => {
                self.next().unwrap();
                TOKEN_MORE_OR_EQ
            }
            '>' => TOKEN_MORE,
            '$' if self.peek() == Some('{') => {
                self.next().unwrap();
                self.push_ctx(Context::Interpol { brackets: 0 });
                TOKEN_INTERPOL_START
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let kind = match kind {
                    // It's detected as store if it ends with >, but if it
                    // didn't start with <, that's wrong
                    Some(IdentType::Store) | None => IdentType::Ident,
                    Some(kind) => kind,
                };
                self.consume(|c| match c {
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '\'' => true,
                    c => kind == IdentType::Uri && is_valid_uri_char(c),
                });
                match kind {
                    IdentType::Ident => match self.str_since(start) {
                        "assert" => TOKEN_ASSERT,
                        "else" => TOKEN_ELSE,
                        "if" => TOKEN_IF,
                        "in" => TOKEN_IN,
                        "inherit" => TOKEN_INHERIT,
                        "let" => TOKEN_LET,
                        // "or" is a contextual keyword and will be handled in the parser.
                        "or" => TOKEN_OR,
                        "rec" => TOKEN_REC,
                        "then" => TOKEN_THEN,
                        "with" => TOKEN_WITH,
                        _ => TOKEN_IDENT,
                    },
                    IdentType::Uri => TOKEN_URI,
                    IdentType::Path => panic!("paths are checked earlier"),
                    IdentType::Store => panic!("store paths are checked earlier"),
                }
            }
            '"' => {
                self.push_ctx(Context::StringBody { multiline: false });
                TOKEN_STRING_START
            }
            '\'' if self.peek() == Some('\'') => {
                self.next().unwrap();
                self.push_ctx(Context::StringBody { multiline: true });
                TOKEN_STRING_START
            }
            '0'..='9' => {
                self.consume(|c| ('0'..='9').contains(&c));
                if self.peek() == Some('.') {
                    self.next().unwrap();
                    if self.consume(|c| ('0'..='9').contains(&c)) == 0 {
                        return Some(TOKEN_ERROR);
                    }
                    if self.peek() == Some('e') || self.peek() == Some('E') {
                        self.next().unwrap();
                        if self.peek() == Some('-') {
                            self.next().unwrap();
                        }
                        if self.consume(|c| ('0'..='9').contains(&c)) == 0 {
                            return Some(TOKEN_ERROR);
                        }
                    }
                    TOKEN_FLOAT
                } else {
                    TOKEN_INTEGER
                }
            }
            _ => TOKEN_ERROR,
        })
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.state;
        self.next_inner().map(|syntax_kind| (syntax_kind, self.str_since(start)))
    }
}

#[cfg(test)]
mod tests {
    use std::str;

    use super::tokenize;

    fn fuzz<B: AsRef<[u8]>>(b: B) {
        let s = str::from_utf8(b.as_ref()).unwrap();
        println!("`{s}`");
        tokenize(s);
    }

    #[test]
    fn test_fuzz() {
        fuzz("\"");
        fuzz([39, 39, 34]);
        fuzz([39, 39, 39, 39, 92]);
    }
}
