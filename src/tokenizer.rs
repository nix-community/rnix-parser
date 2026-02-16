//! The tokenizer: turns a string into tokens, such as numbers, strings, and keywords

use crate::SyntaxKind::{self, *};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum IdentType {
    Ident,
    PathRel,
    PathAbs,
    PathHome,
    PathSearch,
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
    Path(SyntaxKind),
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

/// Tokenize the given input
pub fn tokenize(input: &str) -> impl Iterator<Item = Token<'_>> + '_ {
    Tokenizer::new(input)
}

struct Tokenizer<'a> {
    ctx: Vec<Context>,
    state: State<'a>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { ctx: Vec::new(), state: State { input, offset: 0 } }
    }
}

impl Tokenizer<'_> {
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
    fn str_since<'a>(&self, past: State<'a>) -> &'a str {
        &past.input[past.offset..self.state.offset]
    }

    fn push_ctx(&mut self, ctx: Context) {
        self.ctx.push(ctx)
    }

    fn pop_ctx(&mut self, ctx: Context) {
        debug_assert!(self
            .ctx
            .last()
            .is_some_and(|c| std::mem::discriminant(c) == std::mem::discriminant(&ctx)));
        self.ctx.pop();
    }

    /// Consumes all next characters.
    /// while the predicate function [f] returns true
    ///
    /// returns the count of consumed characters.
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
                Some('\\') if !multiline => {
                    if self.next().is_none() {
                        return TOKEN_ERROR;
                    }
                }

                Some('\'') if multiline => match self.peek() {
                    None => return TOKEN_ERROR,
                    Some('\'') => {
                        self.next();
                        match self.peek() {
                            Some('\'') | Some('$') => {
                                self.next().unwrap();
                            }
                            Some('\\') => {
                                self.next().unwrap();
                                if self.next().is_none() {
                                    return TOKEN_ERROR;
                                }
                            }
                            _ => {
                                self.state = start;
                                self.pop_ctx(Context::StringBody { multiline: true });
                                self.push_ctx(Context::StringEnd);
                                return TOKEN_STRING_CONTENT;
                            }
                        }
                    }
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

    fn check_path_since(&mut self, past: State, kind: SyntaxKind) -> SyntaxKind {
        self.consume(is_valid_path_char);
        let path_str = self.str_since(past);
        if self.remaining().starts_with("${") {
            self.ctx.push(Context::InterpolStart);
        } else if path_str.ends_with('/') || path_str.contains("//") {
            return TOKEN_ERROR;
        } else {
            self.pop_ctx(Context::Path(kind));
        }
        kind
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
                Some(Context::Path(path_kind)) => {
                    let path_kind = *path_kind;

                    // Search paths (<nixpkgs>) don't support interpolation
                    if path_kind == TOKEN_PATH_SEARCH {
                        if self.peek().is_some_and(is_valid_path_char) {
                            return Some(self.check_path_since(start, path_kind));
                        } else {
                            self.pop_ctx(Context::Path(path_kind));
                        }
                    } else if self.starts_with_bump("${") {
                        self.ctx.push(Context::Interpol { brackets: 0 });
                        return Some(TOKEN_INTERPOL_START);
                    } else if self.peek().is_some_and(is_valid_path_char) {
                        return Some(self.check_path_since(start, path_kind));
                    } else {
                        self.pop_ctx(Context::Path(path_kind));
                    }
                }
                Some(&Context::StringBody { multiline }) => {
                    let token = self.next_string(multiline);
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
                        Some('\'') => {
                            self.next().unwrap();
                            match self.peek() {
                                Some('\'') => {
                                    self.next().unwrap();
                                    true
                                }
                                _ => false,
                            }
                        }
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
        // First, simple prefix based detection for absolute ("/...") or home relative ("~/...") paths.
        // For absolute paths, we need to be careful to not interfere with operators
        let kind = if self.peek() == Some('/') {
            // For absolute paths, we need to have a valid path character after the '/'
            // AND we need to make sure it's not an operator like //
            if self.remaining().starts_with("//") {
                None // This could be an update operator, let the operator matcher handle it
            } else if self.remaining().starts_with("/${") {
                // Absolute path with interpolation, e.g. /${foo}/bar
                Some(IdentType::PathAbs)
            } else {
                let second_char = self.remaining().chars().nth(1);
                if second_char.is_some_and(|c| is_valid_path_char(c) && c != '*') {
                    Some(IdentType::PathAbs)
                } else {
                    None // Not a path, might be division operator or something else
                }
            }
        } else if self.peek() == Some('~') && self.remaining().chars().nth(1) == Some('/') {
            Some(IdentType::PathHome)
        } else {
            // Fallback to heuristic previously used for relative, search and URI paths.
            let store_path = self.peek() == Some('<');
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
                (Some('/'), Some(c)) if !c.is_whitespace() => {
                    // If the first character of the yet‑to‑be‑consumed token was '/', we treat it as absolute later.
                    // Otherwise it's a relative path.
                    Some(IdentType::PathRel)
                }
                (Some('>'), _) => Some(IdentType::PathSearch),
                (Some(':'), Some(c)) if is_valid_uri_char(c) && !skipped.contains('_') => {
                    Some(IdentType::Uri)
                }
                _ => None,
            }
        };

        let c = self.next()?;

        // First, handle specific operators that might be confused with paths
        if c == '/' && self.peek() == Some('/') {
            // This is the update operator '//'
            self.next().unwrap(); // Consume the second '/'
            return Some(TOKEN_UPDATE);
        }

        if c == '~'
            || matches!(kind, Some(IdentType::PathAbs | IdentType::PathRel))
            || (c == '/' && self.peek().is_some_and(is_valid_path_char))
        {
            return Some({
                // Determine the concrete path token kind to use
                let token_kind = if c == '~' {
                    // we've just consumed '~', ensure '/' exists already consumed by next()
                    if self.next() != Some('/') {
                        return Some(TOKEN_ERROR);
                    }
                    TOKEN_PATH_HOME
                } else {
                    // For abs vs rel: If the first char we consumed was '/', it's absolute.
                    if c == '/' {
                        TOKEN_PATH_ABS
                    } else {
                        TOKEN_PATH_REL
                    }
                };

                // Store the kind in context so subsequent segments are consistent
                self.push_ctx(Context::Path(token_kind));
                self.check_path_since(start, token_kind)
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
                TOKEN_L_BRACE
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
                TOKEN_R_BRACE
            }
            '[' => TOKEN_L_BRACK,
            ']' => TOKEN_R_BRACK,
            '@' => TOKEN_AT,
            ':' => TOKEN_COLON,
            ',' => TOKEN_COMMA,
            '.' => {
                if self.peek().is_some_and(|x| x.is_ascii_digit()) {
                    self.consume(|c| c.is_ascii_digit());
                    self.consume_scientific()
                } else {
                    TOKEN_DOT
                }
            }
            '=' => TOKEN_ASSIGN,
            '?' => TOKEN_QUESTION,
            ';' => TOKEN_SEMICOLON,
            '(' => TOKEN_L_PAREN,
            ')' => TOKEN_R_PAREN,
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
            '<' if self.peek() == Some('|') => {
                self.next().unwrap();
                TOKEN_PIPE_LEFT
            }
            '<' if kind == Some(IdentType::PathSearch) => {
                let content_start = self.state.offset;
                self.consume(is_valid_path_char);
                // Check if we consumed any content between < and >
                if self.state.offset == content_start {
                    // Empty search path <> is not valid
                    TOKEN_LESS
                } else if self.next() != Some('>') {
                    TOKEN_ERROR
                } else {
                    TOKEN_PATH_SEARCH
                }
            }
            '&' if self.peek() == Some('&') => {
                self.next().unwrap();
                TOKEN_AND_AND
            }
            '|' if self.peek() == Some('>') => {
                self.next().unwrap();
                TOKEN_PIPE_RIGHT
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
                    Some(IdentType::PathSearch) | None => IdentType::Ident,
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
                    IdentType::PathAbs | IdentType::PathRel | IdentType::PathHome => {
                        panic!("paths are checked earlier")
                    }
                    IdentType::PathSearch => panic!("search paths are checked earlier"),
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
            fst @ '0'..='9' => {
                // Due to nix's behavior on leading 0's we know immediately
                // That a '0' followed by some other number character MUST tokenize into an INT.
                // More background on this: https://github.com/nix-community/rnix-parser/issues/157
                if fst == '0' && self.peek() != Some('.') {
                    // In that case just consume the remaining number characters to fill the TOKEN_INTEGER
                    self.consume(|c| c.is_ascii_digit());
                    TOKEN_INTEGER
                } else {
                    // If we are not in that edge-case we can just continue with the regular number tokenization
                    self.consume(|c| c.is_ascii_digit());
                    if self.peek() == Some('.') {
                        self.next().unwrap();
                        self.consume(|c| c.is_ascii_digit());
                        self.consume_scientific()
                    } else {
                        TOKEN_INTEGER
                    }
                }
            }
            _ => TOKEN_ERROR,
        })
    }

    fn consume_scientific(&mut self) -> SyntaxKind {
        if self.peek() == Some('e') || self.peek() == Some('E') {
            self.next().unwrap();
            if self.peek() == Some('-') || self.peek() == Some('+') {
                self.next().unwrap();
            }
            if self.consume(|c| c.is_ascii_digit()) == 0 {
                return TOKEN_ERROR;
            }
        }
        TOKEN_FLOAT
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.state;
        self.next_inner().map(|syntax_kind| (syntax_kind, self.str_since(start)))
    }
}
