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
                    self.consume_scientific()
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
                    self.consume_scientific()
                } else {
                    TOKEN_INTEGER
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
            if self.consume(|c| ('0'..='9').contains(&c)) == 0 {
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

#[cfg(test)]
mod tests {
    use std::str;

    use super::{tokenize, SyntaxKind::*, Token};

    fn check<'a, I: IntoIterator<Item = Token<'a>>>(s: &'a str, ts: I) {
        let actual = tokenize(s);
        let expected = ts.into_iter().collect::<Vec<_>>();
        assert_eq!(
            actual, expected,
            "string
{s}
was tokenized into
{actual:#?}
but expected
{expected:#?}",
        )
    }

    fn path(path: &str) -> [Token<'_>; 1] {
        [(TOKEN_PATH, path)]
    }
    fn error(token: &str) -> [Token<'_>; 1] {
        [(TOKEN_ERROR, token)]
    }

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

    #[test]
    fn basic_int_set() {
        check(
            "{ int = 42; }",
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "int"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ASSIGN, "="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "42"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_CURLY_B_CLOSE, "}"),
            ],
        );
    }
    #[test]
    fn basic_float_set() {
        check(
            "{ float = 1.234; }",
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "float"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ASSIGN, "="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_FLOAT, "1.234"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_CURLY_B_CLOSE, "}"),
            ],
        );
        check(
            ".5 + 0.5",
            [
                (TOKEN_FLOAT, ".5"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ADD, "+"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_FLOAT, "0.5"),
            ],
        );
        check(
            "{ scientific = 1.1e4; uppercase = 123.4E-2; }",
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "scientific"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ASSIGN, "="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_FLOAT, "1.1e4"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "uppercase"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ASSIGN, "="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_FLOAT, "123.4E-2"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_CURLY_B_CLOSE, "}"),
            ],
        );
    }
    #[test]
    fn float_scientific_no_leading_zero() {
        check(".5e1", [(TOKEN_FLOAT, ".5e1")]);
    }
    #[test]
    fn float_scientific_plus() {
        check("1.2e+3", [(TOKEN_FLOAT, "1.2e+3")]);
    }
    #[test]
    fn basic_string_set() {
        check(
            r#"{ string = "Hello \"World\""; }"#,
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "string"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ASSIGN, "="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "\""),
                (TOKEN_STRING_CONTENT, r#"Hello \"World\""#),
                (TOKEN_STRING_END, "\""),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_CURLY_B_CLOSE, "}"),
            ],
        );
    }
    #[test]
    fn multiline() {
        check(
            r#"{
    multiline = ''
        This is a multiline string :D
          indented by two
        \'\'\'\'\
        ''${ interpolation was escaped }
        ''\${ interpolation was escaped }
        two single quotes: '''
        three single quotes: ''''
    '';
}"#,
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_WHITESPACE, "\n    "),
                (TOKEN_IDENT, "multiline"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ASSIGN, "="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "''"),
                (
                    TOKEN_STRING_CONTENT,
                    r#"
        This is a multiline string :D
          indented by two
        \'\'\'\'\
        ''${ interpolation was escaped }
        ''\${ interpolation was escaped }
        two single quotes: '''
        three single quotes: ''''
    "#,
                ),
                (TOKEN_STRING_END, "''"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, "\n"),
                (TOKEN_CURLY_B_CLOSE, "}"),
            ],
        );
    }
    #[test]
    fn special_escape() {
        check(
            r#" "$${test}" "#,
            [
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "\""),
                (TOKEN_STRING_CONTENT, r#"$${test}"#),
                (TOKEN_STRING_END, "\""),
                (TOKEN_WHITESPACE, " "),
            ],
        );
        check(
            r#" ''$${test}'' "#,
            [
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "''"),
                (TOKEN_STRING_CONTENT, r#"$${test}"#),
                (TOKEN_STRING_END, "''"),
                (TOKEN_WHITESPACE, " "),
            ],
        );
    }
    #[test]
    fn interpolation() {
        check(
            r#" "Hello, ${ { world = "World"; }.world }!" "#,
            [
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "\""),
                (TOKEN_STRING_CONTENT, "Hello, "),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "world"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ASSIGN, "="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "\""),
                (TOKEN_STRING_CONTENT, r#"World"#),
                (TOKEN_STRING_END, "\""),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_CURLY_B_CLOSE, "}"),
                (TOKEN_DOT, "."),
                (TOKEN_IDENT, "world"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_CONTENT, "!"),
                (TOKEN_STRING_END, "\""),
                (TOKEN_WHITESPACE, " "),
            ],
        );
        check(
            r#" "\$${test}" "#,
            [
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "\""),
                (TOKEN_STRING_CONTENT, "\\$"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "test"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_END, "\""),
                (TOKEN_WHITESPACE, " "),
            ],
        );
        check(
            r#" ''''$${test}'' "#,
            [
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "''"),
                (TOKEN_STRING_CONTENT, "''$"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "test"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_END, "''"),
                (TOKEN_WHITESPACE, " "),
            ],
        );
        check(
            r#" "${test}#123" "#,
            [
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "\""),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "test"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_CONTENT, "#123"),
                (TOKEN_STRING_END, "\""),
                (TOKEN_WHITESPACE, " "),
            ],
        );
        check(
            " ''\n${test}'' ",
            [
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "''"),
                (TOKEN_STRING_CONTENT, "\n"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "test"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_END, "''"),
                (TOKEN_WHITESPACE, " "),
            ],
        );
        check(
            r#" "${hello} ${world}" "#,
            [
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "\""),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "hello"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_CONTENT, " "),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "world"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_END, "\""),
                (TOKEN_WHITESPACE, " "),
            ],
        );
        check(
            r#" ''${"${var}"}'' "#,
            [
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "''"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_STRING_START, "\""),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "var"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_END, "\""),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_END, "''"),
                (TOKEN_WHITESPACE, " "),
            ],
        );
        check(
            r#" ''dont '${escape} me'' "#,
            [
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "''"),
                (TOKEN_STRING_CONTENT, "dont '"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "escape"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_CONTENT, " me"),
                (TOKEN_STRING_END, "''"),
                (TOKEN_WHITESPACE, " "),
            ],
        );
    }
    #[test]
    fn comments() {
        check("/**/", [(TOKEN_COMMENT, "/**/")]);
        check("/***/", [(TOKEN_COMMENT, "/***/")]);
        check(
            "{ a = /* multiline * comment */ 123;# single line\n} # single line at the end",
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "a"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ASSIGN, "="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_COMMENT, "/* multiline * comment */"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "123"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_COMMENT, "# single line"),
                (TOKEN_WHITESPACE, "\n"),
                (TOKEN_CURLY_B_CLOSE, "}"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_COMMENT, "# single line at the end"),
            ],
        );
    }
    #[test]
    fn math() {
        check(
            "1 + 2 * 3",
            [
                (TOKEN_INTEGER, "1"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ADD, "+"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_MUL, "*"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "3"),
            ],
        );
        check(
            "5 * -(3 - 2)",
            [
                (TOKEN_INTEGER, "5"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_MUL, "*"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_SUB, "-"),
                (TOKEN_PAREN_OPEN, "("),
                (TOKEN_INTEGER, "3"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_SUB, "-"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_PAREN_CLOSE, ")"),
            ],
        );
        check(
            "a/ 3",
            // <- could get mistaken for a path
            [(TOKEN_IDENT, "a"), (TOKEN_DIV, "/"), (TOKEN_WHITESPACE, " "), (TOKEN_INTEGER, "3")],
        );
    }
    #[test]
    fn let_in() {
        check(
            "let a = 3; in a",
            [
                (TOKEN_LET, "let"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "a"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ASSIGN, "="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "3"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IN, "in"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "a"),
            ],
        );
    }
    #[test]
    fn with() {
        check(
            "with namespace; expr",
            [
                (TOKEN_WITH, "with"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "namespace"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "expr"),
            ],
        );
    }
    #[test]
    fn paths() {
        check("/hello/world", path("/hello/world"));
        check("hello/world", path("hello/world"));
        check("hello_/world", path("hello_/world"));
        check("a+3/5+b", path("a+3/5+b"));
        check("1-2/3", path("1-2/3"));
        check("./hello/world", path("./hello/world"));
        check("~/hello/world", path("~/hello/world"));
        check("<hello/world>", path("<hello/world>"));
        check("~", error("~"));
        check("~/", error("~/"));
        check("/a/", error("/a/"));
    }
    #[test]
    fn test_path_no_newline() {
        check(
            "import ./.\n",
            [
                (TOKEN_IDENT, "import"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_PATH, "./."),
                (TOKEN_WHITESPACE, "\n"),
            ],
        );
    }
    #[test]
    fn test_path_interpol() {
        check(
            "./${foo}",
            [
                (TOKEN_PATH, "./"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "foo"),
                (TOKEN_INTERPOL_END, "}"),
            ],
        );
        check(
            "./${foo} bar",
            [
                (TOKEN_PATH, "./"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "foo"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "bar"),
            ],
        );
        check(
            "./${foo}${bar}",
            [
                (TOKEN_PATH, "./"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "foo"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "bar"),
                (TOKEN_INTERPOL_END, "}"),
            ],
        );
        check(
            "./${foo}let",
            [
                (TOKEN_PATH, "./"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "foo"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_PATH, "let"),
            ],
        );
        check(
            "./${foo}.jpg",
            [
                (TOKEN_PATH, "./"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "foo"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_PATH, ".jpg"),
            ],
        );
        check(
            "./${foo}/",
            [
                (TOKEN_PATH, "./"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "foo"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_ERROR, "/"),
            ],
        );
        check(
            "./${foo}a${bar}",
            [
                (TOKEN_PATH, "./"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "foo"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_PATH, "a"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "bar"),
                (TOKEN_INTERPOL_END, "}"),
            ],
        );
        check(
            "\"./${foo}\"",
            [
                (TOKEN_STRING_START, "\""),
                (TOKEN_STRING_CONTENT, "./"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "foo"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_END, "\""),
            ],
        );
    }
    #[test]
    fn uri() {
        check(
            "https://google.com/?q=Hello+World",
            [(TOKEN_URI, "https://google.com/?q=Hello+World")],
        );
    }
    #[test]
    fn uri_with_underscore() {
        check(
            "https://goo_gle.com/?q=Hello+World",
            [(TOKEN_URI, "https://goo_gle.com/?q=Hello+World")],
        );
    }
    #[test]
    fn list() {
        check(
            r#"[a 2 3 "lol"]"#,
            [
                (TOKEN_SQUARE_B_OPEN, "["),
                (TOKEN_IDENT, "a"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "3"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "\""),
                (TOKEN_STRING_CONTENT, r#"lol"#),
                (TOKEN_STRING_END, "\""),
                (TOKEN_SQUARE_B_CLOSE, "]"),
            ],
        );
        check(
            "[1] ++ [2] ++ [3]",
            [
                (TOKEN_SQUARE_B_OPEN, "["),
                (TOKEN_INTEGER, "1"),
                (TOKEN_SQUARE_B_CLOSE, "]"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_CONCAT, "++"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_SQUARE_B_OPEN, "["),
                (TOKEN_INTEGER, "2"),
                (TOKEN_SQUARE_B_CLOSE, "]"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_CONCAT, "++"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_SQUARE_B_OPEN, "["),
                (TOKEN_INTEGER, "3"),
                (TOKEN_SQUARE_B_CLOSE, "]"),
            ],
        );
    }
    #[test]
    fn lambda() {
        check(
            "a: b: a + b",
            [
                (TOKEN_IDENT, "a"),
                (TOKEN_COLON, ":"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "b"),
                (TOKEN_COLON, ":"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "a"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ADD, "+"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "b"),
            ],
        );
    }
    #[test]
    fn lambda_arg_underscore() {
        check("_:null", [(TOKEN_IDENT, "_"), (TOKEN_COLON, ":"), (TOKEN_IDENT, "null")]);
    }
    #[test]
    fn patterns() {
        check(
            r#"{ a, b ? "default", ... } @ outer"#,
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "a"),
                (TOKEN_COMMA, ","),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "b"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_QUESTION, "?"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "\""),
                (TOKEN_STRING_CONTENT, "default"),
                (TOKEN_STRING_END, "\""),
                (TOKEN_COMMA, ","),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ELLIPSIS, "..."),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_CURLY_B_CLOSE, "}"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_AT, "@"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "outer"),
            ],
        );
    }
    #[test]
    fn combine() {
        check("a//b", [(TOKEN_IDENT, "a"), (TOKEN_UPDATE, "//"), (TOKEN_IDENT, "b")]);
    }
    #[test]
    fn ifs() {
        check(
            "false -> !false && false == true || true",
            [
                (TOKEN_IDENT, "false"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IMPLICATION, "->"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INVERT, "!"),
                (TOKEN_IDENT, "false"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_AND_AND, "&&"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "false"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_EQUAL, "=="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "true"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_OR_OR, "||"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "true"),
            ],
        );
        check(
            "1 < 2 && 2 <= 2 && 2 > 1 && 2 >= 2",
            [
                (TOKEN_INTEGER, "1"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_LESS, "<"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_AND_AND, "&&"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_LESS_OR_EQ, "<="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_AND_AND, "&&"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_MORE, ">"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "1"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_AND_AND, "&&"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_MORE_OR_EQ, ">="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
            ],
        );
        check(
            "1 == 1 && 2 != 3",
            [
                (TOKEN_INTEGER, "1"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_EQUAL, "=="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "1"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_AND_AND, "&&"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_NOT_EQUAL, "!="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "3"),
            ],
        );
        check(
            "a:[ b ]",
            [
                (TOKEN_IDENT, "a"),
                (TOKEN_COLON, ":"),
                (TOKEN_SQUARE_B_OPEN, "["),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "b"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_SQUARE_B_CLOSE, "]"),
            ],
        );
        check(
            "a:( b )",
            [
                (TOKEN_IDENT, "a"),
                (TOKEN_COLON, ":"),
                (TOKEN_PAREN_OPEN, "("),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "b"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_PAREN_CLOSE, ")"),
            ],
        );
        check(
            "if false then 1 else if true then 2 else 3",
            [
                (TOKEN_IF, "if"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "false"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_THEN, "then"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "1"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ELSE, "else"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IF, "if"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "true"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_THEN, "then"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ELSE, "else"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "3"),
            ],
        );
        check(
            "x>=y",
            // <- could be confused with store path because of the '>'
            [(TOKEN_IDENT, "x"), (TOKEN_MORE_OR_EQ, ">="), (TOKEN_IDENT, "y")],
        );
    }
    #[test]
    fn dynamic_attrs() {
        check(
            "a.${b}.c",
            [
                (TOKEN_IDENT, "a"),
                (TOKEN_DOT, "."),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "b"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_DOT, "."),
                (TOKEN_IDENT, "c"),
            ],
        );
        check(
            r#"a.${ { b = "${test}"; }.b }.c"#,
            [
                (TOKEN_IDENT, "a"),
                (TOKEN_DOT, "."),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "b"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ASSIGN, "="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "\""),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "test"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_END, "\""),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_CURLY_B_CLOSE, "}"),
                (TOKEN_DOT, "."),
                (TOKEN_IDENT, "b"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_DOT, "."),
                (TOKEN_IDENT, "c"),
            ],
        );
    }
}
