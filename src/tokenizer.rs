//! The tokenizer: turns a string into tokens, such as numbers, strings, and keywords

use smol_str::SmolStr;

use crate::SyntaxKind::{self, *};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum IdentType {
    Ident,
    Path,
    Store,
    Uri,
}

fn is_valid_path_char(c: char) -> bool {
    match c {
        'a'..='z' | 'A'..='Z' | '0'..='9' | '/' | '_' | '.' | '+' | '-' => true,
        _ => false,
    }
}

fn is_valid_uri_char(c: char) -> bool {
    match c {
        '%' | '?' | ':' | '@' | '&' | '=' | '$' | ',' | '!' | '~' | '*' | '\'' => true,
        _ => is_valid_path_char(c),
    }
}

#[derive(Clone, Copy)]
struct Interpol {
    brackets: u32,
    string: bool,
    multiline: bool,
}
#[derive(Clone, Copy)]
enum Todo {
    StringBody { multiline: bool },
    StringEnd,
    InterpolStart,
}
#[derive(Clone, Copy, Default)]
struct Context {
    interpol: Option<Interpol>,
    todo: Option<Todo>,
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

/// The tokenizer. You may want to use the `tokenize` convenience function from this module instead.
pub struct Tokenizer<'a> {
    ctx: Vec<Context>,
    state: State<'a>,
}
impl<'a> Tokenizer<'a> {
    /// Create a new instance
    pub fn new(input: &'a str) -> Self {
        Self { ctx: vec![Context::default()], state: State { input, offset: 0 } }
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
    fn string_since(&self, past: State) -> SmolStr {
        SmolStr::new(&past.input[past.offset..self.state.offset])
    }

    fn consume<F>(&mut self, mut f: F) -> usize
    where
        F: FnMut(char) -> bool,
    {
        let mut len = 0;
        while self.peek().map(|c| f(c)).unwrap_or(false) {
            self.next().unwrap();
            len += 1;
        }
        len
    }
    fn next_string(&mut self, multiline: bool) -> SyntaxKind {
        loop {
            let start = self.state;
            match self.next() {
                None => return TOKEN_ERROR,
                Some('"') if !multiline => {
                    self.state = start;
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
                        Some('\'') | Some('\\') | Some('$') => {
                            self.next().unwrap();
                        }
                        _ => {
                            self.state = start;
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
                        self.ctx.push(Context {
                            interpol: Some(Interpol { brackets: 0, string: true, multiline }),
                            todo: Some(Todo::InterpolStart),
                            ..Default::default()
                        });
                        return TOKEN_STRING_CONTENT;
                    }
                    _ => (),
                },
                Some(_) => (),
            }
        }
    }
}
impl<'a> Iterator for Tokenizer<'a> {
    type Item = (SyntaxKind, SmolStr);

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.state;

        // Handle already started multi-token
        loop {
            let todo = &mut self.ctx.last_mut().unwrap().todo;
            match todo.take() {
                Some(Todo::InterpolStart) => {
                    if self.starts_with_bump("${") {
                        return Some((TOKEN_INTERPOL_START, self.string_since(start)));
                    }
                }
                Some(Todo::StringBody { multiline }) => {
                    *todo = Some(Todo::StringEnd);
                    let token = self.next_string(multiline);
                    if self.state == start {
                        continue;
                    }
                    return Some((token, self.string_since(start)));
                }
                Some(Todo::StringEnd) => {
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
                    if !status {
                        return Some((TOKEN_ERROR, self.string_since(start)));
                    }

                    return Some((TOKEN_STRING_END, self.string_since(start)));
                }
                _ => (),
            }
            break;
        }

        if self.consume(char::is_whitespace) > 0 {
            return Some((TOKEN_WHITESPACE, self.string_since(start)));
        }

        if self.peek() == Some('#') {
            self.consume(|c| c != '\n');
            return Some((TOKEN_COMMENT, self.string_since(start)));
        }
        if self.starts_with_bump("/*") {
            loop {
                self.consume(|c| c != '*');
                self.next(); // consume the '*', if any
                match self.peek() {
                    None => return Some((TOKEN_ERROR, self.string_since(start))),
                    Some('/') => {
                        self.next().unwrap();
                        return Some((TOKEN_COMMENT, self.string_since(start)));
                    }
                    _ => (),
                }
            }
        }

        if self.starts_with_bump("...") {
            return Some((TOKEN_ELLIPSIS, self.string_since(start)));
        }

        // Check if it's a path
        let store_path = self.peek() == Some('<');
        let kind = {
            let mut lookahead = self.remaining().chars().skip_while(|c| match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '.' | '+' | '-' => true,
                '<' | '/' => store_path,
                _ => false,
            });
            match (lookahead.next(), lookahead.next()) {
                // a//b parses as Update(a, b)
                (Some('/'), Some('/')) => None,
                (Some('/'), Some('*')) => None,
                (Some('/'), Some(c)) if !c.is_whitespace() => Some(IdentType::Path),
                (Some('>'), _) => Some(IdentType::Store),
                (Some(':'), Some(c)) if is_valid_uri_char(c) => Some(IdentType::Uri),
                _ => None,
            }
        };

        let c = self.next()?;

        if c == '~' || kind == Some(IdentType::Path) {
            if c == '~' && self.next() != Some('/') {
                return Some((TOKEN_ERROR, self.string_since(start)));
            }
            self.consume(is_valid_path_char);
            let ident = self.string_since(start);
            if ident.ends_with('/') {
                return Some((TOKEN_ERROR, ident));
            }
            return Some((TOKEN_PATH, ident));
        }

        match c {
            '=' if self.peek() == Some('=') => {
                self.next().unwrap();
                Some((TOKEN_EQUAL, self.string_since(start)))
            }
            '!' if self.peek() == Some('=') => {
                self.next().unwrap();
                Some((TOKEN_NOT_EQUAL, self.string_since(start)))
            }
            '!' => Some((TOKEN_INVERT, self.string_since(start))),
            '{' => {
                if let Some(Interpol { ref mut brackets, .. }) =
                    self.ctx.last_mut().unwrap().interpol
                {
                    *brackets += 1;
                }
                Some((TOKEN_CURLY_B_OPEN, self.string_since(start)))
            }
            '}' => {
                if let Some(Interpol { ref mut brackets, string, multiline }) =
                    self.ctx.last_mut().unwrap().interpol
                {
                    match brackets.checked_sub(1) {
                        Some(new) => *brackets = new,
                        None => {
                            self.ctx.pop().unwrap();

                            if string {
                                self.ctx.last_mut().unwrap().todo =
                                    Some(Todo::StringBody { multiline });
                                return Some((TOKEN_INTERPOL_END, self.string_since(start)));
                            } else {
                                return Some((TOKEN_DYNAMIC_END, self.string_since(start)));
                            }
                        }
                    }
                }
                Some((TOKEN_CURLY_B_CLOSE, self.string_since(start)))
            }
            '[' => Some((TOKEN_SQUARE_B_OPEN, self.string_since(start))),
            ']' => Some((TOKEN_SQUARE_B_CLOSE, self.string_since(start))),
            '@' => Some((TOKEN_AT, self.string_since(start))),
            ':' => Some((TOKEN_COLON, self.string_since(start))),
            ',' => Some((TOKEN_COMMA, self.string_since(start))),
            '.' => {
                if self.peek().map(|x| x >= '0' && x <= '9').unwrap_or(false) {
                    self.consume(|c| c >= '0' && c <= '9');
                    Some((TOKEN_FLOAT, self.string_since(start)))
                } else {
                    Some((TOKEN_DOT, self.string_since(start)))
                }
            },
            '=' => Some((TOKEN_ASSIGN, self.string_since(start))),
            '?' => Some((TOKEN_QUESTION, self.string_since(start))),
            ';' => Some((TOKEN_SEMICOLON, self.string_since(start))),
            '(' => Some((TOKEN_PAREN_OPEN, self.string_since(start))),
            ')' => Some((TOKEN_PAREN_CLOSE, self.string_since(start))),
            '+' if self.peek() == Some('+') => {
                self.next().unwrap();
                Some((TOKEN_CONCAT, self.string_since(start)))
            }
            '-' if self.peek() == Some('>') => {
                self.next().unwrap();
                Some((TOKEN_IMPLICATION, self.string_since(start)))
            }
            '/' if self.peek() == Some('/') => {
                self.next().unwrap();
                Some((TOKEN_UPDATE, self.string_since(start)))
            }
            '+' => Some((TOKEN_ADD, self.string_since(start))),
            '-' => Some((TOKEN_SUB, self.string_since(start))),
            '*' => Some((TOKEN_MUL, self.string_since(start))),
            '/' => Some((TOKEN_DIV, self.string_since(start))),
            '<' if kind == Some(IdentType::Store) => {
                self.consume(is_valid_path_char);
                if self.next() != Some('>') {
                    return Some((TOKEN_ERROR, self.string_since(start)));
                }
                Some((TOKEN_PATH, self.string_since(start)))
            }
            '&' if self.peek() == Some('&') => {
                self.next().unwrap();
                Some((TOKEN_AND, self.string_since(start)))
            }
            '|' if self.peek() == Some('|') => {
                self.next().unwrap();
                Some((TOKEN_OR, self.string_since(start)))
            }
            '<' if self.peek() == Some('=') => {
                self.next().unwrap();
                Some((TOKEN_LESS_OR_EQ, self.string_since(start)))
            }
            '<' => Some((TOKEN_LESS, self.string_since(start))),
            '>' if self.peek() == Some('=') => {
                self.next().unwrap();
                Some((TOKEN_MORE_OR_EQ, self.string_since(start)))
            }
            '>' => Some((TOKEN_MORE, self.string_since(start))),
            '$' if self.peek() == Some('{') => {
                self.next().unwrap();
                self.ctx.push(Context {
                    interpol: Some(Interpol { brackets: 0, string: false, multiline: false }),
                    ..Default::default()
                });
                Some((TOKEN_DYNAMIC_START, self.string_since(start)))
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let kind = match kind {
                    // It's detected as store if it ends with >, but if it
                    // didn't start with <, that's wrong
                    Some(IdentType::Store) | None => IdentType::Ident,
                    Some(kind) => kind,
                };
                assert_ne!(kind, IdentType::Path, "paths are checked earlier");
                self.consume(|c| match c {
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '\'' => true,
                    c => kind == IdentType::Uri && is_valid_uri_char(c),
                });
                let ident = self.string_since(start);
                let syntax_kind = match kind {
                    IdentType::Ident => match &*ident {
                        "assert" => TOKEN_ASSERT,
                        "else" => TOKEN_ELSE,
                        "if" => TOKEN_IF,
                        "in" => TOKEN_IN,
                        "inherit" => TOKEN_INHERIT,
                        "let" => TOKEN_LET,
                        "rec" => TOKEN_REC,
                        "then" => TOKEN_THEN,
                        "with" => TOKEN_WITH,
                        _ => TOKEN_IDENT,
                    },
                    IdentType::Path | IdentType::Store => TOKEN_PATH,
                    IdentType::Uri => TOKEN_URI,
                };
                Some((syntax_kind, ident))
            }
            '"' => {
                self.ctx.last_mut().unwrap().todo = Some(Todo::StringBody { multiline: false });
                Some((TOKEN_STRING_START, self.string_since(start)))
            }
            '\'' if self.peek() == Some('\'') => {
                self.next().unwrap();
                self.ctx.last_mut().unwrap().todo = Some(Todo::StringBody { multiline: true });
                Some((TOKEN_STRING_START, self.string_since(start)))
            }
            '0'..='9' => {
                self.consume(|c| c >= '0' && c <= '9');
                if self.peek() == Some('.') {
                    self.next().unwrap();
                    if self.consume(|c| c >= '0' && c <= '9') == 0 {
                        return Some((TOKEN_ERROR, self.string_since(start)));
                    }
                    if self.peek() == Some('e') || self.peek() == Some('E') {
                        self.next().unwrap();
                        if self.peek() == Some('-') {
                            self.next().unwrap();
                        }
                        if self.consume(|c| c >= '0' && c <= '9') == 0 {
                            return Some((TOKEN_ERROR, self.string_since(start)));
                        }
                    }
                    Some((TOKEN_FLOAT, self.string_since(start)))
                } else {
                    Some((TOKEN_INTEGER, self.string_since(start)))
                }
            }
            _ => Some((TOKEN_ERROR, self.string_since(start))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        SyntaxKind::{self, *},
        Tokenizer,
    };
    use smol_str::SmolStr;

    fn tokenize(input: &str) -> Vec<(SyntaxKind, SmolStr)> {
        Tokenizer::new(input).collect()
    }

    macro_rules! tokens {
        ($(($token:expr, $str:expr),)*) => {
            vec![$(($token, $str.into()),)*]
        };
        ($(($token:expr, $str:expr)),*) => {
            vec![$(($token, $str.into())),*]
        };
    }

    #[test]
    fn basic_int_set() {
        assert_eq!(
            tokenize("{ int = 42; }"),
            tokens![
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
        assert_eq!(
            tokenize("{ float = 1.234; }"),
            tokens![
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
        assert_eq!(
            tokenize(".5 + 0.5"),
            tokens![
                (TOKEN_FLOAT, ".5"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ADD, "+"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_FLOAT, "0.5"),
            ],
        );
        assert_eq!(
            tokenize("{ scientific = 1.1e4; uppercase = 123.4E-2; }"),
            tokens![
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
    fn basic_string_set() {
        assert_eq!(
            tokenize(r#"{ string = "Hello \"World\""; }"#),
            tokens![
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
        assert_eq!(
            tokenize(
                r#"{
    multiline = ''
        This is a multiline string :D
          indented by two
        \'\'\'\'\
        ''${ interpolation was escaped }
        two single quotes: '''
        three single quotes: ''''
    '';
}"#
            ),
            tokens![
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
        two single quotes: '''
        three single quotes: ''''
    "#
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
        assert_eq!(
            tokenize(r#" "$${test}" "#),
            tokens![
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "\""),
                (TOKEN_STRING_CONTENT, r#"$${test}"#),
                (TOKEN_STRING_END, "\""),
                (TOKEN_WHITESPACE, " "),
            ]
        );
        assert_eq!(
            tokenize(r#" ''$${test}'' "#),
            tokens![
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "''"),
                (TOKEN_STRING_CONTENT, r#"$${test}"#),
                (TOKEN_STRING_END, "''"),
                (TOKEN_WHITESPACE, " "),
            ]
        );
    }
    #[test]
    fn interpolation() {
        assert_eq!(
            tokenize(r#" "Hello, ${ { world = "World"; }.world }!" "#),
            tokens![
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
            ]
        );
        assert_eq!(
            tokenize(r#" "\$${test}" "#),
            tokens![
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "\""),
                (TOKEN_STRING_CONTENT, "\\$"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "test"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_END, "\""),
                (TOKEN_WHITESPACE, " "),
            ]
        );
        assert_eq!(
            tokenize(r#" ''''$${test}'' "#),
            tokens![
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "''"),
                (TOKEN_STRING_CONTENT, "''$"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "test"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_END, "''"),
                (TOKEN_WHITESPACE, " "),
            ]
        );
        assert_eq!(
            tokenize(r#" "${test}#123" "#),
            tokens![
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "\""),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "test"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_CONTENT, "#123"),
                (TOKEN_STRING_END, "\""),
                (TOKEN_WHITESPACE, " "),
            ]
        );
        assert_eq!(
            tokenize(" ''\n${test}'' "),
            tokens![
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING_START, "''"),
                (TOKEN_STRING_CONTENT, "\n"),
                (TOKEN_INTERPOL_START, "${"),
                (TOKEN_IDENT, "test"),
                (TOKEN_INTERPOL_END, "}"),
                (TOKEN_STRING_END, "''"),
                (TOKEN_WHITESPACE, " "),
            ]
        );
        assert_eq!(
            tokenize(r#" "${hello} ${world}" "#),
            tokens![
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
            ]
        );
        assert_eq!(
            tokenize(r#" ''${"${var}"}'' "#),
            tokens![
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
            ]
        );
        assert_eq!(
            tokenize(r#" ''dont '${escape} me'' "#),
            tokens![
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
        assert_eq!(tokenize("/**/"), tokens![(TOKEN_COMMENT, "/**/")]);
        assert_eq!(tokenize("/***/"), tokens![(TOKEN_COMMENT, "/***/")]);
        assert_eq!(
            tokenize(
                "{ a = /* multiline * comment */ 123;# single line\n} # single line at the end"
            ),
            tokens![
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
            ]
        );
    }
    #[test]
    fn math() {
        assert_eq!(
            tokenize("1 + 2 * 3"),
            tokens![
                (TOKEN_INTEGER, "1"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ADD, "+"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_MUL, "*"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "3"),
            ]
        );
        assert_eq!(
            tokenize("5 * -(3 - 2)"),
            tokens![
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
            ]
        );
        assert_eq!(
            tokenize("a/ 3"), // <- could get mistaken for a path
            tokens![
                (TOKEN_IDENT, "a"),
                (TOKEN_DIV, "/"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "3"),
            ]
        );
    }
    #[test]
    fn let_in() {
        assert_eq!(
            tokenize("let a = 3; in a"),
            tokens![
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
            ]
        );
    }
    #[test]
    fn with() {
        assert_eq!(
            tokenize("with namespace; expr"),
            tokens![
                (TOKEN_WITH, "with"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "namespace"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "expr"),
            ]
        );
    }
    #[test]
    fn paths() {
        fn path(path: &str) -> Vec<(SyntaxKind, SmolStr)> {
            tokens![(TOKEN_PATH, path)]
        }
        assert_eq!(tokenize("/hello/world"), path("/hello/world"));
        assert_eq!(tokenize("hello/world"), path("hello/world"));
        assert_eq!(tokenize("a+3/5+b"), path("a+3/5+b"));
        assert_eq!(tokenize("1-2/3"), path("1-2/3"));
        assert_eq!(tokenize("./hello/world"), path("./hello/world"));
        assert_eq!(tokenize("~/hello/world"), path("~/hello/world"));
        assert_eq!(tokenize("<hello/world>"), path("<hello/world>"));
    }
    #[test]
    fn uri() {
        assert_eq!(
            tokenize("https://google.com/?q=Hello+World"),
            tokens![(TOKEN_URI, "https://google.com/?q=Hello+World")]
        );
    }
    #[test]
    fn list() {
        assert_eq!(
            tokenize(r#"[a 2 3 "lol"]"#),
            tokens![
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
            ]
        );
        assert_eq!(
            tokenize("[1] ++ [2] ++ [3]"),
            tokens![
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
            ]
        );
    }
    #[test]
    fn lambda() {
        assert_eq!(
            tokenize("a: b: a + b"),
            tokens![
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
            ]
        );
    }
    #[test]
    fn patterns() {
        assert_eq!(
            tokenize(r#"{ a, b ? "default", ... } @ outer"#),
            tokens![
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
            ]
        );
    }
    #[test]
    fn combine() {
        assert_eq!(
            tokenize("a//b"),
            tokens![(TOKEN_IDENT, "a"), (TOKEN_UPDATE, "//"), (TOKEN_IDENT, "b")]
        );
    }
    #[test]
    fn ifs() {
        assert_eq!(
            tokenize("false -> !false && false == true || true"),
            tokens![
                (TOKEN_IDENT, "false"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IMPLICATION, "->"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INVERT, "!"),
                (TOKEN_IDENT, "false"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_AND, "&&"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "false"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_EQUAL, "=="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "true"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_OR, "||"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "true"),
            ]
        );
        assert_eq!(
            tokenize("1 < 2 && 2 <= 2 && 2 > 1 && 2 >= 2"),
            tokens![
                (TOKEN_INTEGER, "1"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_LESS, "<"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_AND, "&&"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_LESS_OR_EQ, "<="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_AND, "&&"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_MORE, ">"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "1"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_AND, "&&"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_MORE_OR_EQ, ">="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
            ]
        );
        assert_eq!(
            tokenize("1 == 1 && 2 != 3"),
            tokens![
                (TOKEN_INTEGER, "1"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_EQUAL, "=="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "1"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_AND, "&&"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_NOT_EQUAL, "!="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "3"),
            ]
        );
        assert_eq!(
            tokenize("a:[ b ]"),
            tokens![
                (TOKEN_IDENT, "a"),
                (TOKEN_COLON, ":"),
                (TOKEN_SQUARE_B_OPEN, "["),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "b"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_SQUARE_B_CLOSE, "]"),
            ]
        );
        assert_eq!(
            tokenize("a:( b )"),
            tokens![
                (TOKEN_IDENT, "a"),
                (TOKEN_COLON, ":"),
                (TOKEN_PAREN_OPEN, "("),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "b"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_PAREN_CLOSE, ")"),
            ]
        );
        assert_eq!(
            tokenize("if false then 1 else if true then 2 else 3"),
            tokens![
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
            ]
        );
        assert_eq!(
            tokenize("x>=y"), // <- could be confused with store path because of the '>'
            tokens![(TOKEN_IDENT, "x"), (TOKEN_MORE_OR_EQ, ">="), (TOKEN_IDENT, "y")]
        );
    }
    #[test]
    fn dynamic_attrs() {
        assert_eq!(
            tokenize("a.${b}.c"),
            tokens![
                (TOKEN_IDENT, "a"),
                (TOKEN_DOT, "."),
                (TOKEN_DYNAMIC_START, "${"),
                (TOKEN_IDENT, "b"),
                (TOKEN_DYNAMIC_END, "}"),
                (TOKEN_DOT, "."),
                (TOKEN_IDENT, "c"),
            ]
        );
        assert_eq!(
            tokenize(r#"a.${ { b = "${test}"; }.b }.c"#),
            tokens![
                (TOKEN_IDENT, "a"),
                (TOKEN_DOT, "."),
                (TOKEN_DYNAMIC_START, "${"),
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
                (TOKEN_DYNAMIC_END, "}"),
                (TOKEN_DOT, "."),
                (TOKEN_IDENT, "c"),
            ]
        );
    }
}
