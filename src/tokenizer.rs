//! The tokenizer: turns a string into tokens, such as numbers, strings, and keywords

use rowan::{SmolStr, SyntaxKind};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum IdentType {
    Ident,
    Path,
    Store,
    Uri
}

pub mod tokens {
    use rowan::SyntaxKind;

    // Internals
    pub const TOKEN_COMMENT: SyntaxKind = SyntaxKind(0);
    pub const TOKEN_ERROR: SyntaxKind = SyntaxKind(1);
    pub const TOKEN_WHITESPACE: SyntaxKind = SyntaxKind(2);

    // Keywords
    pub const TOKEN_ASSERT: SyntaxKind = SyntaxKind(3);
    pub const TOKEN_ELSE: SyntaxKind = SyntaxKind(4);
    pub const TOKEN_IF: SyntaxKind = SyntaxKind(5);
    pub const TOKEN_IN: SyntaxKind = SyntaxKind(6);
    pub const TOKEN_INHERIT: SyntaxKind = SyntaxKind(7);
    pub const TOKEN_LET: SyntaxKind = SyntaxKind(8);
    pub const TOKEN_REC: SyntaxKind = SyntaxKind(9);
    pub const TOKEN_THEN: SyntaxKind = SyntaxKind(10);
    pub const TOKEN_WITH: SyntaxKind = SyntaxKind(11);

    // Symbols
    pub const TOKEN_CURLY_B_OPEN: SyntaxKind = SyntaxKind(12);
    pub const TOKEN_CURLY_B_CLOSE: SyntaxKind = SyntaxKind(13);
    pub const TOKEN_SQUARE_B_OPEN: SyntaxKind = SyntaxKind(14);
    pub const TOKEN_SQUARE_B_CLOSE: SyntaxKind = SyntaxKind(15);
    pub const TOKEN_ASSIGN: SyntaxKind = SyntaxKind(16);
    pub const TOKEN_AT: SyntaxKind = SyntaxKind(17);
    pub const TOKEN_COLON: SyntaxKind = SyntaxKind(18);
    pub const TOKEN_COMMA: SyntaxKind = SyntaxKind(19);
    pub const TOKEN_DOT: SyntaxKind = SyntaxKind(20);
    pub const TOKEN_ELLIPSIS: SyntaxKind = SyntaxKind(21);
    pub const TOKEN_QUESTION: SyntaxKind = SyntaxKind(22);
    pub const TOKEN_SEMICOLON: SyntaxKind = SyntaxKind(23);

    // Operators
    pub const TOKEN_PAREN_OPEN: SyntaxKind = SyntaxKind(24);
    pub const TOKEN_PAREN_CLOSE: SyntaxKind = SyntaxKind(25);
    pub const TOKEN_CONCAT: SyntaxKind = SyntaxKind(26);
    pub const TOKEN_INVERT: SyntaxKind = SyntaxKind(27);
    pub const TOKEN_MERGE: SyntaxKind = SyntaxKind(28);

    pub const TOKEN_ADD: SyntaxKind = SyntaxKind(29);
    pub const TOKEN_SUB: SyntaxKind = SyntaxKind(30);
    pub const TOKEN_MUL: SyntaxKind = SyntaxKind(31);
    pub const TOKEN_DIV: SyntaxKind = SyntaxKind(32);

    pub const TOKEN_AND: SyntaxKind = SyntaxKind(33);
    pub const TOKEN_EQUAL: SyntaxKind = SyntaxKind(34);
    pub const TOKEN_IMPLICATION: SyntaxKind = SyntaxKind(35);
    pub const TOKEN_LESS: SyntaxKind = SyntaxKind(36);
    pub const TOKEN_LESS_OR_EQ: SyntaxKind = SyntaxKind(37);
    pub const TOKEN_MORE: SyntaxKind = SyntaxKind(38);
    pub const TOKEN_MORE_OR_EQ: SyntaxKind = SyntaxKind(39);
    pub const TOKEN_NOT_EQUAL: SyntaxKind = SyntaxKind(40);
    pub const TOKEN_OR: SyntaxKind = SyntaxKind(41);

    // Identifiers and values
    pub const TOKEN_DYNAMIC_END: SyntaxKind = SyntaxKind(42);
    pub const TOKEN_DYNAMIC_START: SyntaxKind = SyntaxKind(43);
    pub const TOKEN_FLOAT: SyntaxKind = SyntaxKind(44);
    pub const TOKEN_IDENT: SyntaxKind = SyntaxKind(45);
    pub const TOKEN_INTEGER: SyntaxKind = SyntaxKind(46);
    pub const TOKEN_INTERPOL_END: SyntaxKind = SyntaxKind(47);
    pub const TOKEN_INTERPOL_END_START: SyntaxKind = SyntaxKind(48);
    pub const TOKEN_INTERPOL_START: SyntaxKind = SyntaxKind(49);
    pub const TOKEN_PATH: SyntaxKind = SyntaxKind(50);
    pub const TOKEN_STRING: SyntaxKind = SyntaxKind(51);

    pub mod token_helpers {
        use super::*;

        /// Returns true if this token is a value, such as an integer or a string
        pub fn is_value(token: SyntaxKind) -> bool {
            match token {
                TOKEN_FLOAT | TOKEN_INTEGER | TOKEN_PATH | TOKEN_STRING => true,
                _ => false
            }
        }
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
        pub fn is_fn_arg(token: SyntaxKind) -> bool {
            match token {
                TOKEN_REC | TOKEN_CURLY_B_OPEN | TOKEN_SQUARE_B_OPEN | TOKEN_PAREN_OPEN
                    | TOKEN_INTERPOL_START | TOKEN_IDENT => true,
                _ => token_helpers::is_value(token)
            }
        }
        /// Returns true if this token is a comment, whitespace, or similar, and
        /// should be skipped over by the parser.
        pub fn is_trivia(token: SyntaxKind) -> bool {
            match token {
                TOKEN_COMMENT | TOKEN_ERROR | TOKEN_WHITESPACE => true,
                _ => false
            }
        }
    }
}
use self::tokens::*;

fn is_valid_path_char(c: char) -> bool {
    match c {
        'a'..='z' | 'A'..='Z' | '0'..='9' | '/' | '_' | '.' | '+' | '-' => true,
        _ => false
    }
}

#[derive(Clone, Copy)]
enum Context {
    Interpol {
        brackets: u32,
        string: bool,
        multiline: bool
    }
}

#[derive(Clone, Copy)]
struct State<'a> {
    input: &'a str,
    offset: usize,
}

/// The tokenizer. You may want to use the `tokenize` convenience function from this module instead.
pub struct Tokenizer<'a> {
    ctx: Vec<Context>,
    state: State<'a>
}
impl<'a> Tokenizer<'a> {
    /// Create a new instance
    pub fn new(input: &'a str) -> Self {
        Self {
            ctx: Vec::new(),
            state: State {
                input,
                offset: 0,
            },
        }
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
    fn string_since(&self, past: State) -> SmolStr {
        SmolStr::new(&past.input[past.offset..self.state.offset])
    }

    fn consume<F>(&mut self, mut f: F) -> usize
        where F: FnMut(char) -> bool
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
            match self.next() {
                None => return TOKEN_ERROR,
                Some('"') if !multiline => return TOKEN_STRING,
                Some('\\') if !multiline => match self.next() {
                    None => return TOKEN_ERROR,
                    Some(_) => ()
                },

                Some('\'') if multiline => match self.next() {
                    None => return TOKEN_ERROR,
                    Some('\'') => match self.peek() {
                        Some('\'') | Some('\\') | Some('$') => { self.next().unwrap(); },
                        _ => return TOKEN_STRING
                    },
                    Some(_) => ()
                },

                Some('$') => match self.peek() {
                    Some('$') => { self.next().unwrap(); },
                    Some('{') => {
                        self.next().unwrap();
                        self.ctx.push(Context::Interpol {
                            brackets: 0,
                            string: true,
                            multiline
                        });
                        return TOKEN_INTERPOL_START;
                    },
                    _ => ()
                }
                Some(_) => ()
            }
        }
    }
}
impl<'a> Iterator for Tokenizer<'a> {
    type Item = (SyntaxKind, SmolStr);

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.state;

        if self.consume(char::is_whitespace) > 0 {
            return Some((TOKEN_WHITESPACE, self.string_since(start)));
        }

        if self.peek() == Some('#') {
            self.consume(|c| c != '\n');
            return Some((TOKEN_COMMENT, self.string_since(start)));
        }
        if self.remaining().starts_with("/*") {
            self.next().unwrap();
            self.next().unwrap();
            loop {
                self.consume(|c| c != '*');
                self.next(); // consume the '*', if any
                match self.peek() {
                    None => return Some((TOKEN_ERROR, self.string_since(start))),
                    Some('/') => {
                        self.next().unwrap();
                        return Some((TOKEN_COMMENT, self.string_since(start)));
                    },
                    _ => ()
                }
            }
        }

        if self.remaining().starts_with("...") {
            self.state.offset += 3;
            return Some((TOKEN_ELLIPSIS, self.string_since(start)));
        }

        // Check if it's a path
        let store_path = self.peek() == Some('<');
        let kind = {
            let mut lookahead = self.remaining().chars().skip_while(|c| match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '.' | '+' | '-' => true,
                '<' | '/' => store_path,
                _ => false
            });
            match (lookahead.next(), lookahead.next()) {
                // a//b parses as Merge(a, b)
                (Some('/'), Some('/')) => None,
                (Some('/'), Some('*')) => None,
                (Some('/'), Some(c)) if !c.is_whitespace() => Some(IdentType::Path),
                (Some('>'), _) => Some(IdentType::Store),
                (Some(':'), Some(c)) if !c.is_whitespace() => Some(IdentType::Uri),
                _ => None
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
            '=' if self.peek() == Some('=') => { self.next().unwrap(); Some((TOKEN_EQUAL, self.string_since(start))) },
            '!' if self.peek() == Some('=') => { self.next().unwrap(); Some((TOKEN_NOT_EQUAL, self.string_since(start))) },
            '!' => Some((TOKEN_INVERT, self.string_since(start))),
            '{' => {
                if let Some(Context::Interpol { ref mut brackets, .. }) = self.ctx.last_mut() {
                    *brackets += 1;
                }
                Some((TOKEN_CURLY_B_OPEN, self.string_since(start)))
            },
            '}' => {
                if let Some(&mut Context::Interpol { ref mut brackets, string, multiline }) = self.ctx.last_mut() {
                    match brackets.checked_sub(1) {
                        Some(new) => *brackets = new,
                        None => {
                            self.ctx.pop().unwrap();

                            if string {
                                return Some((match self.next_string(multiline) {
                                    TOKEN_STRING => TOKEN_INTERPOL_END,
                                    TOKEN_INTERPOL_START => TOKEN_INTERPOL_END_START,
                                    other => other
                                }, self.string_since(start)));
                            } else {
                                return Some((TOKEN_DYNAMIC_END, self.string_since(start)))
                            }
                        }
                    }
                }
                Some((TOKEN_CURLY_B_CLOSE, self.string_since(start)))
            },
            '[' => Some((TOKEN_SQUARE_B_OPEN, self.string_since(start))),
            ']' => Some((TOKEN_SQUARE_B_CLOSE, self.string_since(start))),
            '@' => Some((TOKEN_AT, self.string_since(start))),
            ':' => Some((TOKEN_COLON, self.string_since(start))),
            ',' => Some((TOKEN_COMMA, self.string_since(start))),
            '.' => Some((TOKEN_DOT, self.string_since(start))),
            '=' => Some((TOKEN_ASSIGN, self.string_since(start))),
            '?' => Some((TOKEN_QUESTION, self.string_since(start))),
            ';' => Some((TOKEN_SEMICOLON, self.string_since(start))),
            '(' => Some((TOKEN_PAREN_OPEN, self.string_since(start))),
            ')' => Some((TOKEN_PAREN_CLOSE, self.string_since(start))),
            '+' if self.peek() == Some('+') => { self.next().unwrap(); Some((TOKEN_CONCAT, self.string_since(start))) },
            '-' if self.peek() == Some('>') => { self.next().unwrap(); Some((TOKEN_IMPLICATION, self.string_since(start))) },
            '/' if self.peek() == Some('/') => { self.next().unwrap(); Some((TOKEN_MERGE, self.string_since(start))) },
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
            },
            '&' if self.peek() == Some('&') => { self.next().unwrap(); Some((TOKEN_AND, self.string_since(start))) },
            '|' if self.peek() == Some('|') => { self.next().unwrap(); Some((TOKEN_OR, self.string_since(start))) },
            '<' if self.peek() == Some('=') => { self.next().unwrap(); Some((TOKEN_LESS_OR_EQ, self.string_since(start))) },
            '<' => Some((TOKEN_LESS, self.string_since(start))),
            '>' if self.peek() == Some('=') => { self.next().unwrap(); Some((TOKEN_MORE_OR_EQ, self.string_since(start))) },
            '>' => Some((TOKEN_MORE, self.string_since(start))),
            '$' if self.peek() == Some('{') => {
                self.next().unwrap();
                self.ctx.push(Context::Interpol {
                    brackets: 0,
                    string: false,
                    multiline: false
                });
                Some((TOKEN_DYNAMIC_START, self.string_since(start)))
            },
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
                    ':' | '?' | '@' | '&' | '=' | '$' | ',' | '!'
                        | '~' | '*' | '%' => kind == IdentType::Uri,
                    c => kind == IdentType::Uri && is_valid_path_char(c),
                });
                let ident = self.string_since(start);
                if kind == IdentType::Ident {
                    Some((match &*ident {
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
                    }, ident))
                } else {
                    Some((TOKEN_PATH, ident))
                }
            },
            '"' => Some((self.next_string(false), self.string_since(start))),
            '\'' if self.peek() == Some('\'') => {
                self.next().unwrap();
                Some((self.next_string(true), self.string_since(start)))
            },
            '0'..='9' => {
                self.consume(|c| c >= '0' && c <= '9');
                if self.peek() == Some('.') {
                    self.next().unwrap();
                    if self.consume(|c| c >= '0' && c <= '9') == 0 {
                        return Some((TOKEN_ERROR, self.string_since(start)));
                    }
                    Some((TOKEN_FLOAT, self.string_since(start)))
                } else {
                    Some((TOKEN_INTEGER, self.string_since(start)))
                }
            },
            _ => Some((TOKEN_ERROR, self.string_since(start)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{SyntaxKind, Tokenizer, tokens::*};
    use rowan::SmolStr;

    fn tokenize(input: &str) -> Vec<(SyntaxKind, SmolStr)> {
        Tokenizer::new(input).collect()
    }

    macro_rules! tokens {
        ($(($token:expr, $str:expr)),*) => {
            vec![$(($token, $str.into())),*]
        }
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
               (TOKEN_CURLY_B_CLOSE, "}")
            ]
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
               (TOKEN_CURLY_B_CLOSE, "}")
            ]
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
               (TOKEN_STRING, r#""Hello \"World\"""#),
               (TOKEN_SEMICOLON, ";"),
               (TOKEN_WHITESPACE, " "),
               (TOKEN_CURLY_B_CLOSE, "}")
            ]
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
            tokens![
               (TOKEN_CURLY_B_OPEN, "{"),
               (TOKEN_WHITESPACE, "\n    "),
               (TOKEN_IDENT, "multiline"),
               (TOKEN_WHITESPACE, " "),
               (TOKEN_ASSIGN, "="),
               (TOKEN_WHITESPACE, " "),
               (TOKEN_STRING, r#"''
            
                  
        This is a multiline string :D
          indented by two
        \'\'\'\'\
        ''${ interpolation was escaped }
        two single quotes: '''
        three single quotes: ''''
    ''"#),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, "\n"),
                (TOKEN_CURLY_B_CLOSE, "}")
            ]
        );
    }
    #[test]
    fn special_escape() {
        assert_eq!(
            tokenize(r#" "$${test}" "#),
            tokens![
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING, r#""$${test}""#),
                (TOKEN_WHITESPACE, " ")
            ]
        );
        assert_eq!(
            tokenize(r#" ''$${test}'' "#),
            tokens![
                (TOKEN_WHITESPACE, " "),
                (TOKEN_STRING, r#"''$${test}''"#),
                (TOKEN_WHITESPACE, " ")
            ]
        );
    }
    #[test]
    fn interpolation() {
        assert_eq!(
            tokenize(r#" "Hello, ${ { world = "World"; }.world }!" "#),
            tokens![
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTERPOL_START, r#""Hello, ${"#),
                    (TOKEN_WHITESPACE, " "),
                    (TOKEN_CURLY_B_OPEN, "{"),
                    (TOKEN_WHITESPACE, " "),
                    (TOKEN_IDENT, "world"),
                    (TOKEN_WHITESPACE, " "),
                    (TOKEN_ASSIGN, "="),
                    (TOKEN_WHITESPACE, " "),
                    (TOKEN_STRING, r#""World""#),
                    (TOKEN_SEMICOLON, ";"),
                    (TOKEN_WHITESPACE, " "),
                    (TOKEN_CURLY_B_CLOSE, "}"),
                    (TOKEN_DOT, "."),
                    (TOKEN_IDENT, "world"),
                    (TOKEN_WHITESPACE, " "),
                (TOKEN_INTERPOL_END, r#"}!""#),
                (TOKEN_WHITESPACE, " ")
            ]
        );
        assert_eq!(
            tokenize(r#" "\$${test}" "#),
            tokens![
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTERPOL_START, r#""\$${"#),
                    (TOKEN_IDENT, "test"),
                (TOKEN_INTERPOL_END, r#"}""#),
                (TOKEN_WHITESPACE, " ")
            ]
        );
        assert_eq!(
            tokenize(r#" ''''$${test}'' "#),
            tokens![
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTERPOL_START, r#"''''$${"#),
                    (TOKEN_IDENT, "test"),
                (TOKEN_INTERPOL_END, r#"}''"#),
                (TOKEN_WHITESPACE, " ")
            ]
        );
        assert_eq!(
            tokenize(r#" "${test}#123" "#),
            tokens![
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTERPOL_START, r#""${"#),
                    (TOKEN_IDENT, "test"),
                (TOKEN_INTERPOL_END, r#"}#123""#),
                (TOKEN_WHITESPACE, " ")
            ]
        );
        assert_eq!(
            tokenize(" ''\n${test}'' "),
            tokens![
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTERPOL_START, "''\n${"),
                    (TOKEN_IDENT, "test"),
                (TOKEN_INTERPOL_END, r#"}''"#),
                (TOKEN_WHITESPACE, " ")
            ]
        );
        assert_eq!(
            tokenize(r#" "${hello} ${world}" "#),
            tokens![
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTERPOL_START, r#""${"#),
                    (TOKEN_IDENT, "hello"),
                (TOKEN_INTERPOL_END_START, r#"} ${"#),
                    (TOKEN_IDENT, "world"),
                (TOKEN_INTERPOL_END, r#"}""#),
                (TOKEN_WHITESPACE, " ")
            ]
        );
        assert_eq!(
            tokenize(r#" ''${"${var}"}'' "#),
            tokens![
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTERPOL_START, r#"''${"#),
                    (TOKEN_INTERPOL_START, r#""${"#),
                        (TOKEN_IDENT, "var"),
                    (TOKEN_INTERPOL_END, r#"}""#),
                (TOKEN_INTERPOL_END, r#"}''"#),
                (TOKEN_WHITESPACE, " ")
            ]
        );
    }
    #[test]
    fn comments() {
        assert_eq!(
            tokenize("/**/"),
            tokens![(TOKEN_COMMENT, "/**/")]
        );
        assert_eq!(
            tokenize("/***/"),
            tokens![(TOKEN_COMMENT, "/***/")]
        );
        assert_eq!(
            tokenize("{ a = /* multiline * comment */ 123;# single line\n} # single line at the end"),
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
                (TOKEN_COMMENT, "# single line at the end")
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
                (TOKEN_INTEGER, "3")
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
                (TOKEN_PAREN_CLOSE, ")")
            ]
        );
        assert_eq!(
            tokenize("a/ 3"), // <- could get mistaken for a path
            tokens![
                (TOKEN_IDENT, "a"),
                (TOKEN_DIV, "/"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "3")
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
                (TOKEN_IDENT, "a")
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
                (TOKEN_IDENT, "expr")
            ]
        );
    }
    #[test]
    fn paths() {
        fn path(path: &str) -> Vec<(SyntaxKind, SmolStr)> {
            tokens![(TOKEN_PATH, path)]
        }
        assert_eq!(tokenize("/hello/world"),  path("/hello/world"));
        assert_eq!(tokenize("hello/world"),   path("hello/world"));
        assert_eq!(tokenize("a+3/5+b"),       path("a+3/5+b"));
        assert_eq!(tokenize("1-2/3"),         path("1-2/3"));
        assert_eq!(tokenize("./hello/world"), path("./hello/world"));
        assert_eq!(tokenize("~/hello/world"), path("~/hello/world"));
        assert_eq!(tokenize("<hello/world>"), path("<hello/world>"));
        assert_eq!(
            tokenize("https://google.com/?q=Hello+World"),
            path("https://google.com/?q=Hello+World")
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
                (TOKEN_STRING, r#""lol""#),
                (TOKEN_SQUARE_B_CLOSE, "]")
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
                (TOKEN_SQUARE_B_CLOSE, "]")
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
                (TOKEN_IDENT, "b")
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
                (TOKEN_STRING, r#""default""#),
                (TOKEN_COMMA, ","),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ELLIPSIS, "..."),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_CURLY_B_CLOSE, "}"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_AT, "@"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "outer")
            ]
        );
    }
    #[test]
    fn combine() {
        assert_eq!(
            tokenize("a//b"),
            tokens![
                (TOKEN_IDENT, "a"),
                (TOKEN_MERGE, "//"),
                (TOKEN_IDENT, "b")
            ]
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
                (TOKEN_IDENT, "true")
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
                (TOKEN_INTEGER, "2")
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
                (TOKEN_INTEGER, "3")
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
                        (TOKEN_INTEGER, "3")
            ]
        );
        assert_eq!(
            tokenize("x>=y"), // <- could be confused with store path because of the '>'
            tokens![
                (TOKEN_IDENT, "x"),
                (TOKEN_MORE_OR_EQ, ">="),
                (TOKEN_IDENT, "y")
            ]
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
                (TOKEN_IDENT, "c")
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
                    (TOKEN_INTERPOL_START, r#""${"#),
                        (TOKEN_IDENT, "test"),
                    (TOKEN_INTERPOL_END, r#"}""#),
                    (TOKEN_SEMICOLON, ";"),
                    (TOKEN_WHITESPACE, " "),
                    (TOKEN_CURLY_B_CLOSE, "}"),
                    (TOKEN_DOT, "."),
                    (TOKEN_IDENT, "b"),
                    (TOKEN_WHITESPACE, " "),
                (TOKEN_DYNAMIC_END, "}"),
                (TOKEN_DOT, "."),
                (TOKEN_IDENT, "c")
            ]
        );
    }
}
