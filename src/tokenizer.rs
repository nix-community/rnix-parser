//! The tokenizer: turns a string into tokens, such as numbers, strings, and keywords

use rowan::SmolStr;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum IdentType {
    Ident,
    Path,
    Store,
    Uri
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    // Internals
    Comment,
    Error,
    Whitespace,

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
    ParenOpen,
    ParenClose,
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
    DynamicEnd,
    DynamicStart,
    Ident,
    InterpolEnd,
    InterpolEndStart,
    InterpolStart,
    Value
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
    pub fn is_fn_arg(self) -> bool {
        match self {
            Token::Rec | Token::CurlyBOpen | Token::SquareBOpen | Token::ParenOpen
                | Token::Ident | Token::Value | Token::InterpolStart => true,
            _ => false
        }
    }
    /// Returns true if this token is a comment, whitespace, or similar, and
    /// should be skipped over by the parser.
    pub fn is_trivia(self) -> bool {
        match self {
            Token::Comment | Token::Error | Token::Whitespace => true,
            _ => false
        }
    }
}

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
    fn next_string(&mut self, multiline: bool) -> Token {
        loop {
            match self.next() {
                None => return Token::Error,
                Some('"') if !multiline => return Token::Value,
                Some('\\') if !multiline => match self.next() {
                    None => return Token::Error,
                    Some(_) => ()
                },

                Some('\'') if multiline => match self.next() {
                    None => return Token::Error,
                    Some('\'') => match self.peek() {
                        Some('\'') | Some('\\') | Some('$') => { self.next().unwrap(); },
                        _ => return Token::Value
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
                        return Token::InterpolStart;
                    },
                    _ => ()
                }
                Some(_) => ()
            }
        }
    }
}
impl<'a> Iterator for Tokenizer<'a> {
    type Item = (Token, SmolStr);

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.state;

        if self.consume(char::is_whitespace) > 0 {
            return Some((Token::Whitespace, self.string_since(start)));
        }

        if self.peek() == Some('#') {
            self.consume(|c| c != '\n');
            self.next(); // consume the newline, if any
            return Some((Token::Comment, self.string_since(start)));
        }
        if self.remaining().starts_with("/*") {
            loop {
                self.consume(|c| c != '*');
                self.next(); // consume the '*', if any
                match self.next() {
                    None => return Some((Token::Error, self.string_since(start))),
                    Some('/') => return Some((Token::Comment, self.string_since(start))),
                    _ => ()
                }
            }
        }

        if self.remaining().starts_with("...") {
            self.state.offset += 3;
            return Some((Token::Ellipsis, self.string_since(start)));
        }

        // Check if it's a path
        let store_path = self.peek() == Some('<');
        let mut lookahead = self.remaining().chars().skip_while(|c| match c {
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

        let c = self.next()?;

        if c == '~' || kind == Some(IdentType::Path) {
            if c == '~' && self.next() != Some('/') {
                return Some((Token::Error, self.string_since(start)));
            }
            self.consume(is_valid_path_char);
            let ident = self.string_since(start);
            if ident.ends_with('/') {
                return Some((Token::Error, ident));
            }
            return Some((Token::Value, ident));
        }

        match c {
            '=' if self.peek() == Some('=') => { self.next().unwrap(); Some((Token::Equal, self.string_since(start))) },
            '!' if self.peek() == Some('=') => { self.next().unwrap(); Some((Token::NotEqual, self.string_since(start))) },
            '!' => Some((Token::Invert, self.string_since(start))),
            '{' => {
                if let Some(Context::Interpol { ref mut brackets, .. }) = self.ctx.last_mut() {
                    *brackets += 1;
                }
                Some((Token::CurlyBOpen, self.string_since(start)))
            },
            '}' => {
                if let Some(&mut Context::Interpol { ref mut brackets, string, multiline }) = self.ctx.last_mut() {
                    match brackets.checked_sub(1) {
                        Some(new) => *brackets = new,
                        None => {
                            self.ctx.pop().unwrap();

                            if string {
                                return Some((match self.next_string(multiline) {
                                    Token::Value => Token::InterpolEnd,
                                    Token::InterpolStart => Token::InterpolEndStart,
                                    token => unreachable!("unexpected value from next_string: {:?}", token)
                                }, self.string_since(start)));
                            } else {
                                return Some((Token::DynamicEnd, self.string_since(start)))
                            }
                        }
                    }
                }
                Some((Token::CurlyBClose, self.string_since(start)))
            },
            '[' => Some((Token::SquareBOpen, self.string_since(start))),
            ']' => Some((Token::SquareBClose, self.string_since(start))),
            '@' => Some((Token::At, self.string_since(start))),
            ':' => Some((Token::Colon, self.string_since(start))),
            ',' => Some((Token::Comma, self.string_since(start))),
            '.' => Some((Token::Dot, self.string_since(start))),
            '=' => Some((Token::Assign, self.string_since(start))),
            '?' => Some((Token::Question, self.string_since(start))),
            ';' => Some((Token::Semicolon, self.string_since(start))),
            '(' => Some((Token::ParenOpen, self.string_since(start))),
            ')' => Some((Token::ParenClose, self.string_since(start))),
            '+' if self.peek() == Some('+') => { self.next().unwrap(); Some((Token::Concat, self.string_since(start))) },
            '-' if self.peek() == Some('>') => { self.next().unwrap(); Some((Token::Implication, self.string_since(start))) },
            '/' if self.peek() == Some('/') => { self.next().unwrap(); Some((Token::Merge, self.string_since(start))) },
            '+' => Some((Token::Add, self.string_since(start))),
            '-' => Some((Token::Sub, self.string_since(start))),
            '*' => Some((Token::Mul, self.string_since(start))),
            '/' => Some((Token::Div, self.string_since(start))),
            '<' if kind == Some(IdentType::Store) => {
                self.consume(is_valid_path_char);
                if self.next() != Some('>') {
                    return Some((Token::Error, self.string_since(start)));
                }
                Some((Token::Value, self.string_since(start)))
            },
            '&' if self.peek() == Some('&') => { self.next().unwrap(); Some((Token::And, self.string_since(start))) },
            '|' if self.peek() == Some('|') => { self.next().unwrap(); Some((Token::Or, self.string_since(start))) },
            '<' if self.peek() == Some('=') => { self.next().unwrap(); Some((Token::LessOrEq, self.string_since(start))) },
            '<' => Some((Token::Less, self.string_since(start))),
            '>' if self.peek() == Some('=') => { self.next().unwrap(); Some((Token::MoreOrEq, self.string_since(start))) },
            '>' => Some((Token::More, self.string_since(start))),
            '$' if self.peek() == Some('{') => {
                self.next().unwrap();
                self.ctx.push(Context::Interpol {
                    brackets: 0,
                    string: false,
                    multiline: false
                });
                Some((Token::DynamicStart, self.string_since(start)))
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
                        "assert" => Token::Assert,
                        "else" => Token::Else,
                        "if" => Token::If,
                        "import" => Token::Import,
                        "in" => Token::In,
                        "inherit" => Token::Inherit,
                        "let" => Token::Let,
                        "rec" => Token::Rec,
                        "then" => Token::Then,
                        "with" => Token::With,

                        "true" | "false" | "null" => Token::Value,
                        _ => Token::Ident,
                    }, ident))
                } else {
                    Some((Token::Value, ident))
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
                        return Some((Token::Error, self.string_since(start)));
                    }
                }

                Some((Token::Value, self.string_since(start)))
            },
            _ => Some((Token::Error, self.string_since(start)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Token, Tokenizer};
    use rowan::SmolStr;

    fn tokenize(input: &str) -> Vec<(Token, SmolStr)> {
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
               (Token::CurlyBOpen, "{"),
               (Token::Whitespace, " "),
               (Token::Ident, "int"),
               (Token::Whitespace, " "),
               (Token::Assign, "="),
               (Token::Whitespace, " "),
               (Token::Value, "42"),
               (Token::Semicolon, ";"),
               (Token::Whitespace, " "),
               (Token::CurlyBClose, "}")
            ]
        );
    }
    #[test]
    fn basic_float_set() {
        assert_eq!(
            tokenize("{ float = 1.234; }"),
            tokens![
               (Token::CurlyBOpen, "{"),
               (Token::Whitespace, " "),
               (Token::Ident, "float"),
               (Token::Whitespace, " "),
               (Token::Assign, "="),
               (Token::Whitespace, " "),
               (Token::Value, "1.234"),
               (Token::Semicolon, ";"),
               (Token::Whitespace, " "),
               (Token::CurlyBClose, "}")
            ]
        );
    }
    #[test]
    fn basic_string_set() {
        assert_eq!(
            tokenize(r#"{ string = "Hello \"World\""; }"#),
            tokens![
               (Token::CurlyBOpen, "{"),
               (Token::Whitespace, " "),
               (Token::Ident, "string"),
               (Token::Whitespace, " "),
               (Token::Assign, "="),
               (Token::Whitespace, " "),
               (Token::Value, r#""Hello \"World\"""#),
               (Token::Semicolon, ";"),
               (Token::Whitespace, " "),
               (Token::CurlyBClose, "}")
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
               (Token::CurlyBOpen, "{"),
               (Token::Whitespace, "\n    "),
               (Token::Ident, "multiline"),
               (Token::Whitespace, " "),
               (Token::Assign, "="),
               (Token::Whitespace, " "),
               (Token::Value, r#"''
            
                  
        This is a multiline string :D
          indented by two
        \'\'\'\'\
        ''${ interpolation was escaped }
        two single quotes: '''
        three single quotes: ''''
    ''"#),
                (Token::Semicolon, ";"),
                (Token::Whitespace, "\n"),
                (Token::CurlyBClose, "}")
            ]
        );
    }
    #[test]
    fn special_escape() {
        assert_eq!(
            tokenize(r#" "$${test}" "#),
            tokens![
                (Token::Whitespace, " "),
                (Token::Value, r#""$${test}""#),
                (Token::Whitespace, " ")
            ]
        );
        assert_eq!(
            tokenize(r#" ''$${test}'' "#),
            tokens![
                (Token::Whitespace, " "),
                (Token::Value, r#"''$${test}''"#),
                (Token::Whitespace, " ")
            ]
        );
    }
    #[test]
    fn interpolation() {
        assert_eq!(
            tokenize(r#" "Hello, ${ { world = "World"; }.world }!" "#),
            tokens![
                (Token::Whitespace, " "),
                (Token::InterpolStart, r#""Hello, ${"#),
                    (Token::Whitespace, " "),
                    (Token::CurlyBOpen, "{"),
                    (Token::Whitespace, " "),
                    (Token::Ident, "world"),
                    (Token::Whitespace, " "),
                    (Token::Assign, "="),
                    (Token::Whitespace, " "),
                    (Token::Value, r#""World""#),
                    (Token::Semicolon, ";"),
                    (Token::Whitespace, " "),
                    (Token::CurlyBClose, "}"),
                    (Token::Dot, "."),
                    (Token::Ident, "world"),
                    (Token::Whitespace, " "),
                (Token::InterpolEnd, r#"}!""#),
                (Token::Whitespace, " ")
            ]
        );
        assert_eq!(
            tokenize(r#" "\$${test}" "#),
            tokens![
                (Token::Whitespace, " "),
                (Token::InterpolStart, r#""\$${"#),
                    (Token::Ident, "test"),
                (Token::InterpolEnd, r#"}""#),
                (Token::Whitespace, " ")
            ]
        );
        assert_eq!(
            tokenize(r#" ''''$${test}'' "#),
            tokens![
                (Token::Whitespace, " "),
                (Token::InterpolStart, r#"''''$${"#),
                    (Token::Ident, "test"),
                (Token::InterpolEnd, r#"}''"#),
                (Token::Whitespace, " ")
            ]
        );
        assert_eq!(
            tokenize(r#" "${test}#123" "#),
            tokens![
                (Token::Whitespace, " "),
                (Token::InterpolStart, r#""${"#),
                    (Token::Ident, "test"),
                (Token::InterpolEnd, r#"}#123""#),
                (Token::Whitespace, " ")
            ]
        );
        assert_eq!(
            tokenize(" ''\n${test}'' "),
            tokens![
                (Token::Whitespace, " "),
                (Token::InterpolStart, "''\n${"),
                    (Token::Ident, "test"),
                (Token::InterpolEnd, r#"}''"#),
                (Token::Whitespace, " ")
            ]
        );
        assert_eq!(
            tokenize(r#" "${hello} ${world}" "#),
            tokens![
                (Token::Whitespace, " "),
                (Token::InterpolStart, r#""${"#),
                    (Token::Ident, "hello"),
                (Token::InterpolEndStart, r#"} ${"#),
                    (Token::Ident, "world"),
                (Token::InterpolEnd, r#"}""#),
                (Token::Whitespace, " ")
            ]
        );
        assert_eq!(
            tokenize(r#" ''${"${var}"}'' "#),
            tokens![
                (Token::Whitespace, " "),
                (Token::InterpolStart, r#"''${"#),
                    (Token::InterpolStart, r#""${"#),
                        (Token::Ident, "var"),
                    (Token::InterpolEnd, r#"}""#),
                (Token::InterpolEnd, r#"}''"#),
                (Token::Whitespace, " ")
            ]
        );
    }
    #[test]
    fn comments() {
        assert_eq!(
            tokenize("{ a = /* multiline * comment */ 123;# single line\n} # single line at the end"),
            tokens![
                (Token::CurlyBOpen, "{"),
                (Token::Whitespace, " "),
                (Token::Ident, "a"),
                (Token::Whitespace, " "),
                (Token::Assign, "="),
                (Token::Whitespace, " "),
                (Token::Comment, "/* multiline * comment */"),
                (Token::Whitespace, " "),
                (Token::Value, "123"),
                (Token::Semicolon, ";"),
                (Token::Comment, "# single line\n"),
                (Token::CurlyBClose, "}"),
                (Token::Whitespace, " "),
                (Token::Comment, "# single line at the end")
            ]
        );
    }
    #[test]
    fn math() {
        assert_eq!(
            tokenize("1 + 2 * 3"),
            tokens![
                (Token::Value, "1"),
                (Token::Whitespace, " "),
                (Token::Add, "+"),
                (Token::Whitespace, " "),
                (Token::Value, "2"),
                (Token::Whitespace, " "),
                (Token::Mul, "*"),
                (Token::Whitespace, " "),
                (Token::Value, "3")
            ]
        );
        assert_eq!(
            tokenize("5 * -(3 - 2)"),
            tokens![
                (Token::Value, "5"),
                (Token::Whitespace, " "),
                (Token::Mul, "*"),
                (Token::Whitespace, " "),
                (Token::Sub, "-"),
                (Token::ParenOpen, "("),
                (Token::Value, "3"),
                (Token::Whitespace, " "),
                (Token::Sub, "-"),
                (Token::Whitespace, " "),
                (Token::Value, "2"),
                (Token::ParenClose, ")")
            ]
        );
        assert_eq!(
            tokenize("a/ 3"), // <- could get mistaken for a path
            tokens![
                (Token::Ident, "a"),
                (Token::Div, "/"),
                (Token::Whitespace, " "),
                (Token::Value, "3")
            ]
        );
    }
    #[test]
    fn let_in() {
        assert_eq!(
            tokenize("let a = 3; in a"),
            tokens![
                (Token::Let, "let"),
                (Token::Whitespace, " "),
                (Token::Ident, "a"),
                (Token::Whitespace, " "),
                (Token::Assign, "="),
                (Token::Whitespace, " "),
                (Token::Value, "3"),
                (Token::Semicolon, ";"),
                (Token::Whitespace, " "),
                (Token::In, "in"),
                (Token::Whitespace, " "),
                (Token::Ident, "a")
            ]
        );
    }
    #[test]
    fn with() {
        assert_eq!(
            tokenize("with namespace; expr"),
            tokens![
                (Token::With, "with"),
                (Token::Whitespace, " "),
                (Token::Ident, "namespace"),
                (Token::Semicolon, ";"),
                (Token::Whitespace, " "),
                (Token::Ident, "expr")
            ]
        );
    }
    #[test]
    fn paths() {
        fn path(path: &str) -> Vec<(Token, SmolStr)> {
            tokens![(Token::Value, path)]
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
    fn import() {
        assert_eq!(
            tokenize("import <nixpkgs>"),
            tokens![
                (Token::Import, "import"),
                (Token::Whitespace, " "),
                (Token::Value, "<nixpkgs>")
            ]
        );
    }
    #[test]
    fn list() {
        assert_eq!(
            tokenize(r#"[a 2 3 "lol"]"#),
            tokens![
                (Token::SquareBOpen, "["),
                (Token::Ident, "a"),
                (Token::Whitespace, " "),
                (Token::Value, "2"),
                (Token::Whitespace, " "),
                (Token::Value, "3"),
                (Token::Whitespace, " "),
                (Token::Value, r#""lol""#),
                (Token::SquareBClose, "]")
            ]
        );
        assert_eq!(
            tokenize("[1] ++ [2] ++ [3]"),
            tokens![
                (Token::SquareBOpen, "["),
                (Token::Value, "1"),
                (Token::SquareBClose, "]"),
                (Token::Whitespace, " "),
                (Token::Concat, "++"),
                (Token::Whitespace, " "),
                (Token::SquareBOpen, "["),
                (Token::Value, "2"),
                (Token::SquareBClose, "]"),
                (Token::Whitespace, " "),
                (Token::Concat, "++"),
                (Token::Whitespace, " "),
                (Token::SquareBOpen, "["),
                (Token::Value, "3"),
                (Token::SquareBClose, "]")
            ]
        );
    }
    #[test]
    fn functions() {
        assert_eq!(
            tokenize("a: b: a + b"),
            tokens![
                (Token::Ident, "a"),
                (Token::Colon, ":"),
                (Token::Whitespace, " "),
                (Token::Ident, "b"),
                (Token::Colon, ":"),
                (Token::Whitespace, " "),
                (Token::Ident, "a"),
                (Token::Whitespace, " "),
                (Token::Add, "+"),
                (Token::Whitespace, " "),
                (Token::Ident, "b")
            ]
        );
    }
    #[test]
    fn patterns() {
        assert_eq!(
            tokenize(r#"{ a, b ? "default", ... } @ outer"#),
            tokens![
                (Token::CurlyBOpen, "{"),
                (Token::Whitespace, " "),
                (Token::Ident, "a"),
                (Token::Comma, ","),
                (Token::Whitespace, " "),
                (Token::Ident, "b"),
                (Token::Whitespace, " "),
                (Token::Question, "?"),
                (Token::Whitespace, " "),
                (Token::Value, r#""default""#),
                (Token::Comma, ","),
                (Token::Whitespace, " "),
                (Token::Ellipsis, "..."),
                (Token::Whitespace, " "),
                (Token::CurlyBClose, "}"),
                (Token::Whitespace, " "),
                (Token::At, "@"),
                (Token::Whitespace, " "),
                (Token::Ident, "outer")
            ]
        );
    }
    #[test]
    fn combine() {
        assert_eq!(
            tokenize("a//b"),
            tokens![
                (Token::Ident, "a"),
                (Token::Merge, "//"),
                (Token::Ident, "b")
            ]
        );
    }
    #[test]
    fn ifs() {
        assert_eq!(
            tokenize("false -> !false && false == true || true"),
            tokens![
                (Token::Value, "false"),
                (Token::Whitespace, " "),
                (Token::Implication, "->"),
                (Token::Whitespace, " "),
                (Token::Invert, "!"),
                (Token::Value, "false"),
                (Token::Whitespace, " "),
                (Token::And, "&&"),
                (Token::Whitespace, " "),
                (Token::Value, "false"),
                (Token::Whitespace, " "),
                (Token::Equal, "=="),
                (Token::Whitespace, " "),
                (Token::Value, "true"),
                (Token::Whitespace, " "),
                (Token::Or, "||"),
                (Token::Whitespace, " "),
                (Token::Value, "true")
            ]
        );
        assert_eq!(
            tokenize("1 < 2 && 2 <= 2 && 2 > 1 && 2 >= 2"),
            tokens![
                (Token::Value, "1"),
                (Token::Whitespace, " "),
                (Token::Less, "<"),
                (Token::Whitespace, " "),
                (Token::Value, "2"),

                (Token::Whitespace, " "),
                (Token::And, "&&"),
                (Token::Whitespace, " "),

                (Token::Value, "2"),
                (Token::Whitespace, " "),
                (Token::LessOrEq, "<="),
                (Token::Whitespace, " "),
                (Token::Value, "2"),

                (Token::Whitespace, " "),
                (Token::And, "&&"),
                (Token::Whitespace, " "),

                (Token::Value, "2"),
                (Token::Whitespace, " "),
                (Token::More, ">"),
                (Token::Whitespace, " "),
                (Token::Value, "1"),

                (Token::Whitespace, " "),
                (Token::And, "&&"),
                (Token::Whitespace, " "),

                (Token::Value, "2"),
                (Token::Whitespace, " "),
                (Token::MoreOrEq, ">="),
                (Token::Whitespace, " "),
                (Token::Value, "2")
            ]
        );
        assert_eq!(
            tokenize("1 == 1 && 2 != 3"),
            tokens![
                (Token::Value, "1"),
                (Token::Whitespace, " "),
                (Token::Equal, "=="),
                (Token::Whitespace, " "),
                (Token::Value, "1"),
                (Token::Whitespace, " "),
                (Token::And, "&&"),
                (Token::Whitespace, " "),
                (Token::Value, "2"),
                (Token::Whitespace, " "),
                (Token::NotEqual, "!="),
                (Token::Whitespace, " "),
                (Token::Value, "3")
            ]
        );
        assert_eq!(
            tokenize("if false then 1 else if true then 2 else 3"),
            tokens![
                (Token::If, "if"),
                (Token::Whitespace, " "),
                (Token::Value, "false"),
                (Token::Whitespace, " "),
                (Token::Then, "then"),
                    (Token::Whitespace, " "),
                    (Token::Value, "1"),
                (Token::Whitespace, " "),
                (Token::Else, "else"),
                    (Token::Whitespace, " "),
                    (Token::If, "if"),
                    (Token::Whitespace, " "),
                    (Token::Value, "true"),
                    (Token::Whitespace, " "),
                    (Token::Then, "then"),
                        (Token::Whitespace, " "),
                        (Token::Value, "2"),
                    (Token::Whitespace, " "),
                    (Token::Else, "else"),
                        (Token::Whitespace, " "),
                        (Token::Value, "3")
            ]
        );
        assert_eq!(
            tokenize("x>=y"), // <- could be confused with store path because of the '>'
            tokens![
                (Token::Ident, "x"),
                (Token::MoreOrEq, ">="),
                (Token::Ident, "y")
            ]
        );
    }
    #[test]
    fn dynamic_attrs() {
        assert_eq!(
            tokenize("a.${b}.c"),
            tokens![
                (Token::Ident, "a"),
                (Token::Dot, "."),
                (Token::DynamicStart, "${"),
                    (Token::Ident, "b"),
                (Token::DynamicEnd, "}"),
                (Token::Dot, "."),
                (Token::Ident, "c")
            ]
        );
        assert_eq!(
            tokenize(r#"a.${ { b = "${test}"; }.b }.c"#),
            tokens![
                (Token::Ident, "a"),
                (Token::Dot, "."),
                (Token::DynamicStart, "${"),
                    (Token::Whitespace, " "),
                    (Token::CurlyBOpen, "{"),
                    (Token::Whitespace, " "),
                    (Token::Ident, "b"),
                    (Token::Whitespace, " "),
                    (Token::Assign, "="),
                    (Token::Whitespace, " "),
                    (Token::InterpolStart, r#""${"#),
                        (Token::Ident, "test"),
                    (Token::InterpolEnd, r#"}""#),
                    (Token::Semicolon, ";"),
                    (Token::Whitespace, " "),
                    (Token::CurlyBClose, "}"),
                    (Token::Dot, "."),
                    (Token::Ident, "b"),
                    (Token::Whitespace, " "),
                (Token::DynamicEnd, "}"),
                (Token::Dot, "."),
                (Token::Ident, "c")
            ]
        );
    }
}
