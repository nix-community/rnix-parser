//! The types: Such as strings or integers
use std::fmt;

use crate::{
    types::{self, TypedNode},
    NodeOrToken,
    SyntaxKind::{self, *},
    SyntaxNode,
};

/// An anchor point for a path, such as if it's relative or absolute
#[derive(Clone, Debug, PartialEq)]
pub enum Anchor {
    Absolute,
    Relative,
    Home,
    Store,
}

/// A value, such as a string or integer
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Float(f64),
    Integer(i64),
    String(String),
    Path(Anchor, String),
}

impl From<i64> for Value {
    fn from(val: i64) -> Value {
        Value::Integer(val)
    }
}
impl From<f64> for Value {
    fn from(val: f64) -> Value {
        Value::Float(val)
    }
}

/// Interpret escape sequences in the nix string and return the converted value
pub fn unescape(input: &str, multiline: bool) -> String {
    let mut output = String::new();
    let mut input = input.chars().peekable();
    loop {
        match input.next() {
            None => break,
            Some('"') if multiline => break,
            Some('\\') if !multiline => match input.next() {
                None => break,
                Some('n') => output.push('\n'),
                Some('r') => output.push('\r'),
                Some('t') => output.push('\t'),
                Some(c) => output.push(c),
            },
            Some('\'') if multiline => match input.next() {
                None => break,
                Some('\'') => match input.peek() {
                    Some('\'') => {
                        input.next().unwrap();
                        output.push_str("''");
                    }
                    Some('$') => {
                        input.next().unwrap();
                        output.push('$');
                    }
                    Some('\\') => {
                        input.next().unwrap();
                        match input.next() {
                            None => break,
                            Some('n') => output.push('\n'),
                            Some('r') => output.push('\r'),
                            Some('t') => output.push('\t'),
                            Some(c) => output.push(c),
                        }
                    }
                    _ => break,
                },
                Some(c) => {
                    output.push('\'');
                    output.push(c);
                }
            },
            Some(c) => output.push(c),
        }
    }
    output
}

pub(crate) fn indention<'a>(s: &'a str) -> impl Iterator<Item = char> + 'a {
    s.chars().take_while(|&c| c != '\n' && c.is_whitespace())
}

/// Remove common indention in string
pub fn remove_common_indent(input: &str) -> String {
    let mut common = std::usize::MAX;
    for line in input.lines() {
        let indent = indention(line).count();
        if line.chars().count() == indent {
            // line is empty, ignore indention
            continue;
        }
        common = common.min(indent);
    }

    remove_indent(input, true, common)
}
/// Remove a specified max value of indention from each line in a string after
/// a specified starting point
pub fn remove_indent(input: &str, initial: bool, indent: usize) -> String {
    let mut output = String::new();
    let mut start = 0;
    if initial {
        // If the first line is whitespace, ignore it completely
        let iter = input.chars().take_while(|&c| c != '\n');
        if iter.clone().all(char::is_whitespace) {
            start += iter.map(char::len_utf8).sum::<usize>() + /* newline */ 1;
            if start >= input.len() {
                // There's nothing after this whitespace line
                return output;
            }
        } else {
            // Otherwise, skip like normal
            start += indention(input).take(indent).map(char::len_utf8).sum::<usize>();
        }
    }
    loop {
        start += indention(&input[start..]).take(indent).map(char::len_utf8).sum::<usize>();
        let end = input[start..].find('\n').map(|i| start + i + 1);
        {
            let end = end.unwrap_or(input.len());
            output.push_str(&input[start..end]);
        }
        start = match end {
            Some(end) => end,
            None => break,
        };
    }
    output
}
/// Remove any trailing whitespace from a string
pub fn remove_trailing(string: &mut String) {
    let trailing: usize = string
        .chars()
        .rev()
        .take_while(|&c| c != '\n' && c.is_whitespace())
        .map(char::len_utf8)
        .sum();
    let len = string.len();
    string.drain(len - trailing..);
}

/// An error that occured when parsing a value from a string
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValueError {
    Float(std::num::ParseFloatError),
    Integer(std::num::ParseIntError),
    StorePath,
    Unknown,
}
impl From<std::num::ParseFloatError> for ValueError {
    fn from(err: std::num::ParseFloatError) -> Self {
        ValueError::Float(err)
    }
}
impl From<std::num::ParseIntError> for ValueError {
    fn from(err: std::num::ParseIntError) -> Self {
        ValueError::Integer(err)
    }
}

impl fmt::Display for ValueError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueError::Float(err) => write!(f, "failed to parse float: {}", err),
            ValueError::Integer(err) => write!(f, "failed to parse int: {}", err),
            ValueError::StorePath => write!(f, "failed to parse store path"),
            ValueError::Unknown => write!(f, "unknown value kind"),
        }
    }
}

impl std::error::Error for ValueError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ValueError::Float(err) => Some(err),
            ValueError::Integer(err) => Some(err),
            ValueError::StorePath | ValueError::Unknown => None,
        }
    }
}

impl Value {
    /// Parse a token kind and string into a typed value
    pub fn from_token(token: SyntaxKind, s: &str) -> Result<Self, ValueError> {
        let value = match token {
            TOKEN_FLOAT => Value::Float(s.parse()?),
            TOKEN_INTEGER => Value::Integer(s.parse()?),
            TOKEN_PATH => {
                if s.starts_with('<') {
                    let len = s.len();
                    if len < 2 || !s.ends_with('>') {
                        return Err(ValueError::StorePath);
                    }
                    Value::Path(Anchor::Store, String::from(&s[1..len - 1]))
                } else if s.starts_with("~/") {
                    Value::Path(Anchor::Home, String::from(&s[2..]))
                } else if s.starts_with('/') {
                    Value::Path(Anchor::Absolute, String::from(s))
                } else {
                    Value::Path(Anchor::Relative, String::from(s))
                }
            }
            TOKEN_URI => Value::String(String::from(s)),
            _ => return Err(ValueError::Unknown),
        };
        Ok(value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StrPart {
    Literal(String),
    Ast(SyntaxNode),
}
pub(crate) fn string_parts(string: &types::Str) -> Vec<StrPart> {
    let mut parts = Vec::new();
    let mut literals = 0;
    let mut common = std::usize::MAX;
    let multiline = string.first_token().map_or(false, |t| t.text().as_str() == "''");
    let mut last_was_ast = false;

    for child in string.node().children_with_tokens() {
        match &child {
            NodeOrToken::Token(token) if token.kind() == TOKEN_STRING_CONTENT => {
                let text: &str = token.text();

                let line_count = text.lines().count();
                let next_is_ast = child
                    .next_sibling_or_token()
                    .map_or(false, |child| child.kind() == NODE_STRING_INTERPOL);
                for (i, line) in text.lines().enumerate().skip(if last_was_ast { 1 } else { 0 }) {
                    let indent: usize = indention(line).count();
                    if (i != line_count - 1 || !next_is_ast) && indent == line.chars().count() {
                        // line is empty and not the start of an
                        // interpolation, ignore indention
                        continue;
                    }
                    common = common.min(indent);
                }
                parts.push(StrPart::Literal(text.to_string()));
                literals += 1;
            }
            NodeOrToken::Token(token) => {
                assert!(token.kind() == TOKEN_STRING_START || token.kind() == TOKEN_STRING_END)
            }
            NodeOrToken::Node(node) => {
                assert_eq!(node.kind(), NODE_STRING_INTERPOL);
                parts.push(StrPart::Ast(node.clone()));
                last_was_ast = true;
            }
        }
    }

    let mut i = 0;
    for part in parts.iter_mut() {
        if let StrPart::Literal(ref mut text) = part {
            if multiline {
                *text = remove_indent(text, i == 0, common);
                if i == literals - 1 {
                    // Last index
                    remove_trailing(text);
                }
            }
            *text = unescape(text, multiline);
            i += 1;
        }
    }

    parts
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_unescapes() {
        assert_eq!(unescape(r#"Hello\n\"World\" :D"#, false), "Hello\n\"World\" :D");
        assert_eq!(unescape(r#"Hello''\n'''World''' :D"#, true), "Hello\n''World'' :D");
    }
    #[test]
    fn string_remove_common_indent() {
        assert_eq!(remove_common_indent("\n  \n    \n \n "), "\n\n\n");
        assert_eq!(remove_common_indent("\n  \n    \n a\n"), " \n   \na\n");
        assert_eq!(remove_common_indent("  \n    \n a\n"), "   \na\n");
    }
    #[test]
    fn parts() {
        use crate::{types::Str, NixLanguage, SmolStr, SyntaxNode};
        use rowan::{GreenNodeBuilder, Language};

        fn string_node(content: &str) -> Str {
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(NixLanguage::kind_to_raw(NODE_STRING));
            builder.token(NixLanguage::kind_to_raw(TOKEN_STRING_START), SmolStr::new("''"));
            builder.token(NixLanguage::kind_to_raw(TOKEN_STRING_CONTENT), SmolStr::new(content));
            builder.token(NixLanguage::kind_to_raw(TOKEN_STRING_END), SmolStr::new("''"));
            builder.finish_node();

            Str::cast(SyntaxNode::new_root(builder.finish())).unwrap()
        }

        assert_eq!(
            string_parts(&string_node(
                r#"
                        |trailing-whitespace
                              |trailing-whitespace
                    This is a multiline string :D
                      indented by two
                    \'\'\'\'\
                    ''${ interpolation was escaped }
                    two single quotes: '''
                    three single quotes: ''''
                "#.replace("|trailing-whitespace", "").as_str()
            )),
            vec![
                StrPart::Literal(String::from(
                    // Get the below with nix repl
                    "    \n          \nThis is a multiline string :D\n  indented by two\n\\'\\'\\'\\'\\\n${ interpolation was escaped }\ntwo single quotes: ''\nthree single quotes: '''\n"
                ))
            ]
        );
    }
    #[test]
    fn values() {
        assert_eq!(
            Value::from_token(TOKEN_PATH, "<nixpkgs>"),
            Ok(Value::Path(Anchor::Store, "nixpkgs".into()))
        );
        assert_eq!(
            Value::from_token(TOKEN_PATH, "~/path/to/thing"),
            Ok(Value::Path(Anchor::Home, "path/to/thing".into()))
        );
        assert_eq!(
            Value::from_token(TOKEN_PATH, "/path/to/thing"),
            Ok(Value::Path(Anchor::Absolute, "/path/to/thing".into()))
        );
        assert_eq!(
            Value::from_token(TOKEN_PATH, "path/to/thing"),
            Ok(Value::Path(Anchor::Relative, "path/to/thing".into()))
        );
        assert_eq!(
            Value::from_token(TOKEN_URI, "https:path"),
            Ok(Value::String("https:path".into()))
        );

        assert_eq!(Value::from_token(TOKEN_INTEGER, "123"), Ok(Value::Integer(123)));
        assert_eq!(Value::from_token(TOKEN_FLOAT, "1.234"), Ok(Value::Float(1.234)));
    }
}
