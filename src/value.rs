//! The types: Such as strings or integers

use crate::{
    parser::nodes::*,
    types::{self, TypedNode}
};
use rowan::{SyntaxNode, SyntaxKind};
use failure::Fail;

/// An anchor point for a path, such as if it's relative or absolute
#[derive(Clone, Debug, PartialEq)]
pub enum Anchor {
    Absolute,
    Relative,
    Home,
    Store,
    Uri
}

/// A value, such as a string or integer
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Float(f64),
    Integer(i64),
    Path(Anchor, String)
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
                Some(c) => output.push(c)
            },
            Some('\'') if multiline => match input.next() {
                None => break,
                Some('\'') => match input.peek() {
                    Some('\'') => {
                        input.next().unwrap();
                        output.push_str("''");
                    },
                    Some('$') => {
                        input.next().unwrap();
                        output.push('$');
                    },
                    Some('\\') => {
                        input.next().unwrap();
                        match input.next() {
                            None => break,
                            Some('n') => output.push('\n'),
                            Some('r') => output.push('\r'),
                            Some('t') => output.push('\t'),
                            Some(c) => output.push(c)
                        }
                    },
                    _ => break
                },
                Some(c) => {
                    output.push('\'');
                    output.push(c);
                }
            }
            Some(c) => output.push(c)
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
            None => break
        };
    }
    output
}
/// Remove any trailing whitespace from a string
pub fn remove_trailing(string: &mut String) {
    let trailing: usize = string.chars().rev()
        .take_while(|&c| c != '\n' && c.is_whitespace())
        .map(char::len_utf8)
        .sum();
    let len = string.len();
    string.drain(len-trailing..);
}

/// An error that occured when parsing a value from a string
#[derive(Clone, Debug, Fail, PartialEq, Eq)]
pub enum ValueError {
    #[fail(display = "failed to parse float: {}", _0)]
    Float(#[cause] std::num::ParseFloatError),
    #[fail(display = "failed to parse int: {}", _0)]
    Integer(#[cause] std::num::ParseIntError),
    #[fail(display = "failed to parse store path")]
    StorePath,
    #[fail(display = "unknown value kind")]
    Unknown
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

impl Value {
    /// Parse a token kind and string into a typed value
    pub fn from_token(token: SyntaxKind, s: &str) -> Result<Self, ValueError> {
        match (token, s) {
            (TOKEN_FLOAT, s) => Ok(Value::Float(s.parse()?)),
            (TOKEN_INTEGER, s) => Ok(Value::Integer(s.parse()?)),
            (TOKEN_PATH, s) => if s.starts_with('<') {
                let len = s.len();
                if len < 2 || !s.ends_with('>') {
                    return Err(ValueError::StorePath);
                }
                Ok(Value::Path(Anchor::Store, String::from(&s[1..len-1])))
            } else if s.starts_with("~/") {
                Ok(Value::Path(Anchor::Home, String::from(&s[2..])))
            } else if s.starts_with('/') {
                Ok(Value::Path(Anchor::Absolute, String::from(s)))
            } else if s.contains(':') {
                Ok(Value::Path(Anchor::Uri, String::from(s)))
            } else {
                Ok(Value::Path(Anchor::Relative, String::from(s)))
            },
            _ => Err(ValueError::Unknown)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StrPart<'a> {
    Literal(String),
    Ast(&'a SyntaxNode)
}
pub(crate) fn string_parts(string: &types::Str) -> Vec<StrPart> {
    let mut parts = Vec::new();
    let mut literals = 0;
    let mut common = std::usize::MAX;
    let multiline = string.first_token().map(|t| t.text().as_str()) == Some("''");
    let mut last_was_ast = false;

    for child in string.node().children() {
        let next_is_ast = child.next_sibling().map(|child| child.kind() == NODE_STRING_INTERPOL).unwrap_or(false);
        if child.kind() == NODE_STRING_LITERAL {
            let token = types::tokens(child).next().unwrap();
            let text: &str = token.text();

            let line_count = text.lines().count();
            for (i, line) in text.lines().enumerate().skip(if last_was_ast { 1 } else { 0 }) {
                let indent: usize = indention(line).count();
                if (i != line_count-1 || !next_is_ast) && indent == line.chars().count() {
                    // line is empty and not the start of an
                    // interpolation, ignore indention
                    continue;
                }
                common = common.min(indent);
            }
            parts.push(StrPart::Literal(text.to_string()));
            literals += 1;
        } else {
            parts.push(StrPart::Ast(child));
            last_was_ast = true;
        }
    }

    let mut i = 0;
    for part in parts.iter_mut() {
        if let StrPart::Literal(ref mut text) = part {
            if multiline {
                *text = remove_indent(text, i == 0, common);
                if i == literals-1 {
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
        assert_eq!(
            remove_common_indent("\n  \n    \n \n "),
            "\n\n\n"
        );
        assert_eq!(
            remove_common_indent("\n  \n    \n a\n"),
            " \n   \na\n"
        );
        assert_eq!(
            remove_common_indent("  \n    \n a\n"),
            "   \na\n"
        );
    }
    #[test]
    fn parts() {
        use rowan::{GreenNodeBuilder, SmolStr, SyntaxNode, TreeArc};
        use crate::types::Str;

        fn string_node(content: &str) -> TreeArc<Str> {
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(NODE_STRING);
            builder.token(TOKEN_STRING_START, SmolStr::new("''"));
            builder.start_node(NODE_STRING_LITERAL);
            builder.token(TOKEN_STRING_CONTENT, SmolStr::new(content));
            builder.finish_node();
            builder.token(TOKEN_STRING_END, SmolStr::new("''"));
            builder.finish_node();

            TreeArc::cast(SyntaxNode::new(builder.finish(), None))
        }

        assert_eq!(
            string_parts(&string_node(
                r#"
                        
                              
                    This is a multiline string :D
                      indented by two
                    \'\'\'\'\
                    ''${ interpolation was escaped }
                    two single quotes: '''
                    three single quotes: ''''
                "#
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
        assert_eq!(Value::from_token(TOKEN_PATH, "<nixpkgs>"), Ok(Value::Path(Anchor::Store, "nixpkgs".into())));
        assert_eq!(Value::from_token(TOKEN_PATH, "~/path/to/thing"), Ok(Value::Path(Anchor::Home, "path/to/thing".into())));
        assert_eq!(Value::from_token(TOKEN_PATH, "/path/to/thing"), Ok(Value::Path(Anchor::Absolute, "/path/to/thing".into())));
        assert_eq!(Value::from_token(TOKEN_PATH, "path/to/thing"), Ok(Value::Path(Anchor::Relative, "path/to/thing".into())));
        assert_eq!(Value::from_token(TOKEN_PATH, "https:path"), Ok(Value::Path(Anchor::Uri, "https:path".into())));

        assert_eq!(Value::from_token(TOKEN_INTEGER, "123"), Ok(Value::Integer(123)));
        assert_eq!(Value::from_token(TOKEN_FLOAT, "1.234"), Ok(Value::Float(1.234)));
    }
}
