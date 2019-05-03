//! The types: Such as strings or integers

use crate::tokenizer::tokens::*;
use rowan::SyntaxKind;

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
    Path(Anchor, String),
    Str {
        multiline: bool,
        content: String
    }
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
impl From<String> for Value {
    fn from(val: String) -> Value {
        Value::Str {
            multiline: false,
            content: val
        }
    }
}
impl<'a> From<&'a str> for Value {
    fn from(val: &'a str) -> Value {
        Value::from(String::from(val))
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
    #[fail(display = "failed to parse string")]
    String,
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
            (TOKEN_STRING, s) => if s.starts_with('"') {
                let len = s.len();
                if len < 2 || !s.ends_with('"') {
                    return Err(ValueError::String);
                }
                let content = unescape(&s[1..len-1], false);
                Ok(Value::Str { multiline: false, content })
            } else if s.starts_with("''") {
                let len = s.len();
                if len < 4 || !s.ends_with("''") {
                    return Err(ValueError::String);
                }
                let content = unescape(&s[2..len-2], true);
                let mut content = remove_common_indent(&content);
                remove_trailing(&mut content);
                Ok(Value::Str { multiline: true, content })
            } else {
                Err(ValueError::String)
            },
            _ => Err(ValueError::Unknown)
        }
    }
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
    fn string_both() {
        assert_eq!(
            unescape(&remove_common_indent(
                r#"
                        
                              
                    This is a multiline string :D
                      indented by two
                    \'\'\'\'\
                    ''${ interpolation was escaped }
                    two single quotes: '''
                    three single quotes: ''''
                "#
            ), true),
            // Get the below with nix repl
            "    \n          \nThis is a multiline string :D\n  indented by two\n\\'\\'\\'\\'\\\n${ interpolation was escaped }\ntwo single quotes: ''\nthree single quotes: '''\n"
        );
    }
    #[test]
    fn values() {
        assert_eq!(Value::from_token(TOKEN_STRING, r#""""#), Ok(Value::Str { multiline: false, content: "".into() }));
        assert_eq!(Value::from_token(TOKEN_STRING, r#"''''"#), Ok(Value::Str { multiline: true, content: "".into() }));
        assert_eq!(Value::from_token(TOKEN_STRING, r#""\"""#), Ok(Value::Str { multiline: false, content: "\"".into() }));
        assert_eq!(Value::from_token(TOKEN_STRING, "'''''''"), Ok(Value::Str { multiline: true, content: "''".into() }));

        assert_eq!(Value::from_token(TOKEN_PATH, "<nixpkgs>"), Ok(Value::Path(Anchor::Store, "nixpkgs".into())));
        assert_eq!(Value::from_token(TOKEN_PATH, "~/path/to/thing"), Ok(Value::Path(Anchor::Home, "path/to/thing".into())));
        assert_eq!(Value::from_token(TOKEN_PATH, "/path/to/thing"), Ok(Value::Path(Anchor::Absolute, "/path/to/thing".into())));
        assert_eq!(Value::from_token(TOKEN_PATH, "path/to/thing"), Ok(Value::Path(Anchor::Relative, "path/to/thing".into())));
        assert_eq!(Value::from_token(TOKEN_PATH, "https:path"), Ok(Value::Path(Anchor::Uri, "https:path".into())));

        assert_eq!(Value::from_token(TOKEN_INTEGER, "123"), Ok(Value::Integer(123)));
        assert_eq!(Value::from_token(TOKEN_FLOAT, "1.234"), Ok(Value::Float(1.234)));
    }
}
