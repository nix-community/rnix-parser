//! The types: Such as strings or integers

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
    Bool(bool),
    Float(f64),
    Integer(i64),
    Null,
    Path(Anchor, String),
    Str {
        multiline: bool,
        content: String
    }
}

impl From<bool> for Value {
    fn from(val: bool) -> Value {
        Value::Bool(val)
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
impl From<&str> for Value {
    fn from(val: &str) -> Value {
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
    #[fail(display = "parse int error: {}", _0)]
    ParseFloatError(#[cause] std::num::ParseFloatError),
    #[fail(display = "parse int error: {}", _0)]
    ParseIntError(#[cause] std::num::ParseIntError),
    #[fail(display = "unclosed quote in string")]
    UnclosedQuote,
    #[fail(display = "unclosed bracket in store path")]
    UnclosedStore,
    #[fail(display = "unknown value")]
    Unknown
}
impl From<std::num::ParseFloatError> for ValueError {
    fn from(err: std::num::ParseFloatError) -> Self {
        ValueError::ParseFloatError(err)
    }
}
impl From<std::num::ParseIntError> for ValueError {
    fn from(err: std::num::ParseIntError) -> Self {
        ValueError::ParseIntError(err)
    }
}

impl std::str::FromStr for Value {
    type Err = ValueError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "true"  => return Ok(Value::Bool(true)),
            "false" => return Ok(Value::Bool(false)),
            "null"  => return Ok(Value::Null),
            _ => ()
        }
        if s.starts_with('"') {
            let len = s.len();
            if len <= 2 || !s.ends_with('"') {
                return Err(ValueError::UnclosedQuote);
            }
            let content = unescape(&s[1..len-1], false);
            return Ok(Value::Str { multiline: false, content });
        }
        if s.starts_with("''") {
            let len = s.len();
            if len <= 4 || !s.ends_with("''") {
                return Err(ValueError::UnclosedQuote);
            }
            let content = unescape(&s[2..len-2], true);
            let mut content = remove_common_indent(&content);
            remove_trailing(&mut content);
            return Ok(Value::Str { multiline: true, content });
        }
        if s.starts_with('<') {
            let len = s.len();
            if len < 2 || !s.ends_with('>') {
                return Err(ValueError::UnclosedStore);
            }
            return Ok(Value::Path(Anchor::Store, String::from(&s[1..len-1])))
        }
        if s.starts_with("~/") { return Ok(Value::Path(Anchor::Home, String::from(&s[2..]))); }
        if s.starts_with('/') { return Ok(Value::Path(Anchor::Absolute, String::from(s))); }
        if s.contains(':') { return Ok(Value::Path(Anchor::Uri, String::from(s))) }
        if s.contains('/') { return Ok(Value::Path(Anchor::Relative, String::from(s))); }

        let mut num = true;
        let mut float = false;
        for c in s.chars() {
            if c == '.' {
                if float {
                    num = false;
                    break;
                }
                float = true;
            } else if !c.is_digit(10) {
                num = false;
                break;
            }
        }

        if num {
            if float {
                return Ok(Value::Float(s.parse()?));
            } else {
                return Ok(Value::Integer(s.parse()?));
            }
        }
        Err(ValueError::Unknown)
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
        assert_eq!("false".parse(), Ok(Value::Bool(false)));
        assert_eq!("true".parse(), Ok(Value::Bool(true)));
        assert_eq!("null".parse(), Ok(Value::Null));

        assert_eq!(r#""\"""#.parse(), Ok(Value::Str { multiline: false, content: "\"".into() }));
        assert_eq!("'''''''".parse(), Ok(Value::Str { multiline: true, content: "''".into() }));

        assert_eq!("<nixpkgs>".parse(), Ok(Value::Path(Anchor::Store, "nixpkgs".into())));
        assert_eq!("~/path/to/thing".parse(), Ok(Value::Path(Anchor::Home, "path/to/thing".into())));
        assert_eq!("/path/to/thing".parse(), Ok(Value::Path(Anchor::Absolute, "/path/to/thing".into())));
        assert_eq!("path/to/thing".parse(), Ok(Value::Path(Anchor::Relative, "path/to/thing".into())));
        assert_eq!("https:path".parse(), Ok(Value::Path(Anchor::Uri, "https:path".into())));

        assert_eq!("123".parse(), Ok(Value::Integer(123)));
        assert_eq!("1.234".parse(), Ok(Value::Float(1.234)));
    }
}
