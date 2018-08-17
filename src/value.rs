//! The types: Such as strings or integers

use std::fmt;

#[cfg(feature = "smol_str")]
use smol_str::SmolStr;
#[cfg(not(feature = "smol_str"))]
type SmolStr = String;

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
    Float(String),
    Integer(String),
    Null,
    Path(Anchor, String),
    Str {
        multiline: bool,
        original: SmolStr,
        content: SmolStr
    }
}

impl From<bool> for Value {
    fn from(val: bool) -> Value {
        Value::Bool(val)
    }
}
impl From<i64> for Value {
    fn from(val: i64) -> Value {
        Value::Integer(val.to_string())
    }
}
impl From<f64> for Value {
    fn from(val: f64) -> Value {
        Value::Float(val.to_string())
    }
}
impl From<String> for Value {
    fn from(val: String) -> Value {
        let val = SmolStr::from(val);
        Value::Str {
            multiline: false,
            original: val.clone(),
            content: val
        }
    }
}
impl From<&str> for Value {
    fn from(val: &str) -> Value {
        Value::from(String::from(val))
    }
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Bool(val) => write!(f, "{}", val),
            Value::Float(val) => write!(f, "{}", val),
            Value::Integer(val) => write!(f, "{}", val),
            Value::Null => write!(f, "null"),
            Value::Path(Anchor::Absolute, path) => write!(f, "{}", path),
            Value::Path(Anchor::Relative, path) => write!(f, "{}", path),
            Value::Path(Anchor::Home, path) => write!(f, "~/{}", path),
            Value::Path(Anchor::Store, path) => write!(f, "<{}>", path),
            Value::Path(Anchor::Uri, path) => write!(f, "{}", path),
            Value::Str { multiline, original, content: _ } => if *multiline {
                write!(f, "''{}''", original)
            } else {
                write!(f, "\"{}\"", original)
            }
        }
    }
}
