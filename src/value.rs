#[derive(Clone, Debug, PartialEq)]
pub enum Anchor {
    Absolute,
    Relative,
    Home,
    Store,
    Uri
}

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
