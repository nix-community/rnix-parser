//! Provides a type system for the AST, in some sense
use std::fmt;

use crate::{
    value::{self, StrPart, Value as ParsedValue, ValueError},
    NodeOrToken, SyntaxElement,
    SyntaxKind::{self, *},
    SyntaxNode, SyntaxToken, WalkEvent,
};

macro_rules! typed {
    ($($kind:expr => $name:ident$(: $trait:ident)*$(: { $($block:tt)* })*),*) => {
        $(
            #[derive(Clone)]
            pub struct $name(SyntaxNode);

            impl TypedNode for $name {
                fn cast(from: SyntaxNode) -> Option<Self> {
                    if from.kind() == $kind {
                        Some(Self(from))
                    } else {
                        None
                    }
                }
                fn node(&self) -> &SyntaxNode {
                    &self.0
                }
            }
            $(impl $trait for $name {})*
            $(impl $name { $($block)* })*
        )*
    }
}
macro_rules! nth {
    ($self:expr; $index:expr) => {
        $self.node().children()
            .nth($index)
    };
    ($self:expr; ($kind:ident) $index:expr) => {
        nth!($self; $index).and_then($kind::cast)
    };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Concat,
    IsSet,
    Update,

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
}
impl BinOpKind {
    /// Get the operation kind from a token in the AST
    pub fn from_token(token: SyntaxKind) -> Option<Self> {
        match token {
            TOKEN_CONCAT => Some(BinOpKind::Concat),
            TOKEN_QUESTION => Some(BinOpKind::IsSet),
            TOKEN_UPDATE => Some(BinOpKind::Update),

            TOKEN_ADD => Some(BinOpKind::Add),
            TOKEN_SUB => Some(BinOpKind::Sub),
            TOKEN_MUL => Some(BinOpKind::Mul),
            TOKEN_DIV => Some(BinOpKind::Div),

            TOKEN_AND => Some(BinOpKind::And),
            TOKEN_EQUAL => Some(BinOpKind::Equal),
            TOKEN_IMPLICATION => Some(BinOpKind::Implication),
            TOKEN_LESS => Some(BinOpKind::Less),
            TOKEN_LESS_OR_EQ => Some(BinOpKind::LessOrEq),
            TOKEN_MORE => Some(BinOpKind::More),
            TOKEN_MORE_OR_EQ => Some(BinOpKind::MoreOrEq),
            TOKEN_NOT_EQUAL => Some(BinOpKind::NotEqual),
            TOKEN_OR => Some(BinOpKind::Or),

            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOpKind {
    Invert,
    Negate,
}
impl UnaryOpKind {
    /// Get the operation kind from a token in the AST
    pub fn from_token(token: SyntaxKind) -> Option<Self> {
        match token {
            TOKEN_INVERT => Some(UnaryOpKind::Invert),
            TOKEN_SUB => Some(UnaryOpKind::Negate),
            _ => None,
        }
    }
}

/// A struct that prints out the textual representation of a node in a
/// stable format. See TypedNode::dump.
pub struct TextDump(SyntaxNode);

impl fmt::Display for TextDump {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut indent = 0;
        let mut skip_newline = true;
        for event in self.0.preorder_with_tokens() {
            if skip_newline {
                skip_newline = false;
            } else {
                writeln!(f)?;
            }
            match &event {
                WalkEvent::Enter(enter) => {
                    write!(f, "{:i$}{:?}", "", enter.kind(), i = indent)?;
                    if let NodeOrToken::Token(token) = enter {
                        write!(f, "(\"{}\")", token.text().escape_default())?
                    }
                    write!(f, " {}..{}", enter.text_range().start(), enter.text_range().end())?;
                    if let NodeOrToken::Node(_) = enter {
                        write!(f, " {{")?;
                    }
                    indent += 2;
                }
                WalkEvent::Leave(leave) => {
                    indent -= 2;
                    if let NodeOrToken::Node(_) = leave {
                        write!(f, "{:i$}}}", "", i = indent)?;
                    } else {
                        skip_newline = true;
                    }
                }
            }
        }
        Ok(())
    }
}

/// Internal function to get an iterator over non-trivia tokens
pub(crate) fn tokens(node: &SyntaxNode) -> impl Iterator<Item = SyntaxToken> {
    node.children_with_tokens()
        .filter_map(|element| element.into_token())
        .filter(|token| !token.kind().is_trivia())
}

/// A TypedNode is simply a wrapper around an untyped node to provide a type
/// system in some sense.
pub trait TypedNode: Clone {
    /// Cast an untyped node into this strongly-typed node. This will return
    /// None if the type was not correct.
    fn cast(from: SyntaxNode) -> Option<Self>;
    /// Return a reference to the inner untyped node
    fn node(&self) -> &SyntaxNode;
    /// Return all errors of all children, recursively
    fn errors(&self) -> Vec<SyntaxElement> {
        self.node()
            .descendants_with_tokens()
            // Empty errors can happen if it encounteres EOF while
            // creating them, which in case a root error is added.
            .filter(|node| !node.text_range().is_empty())
            .filter(|node| node.kind() == NODE_ERROR || node.kind() == TOKEN_ERROR)
            .collect()
    }
    /// Return the first non-trivia token
    fn first_token(&self) -> Option<SyntaxToken> {
        tokens(self.node()).next()
    }
    /// Return a dump of the AST. One of the goals is to be a stable
    /// format that can be used in tests.
    fn dump(&self) -> TextDump {
        TextDump(self.node().clone())
    }
}

pub trait TokenWrapper: TypedNode {
    fn as_str(&self) -> &str {
        match &self.node().green().children().next() {
            Some(rowan::NodeOrToken::Token(token)) => token.text().as_str(),
            _ => unreachable!(),
        }
    }
}

/// Provides the function `.entries()`
pub trait EntryHolder: TypedNode {
    /// Return an iterator over all key=value entries
    fn entries(&self) -> Box<dyn Iterator<Item = KeyValue>> {
        Box::new(self.node().children().filter_map(KeyValue::cast))
    }
    /// Return an iterator over all inherit entries
    fn inherits(&self) -> Box<dyn Iterator<Item = Inherit>> {
        Box::new(self.node().children().filter_map(Inherit::cast))
    }
}
/// Provides the function `.inner()` for wrapping types like parenthensis
pub trait Wrapper: TypedNode {
    /// Return the inner value
    fn inner(&self) -> Option<SyntaxNode> {
        nth!(self; 0)
    }
}

pub struct ParsedTypeError(pub SyntaxKind);

pub enum ParsedType {
    Apply(Apply),
    Assert(Assert),
    Key(Key),
    Dynamic(Dynamic),
    Error(Error),
    Ident(Ident),
    IfElse(IfElse),
    Select(Select),
    Inherit(Inherit),
    InheritFrom(InheritFrom),
    Lambda(Lambda),
    LegacyLet(LegacyLet),
    LetIn(LetIn),
    List(List),
    BinOp(BinOp),
    OrDefault(OrDefault),
    Paren(Paren),
    PatBind(PatBind),
    PatEntry(PatEntry),
    Pattern(Pattern),
    Root(Root),
    AttrSet(AttrSet),
    KeyValue(KeyValue),
    Str(Str),
    UnaryOp(UnaryOp),
    Value(Value),
    With(With),
}

impl core::convert::TryFrom<SyntaxNode> for ParsedType {
    type Error = ParsedTypeError;

    fn try_from(node: SyntaxNode) -> Result<Self, ParsedTypeError> {
        match node.kind() {
            NODE_APPLY => Ok(ParsedType::Apply(Apply::cast(node).unwrap())),
            NODE_ASSERT => Ok(ParsedType::Assert(Assert::cast(node).unwrap())),
            NODE_KEY => Ok(ParsedType::Key(Key::cast(node).unwrap())),
            NODE_DYNAMIC => Ok(ParsedType::Dynamic(Dynamic::cast(node).unwrap())),
            NODE_ERROR => Ok(ParsedType::Error(Error::cast(node).unwrap())),
            NODE_IDENT => Ok(ParsedType::Ident(Ident::cast(node).unwrap())),
            NODE_IF_ELSE => Ok(ParsedType::IfElse(IfElse::cast(node).unwrap())),
            NODE_SELECT => Ok(ParsedType::Select(Select::cast(node).unwrap())),
            NODE_INHERIT => Ok(ParsedType::Inherit(Inherit::cast(node).unwrap())),
            NODE_INHERIT_FROM => Ok(ParsedType::InheritFrom(InheritFrom::cast(node).unwrap())),
            NODE_STRING => Ok(ParsedType::Str(Str::cast(node).unwrap())),
            NODE_LAMBDA => Ok(ParsedType::Lambda(Lambda::cast(node).unwrap())),
            NODE_LEGACY_LET => Ok(ParsedType::LegacyLet(LegacyLet::cast(node).unwrap())),
            NODE_LET_IN => Ok(ParsedType::LetIn(LetIn::cast(node).unwrap())),
            NODE_LIST => Ok(ParsedType::List(List::cast(node).unwrap())),
            NODE_BIN_OP => Ok(ParsedType::BinOp(BinOp::cast(node).unwrap())),
            NODE_OR_DEFAULT => Ok(ParsedType::OrDefault(OrDefault::cast(node).unwrap())),
            NODE_PAREN => Ok(ParsedType::Paren(Paren::cast(node).unwrap())),
            NODE_PATTERN => Ok(ParsedType::Pattern(Pattern::cast(node).unwrap())),
            NODE_PAT_BIND => Ok(ParsedType::PatBind(PatBind::cast(node).unwrap())),
            NODE_PAT_ENTRY => Ok(ParsedType::PatEntry(PatEntry::cast(node).unwrap())),
            NODE_ROOT => Ok(ParsedType::Root(Root::cast(node).unwrap())),
            NODE_ATTR_SET => Ok(ParsedType::AttrSet(AttrSet::cast(node).unwrap())),
            NODE_KEY_VALUE => Ok(ParsedType::KeyValue(KeyValue::cast(node).unwrap())),
            NODE_UNARY_OP => Ok(ParsedType::UnaryOp(UnaryOp::cast(node).unwrap())),
            NODE_LITERAL => Ok(ParsedType::Value(Value::cast(node).unwrap())),
            NODE_WITH => Ok(ParsedType::With(With::cast(node).unwrap())),
            other => Err(ParsedTypeError(other)),
        }
    }
}

typed! [
    NODE_IDENT => Ident: TokenWrapper: {
    },
    NODE_LITERAL => Value: TokenWrapper: {
        /// Parse the value
        pub fn to_value(&self) -> Result<ParsedValue, ValueError> {
            ParsedValue::from_token(self.first_token().expect("invalid ast").kind(), self.as_str())
        }
    },

    NODE_APPLY => Apply: {
        /// Return the lambda being applied
        pub fn lambda(&self) -> Option<SyntaxNode> {
            nth!(self; 0)
        }
        /// Return the value which the lambda is being applied with
        pub fn value(&self) -> Option<SyntaxNode> {
            nth!(self; 1)
        }
    },
    NODE_ASSERT => Assert: {
        /// Return the assert condition
        pub fn condition(&self) -> Option<SyntaxNode> {
            nth!(self; 0)
        }
        /// Return the success body
        pub fn body(&self) -> Option<SyntaxNode> {
            nth!(self; 1)
        }
    },
    NODE_KEY => Key: {
        /// Return the path as an iterator of identifiers
        pub fn path<'a>(&'a self) -> impl Iterator<Item = SyntaxNode> + 'a {
            self.node().children()
        }
    },
    NODE_DYNAMIC => Dynamic: Wrapper,
    NODE_ERROR => Error,
    NODE_IF_ELSE => IfElse: {
        /// Return the condition
        pub fn condition(&self) -> Option<SyntaxNode> {
            nth!(self; 0)
        }
        /// Return the success body
        pub fn body(&self) -> Option<SyntaxNode> {
            nth!(self; 1)
        }
        /// Return the else body
        pub fn else_body(&self) -> Option<SyntaxNode> {
            nth!(self; 2)
        }
    },
    NODE_SELECT => Select: {
        /// Return the set being indexed
        pub fn set(&self) -> Option<SyntaxNode> {
            nth!(self; 0)
        }
        /// Return the index
        pub fn index(&self) -> Option<SyntaxNode> {
            nth!(self; 1)
        }
    },
    NODE_INHERIT => Inherit: {
        /// Return the set where keys are being inherited from, if any
        pub fn from(&self) -> Option<InheritFrom> {
            self.node().children()
                .find_map(InheritFrom::cast)
        }
        /// Return all the identifiers being inherited
        pub fn idents(&self) -> impl Iterator<Item = Ident> {
            self.node().children().filter_map(Ident::cast)
        }
    },
    NODE_INHERIT_FROM => InheritFrom: Wrapper,
    NODE_STRING => Str: {
        /// Parse the interpolation into a series of parts
        pub fn parts(&self) -> Vec<StrPart> {
            value::string_parts(self)
        }
    },
    NODE_LAMBDA => Lambda: {
        /// Return the argument of the lambda
        pub fn arg(&self) -> Option<SyntaxNode> {
            nth!(self; 0)
        }
        /// Return the body of the lambda
        pub fn body(&self) -> Option<SyntaxNode> {
            nth!(self; 1)
        }
    },
    NODE_LEGACY_LET => LegacyLet: EntryHolder,
    NODE_LET_IN => LetIn: EntryHolder: {
        /// Return the body
        pub fn body(&self) -> Option<SyntaxNode> {
            self.node().last_child()
        }
    },
    NODE_LIST => List: {
        /// Return an iterator over items in the list
        pub fn items(&self) -> impl Iterator<Item = SyntaxNode> {
            self.node().children()
        }
    },
    NODE_BIN_OP => BinOp: {
        /// Return the left hand side of the binary operation
        pub fn lhs(&self) -> Option<SyntaxNode> {
            nth!(self; 0)
        }
        /// Return the operator
        pub fn operator(&self) -> BinOpKind {
            self.first_token().and_then(|t| BinOpKind::from_token(t.kind())).expect("invalid ast")
        }
        /// Return the right hand side of the binary operation
        pub fn rhs(&self) -> Option<SyntaxNode> {
            nth!(self; 1)
        }
    },
    NODE_OR_DEFAULT => OrDefault: {
        /// Return the indexing operation
        pub fn index(&self) -> Option<Select> {
            nth!(self; (Select) 0)
        }
        /// Return the default value
        pub fn default(&self) -> Option<SyntaxNode> {
            nth!(self; 1)
        }
    },
    NODE_PAREN => Paren: Wrapper,
    NODE_PAT_BIND => PatBind: {
        /// Return the identifier the set is being bound as
        pub fn name(&self) -> Option<Ident> {
            nth!(self; (Ident) 0)
        }
    },
    NODE_PAT_ENTRY => PatEntry: {
        /// Return the identifier the argument is being bound as
        pub fn name(&self) -> Option<Ident> {
            nth!(self; (Ident) 0)
        }
        /// Return the default value, if any
        pub fn default(&self) -> Option<SyntaxNode> {
            self.node().children().nth(1)
        }
    },
    NODE_PATTERN => Pattern: {
        /// Return an iterator over all pattern entries
        pub fn entries(&self) -> impl Iterator<Item = PatEntry> {
            self.node().children().filter_map(PatEntry::cast)
        }
        /// Returns true if this pattern is inexact (has an ellipsis, ...)
        pub fn ellipsis(&self) -> bool {
            self.node().children_with_tokens().any(|node| node.kind() == TOKEN_ELLIPSIS)
        }
        /// Returns a clone of the tree root but without entries where the
        /// callback function returns false
        /// { a, b, c } without b is { a, c }
        pub fn filter_entries<F>(&self, _callback: F) -> Root
            where F: FnMut(&PatEntry) -> bool
        {
            unimplemented!("TODO: filter_entries or better editing API")
        }
    },
    NODE_ROOT => Root: Wrapper,
    NODE_ATTR_SET => AttrSet: EntryHolder: {
        /// Returns true if this set is recursive
        pub fn recursive(&self) -> bool {
            self.node().children_with_tokens().any(|node| node.kind() == TOKEN_REC)
        }
        /// Returns a clone of the tree root but without entries where the
        /// callback function returns false
        /// { a = 2; b = 3; } without 0 is { b = 3; }
        pub fn filter_entries<F>(&self, _callback: F) -> Root
            where F: FnMut(&KeyValue) -> bool
        {
            unimplemented!("TODO: filter_entries or better editing API")
        }
    },
    NODE_KEY_VALUE => KeyValue: {
        /// Return this entry's key
        pub fn key(&self) -> Option<Key> {
            nth!(self; (Key) 0)
        }
        /// Return this entry's value
        pub fn value(&self) -> Option<SyntaxNode> {
            nth!(self; 1)
        }
    },
    NODE_UNARY_OP => UnaryOp: {
        /// Return the operator
        pub fn operator(&self) -> UnaryOpKind {
            self.first_token().and_then(|t| UnaryOpKind::from_token(t.kind())).expect("invalid ast")
        }
        /// Return the value in the operation
        pub fn value(&self) -> Option<SyntaxNode> {
            nth!(self; 0)
        }
    },
    NODE_WITH => With: {
        /// Return the namespace
        pub fn namespace(&self) -> Option<SyntaxNode> {
            nth!(self; 0)
        }
        /// Return the body
        pub fn body(&self) -> Option<SyntaxNode> {
            nth!(self; 1)
        }
    }
];
