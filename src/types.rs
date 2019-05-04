//! Provides a type system for the AST, in some sense

use crate::{
    parser::nodes::*,
    value::{self, Value as ParsedValue, ValueError}
};

use rowan::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, TransparentNewType, TreeArc, WalkEvent};
use std::fmt;

macro_rules! typed {
    ($($kind:expr => $name:ident$(: $trait:ident)*$(: { $($block:tt)* })*),*) => {
        $(
            #[repr(transparent)]
            pub struct $name(SyntaxNode);

            unsafe impl TransparentNewType for $name {
                type Repr = SyntaxNode;
            }
            impl ToOwned for $name {
                type Owned = TreeArc<Self>;
                fn to_owned(&self) -> Self::Owned {
                    TreeArc::cast(self.node().to_owned())
                }
            }
            impl TypedNode for $name {
                fn cast(from: &SyntaxNode) -> Option<&Self> {
                    if from.kind() == $kind.into() {
                        Some(Self::from_repr(from.into_repr()))
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
            .expect("invalid ast")
    };
    ($self:expr; ($kind:ident) $index:expr) => {
        $kind::cast(nth!($self; $index)).expect("invalid ast")
    };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OpKind {
    Concat,
    IsSet,
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
    Or
}
impl OpKind {
    /// Get the operation kind from a token in the AST
    pub fn from_token(token: SyntaxKind) -> Option<Self> {
        match token {
            TOKEN_CONCAT => Some(OpKind::Concat),
            TOKEN_QUESTION => Some(OpKind::IsSet),
            TOKEN_MERGE => Some(OpKind::Merge),

            TOKEN_ADD => Some(OpKind::Add),
            TOKEN_SUB => Some(OpKind::Sub),
            TOKEN_MUL => Some(OpKind::Mul),
            TOKEN_DIV => Some(OpKind::Div),

            TOKEN_AND => Some(OpKind::And),
            TOKEN_EQUAL => Some(OpKind::Equal),
            TOKEN_IMPLICATION => Some(OpKind::Implication),
            TOKEN_LESS => Some(OpKind::Less),
            TOKEN_LESS_OR_EQ => Some(OpKind::LessOrEq),
            TOKEN_MORE => Some(OpKind::More),
            TOKEN_MORE_OR_EQ => Some(OpKind::MoreOrEq),
            TOKEN_NOT_EQUAL => Some(OpKind::NotEqual),
            TOKEN_OR => Some(OpKind::Or),

            _ => None
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOpKind {
    Invert,
    Negate
}
impl UnaryOpKind {
    /// Get the operation kind from a token in the AST
    pub fn from_token(token: SyntaxKind) -> Option<Self> {
        match token {
            TOKEN_INVERT => Some(UnaryOpKind::Invert),
            TOKEN_SUB => Some(UnaryOpKind::Negate),
            _ => None
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InterpolPart<'a> {
    Literal(String),
    Ast(&'a SyntaxNode)
}

/// A struct that prints out the textual representation of a node in a
/// stable format. See TypedNode::dump.
pub struct TextDump<'a>(&'a SyntaxNode);

impl fmt::Display for TextDump<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut indent = 0;
        let mut skip_newline = true;
        for event in self.0.preorder_with_tokens() {
            if skip_newline {
                skip_newline = false;
            } else {
                writeln!(f)?;
            }
            match event {
                WalkEvent::Enter(enter) => {
                    write!(f, "{:i$}{}", "", syntax_name(enter.kind()).expect("invalid ast"), i=indent)?;
                    if let SyntaxElement::Token(token) = enter {
                        write!(f, "(\"{}\")", token.text().escape_default())?
                    }
                    write!(f, " {}..{}", enter.range().start(), enter.range().end())?;
                    if let SyntaxElement::Node(_) = enter {
                        write!(f, " {{")?;
                    }
                    indent += 2;
                },
                WalkEvent::Leave(leave) => {
                    indent -= 2;
                    if let SyntaxElement::Node(_) = leave {
                        write!(f, "{:i$}}}", "", i=indent)?;
                    } else {
                        skip_newline = true;
                    }
                }
            }
        }
        Ok(())
    }
}

/// A TypedNode is simply a wrapper around an untyped node to provide a type
/// system in some sense.
pub trait TypedNode: TransparentNewType<Repr = SyntaxNode> + ToOwned + Sized {
    /// Cast an untyped node into this strongly-typed node. This will return
    /// None if the type was not correct.
    fn cast(from: &SyntaxNode) -> Option<&Self>;
    /// Return a reference to the inner untyped node
    fn node(&self) -> &SyntaxNode;
    /// Return all errors of all children, recursively
    fn errors(&self) -> Vec<SyntaxElement> {
        let mut bucket = Vec::new();
        for event in self.node().preorder_with_tokens() {
            if let WalkEvent::Enter(node) = event {
                // Empty errors can happen if it encounteres EOF while
                // creating them, which in case a root error is added.
                if !node.range().is_empty() && (node.kind() == NODE_ERROR || node.kind() == TOKEN_ERROR) {
                    bucket.push(node);
                }
            }
        }
        bucket
    }
    /// Return the first non-trivia token
    fn first_token(&self) -> Option<SyntaxToken> {
        let mut cursor = self.node().first_child_or_token()?;
        loop {
            if let SyntaxElement::Token(token) = cursor {
                if !token_helpers::is_trivia(token.kind()) {
                    break Some(token);
                }
            }
            cursor = cursor.next_sibling_or_token()?;
        }
    }
    /// Return a dump of the AST. One of the goals is to be a stable
    /// format that can be used in tests.
    fn dump(&self) -> TextDump {
        TextDump(self.node())
    }
}
/// Provides the function `.entries()`
pub trait EntryHolder: TypedNode {
    /// Return an iterator over all key=value entries
    fn entries<'a>(&'a self) -> Box<Iterator<Item = &'a SetEntry> + 'a> {
        Box::new(self.node().children().filter_map(SetEntry::cast))
    }
    /// Return an iterator over all inherit entries
    fn inherits<'a>(&'a self) -> Box<Iterator<Item = &'a Inherit> + 'a> {
        Box::new(self.node().children().filter_map(Inherit::cast))
    }
}
/// Provides the function `.inner()` for wrapping types like parenthensis
pub trait Wrapper: TypedNode {
    /// Return the inner value
    fn inner(&self) -> &SyntaxNode {
        nth!(self; 0)
    }
}

typed! [
    NODE_IDENT => Ident: {
        /// Return the identifier as a string
        pub fn as_str(&self) -> &str {
            self.first_token().expect("invalid ast").text()
        }
    },
    NODE_VALUE => Value: {
        /// Return the value as a string
        pub fn as_str(&self) -> &str {
            self.first_token().expect("invalid ast").text()
        }

        /// Parse the value
        pub fn to_value(&self) -> Result<ParsedValue, ValueError> {
            ParsedValue::from_token(self.first_token().expect("invalid ast").kind(), self.as_str())
        }
    },

    NODE_APPLY => Apply: {
        /// Return the lambda being applied
        pub fn lambda(&self) -> &SyntaxNode {
            nth!(self; 0)
        }
        /// Return the value which the lambda is being applied with
        pub fn value(&self) -> &SyntaxNode {
            nth!(self; 1)
        }
    },
    NODE_ASSERT => Assert: {
        /// Return the assert condition
        pub fn condition(&self) -> &SyntaxNode {
            nth!(self; 0)
        }
        /// Return the success body
        pub fn body(&self) -> &SyntaxNode {
            nth!(self; 1)
        }
    },
    NODE_ATTRIBUTE => Attribute: {
        /// Return the path as an iterator of identifiers
        pub fn path<'a>(&'a self) -> impl Iterator<Item = &SyntaxNode> + 'a {
            self.node().children()
        }
    },
    NODE_DYNAMIC => Dynamic: Wrapper,
    NODE_ERROR => Error,
    NODE_IF_ELSE => IfElse: {
        /// Return the condition
        pub fn condition(&self) -> &SyntaxNode {
            nth!(self; 0)
        }
        /// Return the success body
        pub fn body(&self) -> &SyntaxNode {
            nth!(self; 1)
        }
        /// Return the else body
        pub fn else_body(&self) -> &SyntaxNode {
            nth!(self; 2)
        }
    },
    NODE_INDEX_SET => IndexSet: {
        /// Return the set being indexed
        pub fn set(&self) -> &SyntaxNode {
            nth!(self; 0)
        }
        /// Return the index
        pub fn index(&self) -> &SyntaxNode {
            nth!(self; 1)
        }
    },
    NODE_INHERIT => Inherit: {
        /// Return the set where keys are being inherited from, if any
        pub fn from(&self) -> Option<&InheritFrom> {
            self.node().children()
                .filter_map(InheritFrom::cast)
                .next()
        }
        /// Return all the identifiers being inherited
        pub fn idents(&self) -> impl Iterator<Item = &Ident> {
            self.node().children().filter_map(Ident::cast)
        }
    },
    NODE_INHERIT_FROM => InheritFrom: Wrapper,
    NODE_INTERPOL => Interpol: {
        /// Parse the interpolation into a series of parts
        pub fn parts(&self) -> Vec<InterpolPart> {
            let mut parts = Vec::new();
            let mut literals = 0;
            let mut common = std::usize::MAX;
            let mut multiline = false;

            for child in self.node().children() {
                if let Some(literal) = InterpolLiteral::cast(child) {
                    let token = literal.first_token().unwrap();
                    let mut text: &str = token.text();

                    let start = token.kind() == TOKEN_INTERPOL_START
                        || token.kind() == TOKEN_INTERPOL_END_START;
                    let end = token.kind() == TOKEN_INTERPOL_END
                        || token.kind() == TOKEN_INTERPOL_END_START;

                    match token.kind() {
                        TOKEN_INTERPOL_START => {
                            multiline = if text.starts_with('"') {
                                text = &text[1..];
                                false
                            } else if text.starts_with("''") {
                                text = &text[2..];
                                true
                            } else { false };
                        },
                        TOKEN_INTERPOL_END_START => (),
                        TOKEN_INTERPOL_END => {
                            let len = text.len();
                            if text.ends_with('"') {
                                text = &text[..len-1];
                            } else if text.ends_with("''") {
                                text = &text[..len-2];
                            }
                        },
                        _ => continue
                    }
                    if end && text.starts_with("}") {
                        text = &text[1..];
                    }
                    if start && text.ends_with("${") {
                        let len = text.len();
                        text = &text[..len-2];
                    }
                    let line_count = text.lines().count();
                    for (i, line) in text.lines().enumerate().skip(if end { 1 } else { 0 }) {
                        let indent: usize = value::indention(line).count();
                        if (i != line_count-1 || !start) && indent == line.chars().count() {
                            // line is empty and not the start of an
                            // interpolation, ignore indention
                            continue;
                        }
                        common = common.min(indent);
                    }
                    parts.push(InterpolPart::Literal(text.to_string()));
                    literals += 1;
                } else {
                    parts.push(InterpolPart::Ast(child));
                }
            }

            let mut i = 0;
            for part in parts.iter_mut() {
                if let InterpolPart::Literal(ref mut text) = part {
                    if multiline {
                        *text = value::remove_indent(text, i == 0, common);
                        if i == literals-1 {
                            // Last index
                            value::remove_trailing(text);
                        }
                    }
                    *text = value::unescape(text, multiline);
                    i += 1;
                }
            }

            parts
        }
    },
    NODE_INTERPOL_LITERAL => InterpolLiteral: Wrapper,
    NODE_LAMBDA => Lambda: {
        /// Return the argument of the lambda
        pub fn arg(&self) -> &SyntaxNode {
            nth!(self; 0)
        }
        /// Return the body of the lambda
        pub fn body(&self) -> &SyntaxNode {
            nth!(self; 1)
        }
    },
    NODE_LET => Let: EntryHolder,
    NODE_LET_IN => LetIn: EntryHolder: {
        /// Return the body
        pub fn body(&self) -> &SyntaxNode {
            self.node().last_child().expect("invalid ast")
        }
    },
    NODE_LIST => List: {
        /// Return an iterator over items in the list
        pub fn items(&self) -> impl Iterator<Item = &SyntaxNode> {
            self.node().children()
        }
    },
    NODE_OPERATION => Operation: {
        /// Return the first value in the operation
        pub fn value1(&self) -> &SyntaxNode {
            nth!(self; 0)
        }
        /// Return the operator
        pub fn operator(&self) -> OpKind {
            self.first_token().and_then(|t| OpKind::from_token(t.kind())).expect("invalid ast")
        }
        /// Return the second value in the operation
        pub fn value2(&self) -> &SyntaxNode {
            nth!(self; 1)
        }
    },
    NODE_OR_DEFAULT => OrDefault: {
        /// Return the indexing operation
        pub fn index(&self) -> &IndexSet {
            nth!(self; (IndexSet) 0)
        }
        /// Return the default value
        pub fn default(&self) -> &SyntaxNode {
            nth!(self; 1)
        }
    },
    NODE_PAREN => Paren: Wrapper,
    NODE_PAT_BIND => PatBind: {
        /// Return the identifier the set is being bound as
        pub fn name(&self) -> &Ident {
            nth!(self; (Ident) 0)
        }
    },
    NODE_PAT_ENTRY => PatEntry: {
        /// Return the identifier the argument is being bound as
        pub fn name(&self) -> &Ident {
            nth!(self; (Ident) 0)
        }
        /// Return the default value, if any
        pub fn default(&self) -> Option<&SyntaxNode> {
            self.node().children().nth(1)
        }
    },
    NODE_PATTERN => Pattern: {
        /// Return an iterator over all pattern entries
        pub fn entries(&self) -> impl Iterator<Item = &PatEntry> {
            self.node().children().filter_map(PatEntry::cast)
        }
        /// Returns true if this pattern is inexact (has an ellipsis, ...)
        pub fn ellipsis(&self) -> bool {
            self.node().children_with_tokens().any(|node| node.kind() == TOKEN_ELLIPSIS)
        }
        /// Returns a clone of the tree root but without entries where the
        /// callback function returns false
        /// { a, b, c } without b is { a, c }
        pub fn filter_entries<F>(&self, _callback: F) -> TreeArc<Root>
            where F: FnMut(&PatEntry) -> bool
        {
            unimplemented!("TODO: filter_entries or better editing API")
        }
    },
    NODE_ROOT => Root: Wrapper,
    NODE_SET => Set: EntryHolder: {
        /// Returns true if this set is recursive
        pub fn recursive(&self) -> bool {
            self.node().children_with_tokens().any(|node| node.kind() == TOKEN_REC)
        }
        /// Returns a clone of the tree root but without entries where the
        /// callback function returns false
        /// { a = 2; b = 3; } without 0 is { b = 3; }
        pub fn filter_entries<F>(&self, _callback: F) -> TreeArc<Root>
            where F: FnMut(&SetEntry) -> bool
        {
            unimplemented!("TODO: filter_entries or better editing API")
        }
    },
    NODE_SET_ENTRY => SetEntry: {
        /// Return this entry's key
        pub fn key(&self) -> &Attribute {
            nth!(self; (Attribute) 0)
        }
        /// Return this entry's value
        pub fn value(&self) -> &SyntaxNode {
            nth!(self; 1)
        }
    },
    NODE_UNARY => Unary: {
        /// Return the operator
        pub fn operator(&self) -> UnaryOpKind {
            self.first_token().and_then(|t| UnaryOpKind::from_token(t.kind())).expect("invalid ast")
        }
        /// Return the value in the operation
        pub fn value(&self) -> &SyntaxNode {
            nth!(self; 0)
        }
    },
    NODE_WITH => With: {
        /// Return the namespace
        pub fn namespace(&self) -> &SyntaxNode {
            nth!(self; 0)
        }
        /// Return the body
        pub fn body(&self) -> &SyntaxNode {
            nth!(self; 1)
        }
    }
];
