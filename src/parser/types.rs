//! Provides a type system for the AST, in some sense

use crate::tokenizer::Token;
use super::{ASTKind, Node, NodeType, Types};

use rowan::SmolStr;
use std::marker::PhantomData;

macro_rules! typed {
    ($($kind:expr => $name:ident$(: $trait:ident)*$(: { $($block:tt)* })*),*) => {
        $(
            pub struct $name<'a, R: rowan::TreeRoot<Types>> {
                node: Node<R>,
                _marker: PhantomData<&'a R>
            }

            impl<'a, R: rowan::TreeRoot<Types>> TypedNode<R> for $name<'a, R> {
                type Target = Self;
                fn cast(from: Node<R>) -> Option<Self::Target> {
                    cast_into(from, $kind.into()).map(|node| Self {
                        node,
                        _marker: PhantomData::default()
                    })
                }
                fn node(&self) -> &Node<R> {
                    &self.node
                }
            }
            $(impl<'a, R: rowan::TreeRoot<Types>> $trait<'a, R> for $name<'a, R> {})*
            $(impl<'a, R: rowan::TreeRoot<Types>> $name<'a, R> { $($block)* })*
        )*
    }
}
macro_rules! nth {
    ($self:expr; $index:expr) => {
        $self.node().children()
            .filter(|node| match node.kind() {
                NodeType::Marker(_) => true,
                NodeType::Token(t) => !t.is_trivia()
            })
            .nth($index)
            .expect("invalid ast")
    };
    ($self:expr; ($kind:ident) $index:expr) => {
        $kind::cast(nth!($self; $index)).expect("invalid ast")
    }
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
    pub fn from_token(token: Token) -> Option<Self> {
        match token {
            Token::Concat => Some(OpKind::Concat),
            Token::Question => Some(OpKind::IsSet),
            Token::Merge => Some(OpKind::Merge),

            Token::Add => Some(OpKind::Add),
            Token::Sub => Some(OpKind::Sub),
            Token::Mul => Some(OpKind::Mul),
            Token::Div => Some(OpKind::Div),

            Token::And => Some(OpKind::And),
            Token::Equal => Some(OpKind::Equal),
            Token::Implication => Some(OpKind::Implication),
            Token::Less => Some(OpKind::Less),
            Token::LessOrEq => Some(OpKind::LessOrEq),
            Token::More => Some(OpKind::More),
            Token::MoreOrEq => Some(OpKind::MoreOrEq),
            Token::NotEqual => Some(OpKind::NotEqual),
            Token::Or => Some(OpKind::Or),

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
    pub fn from_token(token: Token) -> Option<Self> {
        match token {
            Token::Invert => Some(UnaryOpKind::Invert),
            Token::Sub => Some(UnaryOpKind::Negate),
            _ => None
        }
    }
}

// TODO: More functions for types

/// A TypedNode is simply a wrapper around an untyped node to provide a type
/// system in some sense.
pub trait TypedNode<R: rowan::TreeRoot<Types>> {
    type Target;
    /// Cast an untyped node into this strongly-typed node. This will return
    /// None if the type was not correct.
    fn cast(from: Node<R>) -> Option<Self::Target>;
    /// Return a reference to the inner untyped node
    fn node(&self) -> &Node<R>;
}
/// Provides the function `.entries()`
pub trait HasEntries<'a, R: rowan::TreeRoot<Types>>: TypedNode<R> {
    /// Return an iterator over all key=value entries
    fn entries(&'a self) -> Box<Iterator<Item = SetEntry<R>> + 'a> {
        Box::new(self.node().children().filter_map(SetEntry::cast))
    }
    /// Return an iterator over all inherit entries
    fn inherits(&'a self) -> Box<Iterator<Item = Inherit<R>> + 'a> {
        Box::new(self.node().children().filter_map(Inherit::cast))
    }
}
/// Provides the function `.inner()`
pub trait HasInner<'a, R: rowan::TreeRoot<Types>>: TypedNode<R> {
    /// Return the inner value
    fn inner(&self) -> Node<R> {
        nth!(self; 1)
    }
}

fn cast_into<R: rowan::TreeRoot<Types>>(from: Node<R>, kind: NodeType) -> Option<Node<R>> {
    if from.kind() == kind {
        Some(from)
    } else {
        None
    }
}

typed! [
    Token::Ident => Ident: {
        /// Return the identifier as string
        pub fn string(&self) -> &str {
            self.node().borrowed().leaf_text().map(SmolStr::as_str).expect("invalid ast")
        }
    },
    Token::Value => Value,

    ASTKind::Apply => Apply: {
        /// Return the lambda being applied
        pub fn lambda(&self) -> Node<R> {
            nth!(self; 0)
        }
        /// Return the value which the lambda is being applied with
        pub fn val(&self) -> Node<R> {
            nth!(self; 1)
        }
    },
    ASTKind::Assert => Assert: {
        /// Return the assert condition
        pub fn condition(&self) -> Node<R> {
            nth!(self; 1)
        }
        /// Return the success body
        pub fn body(&self) -> Node<R> {
            nth!(self; 3)
        }
    },
    ASTKind::Attribute => Attribute: {
        /// Return the path as an iterator of identifiers
        pub fn path(&self) -> impl Iterator<Item = Node<R>> {
            self.node().children()
                .filter(|node| match node.kind() {
                    NodeType::Marker(_) => true,
                    NodeType::Token(t) => (!t.is_trivia() && t != Token::Dot)
                })
        }
    },
    ASTKind::Dynamic => Dynamic: HasInner,
    ASTKind::Error => Error,
    ASTKind::IfElse => IfElse: {
        /// Return the condition
        pub fn condition(&self) -> Node<R> {
            nth!(self; 1)
        }
        /// Return the success body
        pub fn body(&self) -> Node<R> {
            nth!(self; 3)
        }
        /// Return the else body
        pub fn else_body(&self) -> Node<R> {
            nth!(self; 5)
        }
    },
    ASTKind::Import => Import: {
        /// Return the value being imported
        pub fn val(&self) -> Node<R> {
            nth!(self; 1)
        }
    },
    ASTKind::IndexSet => IndexSet: {
        /// Return the set being indexed
        pub fn set(&self) -> Node<R> {
            nth!(self; 0)
        }
        /// Return the index
        pub fn index(&self) -> Node<R> {
            nth!(self; 2)
        }
    },
    ASTKind::Inherit => Inherit: {
        /// Return the set where keys are being inherited from, if any
        pub fn from(&self) -> Option<InheritFrom<R>> {
            self.node().children()
                .filter_map(InheritFrom::cast)
                .next()
        }
    },
    ASTKind::InheritFrom => InheritFrom: HasInner,
    ASTKind::Interpol => Interpol,
    ASTKind::InterpolAst => InterpolAst,
    ASTKind::InterpolLiteral => InterpolLiteral,
    ASTKind::Lambda => Lambda: {
        /// Return the argument of the lambda
        pub fn arg(&self) -> Node<R> {
            nth!(self; 0)
        }
        /// Return the body of the lambda
        pub fn body(&self) -> Node<R> {
            nth!(self; 2)
        }
    },
    ASTKind::Let => Let: HasEntries,
    ASTKind::LetIn => LetIn: HasEntries,
    ASTKind::List => List: {
        /// Return an iterator over items in the list
        pub fn items(&self) -> impl Iterator<Item = ListItem<R>> {
            self.node().children().filter_map(ListItem::cast)
        }
    },
    ASTKind::ListItem => ListItem: {
        /// Return the inner value
        pub fn inner(&self) -> Node<R> {
            nth!(self; 0)
        }
    },
    ASTKind::Operation => Operation: {
        /// Return the first value in the operation
        pub fn val1(&self) -> Node<R> {
            nth!(self; 0)
        }
        /// Return the operator
        pub fn op(&self) -> OpKind {
            match nth!(self; 1).kind() {
                NodeType::Token(token) => OpKind::from_token(token).expect("invalid ast"),
                _ => panic!("invalid ast")
            }
        }
        /// Return the second value in the operation
        pub fn val2(&self) -> Node<R> {
            nth!(self; 2)
        }
    },
    ASTKind::OrDefault => OrDefault: {
        /// Return the indexing operation
        pub fn index(&self) -> IndexSet<R> {
            nth!(self; (IndexSet) 0)
        }
        /// Return the default value
        pub fn default(&self) -> Node<R> {
            nth!(self; 2)
        }
    },
    ASTKind::Paren => Paren: HasInner,
    ASTKind::PatBind => PatBind: {
        /// Return the identifier the set is being bound as
        pub fn name(&self) -> Ident<R> {
            self.node().children().filter_map(Ident::cast).next().expect("invalid ast")
        }
    },
    ASTKind::PatEntry => PatEntry: {
        /// Return the identifier the argument is being bound as
        pub fn name(&self) -> Ident<R> {
            nth!(self; (Ident) 0)
        }
        /// Return the default value, if any
        pub fn default(&self) -> Option<Node<R>> {
            self.node().children()
                .filter(|node| match node.kind() {
                    NodeType::Marker(_) => true,
                    NodeType::Token(token) => !token.is_trivia()
                })
                .nth(2)
        }
    },
    ASTKind::Pattern => Pattern: {
        /// Return an iterator over all pattern entries
        pub fn entries(&self) -> impl Iterator<Item = PatEntry<R>> {
            self.node().children().filter_map(PatEntry::cast)
        }
        /// Returns true if this pattern is inexact (has an ellipsis, ...)
        pub fn ellipsis(&self) -> bool {
            self.node().children().any(|node| node.kind() == NodeType::Token(Token::Ellipsis))
        }
    },
    ASTKind::Set => Set: HasEntries: {
        /// Returns true if this set is recursive
        pub fn recursive(&self) -> bool {
            self.node().children().any(|node| node.kind() == NodeType::Token(Token::Rec))
        }
    },
    ASTKind::SetEntry => SetEntry: {
        /// Return this entry's key
        pub fn key(&self) -> Attribute<R> {
            nth!(self; (Attribute) 0)
        }
        /// Return this entry's value
        pub fn val(&self) -> Node<R> {
            nth!(self; 2)
        }
    },
    ASTKind::Unary => Unary: {
        /// Return the operator
        pub fn op(&self) -> UnaryOpKind {
            match nth!(self; 0).kind() {
                NodeType::Token(token) => UnaryOpKind::from_token(token).expect("invalid ast"),
                _ => panic!("invalid ast")
            }
        }
        /// Return the value in the operation
        pub fn val(&self) -> Node<R> {
            nth!(self; 1)
        }
    },
    ASTKind::With => With: {
        /// Return the namespace
        pub fn namespace(&self) -> Node<R> {
            nth!(self; 1)
        }
        /// Return the body
        pub fn body(&self) -> Node<R> {
            nth!(self; 3)
        }
    }
];
