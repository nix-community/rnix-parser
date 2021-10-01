//! Provides a type system for the AST, in some sense

mod operators;
mod node_ext;
mod token_ext;

use std::marker::PhantomData;

use crate::{
    SyntaxElement,
    SyntaxKind::{self, *},
    SyntaxNode, SyntaxNodeChildren, SyntaxToken,
};

#[derive(Debug, Clone)]
pub struct AstNodeChildren<N> {
    inner: SyntaxNodeChildren,
    _p: PhantomData<N>,
}

impl<N> AstNodeChildren<N> {
    fn new(parent: &SyntaxNode) -> Self {
        AstNodeChildren { inner: parent.children(), _p: PhantomData }
    }
}

impl<N: AstNode> Iterator for AstNodeChildren<N> {
    type Item = N;

    fn next(&mut self) -> Option<N> {
        self.inner.find_map(N::cast)
    }
}

fn first<N: AstNode, NN: AstNode>(parent: &N) -> Option<NN> {
    parent.node().children().find_map(NN::cast)
}

fn nth<N: AstNode, NN: AstNode>(parent: &N, n: usize) -> Option<NN> {
    parent.node().children().flat_map(NN::cast).nth(n)
}

fn children<N: AstNode, NN: AstNode>(parent: &N) -> AstNodeChildren<NN> {
    AstNodeChildren::new(parent.node())
}

fn token<N: AstNode>(parent: &N, kind: SyntaxKind) -> Option<SyntaxToken> {
    parent
        .node()
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .find(|it| it.kind() == kind)
}

/// A TypedNode is simply a wrapper around an untyped node to provide a type
/// system in some sense.
pub trait AstNode {
    fn can_cast(from: &SyntaxNode) -> bool
    where
        Self: Sized;

    /// Cast an untyped node into this strongly-typed node. This will return
    /// None if the type was not correct.
    fn cast(from: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

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

    // fn dump(&self) -> TextDump {
    //     TextDump(self.node().clone())
    // }
}

pub trait TypedToken {
    fn can_cast(from: SyntaxKind) -> bool
    where
        Self: Sized;

    /// Cast an untyped token into this strongly-typed token. This will return
    /// None if the type was not correct.
    fn cast(from: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn token(&self) -> &SyntaxToken;
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

macro_rules! typed {
    (@node $kind:ident => $name:ident: $($tt:tt)*) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name(SyntaxNode);

        impl AstNode for $name {
            fn can_cast(from: &SyntaxNode) -> bool {
                from.kind() == $kind
            }

            fn cast(from: SyntaxNode) -> Option<Self> {
                if Self::can_cast(&from) {
                    Some(Self(from))
                } else {
                    None
                }
            }
            fn node(&self) -> &SyntaxNode {
                &self.0
            }
        }

        // $(impl $trait for $name {})*

        // typed! { @impl name: $name $( $block:tt )* }

        // $(impl $name { $( $block )* })*

        typed!{ @impl $name $($tt)* }
    };
    (@node { $($kind:ident => $typed:ident),* $(,)? } => $name:ident: $($tt:tt)*) => {
        pub enum $name {
            $(
                $typed($typed),
            )*
        }

        impl AstNode for $name {
            fn can_cast(from: &SyntaxNode) -> bool {
                matches!(from.kind(), $($kind)|*)
            }

            fn cast(syntax: SyntaxNode) -> Option<Self> {
                let res = match syntax.kind() {
                    $(
                        $kind => Expr::$typed($typed(syntax))
                    ),*,
                    _ => return None,
                };
                Some(res)
            }

            fn node(&self) -> &SyntaxNode {
                match self {
                    $(
                        Expr::$typed(it) => &it.0,
                    )*
                }
            }
        }

        typed! { @impl $name $($tt)* }
    };

    (@token $kind:ident => $name:ident: $($tt:tt)*) => {
        pub struct $name(SyntaxToken);

        impl TypedToken for $name {
            fn can_cast(kind: SyntaxKind) -> bool {
                kind == $kind
            }

            fn cast(syntax: SyntaxToken) -> Option<Self> {
                if Self::can_cast(syntax.kind()) {
                    Some(Self(syntax))
                } else {
                    None
                }
            }

            fn token(&self) -> &SyntaxToken {
                &self.0
            }
        }

        typed! { @impl $name $($tt)* }
    };

    (@impl $name:ident { $( $item:item )* }, $( $tt:tt )*) => {
        impl $name {
            $( $item )*
        }

        typed! { $($tt)* }
    };
    // (@impl $name:ident , $( $tt:tt )*) => {
    //     typed! { $($tt)* }
    // };

    () => {
    };
}

typed! {
    @node {
        NODE_APPLY => Apply,
        NODE_ASSERT => Assert,
        NODE_KEY => Key,
        NODE_DYNAMIC => Dynamic,
        NODE_ERROR => Error,
        NODE_IF_ELSE => IfElse,
        NODE_SELECT => Select,
        NODE_INHERIT => Inherit,
        NODE_INHERIT_FROM => InheritFrom,
        NODE_STRING => Str,
        NODE_LAMBDA => Lambda,
        NODE_LEGACY_LET => LegacyLet,
        NODE_LET_IN => LetIn,
        NODE_LIST => List,
        NODE_BIN_OP => BinOp,
        NODE_OR_DEFAULT => OrDefault,
        NODE_PAREN => Paren,
        NODE_PATTERN => Pattern,
        NODE_PAT_BIND => PatBind,
        NODE_PAT_ENTRY => PatEntry,
        NODE_ROOT => Root,
        NODE_ATTR_SET => AttrSet,
        NODE_KEY_VALUE => KeyValue,
        NODE_UNARY_OP => UnaryOp,
        // NODE_IDENT => Ident,
        // poaidfuaposidf => Literal,
        NODE_WITH => With,
    } => Expr: { },

    // @node NODE_IDENT => Ident: {
    //     pub fn ident_token(&self) -> Option<SyntaxToken> {
    //         token(self, T![ident])
    //     }
    // }

    @node NODE_APPLY => Apply: {
        /// Return the lambda being applied
        pub fn lambda(&self) -> Option<Expr> {
            first(self)
        }
        /// Return the value which the lambda is being applied with
        pub fn argument(&self) -> Option<Expr> {
            nth(self, 1)
        }
    },
    @node NODE_ASSERT => Assert: {
        pub fn assert(&self) -> Option<SyntaxToken> {
            token(self, T![assert])
        }

        /// Return the assert condition
        pub fn condition(&self) -> Option<Expr> {
            first(self)
        }

        /// Return the success body
        pub fn body(&self) -> Option<Expr> {
            nth(self, 1)
        }
    },
    @node NODE_KEY => Key: {
        // /// Return the path as an iterator of identifiers
        // pub fn path<'a>(&'a self) -> impl Iterator<Item = SyntaxNode> + 'a {
        //     self.node().children()
        // }
    },
    @node NODE_DYNAMIC => Dynamic: { },
    @node NODE_ERROR => Error: { },
    @node NODE_IF_ELSE => IfElse: {
        pub fn if_token(&self) -> Option<SyntaxToken> {
            token(self, T![if])
        }

        /// Return the condition
        pub fn condition(&self) -> Option<Expr> {
            nth(self, 0)
        }

        pub fn then_token(&self) -> Option<SyntaxToken> {
            token(self, T![then])
        }

        /// Return the success body
        pub fn body(&self) -> Option<Expr> {
            nth(self, 1)
        }

        pub fn else_token(&self) -> Option<SyntaxToken> {
            token(self, T![else])
        }

        /// Return the else body
        pub fn else_body(&self) -> Option<Expr> {
            nth(self, 2)
        }
    },
    @node NODE_SELECT => Select: {
        /// Return the set being indexed
        pub fn set(&self) -> Option<Expr> {
            first(self)
        }

        pub fn dot_token(&self) -> Option<SyntaxToken> {
            token(self, T![.])
        }

        /// Return the index
        pub fn index(&self) -> Option<SyntaxNode> {
            nth!(self; 1)
        }
    },
    @node NODE_INHERIT => Inherit: {
        // /// Return the set where keys are being inherited from, if any
        pub fn inherit_token(&self) -> Option<SyntaxToken> {
            token(self, T![inherit])
        }

        // pub fn from(&self) -> Option<InheritFrom> {
        //     self.node().children()
        //         .find_map(InheritFrom::cast)
        // }
        // /// Return all the identifiers being inherited
        // pub fn idents(&self) -> impl Iterator<Item = Ident> {
        //     self.node().children().filter_map(Ident::cast)
        // }
    },

    @node NODE_INHERIT_FROM => InheritFrom: { },
    @node NODE_STRING => Str: {
        // /// Parse the interpolation into a series of parts
        // pub fn parts(&self) -> Vec<StrPart> {
            // value::string_parts(self)
        // }
    },
    @node NODE_LAMBDA => Lambda: {
        // /// Return the argument of the lambda
        pub fn arg(&self) -> Option<SyntaxNode> {
            nth!(self; 0)
        }
        /// Return the body of the lambda
        pub fn body(&self) -> Option<Expr> {
            first(self)
        }
    },
    @node NODE_LEGACY_LET => LegacyLet: {},
    @node NODE_LET_IN => LetIn: {
        // /// Return the body
        // pub fn body(&self) -> Option<SyntaxNode> {
        //     self.node().last_child()
        // }
    },
    @node NODE_LIST => List: {
        // /// Return an iterator over items in the list
        // pub fn items(&self) -> impl Iterator<Item = SyntaxNode> {
        //     self.node().children()
        // }
    },
    @node NODE_BIN_OP => BinOp: {
        /// Return the left hand side of the binary operation
        pub fn lhs(&self) -> Option<Expr> {
            first(self)
        }

        // /// Return the operator
        // pub fn operator(&self) -> BinOpKind {
        //     self.first_token().and_then(|t| BinOpKind::from_token(t.kind())).expect("invalid ast")
        // }

        /// Return the right hand side of the binary operation
        pub fn rhs(&self) -> Option<Expr> {
            nth(self, 1)
        }
    },
    @node NODE_OR_DEFAULT => OrDefault: {
        /// Return the indexing operation
        pub fn index(&self) -> Option<Select> {
            nth!(self; (Select) 0)
        }
        /// Return the default value
        pub fn default(&self) -> Option<SyntaxNode> {
            nth!(self; 1)
        }
    },
    @node NODE_PAREN => Paren: {},

    @node NODE_PAT_BIND => PatBind: {
        // /// Return the identifier the set is being bound as
        // pub fn name(&self) -> Option<Ident> {
        //     nth!(self; (Ident) 0)
        // }
    },

    @node NODE_PAT_ENTRY => PatEntry: {
        /// Return the identifier the argument is being bound as
        // pub fn name(&self) -> Option<Ident> {
        //     nth!(self; (Ident) 0)
        // }
        /// Return the default value, if any
        pub fn default(&self) -> Option<SyntaxNode> {
            self.node().children().nth(1)
        }
    },
    @node NODE_PATTERN => Pattern: {
        /// Return an iterator over all pattern entries
        pub fn entries(&self) -> impl Iterator<Item = PatEntry> {
            children(self)
        }
        /// Returns the identifier bound with {}@identifier if it exists
        // pub fn at(&self) -> Option<Ident> {
        //     let binding = self.node().children().find(|node| node.kind() == NODE_PAT_BIND)?;
        //     binding.children().filter_map(Ident::cast).next()
        // }
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
    @node NODE_ROOT => Root: {},
    @node NODE_ATTR_SET => AttrSet: {
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
    @node NODE_KEY_VALUE => KeyValue: {
        /// Return this entry's key
        pub fn key(&self) -> Option<Key> {
            nth!(self; (Key) 0)
        }
        /// Return this entry's value
        pub fn value(&self) -> Option<SyntaxNode> {
            nth!(self; 1)
        }
    },
    @node NODE_UNARY_OP => UnaryOp: {
        /// Return the operator
        // pub fn operator(&self) -> Option<UnaryOpKind> {
        //     self.first_token().and_then(|t| UnaryOpKind::from_token(t.kind()))
        // }

        /// Return the value in the operation
        pub fn expr(&self) -> Option<Expr> {
            first(self)
        }
    },
    @node NODE_WITH => With: {
        /// Return the namespace
        pub fn namespace(&self) -> Option<Expr> {
            nth(self, 0)
        }

        /// Return the body
        pub fn body(&self) -> Option<Expr> {
            nth(self, 1)
        }
    },
    @token TOKEN_WHITESPACE => Whitespace: { },
    @token TOKEN_COMMENT => Comment: { },
}
