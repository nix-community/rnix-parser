use crate::{SyntaxElement, SyntaxKind::*, SyntaxNode, SyntaxToken};

use super::{
    operators::BinOpKind,
    AstNodeChildren, UnaryOpKind,
    {support::*, AstNode},
};

pub trait EntryHolder: AstNode {
    fn entries(&self) -> AstNodeChildren<Entry>
    where
        Self: Sized,
    {
        children(self)
    }

    fn key_values(&self) -> AstNodeChildren<KeyValue>
    where
        Self: Sized,
    {
        children(self)
    }

    fn inherits(&self) -> AstNodeChildren<Inherit>
    where
        Self: Sized,
    {
        children(self)
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

macro_rules! ast_nodes {
    ($kind:ident => $name:ident $($tt:tt)*) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name(pub(super) SyntaxNode);

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(self.node(), f)
            }
        }

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

        ast_nodes!{ @impl $name $($tt)* }
    };

    ({ $($kind:ident => $typed:ident),* $(,)? } => $name:ident $($tt:tt)*) => {
        pub enum $name {
            $(
                $typed($typed),
            )*
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(self.node(), f)
            }
        }

        impl AstNode for $name {
            fn can_cast(from: &SyntaxNode) -> bool {
                matches!(from.kind(), $($kind)|*)
            }

            fn cast(syntax: SyntaxNode) -> Option<Self> {
                let res = match syntax.kind() {
                    $(
                        $kind => $name::$typed($typed(syntax))
                    ),*,
                    _ => return None,
                };
                Some(res)
            }

            fn node(&self) -> &SyntaxNode {
                match self {
                    $(
                        $name::$typed(it) => &it.0,
                    )*
                }
            }
        }

        ast_nodes! { @impl $name $($tt)* }
    };

    (@impl $name:ident $(: $trait:ident)* $(: { $($item:item)* })?, $($tt:tt)*) => {
        $(impl $name {
            $($item)*
        })?

        $(impl $trait for $name {})*

        ast_nodes! { $($tt)* }
    };

    () => { };
}

ast_nodes! {
     {
        NODE_APPLY => Apply,
        NODE_ASSERT => Assert,
        NODE_DYNAMIC => Dynamic,
        NODE_ERROR => Error,
        NODE_IF_ELSE => IfElse,
        NODE_SELECT => Select,
        NODE_INHERIT => Inherit,
        NODE_INHERIT_FROM => InheritFrom,
        NODE_LITERAL => Literal,
        NODE_LAMBDA => Lambda,
        NODE_LEGACY_LET => LegacyLet,
        NODE_LET_IN => LetIn,
        NODE_LIST => List,
        NODE_BIN_OP => BinOp,
        NODE_OR_DEFAULT => OrDefault,
        NODE_PAREN => Paren,
        NODE_ROOT => Root,
        NODE_ATTR_SET => AttrSet,
        NODE_UNARY_OP => UnaryOp,
        NODE_IDENT => Ident,
        NODE_WITH => With,
    } => Expr,

    NODE_LITERAL => Literal,

    {
        NODE_DYNAMIC => Dynamic,
        NODE_STRING => Str,
    } => Key,

     NODE_IDENT => Ident: {
        pub fn ident_token(&self) -> Option<SyntaxToken> {
            token_u(self, T![ident])
        }
    },

     NODE_APPLY => Apply: {
        /// Return the lambda being applied
        pub fn lambda(&self) -> Option<Expr> {
            first(self)
        }
        /// Return the value which the lambda is being applied with
        pub fn argument(&self) -> Option<Expr> {
            nth(self, 1)
        }
    },
     NODE_ASSERT => Assert: {
        pub fn assert(&self) -> Option<SyntaxToken> {
            token_u(self, T![assert])
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
    //  NODE_KEY => Key: {
        // /// Return the path as an iterator of identifiers
        // pub fn path<'a>(&'a self) -> impl Iterator<Item = SyntaxNode> + 'a {
        //     self.node().children()
        // }
    // },
     NODE_DYNAMIC => Dynamic,
     NODE_ERROR => Error,
     NODE_IF_ELSE => IfElse: {
        pub fn if_token(&self) -> Option<SyntaxToken> {
            token_u(self, T![if])
        }

        /// Return the condition
        pub fn condition(&self) -> Option<Expr> {
            nth(self, 0)
        }

        pub fn then_token(&self) -> Option<SyntaxToken> {
            token_u(self, T![then])
        }

        /// Return the success body
        pub fn body(&self) -> Option<Expr> {
            nth(self, 1)
        }

        pub fn else_token(&self) -> Option<SyntaxToken> {
            token_u(self, T![else])
        }

        /// Return the else body
        pub fn else_body(&self) -> Option<Expr> {
            nth(self, 2)
        }
    },
     NODE_SELECT => Select: {
        /// Return the set being indexed
        pub fn expr(&self) -> Option<Expr> {
            first(self)
        }

        pub fn dot_token(&self) -> Option<SyntaxToken> {
            token_u(self, T![.])
        }

        /// Return the index
        pub fn key(&self) -> Option<Key> {
            first(self)
        }
    },
     NODE_INHERIT => Inherit: {
        // /// Return the set where keys are being inherited from, if any
        pub fn inherit_token(&self) -> Option<SyntaxToken> {
            token_u(self, T![inherit])
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

     NODE_INHERIT_FROM => InheritFrom: { },
     NODE_STRING => Str: {
        // /// Parse the interpolation into a series of parts
        // pub fn parts(&self) -> Vec<StrPart> {
            // value::string_parts(self)
        // }
    },
     NODE_LAMBDA => Lambda: {
        /// Return the argument of the lambda
        pub fn param(&self) -> Option<Param> {
            first(self)
        }

        /// Return the body of the lambda
        pub fn body(&self) -> Option<Expr> {
            first(self)
        }
    },
     NODE_LEGACY_LET => LegacyLet: EntryHolder,
     NODE_LET_IN => LetIn: EntryHolder: {
         pub fn body(&self) -> Option<Expr> {
             first(self)
         }
     },
     NODE_LIST => List: {
        pub fn l_brack_token(&self) -> Option<SyntaxToken> {
            token_u(self, T!["["])
        }

        /// Return an iterator over items in the list
        pub fn items(&self) -> AstNodeChildren<Expr> {
            children(self)
        }

        pub fn r_brack_token(&self) -> Option<SyntaxToken> {
            token_u(self, T!["]"])
        }
    },
     NODE_BIN_OP => BinOp: {
        /// Return the left hand side of the binary operation
        pub fn lhs(&self) -> Option<Expr> {
            first(self)
        }

        // /// Return the operator
        pub fn operator(&self) -> Option<BinOpKind> {
            children_tokens_u(self)
                .find_map(|t| BinOpKind::from_kind(t.kind()))
        }

        /// Return the right hand side of the binary operation
        pub fn rhs(&self) -> Option<Expr> {
            nth(self, 1)
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

     NODE_PAREN => Paren: {
         pub fn l_paren_token(&self) -> Option<SyntaxToken> {
             token_u(self, T!["("])
         }

         pub fn expr(&self) -> Option<Expr> {
             first(self)
         }

         pub fn r_paren_token(&self) -> Option<SyntaxToken> {
             token_u(self, T![")"])
         }
     },

     NODE_PAT_BIND => PatBind: {
        /// Return the identifier the set is being bound as
        pub fn ident(&self) -> Option<Ident> {
            first(self)
        }
    },

     NODE_PAT_ENTRY => PatEntry: {
        /// Return the identifier the argument is being bound as
        pub fn ident(&self) -> Option<Ident> {
            first(self)
        }

        /// Return the default value, if any
        pub fn default(&self) -> Option<SyntaxNode> {
            self.node().children().nth(1)
        }
    },

    {
        NODE_PATTERN => Pattern,
        NODE_IDENT => Ident,
    } => Param,

     NODE_PATTERN => Pattern: {
        pub fn at_token(&self) -> Option<SyntaxToken> {
            token_u(self, T![@])
        }

        /// Return an iterator over all pattern entries
        pub fn entries(&self) -> impl Iterator<Item = PatEntry> {
            children(self)
        }

        pub fn ellipsis_token(&self) -> Option<SyntaxToken> {
            token_u(self, T![...])
        }

        pub fn has_ellipsis(&self) -> bool {
            self.ellipsis_token().is_some()
        }

        /// Returns the identifier bound with {}@identifier if it exists
        pub fn pat_bind(&self) -> Option<PatBind> {
            first(self)
        }
    },
     NODE_ROOT => Root,

     NODE_ATTR_SET => AttrSet: EntryHolder: {
         pub fn rec_token(&self) -> Option<SyntaxToken> {
             token_u(self, T![rec])
         }

        /// Returns true if this set is recursive
        pub fn is_recursive(&self) -> bool {
            self.rec_token().is_some()
        }

        pub fn l_curly_token(&self) -> Option<SyntaxToken> {
            token_u(self, T!["{"])
        }

        pub fn r_curly_token(&self) -> Option<SyntaxToken> {
            token_u(self, T!["}"])
        }
    },

    {
        NODE_INHERIT => Inherit,
        NODE_KEY_VALUE => KeyValue,
    } => Entry,

     NODE_KEY_VALUE => KeyValue: {
        /// Return this entry's key
        pub fn key(&self) -> Option<Key> {
            first(self)
        }

        /// Return this entry's value
        pub fn value(&self) -> Option<Expr> {
            first(self)
        }
    },
     NODE_UNARY_OP => UnaryOp: {
        /// Return the operator
        pub fn operator(&self) -> Option<UnaryOpKind> {
            children_tokens_u(self).find_map(|t| UnaryOpKind::from_kind(t.kind()))
        }

        /// Return the value in the operation
        pub fn expr(&self) -> Option<Expr> {
            first(self)
        }
    },
     NODE_WITH => With: {
        /// Return the namespace
        pub fn namespace(&self) -> Option<Expr> {
            nth(self, 0)
        }

        /// Return the body
        pub fn body(&self) -> Option<Expr> {
            nth(self, 1)
        }
    },
}