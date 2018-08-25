//! Provides a type system for the AST, in some sense

use super::*;

// TODO: More functions for types

macro_rules! invalid_tree {
    () => {{ "This tree isn't valid" }};
}

/// A TypedNode is simply a wrapper around an untyped node to provide a type
/// system in some sense. Allows for convenience traits like HasEntries.
pub trait TypedNode<'a> {
    type Target;
    /// Cast an untyped node into this strongly-typed node. This will return
    /// None if the type was not correct.
    fn cast(from: &'a ASTNode) -> Option<Self::Target>;
    /// Return a reference to the inner untyped node
    fn node(&self) -> &ASTNode;
}
/// Provides the convenience function `.errors()`
pub trait HasErrors<'a>: TypedNode<'a> {
    /// Returns an iterator over all errors. Successfully parsed AST might
    /// still have invalid bits in it, and these can be gathered by this
    /// function.
    fn errors<'b>(&'b self, arena: &'b Arena) -> Box<Iterator<Item = Error> + 'b> {
        Box::new(
            self.node()
                .children(arena)
                .map(move |id| &arena[id])
                .filter_map(Error::cast)
        )
    }
}
/// Provides the convenience function `.entries()`
pub trait HasEntries<'a>: TypedNode<'a> + HasErrors<'a> {
    /// Returns an iterator over all entries. Note: Invalid entries, errors,
    /// don't count here.
    fn entries<'b>(&'b self, arena: &'b Arena) -> Box<Iterator<Item = SetEntry> + 'b> {
        Box::new(
            self.node()
                .children(arena)
                .map(move |id| &arena[id])
                .filter_map(SetEntry::cast)
        )
    }
}

fn cast_into(from: &ASTNode, kind: ASTKind) -> Option<&ASTNode> {
    if from.kind == kind {
        Some(from)
    } else {
        None
    }
}

macro_rules! typed {
    ($($kind:expr => $name:ident$(: $trait:ident)*$(: { $($block:tt)* })*),*) => {
        $(
            pub struct $name<'a>(&'a ASTNode);

            impl<'a> TypedNode<'a> for $name<'a> {
                type Target = Self;
                fn cast(from: &'a ASTNode) -> Option<Self::Target> {
                    cast_into(from, $kind).map($name)
                }
                fn node(&self) -> &ASTNode {
                    &self.0
                }
            }
            $(impl<'a> $trait<'a> for $name<'a> {})*
            $(impl<'a> $name<'a> { $($block)* })*
        )*
    }
}

typed! [
    ASTKind::Apply => Apply,
    ASTKind::Assert => Assert,
    ASTKind::Attribute => Attribute,
    ASTKind::Dynamic => Dynamic,
    ASTKind::Error => Error: {
        /// Return the error message this node holds
        pub fn error(&self) -> &super::Error {
            if let Data::Error(ref err) = self.node().data {
                err
            } else {
                panic!(invalid_tree!());
            }
        }
    },
    ASTKind::Ident => Ident,
    ASTKind::IfElse => IfElse,
    ASTKind::Import => Import,
    ASTKind::IndexSet => IndexSet,
    ASTKind::Inherit => Inherit,
    ASTKind::InheritFrom => InheritFrom,
    ASTKind::Interpol => Interpol,
    ASTKind::InterpolAst => InterpolAst,
    ASTKind::InterpolLiteral => InterpolLiteral,
    ASTKind::Lambda => Lambda,
    ASTKind::Let => Let: HasErrors: HasEntries,
    ASTKind::LetIn => LetIn: HasErrors: HasEntries,
    ASTKind::List => List,
    ASTKind::ListItem => ListItem,
    ASTKind::Operation => Operation,
    ASTKind::OrDefault => OrDefault,
    ASTKind::Paren => Paren,
    ASTKind::PatBind => PatBind,
    ASTKind::PatEntry => PatEntry,
    ASTKind::Pattern => Pattern,
    ASTKind::Set => Set: HasErrors: HasEntries: {
        /// Returns true if this set is recursive
        pub fn recursive(&self, arena: &Arena) -> bool {
            if let Data::Token(_meta, TokenKind::Rec) = &self.node().children(arena)
                    .map(|id| &arena[id])
                    .next()
                    .expect(invalid_tree!()).data {
                true
            } else {
                false
            }
        }
    },
    ASTKind::SetEntry => SetEntry: {
        /// Return this entry's key
        pub fn key<'b>(&self, arena: &'b Arena) -> Attribute<'b> {
            self.node().children(arena)
                .next()
                .map(|id| &arena[id])
                .and_then(Attribute::cast)
                .expect(invalid_tree!())
        }
        /// Return this entry's value
        pub fn val<'b>(&self, arena: &'b Arena) -> &'b ASTNode {
            self.node().children(arena)
                .nth(2)
                .map(|id| &arena[id])
                .expect(invalid_tree!())
        }
    },
    ASTKind::Token => Token,
    ASTKind::Unary => Unary,
    ASTKind::Value => Value,
    ASTKind::With => With
];
