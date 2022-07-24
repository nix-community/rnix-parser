use crate::{NixLanguage, SyntaxKind, SyntaxKind::*, SyntaxNode, SyntaxToken};

use super::{operators::BinOpKind, support::*, AstNode, UnaryOpKind};
use rowan::ast::AstChildren;
use rowan::ast::AstNode as OtherAstNode;

pub trait HasEntry: AstNode {
    fn entries(&self) -> AstChildren<Entry>
    where
        Self: Sized,
    {
        children(self)
    }

    fn attrpath_values(&self) -> AstChildren<AttrpathValue>
    where
        Self: Sized,
    {
        children(self)
    }

    fn inherits(&self) -> AstChildren<Inherit>
    where
        Self: Sized,
    {
        children(self)
    }
}

macro_rules! node {
    (
        #[from($kind:ident)]
        $(#[$meta:meta])*
        struct $name:ident;
    ) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        $(#[$meta])*
        pub struct $name(pub(super) SyntaxNode);

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(self.syntax(), f)
            }
        }

        impl rowan::ast::AstNode for $name {
            type Language = crate::NixLanguage;

            fn can_cast(kind: crate::SyntaxKind) -> bool {
                kind == $kind
            }

            fn cast(from: SyntaxNode) -> Option<Self> {
                if Self::can_cast(from.kind()) {
                    Some(Self(from))
                } else {
                    None
                }
            }
            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }
    };
    (
        #[case($($kind:ident => $variant:ident),* $(,)?)]
        $(#[$meta:meta])*
        enum $name:ident;
    ) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        $(#[$meta])*
        pub enum $name {
            $(
                $variant($variant),
            )*
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(self.syntax(), f)
            }
        }

        impl rowan::ast::AstNode for $name {
            type Language = NixLanguage;

            fn can_cast(kind: SyntaxKind) -> bool {
                matches!(kind, $($kind)|*)
            }

            fn cast(syntax: SyntaxNode) -> Option<Self> {
                let res = match syntax.kind() {
                    $(
                        $kind => $name::$variant($variant(syntax))
                    ),*,
                    _ => return None,
                };
                Some(res)
            }

            fn syntax(&self) -> &SyntaxNode {
                match self {
                    $(
                        $name::$variant(it) => &it.0,
                    )*
                }
            }
        }

        $(
            impl From<$variant> for $name {
                fn from(node: $variant) -> $name { $name::$variant(node) }
            }

            impl TryFrom<$name> for $variant {
                type Error = ();

                fn try_from(node: $name) -> Result<$variant, ()> {
                    match node {
                        $name::$variant(it) => Ok(it),
                        _ => Err(()),
                    }
                }
            }
        )*
    };
}

macro_rules! tg {
    (
        $(#[doc=$doc:tt])?
        $name:ident,
        $token:tt
    ) => {
        $(#[doc=$doc])?
        pub fn $name(&self) -> Option<SyntaxToken> {
            token_u(self, T![$token])
        }
    };
}

macro_rules! ng {
    (
        $(#[$meta:meta])*
        $name:ident,
        $ty:ty,
        $i:expr
    ) => {
        $(#[$meta])*
        pub fn $name(&self) -> Option<$ty> {
            nth(self, $i)
        }
    };
    (
        $(#[$meta:meta])*
        $name:ident,
        [$ty:ty]
    ) => {
        $(#[$meta])*
        pub fn $name(&self) -> AstChildren<$ty> {
            children(self)
        }
    };
}

node! { #[from(NODE_LITERAL)] struct Literal; }

node! {
    #[case(
        NODE_APPLY => Apply,
        NODE_ASSERT => Assert,
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
        NODE_PAREN => Paren,
        NODE_ROOT => Root,
        NODE_ATTR_SET => AttrSet,
        NODE_UNARY_OP => UnaryOp,
        NODE_IDENT => Ident,
        NODE_WITH => With,
        NODE_STRING => Str,
        NODE_HAS_ATTR => HasAttr,
    )]
    /// An expression. The fundamental nix ast type.
    enum Expr;
}

node! {
    #[case(
        NODE_IDENT => Ident,
        NODE_DYNAMIC => Dynamic,
        NODE_STRING => Str,
    )]
    enum Attr;
}

node! { #[from(NODE_IDENT)] struct Ident; }

impl Ident {
    tg! { ident_token, TOKEN_IDENT }
}

node! { #[from(NODE_APPLY)] struct Apply; }

impl Apply {
    ng! { lambda, Expr, 0 }
    ng! { argument, Expr, 1 }
}

node! { #[from(NODE_ASSERT)] struct Assert; }

impl Assert {
    tg! { assert_token, assert }
    ng! { condition, Expr, 0 }
    ng! { body, Expr, 1 }
}

node! { #[from(NODE_ATTRPATH)] struct Attrpath; }

impl Attrpath {
    ng! { attrs, [Attr] }
}

node! { #[from(NODE_DYNAMIC)] struct Dynamic; }

node! { #[from(NODE_ERROR)] struct Error; }

node! { #[from(NODE_IF_ELSE)] struct IfElse; }

impl IfElse {
    tg! { if_token, if }
    ng! { condition, Expr, 0 }
    tg! { then_token, then }
    ng! { body, Expr, 1 }
    tg! { else_token, else }
    ng! { else_body, Expr, 2 }
}

node! { #[from(NODE_SELECT)] struct Select; }

impl Select {
    ng! { expr, Expr, 0 }
    tg! { dot_token, . }
    ng! { attrpath, Attrpath, 0 }
    // we should either add or as a token in the lexer
    // or just find a token with text "or"
    // tg! { or_token, or }
    ng! { default_expr, Expr, 1 }
}

node! { #[from(NODE_INHERIT)] struct Inherit; }

impl Inherit {
    tg! { inherit_token, inherit }
    ng! { from, InheritFrom, 0 }
    ng! { idents, [Ident] }
}

node! { #[from(NODE_INHERIT_FROM)] struct InheritFrom; }

impl InheritFrom {
    tg! { l_paren_token, "(" }
    ng! { expr, Expr, 0 }
    tg! { r_paren_token, ")" }
}

node! { #[from(NODE_STRING)] struct Str; }

node! { #[from(NODE_STRING_INTERPOL)] struct StrInterpol; }

impl StrInterpol {
    ng! { expr, Expr, 0 }
}

node! { #[from(NODE_LAMBDA)] struct Lambda; }

impl Lambda {
    ng! { param, Param, 0 }
    tg! { token_colon, : }
    ng! { body, Expr, 0 }
}

node! { #[from(NODE_LEGACY_LET)] struct LegacyLet; }

impl HasEntry for LegacyLet {}

impl LegacyLet {
    tg! { let_token, let }
    tg! { curly_open_token, "{" }
    tg! { curly_close_token, "}" }
}

node! { #[from(NODE_LET_IN)] struct LetIn; }

impl HasEntry for LetIn {}

impl LetIn {
    tg! { let_token, let }
    tg! { in_token, in }
    ng! { body, Expr, 0 }
}

node! { #[from(NODE_LIST)] struct List; }

impl List {
    tg! { l_brack_token, "[" }
    ng! { items, [Expr] }
    tg! { r_brack_token, "]" }
}

node! { #[from(NODE_BIN_OP)] struct BinOp; }

impl BinOp {
    ng! { lhs, Expr, 0 }

    pub fn operator(&self) -> Option<BinOpKind> {
        children_tokens_u(self).find_map(|t| BinOpKind::from_kind(t.kind()))
    }

    ng! { rhs, Expr, 1 }
}

node! { #[from(NODE_PAREN)] struct Paren; }

impl Paren {
    tg! { l_paren_token, "(" }
    ng! { expr, Expr, 0 }
    tg! { r_paren_token, ")" }
}

node! { #[from(NODE_PAT_BIND)] struct PatBind; }

impl PatBind {
    ng! { ident, Ident, 0 }
}

node! { #[from(NODE_PAT_ENTRY)] struct PatEntry; }

impl PatEntry {
    ng! { ident, Ident, 0 }
    tg! { question_token, ? }
    ng! { default, Expr, 1 }
}

node! {
    #[case(
        NODE_PATTERN => Pattern,
        NODE_IDENT => Ident,
    )]
    enum Param;
}

node! { #[from(NODE_PATTERN)] struct Pattern; }

impl Pattern {
    tg! { at_token, @ }
    ng! { pat_entries, [PatEntry] }
    tg! { ellipsis_token, ... }
    ng! { pat_bind, PatBind, 0 }
}

node! { #[from(NODE_ROOT)] struct Root; }

impl Root {
    ng! { expr, Expr, 0 }
}

node! { #[from(NODE_ATTR_SET)] struct AttrSet; }

impl HasEntry for AttrSet {}

impl AttrSet {
    tg! { rec_token, rec }
    tg! { l_curly_token, "{" }
    tg! { r_curly_token, "}" }
}

node! {
    #[case(
        NODE_INHERIT => Inherit,
        NODE_ATTRPATH_VALUE => AttrpathValue,
    )]
    enum Entry;
}

node! { #[from(NODE_ATTRPATH_VALUE)] struct AttrpathValue; }

impl AttrpathValue {
    ng! { attrpath, Attrpath, 0 }
    tg! { assign_token, = }
    ng! { value, Expr, 0 }
}

node! { #[from(NODE_UNARY_OP)] struct UnaryOp; }

impl UnaryOp {
    pub fn operator(&self) -> Option<UnaryOpKind> {
        children_tokens_u(self).find_map(|t| UnaryOpKind::from_kind(t.kind()))
    }
    ng! { expr, Expr, 0 }
}

node! { #[from(NODE_WITH)] struct With; }

impl With {
    tg! { with_token, with }
    ng! { namespace, Expr, 0 }
    tg! { semicolon_token, ; }
    ng! { body, Expr, 1 }
}

node! { #[from(NODE_HAS_ATTR)] struct HasAttr; }

impl HasAttr {
    ng! { expr, Expr, 0 }
    tg! { question_token, ? }
    ng! { attrpath, Attrpath, 0 }
}
