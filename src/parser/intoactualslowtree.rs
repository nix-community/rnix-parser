//! Alternative AST representation that discards all metadata, such as comments
//! and span information. It also uses an actual tree with Box<_> to create
//! recursive types, which is much, much slower. You should probably not use
//! this, as the cast may be very expensive. But nonetheless this can be useful
//! for unit testing, where entering span information can get very tedious. It
//! can also be used to debug print the AST. You can convert an AST with
//! metadata to this type using `.into_tree()`

use super::{
    ASTNode as ASTMeta,
    ASTType,
    Attribute as AttributeMeta,
    LambdaArg as LambdaArgMeta,
    Interpol as InterpolMeta,
    PatEntry as PatEntryMeta,
    Parens as ParensMeta,
    SetEntry as SetEntryMeta
};
use crate::value::Value;
pub use super::{Operator, Unary};

/// An AST node
#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    // Types
    Interpol {
        multiline: bool,
        parts: Vec<Interpol>
    },
    Lambda(LambdaArg, Box<AST>),
    List(Vec<AST>),
    Set {
        recursive: bool,
        values: Vec<SetEntry>
    },
    Value(Value),
    Var(String),

    // Expressions
    Assert(Box<(AST, AST)>),
    Dynamic(Box<AST>),
    IfElse(Box<(AST, AST, AST)>),
    Import(Box<AST>),
    Let(Vec<SetEntry>),
    LetIn(Vec<SetEntry>, Box<AST>),
    With(Box<(AST, AST)>),

    // Operators
    Apply(Box<(AST, AST)>),
    IndexSet(Box<(AST, AST)>),
    Unary(Unary, Box<AST>),
    OrDefault(Box<(AST, AST, AST)>),

    Operation(Operator, Box<(AST, AST)>)
}
/// A lambda argument type
#[derive(Clone, Debug, PartialEq)]
pub enum LambdaArg {
    Ident(String),
    Pattern {
        args: Vec<PatEntry>,
        bind: Option<String>,
        ellipsis: bool
    }
}
/// An interpolation part
#[derive(Clone, Debug, PartialEq)]
pub enum Interpol {
    Literal(String),
    AST(AST)
}
/// An entry in a pattern
#[derive(Clone, Debug, PartialEq)]
pub struct PatEntry(pub String, pub Option<AST>);
/// An entry in a set
#[derive(Clone, Debug, PartialEq)]
pub enum SetEntry {
    Assign(Vec<AST>, AST),
    Inherit(Option<AST>, Vec<String>)
}

type Arena<'a> = super::Arena<'a, ASTMeta>;

/// Read the documentation for this module
pub trait IntoTree<T> {
    fn into_tree<'a>(ast: T, arena: &mut Arena<'a>) -> Self;
}

impl IntoTree<PatEntryMeta> for PatEntry {
    fn into_tree<'a>(entry: PatEntryMeta, arena: &mut Arena<'a>) -> Self {
        PatEntry(entry.name, entry.default.map(|(_, id)| AST::into_tree(arena.take(id), arena)))
    }
}
impl IntoTree<ParensMeta> for AST {
    fn into_tree<'a>(parens: ParensMeta, arena: &mut Arena<'a>) -> Self {
        let ParensMeta(_open, id, _close) = parens;
        AST::into_tree(arena.take(id), arena)
    }
}
impl IntoTree<SetEntryMeta> for SetEntry {
    fn into_tree<'a>(entry: SetEntryMeta, arena: &mut Arena<'a>) -> Self {
        match entry {
            SetEntryMeta::Assign(AttributeMeta(key), _assign, value, _semi) => SetEntry::Assign(
                key.into_iter().map(|(id, _dot)| AST::into_tree(arena.take(id), arena)).collect(),
                AST::into_tree(arena.take(value), arena)
            ),
            SetEntryMeta::Inherit(_meta, from, values, _semi) => SetEntry::Inherit(
                from.map(|attr| AST::into_tree(attr, arena)),
                values.into_iter().map(|(_meta, value)| value).collect()
            )
        }
    }
}
impl IntoTree<InterpolMeta> for Interpol {
    fn into_tree<'a>(interpol: InterpolMeta, arena: &mut Arena<'a>) -> Self {
        match interpol {
            InterpolMeta::Literal { original: _, content } => Interpol::Literal(content),
            InterpolMeta::AST(id, _close) => Interpol::AST(AST::into_tree(arena.take(id), arena))
        }
    }
}
impl IntoTree<LambdaArgMeta> for LambdaArg {
    fn into_tree<'a>(arg: LambdaArgMeta, arena: &mut Arena<'a>) -> Self {
        match arg {
            LambdaArgMeta::Ident(_meta, name) => LambdaArg::Ident(name),
            LambdaArgMeta::Pattern { args, bind, ellipsis } => LambdaArg::Pattern {
                args: args.1.into_iter()
                    .map(|item| PatEntry::into_tree(item, arena))
                    .collect(),
                bind: bind.map(|bind| bind.name),
                ellipsis: ellipsis.is_some()
            }
        }
    }
}
impl IntoTree<ASTMeta> for AST {
    fn into_tree<'a>(ast: ASTMeta, arena: &mut Arena<'a>) -> Self {
        macro_rules! into {
            (box $($rest:tt)*) => {{
                Box::new(into!($($rest)*))
            }};
            (vec($var:expr) $($rest:tt)*) => {{
                $var.into_iter()
                    .map(|item| into!($($rest)* item))
                    .collect()
            }};
            (id $id:expr) => {{
                into!(AST arena.take($id))
            }};
            (($(id $val:expr),+)) => {{
                ($(into!(id $val)),+)
            }};
            (($($val:expr),+)) => {{
                ($(into!($val)),+)
            }};
            ($val:expr) => {{
                into!(AST $val)
            }};
            ($type:ident $val:expr) => {{
                $type::into_tree($val, arena)
            }};
        }
        match ast.1 {
            // Types
            ASTType::Interpol { meta: _, multiline, parts } =>
                AST::Interpol { multiline, parts: into!(vec(parts) Interpol) },
            ASTType::Lambda(args, _colon, body) => AST::Lambda(into!(LambdaArg args), into!(box id body)),
            ASTType::List(_open, inner, _close) => AST::List(into!(vec(inner) id)),
            ASTType::Parens(inner) => into!(inner),
            ASTType::Set { recursive, values } =>
                AST::Set { recursive: recursive.is_some(), values: into!(vec(values.1) SetEntry) },
            ASTType::Value(_, inner) => AST::Value(inner),
            ASTType::Var(_, inner) => AST::Var(inner),

            //// Expressions
            ASTType::Assert(_assert, cond, _semi, body) => AST::Assert(into!(box (id cond, id body))),
            ASTType::IfElse { if_meta: _, condition, then_meta: _, then_body, else_meta: _, else_body } =>
                AST::IfElse(into!(box (id condition, id then_body, id else_body))),
            ASTType::Import(_import, inner) => AST::Import(into!(box id inner)),
            ASTType::Let(_let, set) => AST::Let(into!(vec(set.1) SetEntry)),
            ASTType::LetIn(_let, set, _in, ast) => AST::LetIn(into!(vec(set) SetEntry), into!(box id ast)),
            ASTType::With(_with, namespace, _semi, body) => AST::With(into!(box (id namespace, id body))),

            //// Operators
            ASTType::Apply(f, arg) => AST::Apply(into!(box (id f, id arg))),
            ASTType::Dynamic { meta: _, ast, close: _ } => AST::Dynamic(into!(box id ast)),
            ASTType::IndexSet(set, _dot, attr) => AST::IndexSet(into!(box (id set, id attr))),
            ASTType::Unary(_meta, op, val) => AST::Unary(op, into!(box id val)),
            ASTType::OrDefault { set, dot: _, attr, or: _, default } =>
                AST::OrDefault(into!(box (id set, id attr, id default))),

            ASTType::Operation(one, (_, op), two) => AST::Operation(op, into!(box (id one, id two)))
        }
    }
}
