//! Alternative AST representation that discards all metadata, such as comments
//! and span information. You should probably not use this, as the cast may be
//! somewhat expensive. It's mostly useful for unit testing, where entering
//! span information can get very tedious. You can convert an AST with metadata
//! to this type using `.into()`

use super::{
    AST as ASTMeta,
    ASTType,
    Attribute as AttributeMeta,
    LambdaArg as LambdaArgMeta,
    Interpol as InterpolMeta,
    PatEntry as PatEntryMeta,
    Parens as ParensMeta,
    SetEntry as SetEntryMeta,
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

    Operation(Box<(AST, Operator, AST)>)
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

fn vec_into<F, T: From<F>>(vec: Vec<F>) -> Vec<T> {
    vec.into_iter()
        .map(|item| T::from(item))
        .collect()
}
fn box_into<F, T: From<F>>(ast: Box<F>) -> Box<T> {
    Box::new(T::from(*ast))
}
fn tuple_into<F, T: From<F>>(ast: Box<(F, F)>) -> Box<(T, T)> {
    Box::new((T::from(ast.0), T::from(ast.1)))
}
fn triple_into<F, T: From<F>>(ast: Box<(F, F, F)>) -> Box<(T, T, T)> {
    Box::new((T::from(ast.0), T::from(ast.1), T::from(ast.2)))
}

impl From<PatEntryMeta> for PatEntry {
    fn from(entry: PatEntryMeta) -> Self {
        PatEntry(entry.name, entry.default.map(|(_, ast)| AST::from(ast)))
    }
}
impl From<ParensMeta> for AST {
    fn from(parens: ParensMeta) -> Self {
        let ParensMeta(_open, inner, _close) = parens;
        AST::from(inner)
    }
}
impl From<SetEntryMeta> for SetEntry {
    fn from(entry: SetEntryMeta) -> Self {
        match entry {
            SetEntryMeta::Assign(AttributeMeta(key), _assign, value, _semi) => SetEntry::Assign(
                key.into_iter().map(|(ast, _dot)| AST::from(ast)).collect(),
                AST::from(value)
            ),
            SetEntryMeta::Inherit(from, values, _semi) => SetEntry::Inherit(
                from.map(AST::from),
                values.into_iter().map(|(_meta, value)| value).collect()
            )
        }
    }
}
impl From<InterpolMeta> for Interpol {
    fn from(interpol: InterpolMeta) -> Self {
        match interpol {
            InterpolMeta::Literal(text) => Interpol::Literal(text),
            InterpolMeta::AST(ast, _close) => Interpol::AST(AST::from(ast))
        }
    }
}
impl From<LambdaArgMeta> for LambdaArg {
    fn from(arg: LambdaArgMeta) -> Self {
        match arg {
            LambdaArgMeta::Ident(_meta, name) => LambdaArg::Ident(name),
            LambdaArgMeta::Pattern { args, bind, ellipsis } => LambdaArg::Pattern {
                args: vec_into(args.1),
                bind: bind.map(|bind| bind.name),
                ellipsis: ellipsis.is_some()
            },
        }
    }
}
impl From<ASTMeta> for AST {
    fn from(ast: ASTMeta) -> Self {
        match ast.1 {
            // Types
            ASTType::Interpol { meta: _, multiline, parts } => AST::Interpol { multiline, parts: vec_into(parts) },
            ASTType::Lambda(args, _colon, body) => AST::Lambda(args.into(), box_into(body)),
            ASTType::List(_open, inner, _close) => AST::List(vec_into(inner)),
            ASTType::Parens(inner) => AST::from(*inner),
            ASTType::Set { recursive, values } =>
                AST::Set { recursive: recursive.is_some(), values: vec_into(values.1) },
            ASTType::Value(_, inner) => AST::Value(inner),
            ASTType::Var(_, inner) => AST::Var(inner),

            // Expressions
            ASTType::Assert(_assert, _semi, inner) => AST::Assert(tuple_into(inner)),
            ASTType::IfElse { if_meta: _, then_meta: _, else_meta: _, bodies } => AST::IfElse(triple_into(bodies)),
            ASTType::Import(_import, inner) => AST::Import(box_into(inner)),
            ASTType::Let(_let, set) => AST::Let(vec_into(set.1)),
            ASTType::LetIn(_let, set, _in, ast) => AST::LetIn(vec_into(set), box_into(ast)),
            ASTType::With(_with, _semi, inner) => AST::With(tuple_into(inner)),

            // Operators
            ASTType::Apply(inner) => AST::Apply(tuple_into(inner)),
            ASTType::Dynamic { meta: _, ast, close: _ } => AST::Dynamic(box_into(ast)),
            ASTType::IndexSet(_dot, inner) => AST::IndexSet(tuple_into(inner)),
            ASTType::Unary(_meta, op, inner) => AST::Unary(op, box_into(inner)),
            ASTType::OrDefault { dot: _, or: _, bodies } => AST::OrDefault(triple_into(bodies)),

            ASTType::Operation(box (one, (_, op), two)) => AST::Operation(Box::new((AST::from(one), op, AST::from(two))))
        }
    }
}
