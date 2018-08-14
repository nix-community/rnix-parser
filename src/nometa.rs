use crate::{
    parser::{
        AST as ASTMeta,
        ASTType,
        FnArg as FnArgMeta,
        Interpol as InterpolMeta,
        PatEntry as PatEntryMeta,
        SetEntry as SetEntryMeta,
    },
    value::Value
};

#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    // Types
    EmptySet,
    Interpol(Vec<Interpol>),
    Lambda(FnArg, Box<AST>),
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
    Concat(Box<(AST, AST)>),
    IndexSet(Box<(AST, AST)>),
    Invert(Box<AST>),
    IsSet(Box<(AST, AST)>),
    Merge(Box<(AST, AST)>),
    Negate(Box<AST>),
    OrDefault(Box<(AST, AST)>),

    Add(Box<(AST, AST)>),
    Sub(Box<(AST, AST)>),
    Mul(Box<(AST, AST)>),
    Div(Box<(AST, AST)>),

    And(Box<(AST, AST)>),
    Equal(Box<(AST, AST)>),
    Less(Box<(AST, AST)>),
    LessOrEq(Box<(AST, AST)>),
    More(Box<(AST, AST)>),
    MoreOrEq(Box<(AST, AST)>),
    NotEqual(Box<(AST, AST)>),
    Or(Box<(AST, AST)>)
}
#[derive(Clone, Debug, PartialEq)]
pub enum FnArg {
    Ident(String),
    Pattern {
        args: Vec<PatEntry>,
        bind: Option<String>,
        exact: bool
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum Interpol {
    Literal(String),
    AST(AST)
}
#[derive(Clone, Debug, PartialEq)]
pub struct PatEntry(pub String, pub Option<AST>);
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
        PatEntry(entry.0, entry.1.map(AST::from))
    }
}
impl From<SetEntryMeta> for SetEntry {
    fn from(entry: SetEntryMeta) -> Self {
        match entry {
            SetEntryMeta::Assign(key, value) => SetEntry::Assign(vec_into(key), AST::from(value)),
            SetEntryMeta::Inherit(from, values) => SetEntry::Inherit(from.map(AST::from), values),
        }
    }
}
impl From<InterpolMeta> for Interpol {
    fn from(interpol: InterpolMeta) -> Self {
        match interpol {
            InterpolMeta::Literal(text) => Interpol::Literal(text),
            InterpolMeta::AST(ast) => Interpol::AST(AST::from(ast))
        }
    }
}
impl From<FnArgMeta> for FnArg {
    fn from(arg: FnArgMeta) -> Self {
        match arg {
            FnArgMeta::Ident(name) => FnArg::Ident(name),
            FnArgMeta::Pattern { args, bind, exact } => FnArg::Pattern { args: vec_into(args), bind, exact },
        }
    }
}
impl From<ASTMeta> for AST {
    fn from(ast: ASTMeta) -> Self {
        match ast.1 {
            // Types
            ASTType::EmptySet => AST::EmptySet,
            ASTType::Interpol(inner) => AST::Interpol(vec_into(inner)),
            ASTType::Lambda(args, body) => AST::Lambda(args.into(), box_into(body)),
            ASTType::List(inner) => AST::List(vec_into(inner)),
            ASTType::Set { recursive, values } => AST::Set { recursive, values: vec_into(values) },
            ASTType::Value(inner) => AST::Value(inner),
            ASTType::Var(inner) => AST::Var(inner),

            // Expressions
            ASTType::Assert(inner) => AST::Assert(tuple_into(inner)),
            ASTType::IfElse(inner) => AST::IfElse(triple_into(inner)),
            ASTType::Import(inner) => AST::Import(box_into(inner)),
            ASTType::Let(set) => AST::Let(vec_into(set)),
            ASTType::LetIn(set, ast) => AST::LetIn(vec_into(set), box_into(ast)),
            ASTType::With(inner) => AST::With(tuple_into(inner)),

            // Operators
            ASTType::Apply(inner) => AST::Apply(tuple_into(inner)),
            ASTType::Dynamic(inner) => AST::Dynamic(box_into(inner)),
            ASTType::Concat(inner) => AST::Concat(tuple_into(inner)),
            ASTType::IndexSet(inner) => AST::IndexSet(tuple_into(inner)),
            ASTType::Invert(inner) => AST::Invert(box_into(inner)),
            ASTType::IsSet(inner) => AST::IsSet(tuple_into(inner)),
            ASTType::Merge(inner) => AST::Merge(tuple_into(inner)),
            ASTType::Negate(inner) => AST::Negate(box_into(inner)),
            ASTType::OrDefault(inner) => AST::OrDefault(tuple_into(inner)),

            ASTType::Add(inner) => AST::Add(tuple_into(inner)),
            ASTType::Sub(inner) => AST::Sub(tuple_into(inner)),
            ASTType::Mul(inner) => AST::Mul(tuple_into(inner)),
            ASTType::Div(inner) => AST::Div(tuple_into(inner)),

            ASTType::And(inner) => AST::And(tuple_into(inner)),
            ASTType::Equal(inner) => AST::Equal(tuple_into(inner)),
            ASTType::Less(inner) => AST::Less(tuple_into(inner)),
            ASTType::LessOrEq(inner) => AST::LessOrEq(tuple_into(inner)),
            ASTType::More(inner) => AST::More(tuple_into(inner)),
            ASTType::MoreOrEq(inner) => AST::MoreOrEq(tuple_into(inner)),
            ASTType::NotEqual(inner) => AST::NotEqual(tuple_into(inner)),
            ASTType::Or(inner) => AST::Or(tuple_into(inner))
        }
    }
}
