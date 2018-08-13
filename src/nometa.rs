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
    Import(Box<AST>),
    Let(Vec<SetEntry>),
    LetIn(Vec<SetEntry>, Box<AST>),
    With(Box<(AST, AST)>),

    // Operators
    Apply(Box<(AST, AST)>),
    Concat(Box<(AST, AST)>),
    IndexSet(Box<AST>, String),
    Negate(Box<AST>),

    Add(Box<(AST, AST)>),
    Sub(Box<(AST, AST)>),
    Mul(Box<(AST, AST)>),
    Div(Box<(AST, AST)>)
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
pub struct SetEntry(pub Vec<String>, pub AST);

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

impl From<PatEntryMeta> for PatEntry {
    fn from(entry: PatEntryMeta) -> Self {
        PatEntry(entry.0, entry.1.map(AST::from))
    }
}
impl From<SetEntryMeta> for SetEntry {
    fn from(entry: SetEntryMeta) -> Self {
        SetEntry(entry.0, AST::from(entry.1))
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
            ASTType::Import(inner) => AST::Import(box_into(inner)),
            ASTType::Let(set) => AST::Let(vec_into(set)),
            ASTType::LetIn(set, ast) => AST::LetIn(vec_into(set), box_into(ast)),
            ASTType::With(inner) => AST::With(tuple_into(inner)),

            // Operators
            ASTType::Apply(inner) => AST::Apply(tuple_into(inner)),
            ASTType::Concat(inner) => AST::Concat(tuple_into(inner)),
            ASTType::IndexSet(set, key) => AST::IndexSet(box_into(set), key),
            ASTType::Negate(inner) => AST::Negate(box_into(inner)),

            ASTType::Add(inner) => AST::Add(tuple_into(inner)),
            ASTType::Sub(inner) => AST::Sub(tuple_into(inner)),
            ASTType::Mul(inner) => AST::Mul(tuple_into(inner)),
            ASTType::Div(inner) => AST::Div(tuple_into(inner))
        }
    }
}
