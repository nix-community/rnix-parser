use crate::SyntaxKind::{self, *};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Concat,
    Update,

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
    Or,
    PipeRight,
    PipeLeft,
}

impl BinOpKind {
    /// Get the operation kind from a SyntaxKind in the AST
    pub fn from_kind(token: SyntaxKind) -> Option<Self> {
        match token {
            TOKEN_CONCAT => Some(BinOpKind::Concat),
            TOKEN_UPDATE => Some(BinOpKind::Update),

            TOKEN_ADD => Some(BinOpKind::Add),
            TOKEN_SUB => Some(BinOpKind::Sub),
            TOKEN_MUL => Some(BinOpKind::Mul),
            TOKEN_DIV => Some(BinOpKind::Div),

            TOKEN_AND_AND => Some(BinOpKind::And),
            TOKEN_EQUAL => Some(BinOpKind::Equal),
            TOKEN_IMPLICATION => Some(BinOpKind::Implication),
            TOKEN_LESS => Some(BinOpKind::Less),
            TOKEN_LESS_OR_EQ => Some(BinOpKind::LessOrEq),
            TOKEN_MORE => Some(BinOpKind::More),
            TOKEN_MORE_OR_EQ => Some(BinOpKind::MoreOrEq),
            TOKEN_NOT_EQUAL => Some(BinOpKind::NotEqual),
            TOKEN_OR_OR => Some(BinOpKind::Or),

            TOKEN_PIPE_RIGHT => Some(BinOpKind::PipeRight),
            TOKEN_PIPE_LEFT => Some(BinOpKind::PipeLeft),

            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOpKind {
    Invert,
    Negate,
}

impl UnaryOpKind {
    /// Get the operation kind from a token in the AST
    pub fn from_kind(kind: SyntaxKind) -> Option<Self> {
        match kind {
            TOKEN_INVERT => Some(UnaryOpKind::Invert),
            TOKEN_SUB => Some(UnaryOpKind::Negate),
            _ => None,
        }
    }
}
