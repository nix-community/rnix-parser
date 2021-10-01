#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Concat,
    IsSet,
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
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOpKind {
    Invert,
    Negate,
}