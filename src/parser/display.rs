use crate::tokenizer::Trivia;
use super::*;

use std::fmt;

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Operator::Concat => "++",
            Operator::Merge => "//",
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::And => "&&",
            Operator::Equal => "==",
            Operator::Implication => "->",
            Operator::IsSet => "?",
            Operator::Less => "<",
            Operator::LessOrEq => "<=",
            Operator::More => ">",
            Operator::MoreOrEq => ">=",
            Operator::NotEqual => "!=",
            Operator::Or => "||"
        })
    }
}
impl fmt::Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Unary::Invert => "!",
            Unary::Negate => "-"
        })
    }
}

fn fmt_trivia(f: &mut fmt::Formatter, trivia: &[Trivia]) -> fmt::Result {
    for trivia in trivia {
        write!(f, "{}", trivia)?;
    }
    Ok(())
}
fn fmt_node<'a>(f: &mut fmt::Formatter, arena: &Arena<'a, ASTNode>, root: &ASTNode) -> fmt::Result {
    macro_rules! fmt {
        (node $node:expr) => {
            fmt_node(f, arena, &arena[$node])?;
        };
        (attr $attr:expr) => {
            for (ident, dot) in $attr {
                fmt!(node *ident);
                if let Some(dot) = dot {
                    fmt!(dot, ".");
                }
            }
        };
        (set $values:expr) => {
            for entry in $values {
                match entry {
                    SetEntry::Assign(Attribute(attr), eq, value, semi) => {
                        fmt!(attr attr);
                        fmt!(eq, "=");
                        fmt!(node *value);
                        fmt!(semi, ";");
                    },
                    SetEntry::Inherit(meta, from, idents, semi) => {
                        fmt!(meta, "inherit");
                        if let Some(Parens(open, from, close)) = from {
                            fmt!(open, "(");
                            fmt!(node *from);
                            fmt!(close, ")");
                        }
                        for (meta, name) in idents {
                            fmt!(meta, "{}", name);
                        }
                        fmt!(semi, ";");
                    }
                }
            }
        };
        ($meta:expr, $fmt:expr $(, $arg:expr)*) => {{
            fmt_trivia(f, &$meta.leading)?;
            write!(f, $fmt $(, $arg)*)?;
            fmt_trivia(f, &$meta.trailing)?;
        }};
    }
    match &root.1 {
        ASTType::Interpol { meta, multiline, parts } => {
            fmt_trivia(f, &meta.leading)?;
            write!(f, "{}", if *multiline { "''" } else { "\"" })?;
            for part in parts {
                match part {
                    Interpol::Literal { original, content: _ } => write!(f, "{}", original)?,
                    Interpol::AST(ast, close) => {
                        write!(f, "{{")?;
                        fmt!(node *ast);
                        fmt!(close, "}}");
                    }
                }
            }
            write!(f, "{}", if *multiline { "''" } else { "\"" })?;
            fmt_trivia(f, &meta.trailing)?;
        },
        ASTType::Lambda(arg, colon, body) => {
            match arg {
                LambdaArg::Ident(meta, name) => fmt!(meta, "{}", name),
                LambdaArg::Pattern { args: Brackets(open, args, close), bind, ellipsis } => {
                    if let Some(bind) = bind {
                        if bind.before {
                            fmt!(bind.ident, "{}", bind.name);
                            fmt!(bind.at, "@");
                        }
                    }
                    fmt!(open, "{{");
                    for arg in args {
                        fmt!(arg.ident, "{}", arg.name);
                        if let Some((question, default)) = &arg.default {
                            fmt!(question, "?");
                            fmt!(node *default);
                        }
                        if let Some(comma) = &arg.comma {
                            fmt!(comma, ",");
                        }
                    }
                    if let Some(ellipsis) = ellipsis {
                        fmt!(ellipsis, "...");
                    }
                    fmt!(close, "}}");
                    if let Some(bind) = bind {
                        if !bind.before {
                            fmt!(bind.at, "@");
                            fmt!(bind.ident, "{}", bind.name);
                        }
                    }
                },
            }
            fmt!(colon, ":");
            fmt!(node *body);
        },
        ASTType::List(open, items, close) => {
            fmt!(open, "[");
            for item in items {
                fmt!(node *item);
            }
            fmt!(close, "]");
        },
        ASTType::Parens(Parens(open, ast, close)) => {
            fmt!(open, "(");
            fmt!(node *ast);
            fmt!(close, ")");
        },
        ASTType::Set { recursive, values: Brackets(open, values, close) } => {
            if let Some(recursive) = recursive {
                fmt!(recursive, "rec");
            }
            fmt!(open, "{{");
            fmt!(set values);
            fmt!(close, "}}");
        },
        ASTType::Value(meta, val) => fmt!(meta, "{}", val),
        ASTType::Var(meta, name) => fmt!(meta, "{}", name),
        ASTType::Assert(assert, condition, semi, body) => {
            fmt!(assert, "assert");
            fmt!(node *condition);
            fmt!(semi, ";");
            fmt!(node *body);
        },
        ASTType::IfElse { if_meta, condition, then_meta, then_body, else_meta, else_body } => {
            fmt!(if_meta, "if");
            fmt!(node *condition);
            fmt!(then_meta, "then");
            fmt!(node *then_body);
            fmt!(else_meta, "else");
            fmt!(node *else_body);
        },
        ASTType::Import(meta, path) => {
            fmt!(meta, "import");
            fmt!(node *path);
        },
        ASTType::Let(let_, Brackets(open, values, close)) => {
            fmt!(let_, "let");
            fmt!(open, "{{");
            fmt!(set values);
            fmt!(close, "}}");
        },
        ASTType::LetIn(let_, values, in_, body) => {
            fmt!(let_, "let");
            fmt!(set values);
            fmt!(in_, "in");
            fmt!(node *body);
        },
        ASTType::With(with, namespace, semi, body) => {
            fmt!(with, "with");
            fmt!(node *namespace);
            fmt!(semi, ";");
            fmt!(node *body);
        },
        ASTType::Apply(f, arg) => {
            fmt!(node *f);
            fmt!(node *arg);
        },
        ASTType::Dynamic { meta, ast, close } => {
            fmt!(meta, "${{");
            fmt!(node *ast);
            fmt!(close, "}}");
        },
        ASTType::IndexSet(set, dot, attr) => {
            fmt!(node *set);
            fmt!(dot, ".");
            fmt!(node *attr);
        },
        ASTType::Unary(meta, op, expr) => {
            fmt!(meta, "{}", op);
            fmt!(node *expr);
        },
        ASTType::OrDefault { set, dot, attr, or, default } => {
            fmt!(node *set);
            fmt!(dot, ".");
            fmt!(node *attr);
            fmt!(or, "or");
            fmt!(node *default);
        },
        ASTType::Operation(x, (meta, op), y) => {
            fmt!(node *x);
            fmt!(meta, "{}", op);
            fmt!(node *y);
        }
    }
    Ok(())
}

impl<'a> fmt::Display for AST<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_node(f, &self.arena, &self.root)
    }
}
