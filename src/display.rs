use crate::{
    parser::*,
    tokenizer::{Interpol as TokenInterpol, Token, TokenKind, Trivia}
};

use std::fmt;

impl fmt::Display for Trivia {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Trivia::Newlines(amount) => for _ in 0..amount {
                write!(f, "\n")?;
            },
            Trivia::Spaces(amount) => for _ in 0..amount {
                write!(f, " ")?;
            },
            Trivia::Tabs(amount) => for _ in 0..amount {
                write!(f, "\t")?;
            },
            Trivia::Comment { span: _, multiline, ref content } => {
                if multiline {
                    write!(f, "/*")?;
                } else {
                    write!(f, "#")?;
                }
                write!(f, "{}", content)?;
                if multiline {
                    write!(f, "*/")?;
                }
            },
        }
        Ok(())
    }
}
impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            TokenKind::Assert => "assert",
            TokenKind::Else => "else",
            TokenKind::If => "if",
            TokenKind::Import => "import",
            TokenKind::In => "in",
            TokenKind::Inherit => "inherit",
            TokenKind::Let => "let",
            TokenKind::Rec => "rec",
            TokenKind::Then => "then",
            TokenKind::With => "with",
            TokenKind::CurlyBOpen => "{",
            TokenKind::CurlyBClose => "}",
            TokenKind::SquareBOpen => "[",
            TokenKind::SquareBClose => "]",
            TokenKind::Assign => "=",
            TokenKind::At => "@",
            TokenKind::Colon => ":",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::Ellipsis => "...",
            TokenKind::Question => "?",
            TokenKind::Semicolon => ";",
            TokenKind::ParenOpen => "(",
            TokenKind::ParenClose => ")",
            TokenKind::Concat => "++",
            TokenKind::Invert => "!",
            TokenKind::Merge => "//",
            TokenKind::Add => "+",
            TokenKind::Sub => "-",
            TokenKind::Mul => "*",
            TokenKind::Div => "/",
            TokenKind::And => "&&",
            TokenKind::Equal => "==",
            TokenKind::Implication => "->",
            TokenKind::Less => "<",
            TokenKind::LessOrEq => "<=",
            TokenKind::More => ">",
            TokenKind::MoreOrEq => ">=",
            TokenKind::NotEqual => "!=",
            TokenKind::Or => "||",
            TokenKind::Dynamic => "${",

            TokenKind::EOF
            | TokenKind::Ident
            | TokenKind::Interpol
            | TokenKind::Value => ""
        })
    }
}

fn fmt_trivia(f: &mut fmt::Formatter, trivia: &[Trivia]) -> fmt::Result {
    for trivia in trivia {
        write!(f, "{}", trivia)?;
    }
    Ok(())
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        macro_rules! fmt {
            ($meta:expr, $fmt:expr $(, $arg:expr)*) => {{
                fmt_trivia(f, &$meta.leading)?;
                write!(f, $fmt $(, $arg)*)?;
                fmt_trivia(f, &$meta.trailing)?;
            }};
        }
        match self {
            Token::Simple(kind) => write!(f, "{}", kind)?,
            Token::Dynamic(tokens, close) => {
                write!(f, "${{")?;
                for (meta, token) in tokens {
                    fmt!(meta, "{}", token);
                }
                fmt!(close, "}}");
            },
            Token::Ident(name) => write!(f, "{}", name)?,
            Token::Interpol { multiline, parts } => {
                if *multiline {
                    write!(f, "''")?;
                } else {
                    write!(f, "\"")?;
                }
                for part in parts {
                    match part {
                        TokenInterpol::Literal { original, content: _, span: _ } => write!(f, "{}", original)?,
                        TokenInterpol::Tokens(tokens, close) => {
                            write!(f, "${{")?;
                            for (meta, token) in tokens {
                                fmt!(meta, "{}", token);
                            }
                            fmt!(close, "}}");
                        }
                    }
                }
                if *multiline {
                    write!(f, "''")?;
                } else {
                    write!(f, "\"")?;
                }
            },
            Token::Value(val) => write!(f, "{}", val)?
        }
        Ok(())
    }
}
fn fmt_node(f: &mut fmt::Formatter, arena: &Arena, id: NodeId) -> fmt::Result {
    let node = &arena[id];

    macro_rules! fmt {
        ($meta:expr, $fmt:expr $(, $arg:expr)*) => {{
            fmt_trivia(f, &$meta.leading)?;
            write!(f, $fmt $(, $arg)*)?;
            fmt_trivia(f, &$meta.trailing)?;
        }};
    }

    match &node.data {
        Data::Error((_span, err)) => panic!("attempt to print out AST, but it has an error: {}", err),
        Data::Ident(meta, name) => fmt!(meta, "{}", name),
        Data::Interpol { meta, multiline } => {
            fmt_trivia(f, &meta.leading)?;
            write!(f, "{}", if *multiline { "''" } else { "\"" })?;
        },
        Data::InterpolLiteral { original, content: _ } => write!(f, "{}", original)?,
        Data::None => (),
        Data::Token(meta, token) => fmt!(meta, "{}", token),
        Data::Value(meta, value) => fmt!(meta, "{}", value)
    }

    if node.kind == ASTKind::InterpolAst {
        write!(f, "${{")?;
    }

    for child in node.children(arena) {
        fmt_node(f, arena, child)?;
    }

    if let Data::Interpol { meta, multiline } = &node.data {
        write!(f, "{}", if *multiline { "''" } else { "\"" })?;
        fmt_trivia(f, &meta.trailing)?;
    }

    Ok(())
}

impl<'a> fmt::Display for AST<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_node(f, &self.arena, self.root)
    }
}
