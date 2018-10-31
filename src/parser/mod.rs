//! The parser: turns a series of tokens into an AST

use crate::tokenizer::Token;

use rowan::{GreenNodeBuilder, SyntaxNode, SmolStr};

//pub mod types;

//use self::types::{Error as ErrorNode, TypedNode};

const OR: &'static str = "or";

/// An error that occured during parsing
#[derive(Clone, Debug, Fail, PartialEq)]
pub enum ParseError {
    #[fail(display = "can't bind pattern here, already bound before")]
    AlreadyBound,
    #[fail(display = "expected {:?}, found {:?}", _0, _1)]
    Expected(Token, Option<Token>),
    #[fail(display = "invalid type! expected {}", _0)]
    InvalidType(&'static str),
    #[fail(display = "unexpected eof")]
    UnexpectedEOF,
    #[fail(display = "unexpected eof, wanted {:?}", _0)]
    UnexpectedEOFWanted(Token),
    #[fail(display = "unexpected token {:?} not applicable in this context", _0)]
    Unexpected(Token)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ASTKind {
    Apply,
    Assert,
    Attribute,
    Dynamic,
    Error,
    Ident,
    IfElse,
    Import,
    IndexSet,
    Inherit,
    InheritFrom,
    Interpol,
    InterpolAst,
    InterpolLiteral,
    Lambda,
    Let,
    LetIn,
    List,
    ListItem,
    Operation,
    OrDefault,
    Paren,
    PatBind,
    PatEntry,
    Pattern,
    Root,
    Set,
    SetEntry,
    Token,
    Unary,
    Value,
    With
}
/// The type of a node in the AST
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NodeType {
    /// This node contains the elements to make up this kind, such as a set
    Marker(ASTKind),
    /// This type is a single token. Will probably not have any children.
    Token(Token)
}

/// Teaches the rowan library about rnix' preferred types
#[derive(Debug)]
pub struct Types;
impl rowan::Types for Types {
    type Kind = NodeType;
    type RootData = Vec<ParseError>;
}

pub type Node<R = rowan::OwnedRoot<Types>> = rowan::SyntaxNode<Types, R>;

struct Parser<I>
    where I: Iterator<Item = (Token, SmolStr)>
{
    builder: GreenNodeBuilder<Types>,
    errors: Vec<ParseError>,

    buffer: Vec<I::Item>,
    iter: I
}
impl<I> Parser<I>
    where I: Iterator<Item = (Token, SmolStr)>
{
    fn new(iter: I) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),

            buffer: Vec::with_capacity(1),
            iter
        }
    }

    fn peek_raw(&mut self) -> Option<&(Token, SmolStr)> {
        if self.buffer.is_empty() {
            if let Some(token) = self.iter.next() {
                self.buffer.push(token);
            }
        }
        self.buffer.last()
    }
    fn bump(&mut self) {
        let next = self.buffer.pop().or_else(|| self.iter.next());
        match next {
            Some((token, s)) => self.builder.leaf(NodeType::Token(token), s),
            None => self.errors.push(ParseError::UnexpectedEOF)
        }
    }
    fn peek_data(&mut self) -> Option<&(Token, SmolStr)> {
        while self.peek_raw().map(|(t, _)| t.is_trivia()).unwrap_or(false) {
            self.bump();
        }
        self.peek_raw()
    }
    fn peek(&mut self) -> Option<Token> {
        self.peek_data().map(|&(t, _)| t)
    }
    fn expect(&mut self, expected: Token) {
        if let Some(actual) = self.peek() {
            if actual != expected {
                self.builder.start_internal(NodeType::Marker(ASTKind::Error));
                while { self.bump(); self.peek().map(|actual| actual != expected).unwrap_or(false) } {}
                self.builder.finish_internal();
            }
            self.bump();
        } else {
            self.errors.push(ParseError::UnexpectedEOFWanted(expected));
        }
    }

    //fn parse_interpol(&mut self, meta: Meta, multiline: bool, values: Vec<TokenInterpol>) -> Result<ASTNode> {
    //    let mut parsed = NodeList::new();
    //    for value in values {
    //        parsed.push(match value {
    //            TokenInterpol::Literal { span, original, content } => self.insert(ASTNode {
    //                kind: ASTKind::InterpolLiteral,
    //                span,
    //                data: Data::InterpolLiteral { original, content },
    //                node: Node::default()
    //            }),
    //            TokenInterpol::Tokens(tokens, close) => {
    //                let parsed = self.parse_branch(tokens)?;
    //                let parsed_span = parsed.span;
    //                let parsed = self.insert(parsed);
    //                let close_span = close.span;
    //                let close = self.insert(ASTNode::from_token_kind(close, TokenKind::CurlyBClose));

    //                let children = self.chain(&[parsed, close]);

    //                self.insert(ASTNode {
    //                    kind: ASTKind::InterpolAst,
    //                    span: parsed_span.until(close_span),
    //                    data: Data::None,
    //                    node: Node::with_child(children)
    //                })
    //            }
    //        }, &mut self.arena);
    //    }
    //    Ok(ASTNode {
    //        kind: ASTKind::Interpol,
    //        span: meta.span,
    //        data: Data::Interpol { meta, multiline },
    //        node: Node::with_child(parsed.node())
    //    })
    //}
    fn next_attr(&mut self) {
        match self.peek() {
            //(meta, Token::Dynamic(tokens, close)) => {
            //    let open = ASTNode::from_token_kind(meta, TokenKind::Dynamic);
            //    let open_span = open.span;
            //    let open = self.insert(open);
            //    let parsed = self.parse_branch(tokens)?;
            //    let parsed = self.insert(parsed);
            //    let close_span = close.span;
            //    let close = self.insert(ASTNode::from_token_kind(close, TokenKind::CurlyBClose));
            //    let children = self.chain(&[open, parsed, close]);
            //    Ok(ASTNode {
            //        kind: ASTKind::Dynamic,
            //        span: open_span.until(close_span),
            //        data: Data::None,
            //        node: Node::with_child(children)
            //    })
            //},
            //(meta, Token::Interpol { multiline, parts }) => self.parse_interpol(meta, multiline, parts),
            Some(Token::Value) => self.bump(),
            _ => self.expect(Token::Ident)
        }
    }
    fn parse_attr(&mut self) {
        self.builder.start_internal(NodeType::Marker(ASTKind::Attribute));
        loop {
            self.next_attr();

            if self.peek() == Some(Token::Dot) {
                self.bump();
            } else {
                break;
            }
        }
        self.builder.finish_internal();
    }
    //fn next_ident(&mut self) -> Result<ASTNode> {
    //    match self.next()? {
    //        (meta, Token::Ident(name)) => Ok(ASTNode::from_ident(meta, name)),
    //        (meta, token) => Err((Some(meta.span), ParseError::Expected(TokenKind::Ident, Some(token.kind()))))
    //    }
    //}
    //fn parse_pattern(&mut self, open: Meta, bind: Option<(ASTNode, ASTNode)>) -> Result<ASTNode> {
    //    let start = bind.as_ref().map(|(ident, _)| ident.span).unwrap_or(open.span);

    //    let mut pattern = NodeList::new();
    //    let bind = if let Some((ident, at)) = bind {
    //        let ident_span = ident.span;
    //        let ident = self.insert(ident);
    //        let at_span = at.span;
    //        let at = self.insert(at);
    //        let children = self.chain(&[ident, at]);

    //        pattern.push(self.insert(ASTNode {
    //            kind: ASTKind::PatBind,
    //            span: ident_span.until(at_span),
    //            data: Data::None,
    //            node: Node::with_child(children)
    //        }), &mut self.arena);
    //        true
    //    } else {
    //        false
    //    };

    //    let open = ASTNode::from_token_kind(open, TokenKind::CurlyBOpen);
    //    let open = self.insert(open);
    //    pattern.push(open, &mut self.arena);

    //    loop {
    //        let mut entry = NodeList::new();

    //        let ident = match self.peek_kind() {
    //            Some(TokenKind::Ellipsis) => {
    //                let ellipsis = ASTNode::from_token(self.next().unwrap());
    //                pattern.push(self.insert(ellipsis), &mut self.arena);
    //                break;
    //            },
    //            Some(TokenKind::CurlyBClose) => break,
    //            _ => self.next_ident()?,
    //        };
    //        let ident_span = ident.span;
    //        let mut end_span = ident_span;
    //        entry.push(self.insert(ident), &mut self.arena);
    //        if self.peek_kind() == Some(TokenKind::Question) {
    //            let question = ASTNode::from_token(self.next().unwrap());
    //            let expr = self.parse_expr()?;
    //            end_span = expr.span;
    //            entry.push_all(&[
    //                self.insert(question),
    //                self.insert(expr),
    //            ], &mut self.arena);
    //        }
    //        let comma = match self.peek_kind() {
    //            Some(TokenKind::Comma) => {
    //                let comma = ASTNode::from_token(self.next().unwrap());
    //                end_span = comma.span;
    //                entry.push(self.insert(comma), &mut self.arena);
    //                true
    //            },
    //            _ => false
    //        };
    //        pattern.push(self.insert(ASTNode {
    //            kind: ASTKind::PatEntry,
    //            span: ident_span.until(end_span),
    //            data: Data::None,
    //            node: Node::with_child(entry.node())
    //        }), &mut self.arena);
    //        if !comma {
    //            break;
    //        }
    //    }

    //    let close = ASTNode::from_token(self.expect(TokenKind::CurlyBClose)?);
    //    let mut end_span = close.span;
    //    pattern.push(self.insert(close), &mut self.arena);

    //    if let Some(TokenKind::At) = self.peek_kind() {
    //        let at = ASTNode::from_token(self.next().unwrap());
    //        if bind {
    //            return Err((Some(at.span), ParseError::AlreadyBound));
    //        }
    //        let at_span = at.span;
    //        let at = self.insert(at);
    //        let ident = self.next_ident()?;
    //        end_span = ident.span;
    //        let ident = self.insert(ident);

    //        let children = self.chain(&[at, ident]);

    //        pattern.push(self.insert(ASTNode {
    //            kind: ASTKind::PatBind,
    //            span: at_span.until(end_span),
    //            data: Data::None,
    //            node: Node::with_child(children)
    //        }), &mut self.arena);
    //    }

    //    let pattern = self.insert(ASTNode {
    //        kind: ASTKind::Pattern,
    //        span: start.until(end_span),
    //        data: Data::None,
    //        node: Node::with_child(pattern.node())
    //    });
    //    let colon = ASTNode::from_token(self.expect(TokenKind::Colon)?);
    //    let colon = self.insert(colon);
    //    let expr = self.parse_expr()?;
    //    let expr_span = expr.span;
    //    let expr = self.insert(expr);

    //    let children = self.chain(&[pattern, colon, expr]);

    //    Ok(ASTNode {
    //        kind: ASTKind::Lambda,
    //        span: start.until(expr_span),
    //        data: Data::None,
    //        node: Node::with_child(children)
    //    })
    //}
    fn parse_set(&mut self, until: Token) {
        loop {
            match self.peek() {
                None => break,
                token if token == Some(until) => break,
                Some(Token::Inherit) => {
                    self.builder.start_internal(NodeType::Marker(ASTKind::Inherit));
                    self.bump();

                    if self.peek() == Some(Token::ParenOpen) {
                        self.builder.start_internal(NodeType::Marker(ASTKind::InheritFrom));
                        self.bump();
                        self.parse_expr();
                        self.expect(Token::ParenClose);
                        self.builder.finish_internal();
                    }

                    while let Some(Token::Ident) = self.peek() {
                        self.bump();
                    }

                    self.expect(Token::Semicolon);
                    self.builder.finish_internal();
                },
                Some(_) => {
                    self.builder.start_internal(NodeType::Marker(ASTKind::SetEntry));
                    self.parse_attr();
                    self.expect(Token::Assign);
                    self.parse_expr();
                    self.expect(Token::Semicolon);
                    self.builder.finish_internal();
                }
            }
        }
        self.bump(); // the final close, like '}'
    }
    fn parse_val(&mut self) {
        let checkpoint = self.builder.wrap_checkpoint();
        match self.peek() {
            Some(Token::ParenOpen) => {
                self.builder.start_internal(NodeType::Marker(ASTKind::Paren));
                self.bump();
                self.parse_expr();
                self.bump();
                self.builder.finish_internal();
            },
            Some(Token::Import) => {
                self.builder.start_internal(NodeType::Marker(ASTKind::Import));
                self.bump();
                self.parse_val();
                self.builder.finish_internal();
            },
            Some(Token::Rec) => {
                self.builder.start_internal(NodeType::Marker(ASTKind::Set));
                self.bump();
                self.expect(Token::CurlyBOpen);
                self.parse_set(Token::CurlyBClose);
                self.builder.finish_internal();
            },
            Some(Token::CurlyBOpen) => {
            //    let temporary = self.next()?;
            //    match (temporary.1.kind(), self.peek_kind()) {
            //        (TokenKind::Ident, Some(TokenKind::Comma))
            //                | (TokenKind::Ident, Some(TokenKind::Question))
            //                | (TokenKind::Ellipsis, Some(TokenKind::CurlyBClose))
            //                | (TokenKind::Ident, Some(TokenKind::CurlyBClose))
            //                | (TokenKind::CurlyBClose, Some(TokenKind::Colon))
            //                | (TokenKind::CurlyBClose, Some(TokenKind::At)) => {
            //            // We did a lookahead, put it back
            //            self.buffer.push(temporary);
            //            self.parse_pattern(meta, None)?
            //        },
            //        _ => {
            //            // We did a lookahead, put it back
            //            self.buffer.push(temporary);
            //
                        self.builder.start_internal(NodeType::Marker(ASTKind::Set));
                        self.bump();
                        self.parse_set(Token::CurlyBClose);
                        self.builder.finish_internal();

            //            let open_span = meta.span;
            //            let open = self.insert(ASTNode::from_token_kind(meta, kind));
            //            let (values, close) = self.parse_set(TokenKind::CurlyBClose)?;
            //            let close_span = close.span;
            //            let close = self.insert(close);

            //            let children = if let Some(values) = values {
            //                self.chain(&[open, values, close])
            //            } else {
            //                self.chain(&[open, close])
            //            };

            //            ASTNode {
            //                kind: ASTKind::Set,
            //                span: open_span.until(close_span),
            //                data: Data::None,
            //                node: Node::with_child(children)
            //            }
            //        }
            //    }
            },
            //(TokenKind::SquareBOpen, _) => {
            //    let mut values = NodeList::new();

            //    let open = ASTNode::from_token_kind(meta, kind);
            //    let open_span = open.span;
            //    values.push(self.insert(open), &mut self.arena);

            //    loop {
            //        match self.peek_kind() {
            //            None | Some(TokenKind::SquareBClose) => break,
            //            _ => {
            //                let val = self.parse_val()?;
            //                let val_span = val.span;
            //                let val = self.insert(val);
            //                values.push(self.insert(ASTNode {
            //                    kind: ASTKind::ListItem,
            //                    span: val_span,
            //                    data: Data::None,
            //                    node: Node::with_child(val)
            //                }), &mut self.arena);
            //            }
            //        }
            //    }
            //    let close = ASTNode::from_token(self.expect(TokenKind::SquareBClose)?);
            //    let close_span = close.span;
            //    values.push(self.insert(close), &mut self.arena);

            //    ASTNode {
            //        kind: ASTKind::List,
            //        span: open_span.until(close_span),
            //        data: Data::None,
            //        node: Node::with_child(values.node())
            //    }
            //},
            //(_, Token::Dynamic(tokens, close)) => {
            //    let open = ASTNode::from_token_kind(meta, kind);
            //    let open_span = open.span;
            //    let open = self.insert(open);
            //    let parsed = self.parse_branch(tokens)?;
            //    let parsed = self.insert(parsed);
            //    let close_span = close.span;
            //    let close = self.insert(ASTNode::from_token_kind(close, TokenKind::CurlyBClose));
            //    let children = self.chain(&[open, parsed, close]);
            //    ASTNode {
            //        kind: ASTKind::Dynamic,
            //        span: open_span.until(close_span),
            //        data: Data::None,
            //        node: Node::with_child(children)
            //    }
            //},
            //(_, Token::Value(val)) => ASTNode::from_value(meta, val),
            //(_, Token::Ident(name)) => if self.peek_kind() == Some(TokenKind::At) {
            //    let ident = ASTNode::from_ident(meta, name);
            //    let at = ASTNode::from_token(self.next().unwrap());
            //    let (open, _) = self.expect(TokenKind::CurlyBOpen)?;

            //    self.parse_pattern(open, Some((ident, at)))?
            //} else {
            //    ASTNode::from_ident(meta, name)
            //},
            //(_, Token::Interpol { multiline, parts }) => self.parse_interpol(meta, multiline, parts)?,
            //(kind, _) => return Err((Some(meta.span), ParseError::Unexpected(kind)))
            Some(Token::Ident)
            | Some(Token::Value) => self.bump(),
            _ => {
                self.builder.start_internal(NodeType::Marker(ASTKind::Error));
                self.bump();
                self.builder.finish_internal();
            }
        }

        while self.peek() == Some(Token::Dot) {
            self.builder.wrap_internal(checkpoint, NodeType::Marker(ASTKind::IndexSet));
            self.bump();
            self.next_attr();
            self.builder.finish_internal();

            match self.peek_data() {
                Some((Token::Ident, s)) if s == OR => {
                    self.builder.wrap_internal(checkpoint, NodeType::Marker(ASTKind::OrDefault));
                    self.bump();
                    self.parse_val();
                    self.builder.finish_internal();
                },
                _ => ()
            }
        }
    }
    fn parse_fn(&mut self) {
        let checkpoint = self.builder.wrap_checkpoint();
        self.parse_val();

        while self.peek().map(|t| t.is_fn_arg()).unwrap_or(false) {
            self.builder.wrap_internal(checkpoint, NodeType::Marker(ASTKind::Apply));
            self.parse_val();
            self.builder.finish_internal();
        }
    }
    fn parse_negate(&mut self) {
        if self.peek() == Some(Token::Sub) {
            self.builder.start_internal(NodeType::Marker(ASTKind::Unary));
            self.bump();
            self.parse_negate();
            self.builder.finish_internal();
        } else {
            self.parse_fn()
        }
    }
    fn handle_operation(&mut self, once: bool, next: fn(&mut Self), ops: &[Token]) {
        let checkpoint = self.builder.wrap_checkpoint();
        next(self);
        while self.peek().map(|t| ops.contains(&t)).unwrap_or(false) {
            self.builder.wrap_internal(checkpoint, NodeType::Marker(ASTKind::Operation));
            self.bump();
            next(self);
            self.builder.finish_internal();
            if once {
                break;
            }
        }
    }
    fn parse_isset(&mut self) {
        self.handle_operation(false, Self::parse_negate, &[Token::Question])
    }
    fn parse_concat(&mut self) {
        self.handle_operation(false, Self::parse_isset, &[Token::Concat])
    }
    fn parse_mul(&mut self) {
        self.handle_operation(false, Self::parse_concat, &[Token::Mul, Token::Div])
    }
    fn parse_add(&mut self) {
        self.handle_operation(false, Self::parse_mul, &[Token::Add, Token::Sub])
    }
    fn parse_invert(&mut self) {
        if self.peek() == Some(Token::Invert) {
            self.builder.start_internal(NodeType::Marker(ASTKind::Unary));
            self.bump();
            self.parse_invert();
            self.builder.finish_internal();
        } else {
            self.parse_add()
        }
    }
    fn parse_merge(&mut self) {
        self.handle_operation(false, Self::parse_invert, &[Token::Merge])
    }
    fn parse_compare(&mut self) {
        self.handle_operation(true, Self::parse_merge, &[Token::Less, Token::LessOrEq, Token::More, Token::MoreOrEq])
    }
    fn parse_equal(&mut self) {
        self.handle_operation(true, Self::parse_compare, &[Token::Equal, Token::NotEqual])
    }
    fn parse_and(&mut self) {
        self.handle_operation(false, Self::parse_equal, &[Token::And])
    }
    fn parse_or(&mut self) {
        self.handle_operation(false, Self::parse_and, &[Token::Or])
    }
    fn parse_implication(&mut self) {
        self.handle_operation(false, Self::parse_or, &[Token::Implication])
    }
    #[inline(always)]
    fn parse_math(&mut self) {
        // Always point this to the lowest-level math function there is
        self.parse_implication()
    }
    /// Parse Nix code into an AST
    pub fn parse_expr(&mut self) {
        match self.peek() {
            Some(Token::Let) => {
                let checkpoint = self.builder.wrap_checkpoint();
                self.bump();

                if self.peek() == Some(Token::CurlyBOpen) {
                    self.builder.wrap_internal(checkpoint, NodeType::Marker(ASTKind::Let));
                    self.bump();
                    self.parse_set(Token::CurlyBClose);
                    self.builder.finish_internal();
                } else {
                    self.builder.wrap_internal(checkpoint, NodeType::Marker(ASTKind::LetIn));
                    self.parse_set(Token::In);
                    self.parse_expr();
                    self.builder.finish_internal();
                }
            },
            Some(Token::With) => {
                self.builder.start_internal(NodeType::Marker(ASTKind::With));
                self.bump();
                self.parse_expr();
                self.expect(Token::Semicolon);
                self.parse_expr();
                self.builder.finish_internal();
            },
            //Some(TokenKind::If) => {
            //    let if_ = ASTNode::from_token(self.next().unwrap());
            //    let if_span = if_.span;
            //    let if_ = self.insert(if_);
            //    let condition = self.parse_expr()?;
            //    let condition = self.insert(condition);
            //    let then = ASTNode::from_token(self.expect(TokenKind::Then)?);
            //    let then = self.insert(then);
            //    let body = self.parse_expr()?;
            //    let body = self.insert(body);
            //    let else_ = ASTNode::from_token(self.expect(TokenKind::Else)?);
            //    let else_ = self.insert(else_);
            //    let else_body = self.parse_expr()?;
            //    let else_body_span = else_body.span;
            //    let else_body = self.insert(else_body);

            //    let children = self.chain(&[if_, condition, then, body, else_, else_body]);

            //    ASTNode {
            //        kind: ASTKind::IfElse,
            //        span: if_span.until(else_body_span),
            //        data: Data::None,
            //        node: Node::with_child(children)
            //    }
            //},
            Some(Token::Assert) => {
                self.builder.start_internal(NodeType::Marker(ASTKind::Assert));
                self.bump();
                self.parse_expr();
                self.expect(Token::Semicolon);
                self.parse_expr();
                self.builder.finish_internal();
            },
            _ => {
                self.parse_math();

                //if val.kind == ASTKind::Ident && self.peek_kind() == Some(TokenKind::Colon) {
                //    let val_span = val.span;
                //    let val = self.insert(val);
                //    let colon = ASTNode::from_token(self.next().unwrap());
                //    let colon = self.insert(colon);
                //    let expr = self.parse_expr()?;
                //    let expr_span = expr.span;
                //    let expr = self.insert(expr);

                //    let children = self.chain(&[val, colon, expr]);

                //    ASTNode {
                //        kind: ASTKind::Lambda,
                //        span: val_span.until(expr_span),
                //        data: Data::None,
                //        node: Node::with_child(children)
                //    }
                //}
            }
        }
    }
}

/// Convenience function for turning an iterator of tokens into an AST
pub fn parse<I>(iter: I) -> Node
    where I: IntoIterator<Item = (Token, SmolStr)>
{
    let mut parser = Parser::new(iter.into_iter());
    parser.builder.start_internal(NodeType::Marker(ASTKind::Root));
    parser.parse_expr();
    parser.builder.finish_internal();
    SyntaxNode::new(parser.builder.finish(), parser.errors)
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::fmt::Write;

    fn stringify(out: &mut String, indent: usize, node: &Node) {
        writeln!(out, "{:indent$}{:?}", "", node, indent = indent).unwrap();
        for child in node.children() {
            stringify(out, indent+2, &child);
        }
    }

    macro_rules! assert_eq {
        ([$(($token:expr, $str:expr)),*], $expected:expr) => {
            let mut actual = String::new();
            stringify(&mut actual, 0, &parse(vec![$(($token, $str.into())),*]));
            if actual != $expected {
                eprintln!("--- Actual ---");
                eprintln!("{}", actual);
                eprintln!("-- Expected ---");
                eprintln!("{}", $expected);
                eprintln!("--- End ---");
                panic!("Tests did not match");
            }
        };
    }

    #[test]
    fn set() {
        assert_eq!(
            [
                (Token::CurlyBOpen, "{"),
                (Token::Whitespace, " "),

                (Token::Ident, "meaning_of_life"),
                (Token::Whitespace, " "),
                (Token::Assign, "="),
                (Token::Whitespace, " "),
                (Token::Value, "42"),
                (Token::Semicolon, ";"),

                (Token::Ident, "H4X0RNUM83R"),
                (Token::Whitespace, " "),
                (Token::Assign, "="),
                (Token::Whitespace, " "),
                (Token::Value, "1.337"),
                (Token::Semicolon, ";"),

                (Token::Whitespace, " "),
                (Token::CurlyBClose, "}")
            ],
            "\
Marker(Root)@[0; 45)
  Marker(Set)@[0; 45)
    Token(CurlyBOpen)@[0; 1)
    Token(Whitespace)@[1; 2)
    Marker(SetEntry)@[2; 23)
      Marker(Attribute)@[2; 18)
        Token(Ident)@[2; 17)
        Token(Whitespace)@[17; 18)
      Token(Assign)@[18; 19)
      Token(Whitespace)@[19; 20)
      Token(Value)@[20; 22)
      Token(Semicolon)@[22; 23)
    Marker(SetEntry)@[23; 43)
      Marker(Attribute)@[23; 35)
        Token(Ident)@[23; 34)
        Token(Whitespace)@[34; 35)
      Token(Assign)@[35; 36)
      Token(Whitespace)@[36; 37)
      Token(Value)@[37; 42)
      Token(Semicolon)@[42; 43)
    Token(Whitespace)@[43; 44)
    Token(CurlyBClose)@[44; 45)
"
        );
        assert_eq!(
            [
                (Token::Rec, "rec"),
                (Token::CurlyBOpen, "{"),
                (Token::Ident, "test"),
                (Token::Assign, "="),
                (Token::Value, "1"),
                (Token::Semicolon, ";"),
                (Token::CurlyBClose, "}")
            ],
            "\
Marker(Root)@[0; 12)
  Marker(Set)@[0; 12)
    Token(Rec)@[0; 3)
    Token(CurlyBOpen)@[3; 4)
    Marker(SetEntry)@[4; 11)
      Marker(Attribute)@[4; 8)
        Token(Ident)@[4; 8)
      Token(Assign)@[8; 9)
      Token(Value)@[9; 10)
      Token(Semicolon)@[10; 11)
    Token(CurlyBClose)@[11; 12)
"
        );
        assert_eq!(
            [
                (Token::CurlyBOpen, "{"),
                (Token::CurlyBClose, "}")
            ],
            "\
Marker(Root)@[0; 2)
  Marker(Set)@[0; 2)
    Token(CurlyBOpen)@[0; 1)
    Token(CurlyBClose)@[1; 2)
"
        );
//        assert_eq!(
//            [
//                TokenKind::CurlyBOpen,
//
//                Token::Ident("a".into()),
//                    TokenKind::Dot, Token::Value("b".into()),
//                TokenKind::Assign, Token::Value(1.into()), TokenKind::Semicolon,
//
//                Token::Interpol {
//                    multiline: false,
//                    parts: vec![
//                        TokenInterpol::Literal {
//                            span: Span::default(),
//                            original: "c".into(),
//                            content: "c".into()
//                        }
//                    ]
//                },
//                TokenKind::Dot, Token::Dynamic(vec![(Meta::default(), Token::Ident("d".into()))], Meta::default()),
//                TokenKind::Assign, Token::Value(2.into()), TokenKind::Semicolon,
//
//                TokenKind::CurlyBClose
//            ],
//            "\
//Set
//  Token = CurlyBOpen
//  SetEntry
//    Attribute
//      Ident = a
//      Token = Dot
//      Value = \"b\"
//    Token = Assign
//    Value = 1
//    Token = Semicolon
//  SetEntry
//    Attribute
//      Interpol { multiline: false }
//        InterpolLiteral = \"c\"
//      Token = Dot
//      Dynamic
//        Token = Dynamic
//        Ident = d
//        Token = CurlyBClose
//    Token = Assign
//    Value = 2
//    Token = Semicolon
//  Token = CurlyBClose
//"
//        );
    }
    #[test]
    fn math() {
        assert_eq!(
            [
                (Token::Value, "1"),
                (Token::Whitespace, " "),
                (Token::Add, "+"),
                (Token::Whitespace, " "),
                (Token::Value, "2"),
                (Token::Whitespace, " "),
                (Token::Add, "+"),
                (Token::Whitespace, " "),
                (Token::Value, "3"),
                (Token::Whitespace, " "),
                (Token::Mul, "*"),
                (Token::Whitespace, " "),
                (Token::Value, "4")
            ],
            "\
Marker(Root)@[0; 13)
  Marker(Operation)@[0; 13)
    Marker(Operation)@[0; 6)
      Token(Value)@[0; 1)
      Token(Whitespace)@[1; 2)
      Token(Add)@[2; 3)
      Token(Whitespace)@[3; 4)
      Token(Value)@[4; 5)
      Token(Whitespace)@[5; 6)
    Token(Add)@[6; 7)
    Marker(Operation)@[7; 13)
      Token(Whitespace)@[7; 8)
      Token(Value)@[8; 9)
      Token(Whitespace)@[9; 10)
      Token(Mul)@[10; 11)
      Token(Whitespace)@[11; 12)
      Token(Value)@[12; 13)
"
        );
        assert_eq!(
            [
                (Token::Value, "5"),
                (Token::Mul, "*"),
                (Token::Sub, "-"),
                (Token::ParenOpen, "("),
                (Token::Value, "3"),
                (Token::Sub, "-"),
                (Token::Value, "2"),
                (Token::ParenClose, ")")
            ],
            "\
Marker(Root)@[0; 8)
  Marker(Operation)@[0; 8)
    Token(Value)@[0; 1)
    Token(Mul)@[1; 2)
    Marker(Unary)@[2; 8)
      Token(Sub)@[2; 3)
      Marker(Paren)@[3; 8)
        Token(ParenOpen)@[3; 4)
        Marker(Operation)@[4; 7)
          Token(Value)@[4; 5)
          Token(Sub)@[5; 6)
          Token(Value)@[6; 7)
        Token(ParenClose)@[7; 8)
"
        );
    }
    #[test]
    fn let_in() {
        assert_eq!(
            [
                (Token::Let, "let"),
                    (Token::Whitespace, " "),
                    (Token::Ident, "a"),
                    (Token::Whitespace, " "),
                    (Token::Assign, "="),
                    (Token::Whitespace, " "),
                    (Token::Value, "42"),
                    (Token::Semicolon, ";"),
                (Token::Whitespace, " "),
                (Token::In, "in"),
                    (Token::Whitespace, " "),
                    (Token::Ident, "a")
            ],
            "\
Marker(Root)@[0; 16)
  Marker(LetIn)@[0; 16)
    Token(Let)@[0; 3)
    Token(Whitespace)@[3; 4)
    Marker(SetEntry)@[4; 11)
      Marker(Attribute)@[4; 6)
        Token(Ident)@[4; 5)
        Token(Whitespace)@[5; 6)
      Token(Assign)@[6; 7)
      Token(Whitespace)@[7; 8)
      Token(Value)@[8; 10)
      Token(Semicolon)@[10; 11)
    Token(Whitespace)@[11; 12)
    Token(In)@[12; 14)
    Token(Whitespace)@[14; 15)
    Token(Ident)@[15; 16)
"
        );
    }
    #[test]
    fn let_legacy_syntax() {
        assert_eq!(
            [
                (Token::Let, "let"),
                (Token::CurlyBOpen, "{"),
                    (Token::Ident, "a"),
                        (Token::Assign, "="),
                        (Token::Value, "42"),
                        (Token::Semicolon, ";"),
                    (Token::Ident, "body"),
                        (Token::Assign, "="),
                        (Token::Ident, "a"),
                        (Token::Semicolon, ";"),
                (Token::CurlyBClose, "}")
            ],
            "\
Marker(Root)@[0; 17)
  Marker(Let)@[0; 17)
    Token(Let)@[0; 3)
    Token(CurlyBOpen)@[3; 4)
    Marker(SetEntry)@[4; 9)
      Marker(Attribute)@[4; 5)
        Token(Ident)@[4; 5)
      Token(Assign)@[5; 6)
      Token(Value)@[6; 8)
      Token(Semicolon)@[8; 9)
    Marker(SetEntry)@[9; 16)
      Marker(Attribute)@[9; 13)
        Token(Ident)@[9; 13)
      Token(Assign)@[13; 14)
      Token(Ident)@[14; 15)
      Token(Semicolon)@[15; 16)
    Token(CurlyBClose)@[16; 17)
"
        );
    }
//    #[test]
//    fn interpolation() {
//        assert_eq!(
//            [
//                Token::Interpol {
//                    multiline: false,
//                    parts: vec![
//                        TokenInterpol::Literal {
//                            span: Span::default(),
//                            original: "Hello, ".into(),
//                            content: "Hello, ".into()
//                        },
//                        TokenInterpol::Tokens(
//                            vec![
//                                (Meta::default(), TokenKind::CurlyBOpen.into()),
//                                (Meta::default(), Token::Ident("world".into())),
//                                (Meta::default(), TokenKind::Assign.into()),
//                                (Meta::default(), Token::Value("World".into())),
//                                (Meta::default(), TokenKind::Semicolon.into()),
//                                (Meta::default(), TokenKind::CurlyBClose.into()),
//                                (Meta::default(), TokenKind::Dot.into()),
//                                (Meta::default(), Token::Ident("world".into()))
//                            ],
//                            Meta::default()
//                        ),
//                        TokenInterpol::Literal {
//                            span: Span::default(),
//                            original: "!".into(),
//                            content: "!".into()
//                        }
//                    ]
//                }
//            ],
//            "\
//Interpol { multiline: false }
//  InterpolLiteral = \"Hello, \"
//  InterpolAst
//    IndexSet
//      Set
//        Token = CurlyBOpen
//        SetEntry
//          Attribute
//            Ident = world
//          Token = Assign
//          Value = \"World\"
//          Token = Semicolon
//        Token = CurlyBClose
//      Token = Dot
//      Ident = world
//    Token = CurlyBClose
//  InterpolLiteral = \"!\"
//"
//        );
//    }
    #[test]
    fn index_set() {
        assert_eq!(
            [
                (Token::Ident, "a"),
                (Token::Dot, "."),
                (Token::Ident, "b"),
                (Token::Dot, "."),
                (Token::Ident, "c")
            ],
            "\
Marker(Root)@[0; 5)
  Marker(IndexSet)@[0; 5)
    Marker(IndexSet)@[0; 3)
      Token(Ident)@[0; 1)
      Token(Dot)@[1; 2)
      Token(Ident)@[2; 3)
    Token(Dot)@[3; 4)
    Token(Ident)@[4; 5)
"
        );
        assert_eq!(
            [
                (Token::CurlyBOpen, "{"),
                    (Token::Ident, "a"),
                        (Token::Dot, "."),
                        (Token::Ident, "b"),
                        (Token::Dot, "."),
                        (Token::Ident, "c"),
                    (Token::Assign, "="),
                    (Token::Value, "1"),
                    (Token::Semicolon, ";"),
                (Token::CurlyBClose, "}")
            ],
            "\
Marker(Root)@[0; 10)
  Marker(Set)@[0; 10)
    Token(CurlyBOpen)@[0; 1)
    Marker(SetEntry)@[1; 9)
      Marker(Attribute)@[1; 6)
        Token(Ident)@[1; 2)
        Token(Dot)@[2; 3)
        Token(Ident)@[3; 4)
        Token(Dot)@[4; 5)
        Token(Ident)@[5; 6)
      Token(Assign)@[6; 7)
      Token(Value)@[7; 8)
      Token(Semicolon)@[8; 9)
    Token(CurlyBClose)@[9; 10)
"
        );
//        assert_eq!(
//            [
//                Token::Ident("test".into()),
//                    TokenKind::Dot, Token::Value("invalid ident".into()),
//                    TokenKind::Dot, Token::Interpol {
//                        multiline: false,
//                        parts: vec![
//                            TokenInterpol::Literal {
//                                span: Span::default(),
//                                original: "hi".into(),
//                                content: "hi".into()
//                            }
//                        ]
//                    },
//                    TokenKind::Dot, Token::Dynamic(
//                        vec![(Meta::default(), Token::Ident("a".into()))],
//                        Meta::default()
//                    )
//            ],
//            "\
//IndexSet
//  IndexSet
//    IndexSet
//      Ident = test
//      Token = Dot
//      Value = \"invalid ident\"
//    Token = Dot
//    Interpol { multiline: false }
//      InterpolLiteral = \"hi\"
//  Token = Dot
//  Dynamic
//    Token = Dynamic
//    Ident = a
//    Token = CurlyBClose
//"
//        );
    }
    #[test]
    fn isset() {
        assert_eq!(
            [
                (Token::Ident, "a"),
                (Token::Question, "?"),
                (Token::Value, "\"b\""),
                (Token::And, "&&"),
                (Token::Value, "true")
            ],
            "\
Marker(Root)@[0; 11)
  Marker(Operation)@[0; 11)
    Marker(Operation)@[0; 5)
      Token(Ident)@[0; 1)
      Token(Question)@[1; 2)
      Token(Value)@[2; 5)
    Token(And)@[5; 7)
    Token(Value)@[7; 11)
"
        );
        assert_eq!(
            [
                (Token::Ident, "a"),
                    (Token::Dot, "."),
                    (Token::Ident, "b"),
                    (Token::Dot, "."),
                    (Token::Ident, "c"),
                (Token::Ident, OR),
                (Token::Value, "1"),
                (Token::Add, "+"),
                (Token::Value, "1")
            ],
            "\
Marker(Root)@[0; 10)
  Marker(Operation)@[0; 10)
    Marker(OrDefault)@[0; 8)
      Marker(IndexSet)@[0; 5)
        Marker(IndexSet)@[0; 3)
          Token(Ident)@[0; 1)
          Token(Dot)@[1; 2)
          Token(Ident)@[2; 3)
        Token(Dot)@[3; 4)
        Token(Ident)@[4; 5)
      Token(Ident)@[5; 7)
      Token(Value)@[7; 8)
    Token(Add)@[8; 9)
    Token(Value)@[9; 10)
"
        );
    }
    #[test]
    fn merge() {
        assert_eq!(
            [
                (Token::CurlyBOpen, "{"),
                (Token::Ident, "a"),
                (Token::Assign, "="),
                (Token::Value, "1"),
                (Token::Semicolon, ";"),
                (Token::CurlyBClose, "}"),
                (Token::Merge, "//"),
                (Token::CurlyBOpen, "{"),
                (Token::Ident, "b"),
                (Token::Assign, "="),
                (Token::Value, "2"),
                (Token::Semicolon, ";"),
                (Token::CurlyBClose, "}")
            ],
            "\
Marker(Root)@[0; 14)
  Marker(Operation)@[0; 14)
    Marker(Set)@[0; 6)
      Token(CurlyBOpen)@[0; 1)
      Marker(SetEntry)@[1; 5)
        Marker(Attribute)@[1; 2)
          Token(Ident)@[1; 2)
        Token(Assign)@[2; 3)
        Token(Value)@[3; 4)
        Token(Semicolon)@[4; 5)
      Token(CurlyBClose)@[5; 6)
    Token(Merge)@[6; 8)
    Marker(Set)@[8; 14)
      Token(CurlyBOpen)@[8; 9)
      Marker(SetEntry)@[9; 13)
        Marker(Attribute)@[9; 10)
          Token(Ident)@[9; 10)
        Token(Assign)@[10; 11)
        Token(Value)@[11; 12)
        Token(Semicolon)@[12; 13)
      Token(CurlyBClose)@[13; 14)
"
        );
    }
    #[test]
    fn with() {
        assert_eq!(
            [
                (Token::With, "with"),
                (Token::Ident, "namespace"),
                (Token::Semicolon, ";"),
                (Token::Ident, "expr")
            ],
            "\
Marker(Root)@[0; 18)
  Marker(With)@[0; 18)
    Token(With)@[0; 4)
    Token(Ident)@[4; 13)
    Token(Semicolon)@[13; 14)
    Token(Ident)@[14; 18)
"
        );
    }
    #[test]
    fn import() {
        assert_eq!(
            [
                (Token::Import, "import"),
                (Token::Value, "<nixpkgs>"),
                (Token::CurlyBOpen, "{"),
                (Token::CurlyBClose, "}")
            ],
            "\
Marker(Root)@[0; 17)
  Marker(Apply)@[0; 17)
    Marker(Import)@[0; 15)
      Token(Import)@[0; 6)
      Token(Value)@[6; 15)
    Marker(Set)@[15; 17)
      Token(CurlyBOpen)@[15; 16)
      Token(CurlyBClose)@[16; 17)
"
        );
    }
    #[test]
    fn assert() {
        assert_eq!(
            [
                (Token::Assert, "assert"),
                (Token::Ident, "a"),
                (Token::Equal, "=="),
                (Token::Ident, "b"),
                (Token::Semicolon, ";"),
                (Token::Value, "\"a == b\"")
            ],
            "\
Marker(Root)@[0; 19)
  Marker(Assert)@[0; 19)
    Token(Assert)@[0; 6)
    Marker(Operation)@[6; 10)
      Token(Ident)@[6; 7)
      Token(Equal)@[7; 9)
      Token(Ident)@[9; 10)
    Token(Semicolon)@[10; 11)
    Token(Value)@[11; 19)
"
        );
    }
    #[test]
    fn inherit() {
        assert_eq!(
            [
                (Token::CurlyBOpen, "{"),
                    (Token::Ident, "a"),
                        (Token::Assign, "="),
                        (Token::Value, "1"),
                        (Token::Semicolon, ";"),
                    (Token::Inherit, "inherit"),
                        (Token::Whitespace, " "),
                        (Token::Ident, "b"),
                        (Token::Whitespace, " "),
                        (Token::Ident, "c"),
                        (Token::Semicolon, ";"),
                    (Token::Inherit, "inherit"),
                        (Token::Whitespace, " "),
                        (Token::ParenOpen, "("),
                        (Token::Ident, "set"),
                        (Token::ParenClose, ")"),
                        (Token::Whitespace, " "),
                        (Token::Ident, "d"),
                        (Token::Whitespace, " "),
                        (Token::Ident, "e"),
                        (Token::Semicolon, ";"),
                (Token::CurlyBClose, "}")
            ],
            "\
Marker(Root)@[0; 36)
  Marker(Set)@[0; 36)
    Token(CurlyBOpen)@[0; 1)
    Marker(SetEntry)@[1; 5)
      Marker(Attribute)@[1; 2)
        Token(Ident)@[1; 2)
      Token(Assign)@[2; 3)
      Token(Value)@[3; 4)
      Token(Semicolon)@[4; 5)
    Marker(Inherit)@[5; 17)
      Token(Inherit)@[5; 12)
      Token(Whitespace)@[12; 13)
      Token(Ident)@[13; 14)
      Token(Whitespace)@[14; 15)
      Token(Ident)@[15; 16)
      Token(Semicolon)@[16; 17)
    Marker(Inherit)@[17; 35)
      Token(Inherit)@[17; 24)
      Token(Whitespace)@[24; 25)
      Marker(InheritFrom)@[25; 30)
        Token(ParenOpen)@[25; 26)
        Token(Ident)@[26; 29)
        Token(ParenClose)@[29; 30)
      Token(Whitespace)@[30; 31)
      Token(Ident)@[31; 32)
      Token(Whitespace)@[32; 33)
      Token(Ident)@[33; 34)
      Token(Semicolon)@[34; 35)
    Token(CurlyBClose)@[35; 36)
"
        );
    }
//    #[test]
//    fn ifs() {
//        assert_eq!(
//            [
//                Token::Value(false.into()), TokenKind::Implication,
//                TokenKind::Invert, Token::Value(false.into()),
//                TokenKind::And,
//                Token::Value(false.into()), TokenKind::Equal, Token::Value(true.into()),
//                TokenKind::Or,
//                Token::Value(true.into())
//            ],
//            "\
//Operation
//  Value = false
//  Token = Implication
//  Operation
//    Operation
//      Unary
//        Token = Invert
//        Value = false
//      Token = And
//      Operation
//        Value = false
//        Token = Equal
//        Value = true
//    Token = Or
//    Value = true
//"
//        );
//        assert_eq!(
//            [
//                Token::Value(1.into()), TokenKind::Less, Token::Value(2.into()),
//                TokenKind::Or,
//                Token::Value(2.into()), TokenKind::LessOrEq, Token::Value(2.into()),
//                TokenKind::And,
//                Token::Value(2.into()), TokenKind::More, Token::Value(1.into()),
//                TokenKind::And,
//                Token::Value(2.into()), TokenKind::MoreOrEq, Token::Value(2.into())
//            ],
//            "\
//Operation
//  Operation
//    Value = 1
//    Token = Less
//    Value = 2
//  Token = Or
//  Operation
//    Operation
//      Operation
//        Value = 2
//        Token = LessOrEq
//        Value = 2
//      Token = And
//      Operation
//        Value = 2
//        Token = More
//        Value = 1
//    Token = And
//    Operation
//      Value = 2
//      Token = MoreOrEq
//      Value = 2
//"
//        );
//        assert_eq!(
//            [
//                Token::Value(1.into()), TokenKind::Equal, Token::Value(1.into()),
//                TokenKind::And,
//                Token::Value(2.into()), TokenKind::NotEqual, Token::Value(3.into())
//            ],
//            "\
//Operation
//  Operation
//    Value = 1
//    Token = Equal
//    Value = 1
//  Token = And
//  Operation
//    Value = 2
//    Token = NotEqual
//    Value = 3
//"
//        );
//        assert_eq!(
//            [
//                TokenKind::If, Token::Value(false.into()), TokenKind::Then,
//                    Token::Value(1.into()),
//                TokenKind::Else,
//                    TokenKind::If, Token::Value(true.into()), TokenKind::Then,
//                        Token::Value(2.into()),
//                    TokenKind::Else,
//                        Token::Value(3.into())
//            ],
//            "\
//IfElse
//  Token = If
//  Value = false
//  Token = Then
//  Value = 1
//  Token = Else
//  IfElse
//    Token = If
//    Value = true
//    Token = Then
//    Value = 2
//    Token = Else
//    Value = 3
//"
//        );
//    }
//    #[test]
//    fn list() {
//        assert_eq!(
//            [
//               TokenKind::SquareBOpen,
//               Token::Ident("a".into()), Token::Value(2.into()), Token::Value(3.into()),
//               Token::Value("lol".into()),
//               TokenKind::SquareBClose
//            ],
//            "\
//List
//  Token = SquareBOpen
//  ListItem
//    Ident = a
//  ListItem
//    Value = 2
//  ListItem
//    Value = 3
//  ListItem
//    Value = \"lol\"
//  Token = SquareBClose
//"
//        );
//        assert_eq!(
//            [
//               TokenKind::SquareBOpen, Token::Value(1.into()), TokenKind::SquareBClose, TokenKind::Concat,
//               TokenKind::SquareBOpen, Token::Value(2.into()), TokenKind::SquareBClose, TokenKind::Concat,
//               TokenKind::SquareBOpen, Token::Value(3.into()), TokenKind::SquareBClose
//            ],
//            "\
//Operation
//  Operation
//    List
//      Token = SquareBOpen
//      ListItem
//        Value = 1
//      Token = SquareBClose
//    Token = Concat
//    List
//      Token = SquareBOpen
//      ListItem
//        Value = 2
//      Token = SquareBClose
//  Token = Concat
//  List
//    Token = SquareBOpen
//    ListItem
//      Value = 3
//    Token = SquareBClose
//"
//        );
//    }
//    #[test]
//    fn functions() {
//        assert_eq!(
//            [
//               Token::Ident("a".into()), TokenKind::Colon, Token::Ident("b".into()), TokenKind::Colon,
//               Token::Ident("a".into()), TokenKind::Add, Token::Ident("b".into())
//            ],
//            "\
//Lambda
//  Ident = a
//  Token = Colon
//  Lambda
//    Ident = b
//    Token = Colon
//    Operation
//      Ident = a
//      Token = Add
//      Ident = b
//"
//        );
//        assert_eq!(
//            [
//                Token::Ident("a".into()), Token::Value(1.into()), Token::Value(2.into()),
//                TokenKind::Add,
//                Token::Value(3.into())
//            ],
//            "\
//Operation
//  Apply
//    Apply
//      Ident = a
//      Value = 1
//    Value = 2
//  Token = Add
//  Value = 3
//"
//        );
//    }
//    #[test]
//    fn patterns() {
//        assert_eq!(
//            [TokenKind::CurlyBOpen, TokenKind::Ellipsis, TokenKind::CurlyBClose, TokenKind::Colon, Token::Value(1.into())],
//            "\
//Lambda
//  Pattern
//    Token = CurlyBOpen
//    Token = Ellipsis
//    Token = CurlyBClose
//  Token = Colon
//  Value = 1
//"
//        );
//        assert_eq!(
//            [
//                TokenKind::CurlyBOpen, TokenKind::CurlyBClose, TokenKind::At, Token::Ident("outer".into()),
//                TokenKind::Colon, Token::Value(1.into())
//            ],
//            "\
//Lambda
//  Pattern
//    Token = CurlyBOpen
//    Token = CurlyBClose
//    PatBind
//      Token = At
//      Ident = outer
//  Token = Colon
//  Value = 1
//"
//        );
//        assert_eq!(
//            [
//                TokenKind::CurlyBOpen,
//                    Token::Ident("a".into()), TokenKind::Comma,
//                    Token::Ident("b".into()), TokenKind::Question, Token::Value("default".into()),
//                TokenKind::CurlyBClose,
//                TokenKind::Colon,
//                Token::Ident("a".into())
//            ],
//            "\
//Lambda
//  Pattern
//    Token = CurlyBOpen
//    PatEntry
//      Ident = a
//      Token = Comma
//    PatEntry
//      Ident = b
//      Token = Question
//      Value = \"default\"
//    Token = CurlyBClose
//  Token = Colon
//  Ident = a
//"
//        );
//        assert_eq!(
//            [
//                TokenKind::CurlyBOpen,
//                    Token::Ident("a".into()), TokenKind::Comma,
//                    Token::Ident("b".into()), TokenKind::Question, Token::Value("default".into()), TokenKind::Comma,
//                    TokenKind::Ellipsis,
//                TokenKind::CurlyBClose,
//                TokenKind::At,
//                Token::Ident("outer".into()),
//                TokenKind::Colon,
//                Token::Ident("outer".into())
//            ],
//            "\
//Lambda
//  Pattern
//    Token = CurlyBOpen
//    PatEntry
//      Ident = a
//      Token = Comma
//    PatEntry
//      Ident = b
//      Token = Question
//      Value = \"default\"
//      Token = Comma
//    Token = Ellipsis
//    Token = CurlyBClose
//    PatBind
//      Token = At
//      Ident = outer
//  Token = Colon
//  Ident = outer
//"
//        );
//        assert_eq!(
//            [
//                Token::Ident("outer".into()), TokenKind::At,
//                TokenKind::CurlyBOpen, Token::Ident("a".into()), TokenKind::CurlyBClose,
//                TokenKind::Colon,
//                Token::Ident("outer".into())
//            ],
//            "\
//Lambda
//  Pattern
//    PatBind
//      Ident = outer
//      Token = At
//    Token = CurlyBOpen
//    PatEntry
//      Ident = a
//    Token = CurlyBClose
//  Token = Colon
//  Ident = outer
//"
//        );
//        assert_eq!(
//            [
//                TokenKind::CurlyBOpen,
//                    Token::Ident("a".into()), TokenKind::Question, TokenKind::CurlyBOpen, TokenKind::CurlyBClose,
//                TokenKind::CurlyBClose, TokenKind::Colon, Token::Ident("a".into())
//            ],
//            "\
//Lambda
//  Pattern
//    Token = CurlyBOpen
//    PatEntry
//      Ident = a
//      Token = Question
//      Set
//        Token = CurlyBOpen
//        Token = CurlyBClose
//    Token = CurlyBClose
//  Token = Colon
//  Ident = a
//"
//        );
//    }
//    #[test]
//    fn dynamic() {
//        assert_eq!(
//            [Token::Dynamic(vec![(Meta::default(), Token::Ident("a".into()))], Meta::default())],
//            "\
//Dynamic
//  Token = Dynamic
//  Ident = a
//  Token = CurlyBClose
//"
//        );
//    }
}
