//! The parser: turns a series of tokens into an AST

use arrayvec::ArrayVec;
use ::{
    tokenizer::{Interpol as TokenInterpol, Meta, Span, Token, TokenKind},
    value::Value
};
use std::{self, fmt};
pub use arenatree::NodeId;
use arenatree::{self, *};

pub mod types;

use self::types::{Error as ErrorNode, TypedNode};

const OR: &'static str = "or";

pub type Arena<'a> = arenatree::Arena<'a, ASTNode>;

/// An error that occured during parsing
#[derive(Clone, Debug, Fail, PartialEq)]
pub enum ParseError {
    #[fail(display = "can't bind pattern here, already bound before")]
    AlreadyBound,
    #[fail(display = "expected {:?}, found {:?}", _0, _1)]
    Expected(TokenKind, Option<TokenKind>),
    #[fail(display = "invalid type! expected {}", _0)]
    InvalidType(&'static str),
    #[fail(display = "unexpected eof")]
    UnexpectedEOF,
    #[fail(display = "unexpected token {:?} not applicable in this context", _0)]
    Unexpected(TokenKind)
}

/// An AST with the arena and node
pub struct AST<'a> {
    pub arena: Arena<'a>,
    pub root: NodeId
}
impl<'a> AST<'a> {
    pub fn errors<'b>(&'b self) -> impl Iterator<Item = ErrorNode> + 'b {
        self.arena.iter().filter_map(ErrorNode::cast)
    }
    fn fmt_node(&self, f: &mut fmt::Formatter, node: NodeId, indent: usize) -> fmt::Result {
        let node = &self.arena[node];
        write!(f, "{fill:indent$}{:?}", node.kind, fill = "", indent = indent)?;
        match &node.data {
            Data::None => writeln!(f),
            Data::Error(err) => writeln!(f, " = ERROR: {}", err.1),
            Data::Ident(_meta, name) => writeln!(f, " = {}", name),
            Data::Interpol { meta: _, multiline } => writeln!(f, " {{ multiline: {} }}", multiline),
            Data::InterpolLiteral { original, content: _ } => writeln!(f, " = \"{}\"", original),
            Data::Token(_meta, token) => writeln!(f, " = {:?}", token),
            Data::Value(_meta, value) => writeln!(f, " = {}", value)
        }?;
        for child in node.children(&self.arena) {
            self.fmt_node(f, child, indent+2)?;
        }
        Ok(())
    }
}
impl<'a> fmt::Debug for AST<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_node(f, self.root, 0)
    }
}

#[derive(Clone, Debug)]
pub enum Data {
    None,
    Error(Error),
    Ident(Meta, String),
    Interpol {
        meta: Meta,
        multiline: bool
    },
    InterpolLiteral {
        original: String,
        content: String
    },
    Token(Meta, TokenKind),
    Value(Meta, Value)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    Set,
    SetEntry,
    Token,
    Unary,
    Value,
    With
}

#[derive(Clone, Debug)]
pub struct ASTNode {
    pub kind: ASTKind,
    pub span: Span,
    pub data: Data,
    pub node: Node
}
impl ASTNode {
    fn from_token_kind(meta: Meta, token: TokenKind) -> Self {
        Self {
            kind: ASTKind::Token,
            span: meta.span,
            data: Data::Token(meta, token),
            node: Node::default()
        }
    }
    fn from_token(tuple: (Meta, Token)) -> Self {
        let (meta, token) = tuple;
        Self::from_token_kind(meta, token.kind())
    }
    fn from_ident(meta: Meta, ident: String) -> Self {
        Self {
            kind: ASTKind::Ident,
            span: meta.span,
            data: Data::Ident(meta, ident),
            node: Node::default()
        }
    }
    fn from_value(meta: Meta, value: Value) -> Self {
        Self {
            kind: ASTKind::Value,
            span: meta.span,
            data: Data::Value(meta, value),
            node: Node::default()
        }
    }
    /// If this node has a child, it returns an iterator over that child and all its siblings.
    /// If this node is a dead end, return an empty iterator.
    pub fn children<'a>(&self, arena: &'a Arena) -> NodeIter<'a, ASTNode> {
        NodeIter {
            arena,
            cursor: self.node.child
        }
    }
}
impl AsRef<Node> for ASTNode {
    fn as_ref(&self) -> &Node {
        &self.node
    }
}
impl AsMut<Node> for ASTNode {
    fn as_mut(&mut self) -> &mut Node {
        &mut self.node
    }
}

pub(crate) type Error = (Option<Span>, ParseError);
pub(crate) type Result<T> = std::result::Result<T, Error>;

macro_rules! math {
    (only_once: $only_once:expr, $self:expr, $next:block, $($token:pat),*) => {{
        let mut val = $next;
        loop {
            match $self.peek_kind() {
                $(Some($token) => {
                    let val_span = val.span;
                    let val_id = $self.insert(val);
                    let operator = ASTNode::from_token($self.next().unwrap());
                    let operator = $self.insert(operator);
                    let expr = $next;
                    let expr_span = expr.span;
                    let expr = $self.insert(expr);

                    let children = $self.chain(&[val_id, operator, expr]);

                    val = ASTNode {
                        kind: ASTKind::Operation,
                        span: val_span.until(expr_span),
                        data: Data::None,
                        node: Node::with_child(children)
                    };
                },)*
                _ => break
            }
        }
        Ok(val)
    }};
    ($self:expr, $next:block, $($token:pat),*) => {{
        math!(only_once: false, $self, $next, $($token),*)
    }};
}

/// The parser. You may want to use the `parse` convenience function from this module instead.
pub struct Parser<'a, I>
    where I: Iterator<Item = (Meta, Token)>
{
    iter: I,
    buffer: ArrayVec<[I::Item; 2]>,
    arena: Arena<'a>
}
impl<'a, I> Parser<'a, I>
    where I: Iterator<Item = (Meta, Token)>
{
    /// Create a new instance
    pub fn new(iter: I) -> Self {
        Self::with_arena(Arena::new(), iter)
    }
    /// Create a new instance with a specified arena
    pub fn with_arena(arena: Arena<'a>, iter: I) -> Self {
        Self {
            iter,
            buffer: ArrayVec::new(),
            arena
        }
    }
    /// Return a reference to the inner arena
    pub fn arena(&self) -> &Arena<'a> {
        &self.arena
    }
    /// Return the owned inner arena
    pub fn into_arena(self) -> Arena<'a> {
        self.arena
    }

    fn parse_branch<T>(&mut self, iter: T) -> Result<ASTNode>
        where T: IntoIterator<Item = (Meta, Token)>
    {
        Parser::with_arena(self.arena.reference(), iter.into_iter())
            .parse_expr()
    }
    fn insert(&mut self, node: ASTNode) -> NodeId {
        self.arena.insert(node)
    }
    fn chain(&mut self, nodes: &[NodeId]) -> NodeId {
        let mut list = NodeList::new();
        list.push_all(nodes, &mut self.arena);
        list.node().expect("chain called on empty list")
    }

    fn peek_meta(&mut self) -> Option<&(Meta, Token)> {
        if self.buffer.is_empty() {
            if let Some(token) = self.iter.next() {
                self.buffer.push(token);
            }
        }
        self.buffer.last()
    }
    fn peek(&mut self) -> Option<&Token> {
        self.peek_meta().map(|(_, token)| token)
    }
    fn peek_kind(&mut self) -> Option<TokenKind> {
        self.peek().map(Token::kind)
    }
    fn next_raw(&mut self) -> Result<I::Item> {
        self.buffer.pop()
            .or_else(|| self.iter.next())
            .ok_or((None, ParseError::UnexpectedEOF))
    }
    fn next(&mut self) -> Result<I::Item> {
        let mut next = self.next_raw()?;

        if let Some(TokenKind::EOF) = self.peek_kind() {
            let (mut meta, _) = self.next_raw()?;
            next.0.trailing.append(&mut meta.leading);
        }
        Ok(next)
    }
    fn expect(&mut self, expected: TokenKind) -> Result<(Meta, Token)> {
        if let Some((meta, actual)) = self.peek_meta() {
            if actual.kind() != expected {
                return Err((Some(meta.span), ParseError::Expected(expected, Some(actual.kind()))));
            }
        } else {
            return Err((None, ParseError::Expected(expected, None)));
        }
        Ok(self.next().unwrap())
    }
    fn recover(&mut self, recover: &[TokenKind]) -> bool {
        loop {
            match self.peek_kind() {
                Some(kind) if recover.contains(&kind) => return true,
                None => return false,
                _ => { self.next().unwrap(); }
            }
        }
    }

    fn parse_interpol(&mut self, meta: Meta, multiline: bool, values: Vec<TokenInterpol>) -> Result<ASTNode> {
        let mut parsed = NodeList::new();
        for value in values {
            parsed.push(match value {
                TokenInterpol::Literal { span, original, content } => self.insert(ASTNode {
                    kind: ASTKind::InterpolLiteral,
                    span,
                    data: Data::InterpolLiteral { original, content },
                    node: Node::default()
                }),
                TokenInterpol::Tokens(tokens, close) => {
                    let parsed = self.parse_branch(tokens)?;
                    let parsed_span = parsed.span;
                    let parsed = self.insert(parsed);
                    let close_span = close.span;
                    let close = self.insert(ASTNode::from_token_kind(close, TokenKind::CurlyBClose));

                    let children = self.chain(&[parsed, close]);

                    self.insert(ASTNode {
                        kind: ASTKind::InterpolAst,
                        span: parsed_span.until(close_span),
                        data: Data::None,
                        node: Node::with_child(children)
                    })
                }
            }, &mut self.arena);
        }
        Ok(ASTNode {
            kind: ASTKind::Interpol,
            span: meta.span,
            data: Data::Interpol { meta, multiline },
            node: Node::with_child(parsed.node())
        })
    }
    fn next_attr(&mut self) -> Result<ASTNode> {
        match self.next()? {
            (meta, Token::Ident(ident)) => Ok(ASTNode::from_ident(meta, ident)),
            (meta, Token::Value(value)) => Ok(ASTNode::from_value(meta, value)),
            (meta, Token::Dynamic(tokens, close)) => {
                let open = ASTNode::from_token_kind(meta, TokenKind::Dynamic);
                let open_span = open.span;
                let open = self.insert(open);
                let parsed = self.parse_branch(tokens)?;
                let parsed = self.insert(parsed);
                let close_span = close.span;
                let close = self.insert(ASTNode::from_token_kind(close, TokenKind::CurlyBClose));
                let children = self.chain(&[open, parsed, close]);
                Ok(ASTNode {
                    kind: ASTKind::Dynamic,
                    span: open_span.until(close_span),
                    data: Data::None,
                    node: Node::with_child(children)
                })
            },
            (meta, Token::Interpol { multiline, parts }) => self.parse_interpol(meta, multiline, parts),
            (meta, token) => Err((Some(meta.span), ParseError::Expected(TokenKind::Ident, Some(token.kind()))))
        }
    }
    fn parse_attr(&mut self) -> Result<ASTNode> {
        let mut path = NodeList::new();
        let mut start = None;
        let mut end;
        loop {
            let attr = self.next_attr()?;
            if start.is_none() {
                start = Some(attr.span);
            }
            end = attr.span;
            path.push(self.insert(attr), &mut self.arena);

            if self.peek_kind() == Some(TokenKind::Dot) {
                let dot = ASTNode::from_token(self.next().unwrap());
                path.push(self.insert(dot), &mut self.arena);
            } else {
                break;
            }
        }
        Ok(ASTNode {
            data: Data::None,
            kind: ASTKind::Attribute,
            span: start.unwrap().until(end),
            node: Node::with_child(path.node())
        })
    }
    fn next_ident(&mut self) -> Result<ASTNode> {
        match self.next()? {
            (meta, Token::Ident(name)) => Ok(ASTNode::from_ident(meta, name)),
            (meta, token) => Err((Some(meta.span), ParseError::Expected(TokenKind::Ident, Some(token.kind()))))
        }
    }
    fn parse_pattern(&mut self, open: Meta, bind: Option<(ASTNode, ASTNode)>) -> Result<ASTNode> {
        let start = bind.as_ref().map(|(ident, _)| ident.span).unwrap_or(open.span);

        let mut pattern = NodeList::new();
        let bind = if let Some((ident, at)) = bind {
            let ident_span = ident.span;
            let ident = self.insert(ident);
            let at_span = at.span;
            let at = self.insert(at);
            let children = self.chain(&[ident, at]);

            pattern.push(self.insert(ASTNode {
                kind: ASTKind::PatBind,
                span: ident_span.until(at_span),
                data: Data::None,
                node: Node::with_child(children)
            }), &mut self.arena);
            true
        } else {
            false
        };

        let open = ASTNode::from_token_kind(open, TokenKind::CurlyBOpen);
        let open = self.insert(open);
        pattern.push(open, &mut self.arena);

        loop {
            let mut entry = NodeList::new();

            let ident = match self.peek_kind() {
                Some(TokenKind::Ellipsis) => {
                    let ellipsis = ASTNode::from_token(self.next().unwrap());
                    pattern.push(self.insert(ellipsis), &mut self.arena);
                    break;
                },
                Some(TokenKind::CurlyBClose) => break,
                _ => self.next_ident()?,
            };
            let ident_span = ident.span;
            let mut end_span = ident_span;
            entry.push(self.insert(ident), &mut self.arena);
            if self.peek_kind() == Some(TokenKind::Question) {
                let question = ASTNode::from_token(self.next().unwrap());
                let expr = self.parse_expr()?;
                end_span = expr.span;
                entry.push_all(&[
                    self.insert(question),
                    self.insert(expr),
                ], &mut self.arena);
            }
            let comma = match self.peek_kind() {
                Some(TokenKind::Comma) => {
                    let comma = ASTNode::from_token(self.next().unwrap());
                    end_span = comma.span;
                    entry.push(self.insert(comma), &mut self.arena);
                    true
                },
                _ => false
            };
            pattern.push(self.insert(ASTNode {
                kind: ASTKind::PatEntry,
                span: ident_span.until(end_span),
                data: Data::None,
                node: Node::with_child(entry.node())
            }), &mut self.arena);
            if !comma {
                break;
            }
        }

        let close = ASTNode::from_token(self.expect(TokenKind::CurlyBClose)?);
        let mut end_span = close.span;
        pattern.push(self.insert(close), &mut self.arena);

        if let Some(TokenKind::At) = self.peek_kind() {
            let at = ASTNode::from_token(self.next().unwrap());
            if bind {
                return Err((Some(at.span), ParseError::AlreadyBound));
            }
            let at_span = at.span;
            let at = self.insert(at);
            let ident = self.next_ident()?;
            end_span = ident.span;
            let ident = self.insert(ident);

            let children = self.chain(&[at, ident]);

            pattern.push(self.insert(ASTNode {
                kind: ASTKind::PatBind,
                span: at_span.until(end_span),
                data: Data::None,
                node: Node::with_child(children)
            }), &mut self.arena);
        }

        let pattern = self.insert(ASTNode {
            kind: ASTKind::Pattern,
            span: start.until(end_span),
            data: Data::None,
            node: Node::with_child(pattern.node())
        });
        let colon = ASTNode::from_token(self.expect(TokenKind::Colon)?);
        let colon = self.insert(colon);
        let expr = self.parse_expr()?;
        let expr_span = expr.span;
        let expr = self.insert(expr);

        let children = self.chain(&[pattern, colon, expr]);

        Ok(ASTNode {
            kind: ASTKind::Lambda,
            span: start.until(expr_span),
            data: Data::None,
            node: Node::with_child(children)
        })
    }
    fn parse_set_entry(&mut self, until: TokenKind, values: &mut NodeList<ASTNode>) -> Result<bool> {
        match self.peek_kind() {
            token if token == Some(until) => return Ok(false),
            Some(TokenKind::Inherit) => {
                let inherit = ASTNode::from_token(self.next().unwrap());
                let inherit_span = inherit.span;
                let inherit = self.insert(inherit);

                let mut vars = NodeList::new();
                vars.push(inherit, &mut self.arena);

                if self.peek_kind() == Some(TokenKind::ParenOpen) {
                    let open = ASTNode::from_token(self.next().unwrap());
                    let open_span = open.span;
                    let open = self.insert(open);
                    let from = self.parse_expr()?;
                    let from = self.insert(from);
                    let close = ASTNode::from_token(self.expect(TokenKind::ParenClose)?);
                    let close_span = close.span;
                    let close = self.insert(close);

                    let children = self.chain(&[open, from, close]);

                    values.push(self.insert(ASTNode {
                        kind: ASTKind::InheritFrom,
                        span: open_span.until(close_span),
                        data: Data::None,
                        node: Node::with_child(children)
                    }), &mut self.arena)
                }

                while let Some(Token::Ident(_)) = self.peek() {
                    let ident = self.next_ident().unwrap();
                    vars.push(self.insert(ident), &mut self.arena);
                }

                let semi = ASTNode::from_token(self.expect(TokenKind::Semicolon)?);
                let semi_span = semi.span;
                let semi = self.insert(semi);

                vars.push(semi, &mut self.arena);

                values.push(self.insert(ASTNode {
                    kind: ASTKind::Inherit,
                    span: inherit_span.until(semi_span),
                    data: Data::None,
                    node: Node::with_child(vars.node())
                }), &mut self.arena);
            },
            _ => {
                let key = self.parse_attr()?;
                let key_span = key.span;
                let key = self.insert(key);
                let assign = ASTNode::from_token(self.expect(TokenKind::Assign)?);
                let assign = self.insert(assign);
                let value = self.parse_expr()?;
                let value = self.insert(value);
                let semi = ASTNode::from_token(self.expect(TokenKind::Semicolon)?);
                let semi_span = semi.span;
                let semi = self.insert(semi);

                let entry = self.chain(&[
                    key,
                    assign,
                    value,
                    semi,
                ]);
                values.push(self.insert(ASTNode {
                    kind: ASTKind::SetEntry,
                    span: key_span.until(semi_span),
                    data: Data::None,
                    node: Node::with_child(entry)
                }), &mut self.arena)
            }
        }
        Ok(true)
    }
    fn parse_set(&mut self, until: TokenKind) -> Result<(Option<NodeId>, ASTNode)> {
        let mut values = NodeList::new();

        loop {
            match self.parse_set_entry(until, &mut values) {
                Ok(true) => (),
                Ok(false) => break,
                Err(err) => {
                    if self.recover(&[TokenKind::Ident, TokenKind::Inherit, until]) {
                        values.push(self.insert(ASTNode {
                            kind: ASTKind::Error,
                            span: Span::default(),
                            data: Data::Error(err),
                            node: Node::default()
                        }), &mut self.arena);
                    } else {
                        return Err(err);
                    }
                }
            }
        }

        let end = ASTNode::from_token(self.next().unwrap()); // Won't break until reached
        Ok((values.node(), end))
    }
    fn parse_val(&mut self) -> Result<ASTNode> {
        let (meta, token) = self.next()?;
        let kind = token.kind();
        let mut val = match (kind, token) {
            (TokenKind::ParenOpen, _) => {
                let open = ASTNode::from_token_kind(meta, TokenKind::ParenOpen);
                let open_span = open.span;
                let open = self.insert(open);
                let expr = self.parse_expr()?;
                let expr = self.insert(expr);
                let close = ASTNode::from_token(self.expect(TokenKind::ParenClose)?);
                let close_span = close.span;
                let close = self.insert(close);

                let children = self.chain(&[open, expr, close]);

                ASTNode {
                    kind: ASTKind::Paren,
                    span: open_span.until(close_span),
                    data: Data::None,
                    node: Node::with_child(children)
                }
            },
            (TokenKind::Import, _) => {
                let import = ASTNode::from_token_kind(meta, kind);
                let import_span = import.span;
                let import = self.insert(import);
                let value = self.parse_val()?;
                let value_span = value.span;
                let value = self.insert(value);

                let children = self.chain(&[import, value]);

                ASTNode {
                    kind: ASTKind::Import,
                    span: import_span.until(value_span),
                    data: Data::None,
                    node: Node::with_child(children)
                }
            },
            (TokenKind::Rec, _) => {
                let rec = ASTNode::from_token_kind(meta, kind);
                let rec_span = rec.span;
                let rec = self.insert(rec);
                let open = ASTNode::from_token(self.expect(TokenKind::CurlyBOpen)?);
                let open = self.insert(open);
                let (values, close) = self.parse_set(TokenKind::CurlyBClose)?;
                let close_span = close.span;
                let close = self.insert(close);

                let children = if let Some(values) = values {
                    self.chain(&[rec, open, values, close])
                } else {
                    self.chain(&[rec, open, close])
                };

                ASTNode {
                    kind: ASTKind::Set,
                    span: rec_span.until(close_span),
                    data: Data::None,
                    node: Node::with_child(children)
                }
            },
            (TokenKind::CurlyBOpen, _) => {
                let temporary = self.next()?;
                match (temporary.1.kind(), self.peek_kind()) {
                    (TokenKind::Ident, Some(TokenKind::Comma))
                            | (TokenKind::Ident, Some(TokenKind::Question))
                            | (TokenKind::Ellipsis, Some(TokenKind::CurlyBClose))
                            | (TokenKind::Ident, Some(TokenKind::CurlyBClose))
                            | (TokenKind::CurlyBClose, Some(TokenKind::Colon))
                            | (TokenKind::CurlyBClose, Some(TokenKind::At)) => {
                        // We did a lookahead, put it back
                        self.buffer.push(temporary);
                        self.parse_pattern(meta, None)?
                    },
                    _ => {
                        // We did a lookahead, put it back
                        self.buffer.push(temporary);

                        let open_span = meta.span;
                        let open = self.insert(ASTNode::from_token_kind(meta, kind));
                        let (values, close) = self.parse_set(TokenKind::CurlyBClose)?;
                        let close_span = close.span;
                        let close = self.insert(close);

                        let children = if let Some(values) = values {
                            self.chain(&[open, values, close])
                        } else {
                            self.chain(&[open, close])
                        };

                        ASTNode {
                            kind: ASTKind::Set,
                            span: open_span.until(close_span),
                            data: Data::None,
                            node: Node::with_child(children)
                        }
                    }
                }
            },
            (TokenKind::SquareBOpen, _) => {
                let mut values = NodeList::new();

                let open = ASTNode::from_token_kind(meta, kind);
                let open_span = open.span;
                values.push(self.insert(open), &mut self.arena);

                loop {
                    match self.peek_kind() {
                        None | Some(TokenKind::SquareBClose) => break,
                        _ => {
                            let val = self.parse_val()?;
                            let val_span = val.span;
                            let val = self.insert(val);
                            values.push(self.insert(ASTNode {
                                kind: ASTKind::ListItem,
                                span: val_span,
                                data: Data::None,
                                node: Node::with_child(val)
                            }), &mut self.arena);
                        }
                    }
                }
                let close = ASTNode::from_token(self.expect(TokenKind::SquareBClose)?);
                let close_span = close.span;
                values.push(self.insert(close), &mut self.arena);

                ASTNode {
                    kind: ASTKind::List,
                    span: open_span.until(close_span),
                    data: Data::None,
                    node: Node::with_child(values.node())
                }
            },
            (_, Token::Dynamic(tokens, close)) => {
                let open = ASTNode::from_token_kind(meta, kind);
                let open_span = open.span;
                let open = self.insert(open);
                let parsed = self.parse_branch(tokens)?;
                let parsed = self.insert(parsed);
                let close_span = close.span;
                let close = self.insert(ASTNode::from_token_kind(close, TokenKind::CurlyBClose));
                let children = self.chain(&[open, parsed, close]);
                ASTNode {
                    kind: ASTKind::Dynamic,
                    span: open_span.until(close_span),
                    data: Data::None,
                    node: Node::with_child(children)
                }
            },
            (_, Token::Value(val)) => ASTNode::from_value(meta, val),
            (_, Token::Ident(name)) => if self.peek_kind() == Some(TokenKind::At) {
                let ident = ASTNode::from_ident(meta, name);
                let at = ASTNode::from_token(self.next().unwrap());
                let (open, _) = self.expect(TokenKind::CurlyBOpen)?;

                self.parse_pattern(open, Some((ident, at)))?
            } else {
                ASTNode::from_ident(meta, name)
            },
            (_, Token::Interpol { multiline, parts }) => self.parse_interpol(meta, multiline, parts)?,
            (kind, _) => return Err((Some(meta.span), ParseError::Unexpected(kind)))
        };

        while self.peek_kind() == Some(TokenKind::Dot) {
            let val_span = val.span;
            let val_id = self.insert(val);
            let dot = ASTNode::from_token(self.next().unwrap());
            let dot = self.insert(dot);
            let attr = self.next_attr()?;
            let attr_span = attr.span;
            let attr = self.insert(attr);

            let or = match self.peek() {
                Some(Token::Ident(s)) if s == OR => true,
                _ => false
            };
            if or {
                let or = self.next_ident().unwrap();
                let or = self.insert(or);
                let default = self.parse_val()?;
                let default_span = default.span;
                let default = self.insert(default);

                let children = self.chain(&[val_id, dot, attr, or, default]);

                val = ASTNode {
                    kind: ASTKind::OrDefault,
                    span: val_span.until(default_span),
                    data: Data::None,
                    node: Node::with_child(children)
                };
            } else {
                let children = self.chain(&[val_id, dot, attr]);

                val = ASTNode {
                    kind: ASTKind::IndexSet,
                    span: val_span.until(attr_span),
                    data: Data::None,
                    node: Node::with_child(children)
                };
            }
        }

        Ok(val)
    }
    fn parse_fn(&mut self) -> Result<ASTNode> {
        let mut val = self.parse_val()?;

        while self.peek_kind().map(|t| t.is_fn_arg()).unwrap_or(false) {
            let val_span = val.span;
            let val_id = self.insert(val);
            let arg = self.parse_val()?;
            let arg_span = arg.span;
            let arg = self.insert(arg);

            let children = self.chain(&[val_id, arg]);

            val = ASTNode {
                kind: ASTKind::Apply,
                span: val_span.until(arg_span),
                data: Data::None,
                node: Node::with_child(children)
            };
        }

        Ok(val)
    }
    fn parse_negate(&mut self) -> Result<ASTNode> {
        if self.peek_kind() == Some(TokenKind::Sub) {
            let token = ASTNode::from_token(self.next().unwrap());
            let token_span = token.span;
            let token = self.insert(token);
            let expr = self.parse_negate()?;
            let expr_span = expr.span;
            let expr = self.insert(expr);

            let children = self.chain(&[token, expr]);

            Ok(ASTNode {
                kind: ASTKind::Unary,
                span: token_span.until(expr_span),
                data: Data::None,
                node: Node::with_child(children)
            })
        } else {
            self.parse_fn()
        }
    }
    fn parse_isset(&mut self) -> Result<ASTNode> {
        math!(self, { self.parse_negate()? }, TokenKind::Question)
    }
    fn parse_concat(&mut self) -> Result<ASTNode> {
        math!(self, { self.parse_isset()? }, TokenKind::Concat)
    }
    fn parse_mul(&mut self) -> Result<ASTNode> {
        math!(
            self, { self.parse_concat()? },
            TokenKind::Mul,
            TokenKind::Div
        )
    }
    fn parse_add(&mut self) -> Result<ASTNode> {
        math!(
            self, { self.parse_mul()? },
            TokenKind::Add,
            TokenKind::Sub
        )
    }
    fn parse_invert(&mut self) -> Result<ASTNode> {
        if self.peek_kind() == Some(TokenKind::Invert) {
            let token = ASTNode::from_token(self.next().unwrap());
            let token_span = token.span;
            let token = self.insert(token);
            let expr = self.parse_invert()?;
            let expr_span = expr.span;
            let expr = self.insert(expr);

            let children = self.chain(&[token, expr]);

            Ok(ASTNode {
                kind: ASTKind::Unary,
                span: token_span.until(expr_span),
                data: Data::None,
                node: Node::with_child(children)
            })
        } else {
            self.parse_add()
        }
    }
    fn parse_merge(&mut self) -> Result<ASTNode> {
        math!(self, { self.parse_invert()? }, TokenKind::Merge)
    }
    fn parse_compare(&mut self) -> Result<ASTNode> {
        math!(
            only_once: true, self, { self.parse_merge()? },
            TokenKind::Less,
            TokenKind::LessOrEq,
            TokenKind::More,
            TokenKind::MoreOrEq
        )
    }
    fn parse_equal(&mut self) -> Result<ASTNode> {
        math!(
            only_once: true, self, { self.parse_compare()? },
            TokenKind::Equal,
            TokenKind::NotEqual
        )
    }
    fn parse_and(&mut self) -> Result<ASTNode> {
        math!(self, { self.parse_equal()? }, TokenKind::And)
    }
    fn parse_or(&mut self) -> Result<ASTNode> {
        math!(self, { self.parse_and()? }, TokenKind::Or)
    }
    fn parse_implication(&mut self) -> Result<ASTNode> {
        math!(self, { self.parse_or()? }, TokenKind::Implication)
    }
    #[inline(always)]
    fn parse_math(&mut self) -> Result<ASTNode> {
        // Always point this to the lowest-level math function there is
        self.parse_implication()
    }
    /// Parse Nix code into an AST
    pub fn parse_expr(&mut self) -> Result<ASTNode> {
        Ok(match self.peek_kind() {
            Some(TokenKind::Let) => {
                let let_ = ASTNode::from_token(self.next().unwrap());
                let let_span = let_.span;
                let let_ = self.insert(let_);
                if self.peek_kind() == Some(TokenKind::CurlyBOpen) {
                    let open = ASTNode::from_token(self.next().unwrap());
                    let open_span = open.span;
                    let open = self.insert(open);
                    let (values, close) = self.parse_set(TokenKind::CurlyBClose)?;
                    let close_span = close.span;
                    let close = self.insert(close);

                    let children = if let Some(values) = values {
                        self.chain(&[let_, open, values, close])
                    } else {
                        self.chain(&[let_, open, close])
                    };

                    ASTNode {
                        kind: ASTKind::Let,
                        span: open_span.until(close_span),
                        data: Data::None,
                        node: Node::with_child(children)
                    }
                } else {
                    let (values, in_) = self.parse_set(TokenKind::In)?;
                    let in_ = self.insert(in_);
                    let expr = self.parse_expr()?;
                    let expr_span = expr.span;
                    let expr = self.insert(expr);

                    let children = if let Some(values) = values {
                        self.chain(&[let_, values, in_, expr])
                    } else {
                        self.chain(&[let_, in_, expr])
                    };

                    ASTNode {
                        kind: ASTKind::LetIn,
                        span: let_span.until(expr_span),
                        data: Data::None,
                        node: Node::with_child(children)
                    }
                }
            },
            Some(TokenKind::With) => {
                let with = ASTNode::from_token(self.next().unwrap());
                let with_span = with.span;
                let with = self.insert(with);
                let namespace = self.parse_expr()?;
                let namespace = self.insert(namespace);
                let semi = ASTNode::from_token(self.expect(TokenKind::Semicolon)?);
                let semi = self.insert(semi);
                let body = self.parse_expr()?;
                let body_span = body.span;
                let body = self.insert(body);

                let children = self.chain(&[with, namespace, semi, body]);

                ASTNode {
                    kind: ASTKind::With,
                    span: with_span.until(body_span),
                    data: Data::None,
                    node: Node::with_child(children)
                }
            },
            Some(TokenKind::If) => {
                let if_ = ASTNode::from_token(self.next().unwrap());
                let if_span = if_.span;
                let if_ = self.insert(if_);
                let condition = self.parse_expr()?;
                let condition = self.insert(condition);
                let then = ASTNode::from_token(self.expect(TokenKind::Then)?);
                let then = self.insert(then);
                let body = self.parse_expr()?;
                let body = self.insert(body);
                let else_ = ASTNode::from_token(self.expect(TokenKind::Else)?);
                let else_ = self.insert(else_);
                let else_body = self.parse_expr()?;
                let else_body_span = else_body.span;
                let else_body = self.insert(else_body);

                let children = self.chain(&[if_, condition, then, body, else_, else_body]);

                ASTNode {
                    kind: ASTKind::IfElse,
                    span: if_span.until(else_body_span),
                    data: Data::None,
                    node: Node::with_child(children)
                }
            },
            Some(TokenKind::Assert) => {
                let assert = ASTNode::from_token(self.next().unwrap());
                let assert_span = assert.span;
                let assert = self.insert(assert);
                let condition = self.parse_expr()?;
                let condition = self.insert(condition);
                let semi = ASTNode::from_token(self.expect(TokenKind::Semicolon)?);
                let semi = self.insert(semi);
                let body = self.parse_expr()?;
                let body_span = body.span;
                let body = self.insert(body);

                let children = self.chain(&[assert, condition, semi, body]);

                ASTNode {
                    kind: ASTKind::Assert,
                    span: assert_span.until(body_span),
                    data: Data::None,
                    node: Node::with_child(children)
                }
            },
            _ => {
                let val = self.parse_math()?;
                if val.kind == ASTKind::Ident && self.peek_kind() == Some(TokenKind::Colon) {
                    let val_span = val.span;
                    let val = self.insert(val);
                    let colon = ASTNode::from_token(self.next().unwrap());
                    let colon = self.insert(colon);
                    let expr = self.parse_expr()?;
                    let expr_span = expr.span;
                    let expr = self.insert(expr);

                    let children = self.chain(&[val, colon, expr]);

                    ASTNode {
                        kind: ASTKind::Lambda,
                        span: val_span.until(expr_span),
                        data: Data::None,
                        node: Node::with_child(children)
                    }
                } else {
                    val
                }
            }
        })
    }
}

/// Convenience function for turning an iterator of tokens into an AST
pub fn parse<I>(iter: I) -> Result<AST<'static>>
    where I: IntoIterator<Item = (Meta, Token)>
{
    let mut parser = Parser::new(iter.into_iter());
    let ast = parser.parse_expr()?;

    let mut arena = parser.into_arena();
    let root = arena.insert(ast);

    Ok(AST { arena, root })
}

#[cfg(test)]
mod tests {
    use ::{
        tokenizer::{Interpol as TokenInterpol, Meta, Span, Token, TokenKind},
        value::{Anchor, Value}
    };
    use super::*;

    macro_rules! assert_eq {
        ([$($token:expr),*], $expected:expr) => {
            let actual = format!("{:?}", parse(vec![$((Meta::default(), $token.into())),*])
                .expect("failed to parse test"));
            if actual != $expected {
                eprintln!("--- Actual ---\n{}--- End ---", actual);
                eprintln!("--- Expected ---\n{}--- End ---", $expected);
                panic!("Tests did not match");
            }
        };
    }

    #[test]
    fn set() {
        assert_eq!(
            [
                TokenKind::CurlyBOpen,

                Token::Ident("meaning_of_life".into()), TokenKind::Assign, Token::Value(42.into()), TokenKind::Semicolon,
                Token::Ident("H4X0RNUM83R".into()), TokenKind::Assign, Token::Value(1.337.into()), TokenKind::Semicolon,

                TokenKind::CurlyBClose
            ],
            "\
Set
  Token = CurlyBOpen
  SetEntry
    Attribute
      Ident = meaning_of_life
    Token = Assign
    Value = 42
    Token = Semicolon
  SetEntry
    Attribute
      Ident = H4X0RNUM83R
    Token = Assign
    Value = 1.337
    Token = Semicolon
  Token = CurlyBClose
"
        );
        assert_eq!(
            [
                TokenKind::Rec, TokenKind::CurlyBOpen,
                Token::Ident("test".into()), TokenKind::Assign, Token::Value(1.into()), TokenKind::Semicolon,
                TokenKind::CurlyBClose
            ],
            "\
Set
  Token = Rec
  Token = CurlyBOpen
  SetEntry
    Attribute
      Ident = test
    Token = Assign
    Value = 1
    Token = Semicolon
  Token = CurlyBClose
"
        );
        assert_eq!(
            [TokenKind::CurlyBOpen, TokenKind::CurlyBClose],
            "\
Set
  Token = CurlyBOpen
  Token = CurlyBClose
"
        );
        assert_eq!(
            [
                TokenKind::CurlyBOpen,

                Token::Ident("a".into()),
                    TokenKind::Dot, Token::Value("b".into()),
                TokenKind::Assign, Token::Value(1.into()), TokenKind::Semicolon,

                Token::Interpol {
                    multiline: false,
                    parts: vec![
                        TokenInterpol::Literal {
                            span: Span::default(),
                            original: "c".into(),
                            content: "c".into()
                        }
                    ]
                },
                TokenKind::Dot, Token::Dynamic(vec![(Meta::default(), Token::Ident("d".into()))], Meta::default()),
                TokenKind::Assign, Token::Value(2.into()), TokenKind::Semicolon,

                TokenKind::CurlyBClose
            ],
            "\
Set
  Token = CurlyBOpen
  SetEntry
    Attribute
      Ident = a
      Token = Dot
      Value = \"b\"
    Token = Assign
    Value = 1
    Token = Semicolon
  SetEntry
    Attribute
      Interpol { multiline: false }
        InterpolLiteral = \"c\"
      Token = Dot
      Dynamic
        Token = Dynamic
        Ident = d
        Token = CurlyBClose
    Token = Assign
    Value = 2
    Token = Semicolon
  Token = CurlyBClose
"
        );
    }
    #[test]
    fn math() {
        assert_eq!(
            [
                Token::Value(1.into()), TokenKind::Add, Token::Value(2.into()), TokenKind::Mul, Token::Value(3.into())
            ],
            "\
Operation
  Value = 1
  Token = Add
  Operation
    Value = 2
    Token = Mul
    Value = 3
"
        );
        assert_eq!(
            [
                Token::Value(5.into()), TokenKind::Mul,
                TokenKind::Sub, TokenKind::ParenOpen,
                    Token::Value(3.into()), TokenKind::Sub, Token::Value(2.into()),
                TokenKind::ParenClose
            ],
            "\
Operation
  Value = 5
  Token = Mul
  Unary
    Token = Sub
    Paren
      Token = ParenOpen
      Operation
        Value = 3
        Token = Sub
        Value = 2
      Token = ParenClose
"
        );
    }
    #[test]
    fn let_in() {
        assert_eq!(
            [
                TokenKind::Let,
                    Token::Ident("a".into()), TokenKind::Assign, Token::Value(42.into()), TokenKind::Semicolon,
                TokenKind::In,
                    Token::Ident("a".into())
            ],
            "\
LetIn
  Token = Let
  SetEntry
    Attribute
      Ident = a
    Token = Assign
    Value = 42
    Token = Semicolon
  Token = In
  Ident = a
"
        );
    }
    #[test]
    fn let_legacy_syntax() {
        assert_eq!(
            [
                TokenKind::Let, TokenKind::CurlyBOpen,
                    Token::Ident("a".into()), TokenKind::Assign, Token::Value(42.into()), TokenKind::Semicolon,
                    Token::Ident("body".into()), TokenKind::Assign, Token::Ident("a".into()), TokenKind::Semicolon,
                TokenKind::CurlyBClose
            ],
            "\
Let
  Token = Let
  Token = CurlyBOpen
  SetEntry
    Attribute
      Ident = a
    Token = Assign
    Value = 42
    Token = Semicolon
  SetEntry
    Attribute
      Ident = body
    Token = Assign
    Ident = a
    Token = Semicolon
  Token = CurlyBClose
"
        );
    }
    #[test]
    fn interpolation() {
        assert_eq!(
            [
                Token::Interpol {
                    multiline: false,
                    parts: vec![
                        TokenInterpol::Literal {
                            span: Span::default(),
                            original: "Hello, ".into(),
                            content: "Hello, ".into()
                        },
                        TokenInterpol::Tokens(
                            vec![
                                (Meta::default(), TokenKind::CurlyBOpen.into()),
                                (Meta::default(), Token::Ident("world".into())),
                                (Meta::default(), TokenKind::Assign.into()),
                                (Meta::default(), Token::Value("World".into())),
                                (Meta::default(), TokenKind::Semicolon.into()),
                                (Meta::default(), TokenKind::CurlyBClose.into()),
                                (Meta::default(), TokenKind::Dot.into()),
                                (Meta::default(), Token::Ident("world".into()))
                            ],
                            Meta::default()
                        ),
                        TokenInterpol::Literal {
                            span: Span::default(),
                            original: "!".into(),
                            content: "!".into()
                        }
                    ]
                }
            ],
            "\
Interpol { multiline: false }
  InterpolLiteral = \"Hello, \"
  InterpolAst
    IndexSet
      Set
        Token = CurlyBOpen
        SetEntry
          Attribute
            Ident = world
          Token = Assign
          Value = \"World\"
          Token = Semicolon
        Token = CurlyBClose
      Token = Dot
      Ident = world
    Token = CurlyBClose
  InterpolLiteral = \"!\"
"
        );
    }
    #[test]
    fn index_set() {
        assert_eq!(
            [
                Token::Ident("a".into()),
                TokenKind::Dot, Token::Ident("b".into()),
                TokenKind::Dot, Token::Ident("c".into())
            ],
            "\
IndexSet
  IndexSet
    Ident = a
    Token = Dot
    Ident = b
  Token = Dot
  Ident = c
"
        );
        assert_eq!(
            [
                TokenKind::CurlyBOpen,
                    Token::Ident("a".into()),
                        TokenKind::Dot, Token::Ident("b".into()),
                        TokenKind::Dot, Token::Ident("c".into()),
                    TokenKind::Assign, Token::Value(1.into()), TokenKind::Semicolon,
                TokenKind::CurlyBClose
            ],
            "\
Set
  Token = CurlyBOpen
  SetEntry
    Attribute
      Ident = a
      Token = Dot
      Ident = b
      Token = Dot
      Ident = c
    Token = Assign
    Value = 1
    Token = Semicolon
  Token = CurlyBClose
"
        );
        assert_eq!(
            [
                Token::Ident("test".into()),
                    TokenKind::Dot, Token::Value("invalid ident".into()),
                    TokenKind::Dot, Token::Interpol {
                        multiline: false,
                        parts: vec![
                            TokenInterpol::Literal {
                                span: Span::default(),
                                original: "hi".into(),
                                content: "hi".into()
                            }
                        ]
                    },
                    TokenKind::Dot, Token::Dynamic(
                        vec![(Meta::default(), Token::Ident("a".into()))],
                        Meta::default()
                    )
            ],
            "\
IndexSet
  IndexSet
    IndexSet
      Ident = test
      Token = Dot
      Value = \"invalid ident\"
    Token = Dot
    Interpol { multiline: false }
      InterpolLiteral = \"hi\"
  Token = Dot
  Dynamic
    Token = Dynamic
    Ident = a
    Token = CurlyBClose
"
        );
    }
    #[test]
    fn isset() {
        assert_eq!(
            [
                Token::Ident("a".into()), TokenKind::Question, Token::Value("b".into()),
                TokenKind::And, Token::Value(true.into())
            ],
            "\
Operation
  Operation
    Ident = a
    Token = Question
    Value = \"b\"
  Token = And
  Value = true
"
        );
        assert_eq!(
            [
                Token::Ident("a".into()),
                    TokenKind::Dot, Token::Ident("b".into()),
                    TokenKind::Dot, Token::Ident("c".into()),
                Token::Ident(OR.into()), Token::Value(1.into()),
                TokenKind::Add, Token::Value(1.into())
            ],
            "\
Operation
  OrDefault
    IndexSet
      Ident = a
      Token = Dot
      Ident = b
    Token = Dot
    Ident = c
    Ident = or
    Value = 1
  Token = Add
  Value = 1
"
        );
    }
    #[test]
    fn merge() {
        assert_eq!(
            [
                TokenKind::CurlyBOpen,
                    Token::Ident("a".into()), TokenKind::Assign, Token::Value(1.into()), TokenKind::Semicolon,
                TokenKind::CurlyBClose,
                TokenKind::Merge,
                TokenKind::CurlyBOpen,
                    Token::Ident("b".into()), TokenKind::Assign, Token::Value(2.into()), TokenKind::Semicolon,
                TokenKind::CurlyBClose
            ],
            "\
Operation
  Set
    Token = CurlyBOpen
    SetEntry
      Attribute
        Ident = a
      Token = Assign
      Value = 1
      Token = Semicolon
    Token = CurlyBClose
  Token = Merge
  Set
    Token = CurlyBOpen
    SetEntry
      Attribute
        Ident = b
      Token = Assign
      Value = 2
      Token = Semicolon
    Token = CurlyBClose
"
        );
    }
    #[test]
    fn with() {
        assert_eq!(
            [
                TokenKind::With, Token::Ident("namespace".into()), TokenKind::Semicolon,
                Token::Ident("expr".into())
            ],
            "\
With
  Token = With
  Ident = namespace
  Token = Semicolon
  Ident = expr
"
        );
    }
    #[test]
    fn import() {
        assert_eq!(
            [
                TokenKind::Import,
                Token::Value(Value::Path(Anchor::Store, "nixpkgs".into())),
                TokenKind::CurlyBOpen, TokenKind::CurlyBClose
            ],
            "\
Apply
  Import
    Token = Import
    Value = <nixpkgs>
  Set
    Token = CurlyBOpen
    Token = CurlyBClose
"
        );
    }
    #[test]
    fn assert() {
        assert_eq!(
            [
                TokenKind::Assert, Token::Ident("a".into()), TokenKind::Equal, Token::Ident("b".into()), TokenKind::Semicolon,
                Token::Value("a == b".into())
            ],
            "\
Assert
  Token = Assert
  Operation
    Ident = a
    Token = Equal
    Ident = b
  Token = Semicolon
  Value = \"a == b\"
"
        );
    }
    #[test]
    fn inherit() {
        assert_eq!(
            [
                TokenKind::CurlyBOpen,
                    Token::Ident("a".into()), TokenKind::Assign, Token::Value(1.into()), TokenKind::Semicolon,
                    TokenKind::Inherit, Token::Ident("b".into()), Token::Ident("c".into()), TokenKind::Semicolon,

                    TokenKind::Inherit, TokenKind::ParenOpen, Token::Ident("set".into()), TokenKind::ParenClose,
                    Token::Ident("c".into()), TokenKind::Semicolon,
                TokenKind::CurlyBClose
            ],
            "\
Set
  Token = CurlyBOpen
  SetEntry
    Attribute
      Ident = a
    Token = Assign
    Value = 1
    Token = Semicolon
  Inherit
    Token = Inherit
    Ident = b
    Ident = c
    Token = Semicolon
  Inherit
    Token = Inherit
    InheritFrom
      Token = ParenOpen
      Ident = set
      Token = ParenClose
    Ident = c
    Token = Semicolon
  Token = CurlyBClose
"
        );
    }
    #[test]
    fn ifs() {
        assert_eq!(
            [
                Token::Value(false.into()), TokenKind::Implication,
                TokenKind::Invert, Token::Value(false.into()),
                TokenKind::And,
                Token::Value(false.into()), TokenKind::Equal, Token::Value(true.into()),
                TokenKind::Or,
                Token::Value(true.into())
            ],
            "\
Operation
  Value = false
  Token = Implication
  Operation
    Operation
      Unary
        Token = Invert
        Value = false
      Token = And
      Operation
        Value = false
        Token = Equal
        Value = true
    Token = Or
    Value = true
"
        );
        assert_eq!(
            [
                Token::Value(1.into()), TokenKind::Less, Token::Value(2.into()),
                TokenKind::Or,
                Token::Value(2.into()), TokenKind::LessOrEq, Token::Value(2.into()),
                TokenKind::And,
                Token::Value(2.into()), TokenKind::More, Token::Value(1.into()),
                TokenKind::And,
                Token::Value(2.into()), TokenKind::MoreOrEq, Token::Value(2.into())
            ],
            "\
Operation
  Operation
    Value = 1
    Token = Less
    Value = 2
  Token = Or
  Operation
    Operation
      Operation
        Value = 2
        Token = LessOrEq
        Value = 2
      Token = And
      Operation
        Value = 2
        Token = More
        Value = 1
    Token = And
    Operation
      Value = 2
      Token = MoreOrEq
      Value = 2
"
        );
        assert_eq!(
            [
                Token::Value(1.into()), TokenKind::Equal, Token::Value(1.into()),
                TokenKind::And,
                Token::Value(2.into()), TokenKind::NotEqual, Token::Value(3.into())
            ],
            "\
Operation
  Operation
    Value = 1
    Token = Equal
    Value = 1
  Token = And
  Operation
    Value = 2
    Token = NotEqual
    Value = 3
"
        );
        assert_eq!(
            [
                TokenKind::If, Token::Value(false.into()), TokenKind::Then,
                    Token::Value(1.into()),
                TokenKind::Else,
                    TokenKind::If, Token::Value(true.into()), TokenKind::Then,
                        Token::Value(2.into()),
                    TokenKind::Else,
                        Token::Value(3.into())
            ],
            "\
IfElse
  Token = If
  Value = false
  Token = Then
  Value = 1
  Token = Else
  IfElse
    Token = If
    Value = true
    Token = Then
    Value = 2
    Token = Else
    Value = 3
"
        );
    }
    #[test]
    fn list() {
        assert_eq!(
            [
               TokenKind::SquareBOpen,
               Token::Ident("a".into()), Token::Value(2.into()), Token::Value(3.into()),
               Token::Value("lol".into()),
               TokenKind::SquareBClose
            ],
            "\
List
  Token = SquareBOpen
  ListItem
    Ident = a
  ListItem
    Value = 2
  ListItem
    Value = 3
  ListItem
    Value = \"lol\"
  Token = SquareBClose
"
        );
        assert_eq!(
            [
               TokenKind::SquareBOpen, Token::Value(1.into()), TokenKind::SquareBClose, TokenKind::Concat,
               TokenKind::SquareBOpen, Token::Value(2.into()), TokenKind::SquareBClose, TokenKind::Concat,
               TokenKind::SquareBOpen, Token::Value(3.into()), TokenKind::SquareBClose
            ],
            "\
Operation
  Operation
    List
      Token = SquareBOpen
      ListItem
        Value = 1
      Token = SquareBClose
    Token = Concat
    List
      Token = SquareBOpen
      ListItem
        Value = 2
      Token = SquareBClose
  Token = Concat
  List
    Token = SquareBOpen
    ListItem
      Value = 3
    Token = SquareBClose
"
        );
    }
    #[test]
    fn functions() {
        assert_eq!(
            [
               Token::Ident("a".into()), TokenKind::Colon, Token::Ident("b".into()), TokenKind::Colon,
               Token::Ident("a".into()), TokenKind::Add, Token::Ident("b".into())
            ],
            "\
Lambda
  Ident = a
  Token = Colon
  Lambda
    Ident = b
    Token = Colon
    Operation
      Ident = a
      Token = Add
      Ident = b
"
        );
        assert_eq!(
            [
                Token::Ident("a".into()), Token::Value(1.into()), Token::Value(2.into()),
                TokenKind::Add,
                Token::Value(3.into())
            ],
            "\
Operation
  Apply
    Apply
      Ident = a
      Value = 1
    Value = 2
  Token = Add
  Value = 3
"
        );
    }
    #[test]
    fn patterns() {
        assert_eq!(
            [TokenKind::CurlyBOpen, TokenKind::Ellipsis, TokenKind::CurlyBClose, TokenKind::Colon, Token::Value(1.into())],
            "\
Lambda
  Pattern
    Token = CurlyBOpen
    Token = Ellipsis
    Token = CurlyBClose
  Token = Colon
  Value = 1
"
        );
        assert_eq!(
            [
                TokenKind::CurlyBOpen, TokenKind::CurlyBClose, TokenKind::At, Token::Ident("outer".into()),
                TokenKind::Colon, Token::Value(1.into())
            ],
            "\
Lambda
  Pattern
    Token = CurlyBOpen
    Token = CurlyBClose
    PatBind
      Token = At
      Ident = outer
  Token = Colon
  Value = 1
"
        );
        assert_eq!(
            [
                TokenKind::CurlyBOpen,
                    Token::Ident("a".into()), TokenKind::Comma,
                    Token::Ident("b".into()), TokenKind::Question, Token::Value("default".into()),
                TokenKind::CurlyBClose,
                TokenKind::Colon,
                Token::Ident("a".into())
            ],
            "\
Lambda
  Pattern
    Token = CurlyBOpen
    PatEntry
      Ident = a
      Token = Comma
    PatEntry
      Ident = b
      Token = Question
      Value = \"default\"
    Token = CurlyBClose
  Token = Colon
  Ident = a
"
        );
        assert_eq!(
            [
                TokenKind::CurlyBOpen,
                    Token::Ident("a".into()), TokenKind::Comma,
                    Token::Ident("b".into()), TokenKind::Question, Token::Value("default".into()), TokenKind::Comma,
                    TokenKind::Ellipsis,
                TokenKind::CurlyBClose,
                TokenKind::At,
                Token::Ident("outer".into()),
                TokenKind::Colon,
                Token::Ident("outer".into())
            ],
            "\
Lambda
  Pattern
    Token = CurlyBOpen
    PatEntry
      Ident = a
      Token = Comma
    PatEntry
      Ident = b
      Token = Question
      Value = \"default\"
      Token = Comma
    Token = Ellipsis
    Token = CurlyBClose
    PatBind
      Token = At
      Ident = outer
  Token = Colon
  Ident = outer
"
        );
        assert_eq!(
            [
                Token::Ident("outer".into()), TokenKind::At,
                TokenKind::CurlyBOpen, Token::Ident("a".into()), TokenKind::CurlyBClose,
                TokenKind::Colon,
                Token::Ident("outer".into())
            ],
            "\
Lambda
  Pattern
    PatBind
      Ident = outer
      Token = At
    Token = CurlyBOpen
    PatEntry
      Ident = a
    Token = CurlyBClose
  Token = Colon
  Ident = outer
"
        );
        assert_eq!(
            [
                TokenKind::CurlyBOpen,
                    Token::Ident("a".into()), TokenKind::Question, TokenKind::CurlyBOpen, TokenKind::CurlyBClose,
                TokenKind::CurlyBClose, TokenKind::Colon, Token::Ident("a".into())
            ],
            "\
Lambda
  Pattern
    Token = CurlyBOpen
    PatEntry
      Ident = a
      Token = Question
      Set
        Token = CurlyBOpen
        Token = CurlyBClose
    Token = CurlyBClose
  Token = Colon
  Ident = a
"
        );
    }
    #[test]
    fn dynamic() {
        assert_eq!(
            [Token::Dynamic(vec![(Meta::default(), Token::Ident("a".into()))], Meta::default())],
            "\
Dynamic
  Token = Dynamic
  Ident = a
  Token = CurlyBClose
"
        );
    }
}
