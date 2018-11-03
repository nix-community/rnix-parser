//! The parser: turns a series of tokens into an AST

use crate::{
    types::{TypedNode, Root},
    tokenizer::Token
};

use rowan::{GreenNodeBuilder, SyntaxNode, SmolStr};
use std::collections::VecDeque;

const OR: &'static str = "or";

/// An error that occured during parsing
#[derive(Clone, Debug, Fail, PartialEq)]
pub enum ParseError {
    #[fail(display = "unexpected input")]
    Unexpected(Node<rowan::OwnedRoot<Types>>),
    #[fail(display = "unexpected eof")]
    UnexpectedEOF,
    #[fail(display = "unexpected eof, wanted {:?}", _0)]
    UnexpectedEOFWanted(Token),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ASTKind {
    Apply,
    Assert,
    Attribute,
    Dynamic,
    Error,
    IfElse,
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
    Unary,
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
impl From<ASTKind> for NodeType {
    fn from(kind: ASTKind) -> Self {
        NodeType::Marker(kind)
    }
}
impl From<Token> for NodeType {
    fn from(kind: Token) -> Self {
        NodeType::Token(kind)
    }
}

/// Teaches the rowan library about rnix' preferred types
#[derive(Debug)]
pub struct Types;
impl rowan::Types for Types {
    type Kind = NodeType;
    type RootData = Vec<ParseError>;
}

pub type Node<R = rowan::OwnedRoot<Types>> = rowan::SyntaxNode<Types, R>;

/// The result of a parse
pub struct AST {
    node: Node
}
impl AST {
    /// Return the root node
    pub fn into_node(self) -> Node {
        self.node
    }
    /// Return a reference to the root node
    pub fn node(&self) -> &Node {
        &self.node
    }
    /// Return a typed root node
    pub fn root<'a>(&'a self) -> Root<rowan::RefRoot<'a, Types>> {
        Root::cast(self.node.borrowed()).unwrap()
    }
    /// Return all errors in the tree, if any
    pub fn errors(&self) -> Vec<ParseError> {
        let mut errors = Vec::new();
        errors.extend_from_slice(self.node.root_data());
        errors.extend(
            self.root().errors().into_iter()
                .map(|node| ParseError::Unexpected(node.owned()))
        );

        errors
    }
    /// Either return the first error in the tree, or if there are none return self
    pub fn as_result(self) -> Result<Self, ParseError> {
        if let Some(err) = self.node.root_data().first() {
            return Err(err.clone());
        }
        if let Some(node) = self.root().errors().first() {
            return Err(ParseError::Unexpected(node.owned()));
        }
        Ok(self)
    }
}

struct Parser<I>
    where I: Iterator<Item = (Token, SmolStr)>
{
    builder: GreenNodeBuilder<Types>,
    errors: Vec<ParseError>,

    buffer: VecDeque<I::Item>,
    iter: I
}
impl<I> Parser<I>
    where I: Iterator<Item = (Token, SmolStr)>
{
    fn new(iter: I) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),

            buffer: VecDeque::with_capacity(1),
            iter
        }
    }

    fn peek_raw(&mut self) -> Option<&(Token, SmolStr)> {
        if self.buffer.is_empty() {
            if let Some(token) = self.iter.next() {
                self.buffer.push_back(token);
            }
        }
        self.buffer.front()
    }
    fn bump(&mut self) {
        let next = self.buffer.pop_front().or_else(|| self.iter.next());
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

    fn parse_dynamic(&mut self) {
        self.builder.start_internal(NodeType::Marker(ASTKind::Dynamic));
        self.bump();
        while self.peek().map(|t| t != Token::DynamicEnd).unwrap_or(false) {
            self.parse_expr();
        }
        self.bump();
        self.builder.finish_internal();
    }
    fn parse_interpol(&mut self) {
        self.builder.start_internal(NodeType::Marker(ASTKind::Interpol));

        self.builder.start_internal(NodeType::Marker(ASTKind::InterpolLiteral));
        self.bump();
        self.builder.finish_internal();

        self.builder.start_internal(NodeType::Marker(ASTKind::InterpolAst));
        loop {
            match self.peek() {
                None | Some(Token::InterpolEnd) => {
                    self.builder.finish_internal();

                    self.builder.start_internal(NodeType::Marker(ASTKind::InterpolLiteral));
                    self.bump();
                    self.builder.finish_internal();
                    break;
                },
                Some(Token::InterpolEndStart) => {
                    self.builder.finish_internal();

                    self.builder.start_internal(NodeType::Marker(ASTKind::InterpolLiteral));
                    self.bump();
                    self.builder.finish_internal();

                    self.builder.start_internal(NodeType::Marker(ASTKind::InterpolAst));
                },
                Some(_) => self.parse_expr()
            }
        }

        self.builder.finish_internal();
    }
    fn next_attr(&mut self) {
        match self.peek() {
            Some(Token::DynamicStart) => self.parse_dynamic(),
            Some(Token::InterpolStart) => self.parse_interpol(),
            Some(Token::String) => self.bump(),
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
    fn parse_pattern(&mut self, bound: bool) {
        if self.peek().map(|t| t == Token::CurlyBClose).unwrap_or(true) {
            self.bump();
        } else {
            loop {
                match self.peek() {
                    Some(Token::CurlyBClose) => {
                        self.bump();
                        break;
                    },
                    Some(Token::Ellipsis) => {
                        self.bump();
                        self.expect(Token::CurlyBClose);
                        break;
                    },
                    Some(Token::Ident) => {
                        self.builder.start_internal(NodeType::Marker(ASTKind::PatEntry));
                        self.bump();
                        if let Some(Token::Question) = self.peek() {
                            self.bump();
                            self.parse_expr();
                        }
                        self.builder.finish_internal();

                        match self.peek() {
                            Some(Token::Comma) => self.bump(),
                            _ => {
                                self.expect(Token::CurlyBClose);
                                break;
                            },
                        }
                    },
                    None => {
                        self.errors.push(ParseError::UnexpectedEOFWanted(Token::Ident));
                        break;
                    },
                    Some(_) => {
                        self.builder.start_internal(NodeType::Marker(ASTKind::Error));
                        self.bump();
                        self.builder.finish_internal();
                    }
                }
            }
        }

        if self.peek() == Some(Token::At) {
            let kind = if bound { ASTKind::Error } else { ASTKind::PatBind };
            self.builder.start_internal(NodeType::Marker(kind));
            self.bump();
            self.expect(Token::Ident);
            self.builder.finish_internal();
        }
    }
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
        let checkpoint = self.builder.checkpoint();
        match self.peek() {
            Some(Token::ParenOpen) => {
                self.builder.start_internal(NodeType::Marker(ASTKind::Paren));
                self.bump();
                self.parse_expr();
                self.bump();
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
                // Do a lookahead:
                let mut peek = [None, None];
                for i in 0..2 {
                    let mut token;
                    peek[i] = loop {
                        token = self.iter.next();
                        let kind = token.as_ref().map(|&(t, _)| t);
                        if let Some(token) = token {
                            self.buffer.push_back(token);
                        }
                        if kind.map(|t| !t.is_trivia()).unwrap_or(true) {
                            break kind;
                        }
                    };
                }

                match peek {
                    [Some(Token::Ident), Some(Token::Comma)]
                    | [Some(Token::Ident), Some(Token::Question)]
                    | [Some(Token::Ident), Some(Token::CurlyBClose)]
                    | [Some(Token::Ellipsis), Some(Token::CurlyBClose)]
                    | [Some(Token::CurlyBClose), Some(Token::Colon)]
                    | [Some(Token::CurlyBClose), Some(Token::At)] => {
                        // This looks like a pattern
                        self.builder.start_internal(NodeType::Marker(ASTKind::Lambda));

                        self.builder.start_internal(NodeType::Marker(ASTKind::Pattern));
                        self.bump();
                        self.parse_pattern(false);
                        self.builder.finish_internal();

                        self.expect(Token::Colon);
                        self.parse_expr();

                        self.builder.finish_internal();
                    },
                    _ => {
                        // This looks like a set
                        self.builder.start_internal(NodeType::Marker(ASTKind::Set));
                        self.bump();
                        self.parse_set(Token::CurlyBClose);
                        self.builder.finish_internal();
                    }
                }
            },
            Some(Token::SquareBOpen) => {
                self.builder.start_internal(NodeType::Marker(ASTKind::List));
                self.bump();
                while self.peek().map(|t| t != Token::SquareBClose).unwrap_or(false) {
                    self.builder.start_internal(NodeType::Marker(ASTKind::ListItem));
                    self.parse_val();
                    self.builder.finish_internal();
                }
                self.bump();
                self.builder.finish_internal();
            },
            Some(Token::DynamicStart) => self.parse_dynamic(),
            Some(Token::InterpolStart) => self.parse_interpol(),
            Some(t) if t.is_value() => self.bump(),
            Some(Token::Ident) => {
                let checkpoint = self.builder.checkpoint();
                self.bump();

                match self.peek() {
                    Some(Token::Colon) => {
                        self.builder.start_internal_at(checkpoint, NodeType::Marker(ASTKind::Lambda));
                        self.bump();
                        self.parse_expr();
                        self.builder.finish_internal();
                    },
                    Some(Token::At) => {
                        self.builder.start_internal_at(checkpoint, NodeType::Marker(ASTKind::Lambda));
                        self.builder.start_internal_at(checkpoint, NodeType::Marker(ASTKind::Pattern));
                        self.builder.start_internal_at(checkpoint, NodeType::Marker(ASTKind::PatBind));
                        self.bump();
                        self.builder.finish_internal(); // PatBind

                        self.expect(Token::CurlyBOpen);
                        self.parse_pattern(true);
                        self.builder.finish_internal(); // Pattern

                        self.expect(Token::Colon);
                        self.parse_expr();
                        self.builder.finish_internal(); // Lambda
                    },
                    _ => ()
                }
            },
            _ => {
                self.builder.start_internal(NodeType::Marker(ASTKind::Error));
                self.bump();
                self.builder.finish_internal();
            }
        }

        while self.peek() == Some(Token::Dot) {
            self.builder.start_internal_at(checkpoint, NodeType::Marker(ASTKind::IndexSet));
            self.bump();
            self.next_attr();
            self.builder.finish_internal();

            if self.peek_data().map(|&(t, ref s)| t == Token::Ident && s == OR).unwrap_or(false) {
                self.builder.start_internal_at(checkpoint, NodeType::Marker(ASTKind::OrDefault));
                self.bump();
                self.parse_val();
                self.builder.finish_internal();
            }
        }
    }
    fn parse_fn(&mut self) {
        let checkpoint = self.builder.checkpoint();
        self.parse_val();

        while self.peek().map(|t| t.is_fn_arg()).unwrap_or(false) {
            self.builder.start_internal_at(checkpoint, NodeType::Marker(ASTKind::Apply));
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
        let checkpoint = self.builder.checkpoint();
        next(self);
        while self.peek().map(|t| ops.contains(&t)).unwrap_or(false) {
            self.builder.start_internal_at(checkpoint, NodeType::Marker(ASTKind::Operation));
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
                let checkpoint = self.builder.checkpoint();
                self.bump();

                if self.peek() == Some(Token::CurlyBOpen) {
                    self.builder.start_internal_at(checkpoint, NodeType::Marker(ASTKind::Let));
                    self.bump();
                    self.parse_set(Token::CurlyBClose);
                    self.builder.finish_internal();
                } else {
                    self.builder.start_internal_at(checkpoint, NodeType::Marker(ASTKind::LetIn));
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
            Some(Token::If) => {
                self.builder.start_internal(NodeType::Marker(ASTKind::IfElse));
                self.bump();
                self.parse_expr();
                self.expect(Token::Then);
                self.parse_expr();
                self.expect(Token::Else);
                self.parse_expr();
                self.builder.finish_internal();
            },
            Some(Token::Assert) => {
                self.builder.start_internal(NodeType::Marker(ASTKind::Assert));
                self.bump();
                self.parse_expr();
                self.expect(Token::Semicolon);
                self.parse_expr();
                self.builder.finish_internal();
            },
            _ => self.parse_math()
        }
    }
}

/// Parse tokens into an AST
pub fn parse<I>(iter: I) -> AST
    where I: IntoIterator<Item = (Token, SmolStr)>
{
    let mut parser = Parser::new(iter.into_iter());
    parser.builder.start_internal(NodeType::Marker(ASTKind::Root));
    parser.parse_expr();
    if parser.peek().is_some() {
        parser.builder.start_internal(NodeType::Marker(ASTKind::Error));
        while parser.peek().is_some() {
            parser.bump();
        }
        parser.builder.finish_internal();
    }
    parser.builder.finish_internal();
    AST {
        node: SyntaxNode::new(parser.builder.finish(), parser.errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use rowan::WalkEvent;
    use std::fmt::Write;

    fn stringify(node: Node<rowan::RefRoot<Types>>) -> String {
        let mut out = String::new();
        let mut indent = 0;
        for event in node.preorder() {
            match event {
                WalkEvent::Enter(node) => {
                    writeln!(out, "{:indent$}{:?}", "", node, indent = indent).unwrap();
                    indent += 2;
                },
                WalkEvent::Leave(_) =>
                    indent -= 2
            }
        }
        out
    }

    macro_rules! assert_eq {
        ([$(($token:expr, $str:expr)),*], $expected:expr) => {
            let parsed = parse(vec![$(($token, $str.into())),*]).as_result().expect("error occured when parsing");

            let actual = stringify(parsed.node().borrowed());
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
                (Token::Integer, "42"),
                (Token::Semicolon, ";"),

                (Token::Ident, "H4X0RNUM83R"),
                (Token::Whitespace, " "),
                (Token::Assign, "="),
                (Token::Whitespace, " "),
                (Token::Float, "1.337"),
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
      Token(Integer)@[20; 22)
      Token(Semicolon)@[22; 23)
    Marker(SetEntry)@[23; 43)
      Marker(Attribute)@[23; 35)
        Token(Ident)@[23; 34)
        Token(Whitespace)@[34; 35)
      Token(Assign)@[35; 36)
      Token(Whitespace)@[36; 37)
      Token(Float)@[37; 42)
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
                (Token::Integer, "1"),
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
      Token(Integer)@[9; 10)
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
        assert_eq!(
            [
                (Token::CurlyBOpen, "{"),

                (Token::Ident, "a"),
                    (Token::Dot, "."),
                    (Token::Ident, "b"),
                (Token::Assign, "="),
                (Token::Integer, "2"),
                (Token::Semicolon, ";"),

                (Token::InterpolStart, "\"${"),
                    (Token::Ident, "c"),
                (Token::InterpolEnd, "}\""),
                    (Token::Dot, "."),
                    (Token::DynamicStart, "${"),
                        (Token::Ident, "d"),
                    (Token::DynamicEnd, "${"),
                (Token::Assign, "="),
                (Token::Integer, "3"),
                (Token::Semicolon, ";"),

                (Token::CurlyBClose, "}")
            ],
            "\
Marker(Root)@[0; 23)
  Marker(Set)@[0; 23)
    Token(CurlyBOpen)@[0; 1)
    Marker(SetEntry)@[1; 7)
      Marker(Attribute)@[1; 4)
        Token(Ident)@[1; 2)
        Token(Dot)@[2; 3)
        Token(Ident)@[3; 4)
      Token(Assign)@[4; 5)
      Token(Integer)@[5; 6)
      Token(Semicolon)@[6; 7)
    Marker(SetEntry)@[7; 22)
      Marker(Attribute)@[7; 19)
        Marker(Interpol)@[7; 13)
          Marker(InterpolLiteral)@[7; 10)
            Token(InterpolStart)@[7; 10)
          Marker(InterpolAst)@[10; 11)
            Token(Ident)@[10; 11)
          Marker(InterpolLiteral)@[11; 13)
            Token(InterpolEnd)@[11; 13)
        Token(Dot)@[13; 14)
        Marker(Dynamic)@[14; 19)
          Token(DynamicStart)@[14; 16)
          Token(Ident)@[16; 17)
          Token(DynamicEnd)@[17; 19)
      Token(Assign)@[19; 20)
      Token(Integer)@[20; 21)
      Token(Semicolon)@[21; 22)
    Token(CurlyBClose)@[22; 23)
"
        );
    }
    #[test]
    fn math() {
        assert_eq!(
            [
                (Token::Integer, "1"),
                (Token::Whitespace, " "),
                (Token::Add, "+"),
                (Token::Whitespace, " "),
                (Token::Integer, "2"),
                (Token::Whitespace, " "),
                (Token::Add, "+"),
                (Token::Whitespace, " "),
                (Token::Integer, "3"),
                (Token::Whitespace, " "),
                (Token::Mul, "*"),
                (Token::Whitespace, " "),
                (Token::Integer, "4")
            ],
            "\
Marker(Root)@[0; 13)
  Marker(Operation)@[0; 13)
    Marker(Operation)@[0; 6)
      Token(Integer)@[0; 1)
      Token(Whitespace)@[1; 2)
      Token(Add)@[2; 3)
      Token(Whitespace)@[3; 4)
      Token(Integer)@[4; 5)
      Token(Whitespace)@[5; 6)
    Token(Add)@[6; 7)
    Marker(Operation)@[7; 13)
      Token(Whitespace)@[7; 8)
      Token(Integer)@[8; 9)
      Token(Whitespace)@[9; 10)
      Token(Mul)@[10; 11)
      Token(Whitespace)@[11; 12)
      Token(Integer)@[12; 13)
"
        );
        assert_eq!(
            [
                (Token::Integer, "5"),
                (Token::Mul, "*"),
                (Token::Sub, "-"),
                (Token::ParenOpen, "("),
                (Token::Integer, "3"),
                (Token::Sub, "-"),
                (Token::Integer, "2"),
                (Token::ParenClose, ")")
            ],
            "\
Marker(Root)@[0; 8)
  Marker(Operation)@[0; 8)
    Token(Integer)@[0; 1)
    Token(Mul)@[1; 2)
    Marker(Unary)@[2; 8)
      Token(Sub)@[2; 3)
      Marker(Paren)@[3; 8)
        Token(ParenOpen)@[3; 4)
        Marker(Operation)@[4; 7)
          Token(Integer)@[4; 5)
          Token(Sub)@[5; 6)
          Token(Integer)@[6; 7)
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
                    (Token::Integer, "42"),
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
      Token(Integer)@[8; 10)
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
                        (Token::Integer, "42"),
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
      Token(Integer)@[6; 8)
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
    #[test]
    fn interpolation() {
        assert_eq!(
            [
                (Token::Whitespace, " "),
                (Token::InterpolStart, r#""Hello, ${"#),
                    (Token::Whitespace, " "),
                    (Token::CurlyBOpen, "{"),
                    (Token::Whitespace, " "),
                    (Token::Ident, "world"),
                    (Token::Whitespace, " "),
                    (Token::Assign, "="),
                    (Token::Whitespace, " "),
                    (Token::String, r#""World""#),
                    (Token::Semicolon, ";"),
                    (Token::Whitespace, " "),
                    (Token::CurlyBClose, "}"),
                    (Token::Dot, "."),
                    (Token::Ident, "world"),
                    (Token::Whitespace, " "),
                (Token::InterpolEnd, r#"}!""#),
                (Token::Whitespace, " ")
            ],
            "\
Marker(Root)@[0; 43)
  Token(Whitespace)@[0; 1)
  Marker(Interpol)@[1; 42)
    Marker(InterpolLiteral)@[1; 11)
      Token(InterpolStart)@[1; 11)
    Marker(InterpolAst)@[11; 39)
      Token(Whitespace)@[11; 12)
      Marker(IndexSet)@[12; 38)
        Marker(Set)@[12; 32)
          Token(CurlyBOpen)@[12; 13)
          Token(Whitespace)@[13; 14)
          Marker(SetEntry)@[14; 30)
            Marker(Attribute)@[14; 20)
              Token(Ident)@[14; 19)
              Token(Whitespace)@[19; 20)
            Token(Assign)@[20; 21)
            Token(Whitespace)@[21; 22)
            Token(String)@[22; 29)
            Token(Semicolon)@[29; 30)
          Token(Whitespace)@[30; 31)
          Token(CurlyBClose)@[31; 32)
        Token(Dot)@[32; 33)
        Token(Ident)@[33; 38)
      Token(Whitespace)@[38; 39)
    Marker(InterpolLiteral)@[39; 42)
      Token(InterpolEnd)@[39; 42)
  Token(Whitespace)@[42; 43)
"
        );
      assert_eq!(
          [
                (Token::Whitespace, " "),
                (Token::InterpolStart, r#""${"#),
                    (Token::Ident, "hello"),
                (Token::InterpolEndStart, r#"} ${"#),
                    (Token::Ident, "world"),
                (Token::InterpolEnd, r#"}""#),
                (Token::Whitespace, " ")
          ],
          "\
Marker(Root)@[0; 21)
  Token(Whitespace)@[0; 1)
  Marker(Interpol)@[1; 20)
    Marker(InterpolLiteral)@[1; 4)
      Token(InterpolStart)@[1; 4)
    Marker(InterpolAst)@[4; 9)
      Token(Ident)@[4; 9)
    Marker(InterpolLiteral)@[9; 13)
      Token(InterpolEndStart)@[9; 13)
    Marker(InterpolAst)@[13; 18)
      Token(Ident)@[13; 18)
    Marker(InterpolLiteral)@[18; 20)
      Token(InterpolEnd)@[18; 20)
  Token(Whitespace)@[20; 21)
"
      );
      assert_eq!(
          [
                (Token::Whitespace, " "),
                (Token::InterpolStart, r#"''${"#),
                    (Token::InterpolStart, r#""${"#),
                        (Token::Ident, "var"),
                    (Token::InterpolEnd, r#"}""#),
                (Token::InterpolEnd, r#"}''"#),
                (Token::Whitespace, " ")
          ],
          "\
Marker(Root)@[0; 17)
  Token(Whitespace)@[0; 1)
  Marker(Interpol)@[1; 16)
    Marker(InterpolLiteral)@[1; 5)
      Token(InterpolStart)@[1; 5)
    Marker(InterpolAst)@[5; 13)
      Marker(Interpol)@[5; 13)
        Marker(InterpolLiteral)@[5; 8)
          Token(InterpolStart)@[5; 8)
        Marker(InterpolAst)@[8; 11)
          Token(Ident)@[8; 11)
        Marker(InterpolLiteral)@[11; 13)
          Token(InterpolEnd)@[11; 13)
    Marker(InterpolLiteral)@[13; 16)
      Token(InterpolEnd)@[13; 16)
  Token(Whitespace)@[16; 17)
"
      );
    }
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
                    (Token::Integer, "1"),
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
      Token(Integer)@[7; 8)
      Token(Semicolon)@[8; 9)
    Token(CurlyBClose)@[9; 10)
"
        );
        assert_eq!(
            [
                (Token::Ident, "test"),
                    (Token::Dot, "."),
                    (Token::String, "\"invalid ident\""),
                    (Token::Dot, "."),
                    (Token::InterpolStart, "\"${"),
                        (Token::Ident, "hi"),
                    (Token::InterpolEnd, "}\""),
                    (Token::Dot, "."),
                    (Token::DynamicStart, "${"),
                        (Token::Ident, "a"),
                    (Token::DynamicEnd, "}")
            ],
            "\
Marker(Root)@[0; 33)
  Marker(IndexSet)@[0; 33)
    Marker(IndexSet)@[0; 28)
      Marker(IndexSet)@[0; 20)
        Token(Ident)@[0; 4)
        Token(Dot)@[4; 5)
        Token(String)@[5; 20)
      Token(Dot)@[20; 21)
      Marker(Interpol)@[21; 28)
        Marker(InterpolLiteral)@[21; 24)
          Token(InterpolStart)@[21; 24)
        Marker(InterpolAst)@[24; 26)
          Token(Ident)@[24; 26)
        Marker(InterpolLiteral)@[26; 28)
          Token(InterpolEnd)@[26; 28)
    Token(Dot)@[28; 29)
    Marker(Dynamic)@[29; 33)
      Token(DynamicStart)@[29; 31)
      Token(Ident)@[31; 32)
      Token(DynamicEnd)@[32; 33)
"
        );
    }
    #[test]
    fn isset() {
        assert_eq!(
            [
                (Token::Ident, "a"),
                (Token::Question, "?"),
                (Token::String, "\"b\""),
                (Token::And, "&&"),
                (Token::Ident, "true")
            ],
            "\
Marker(Root)@[0; 11)
  Marker(Operation)@[0; 11)
    Marker(Operation)@[0; 5)
      Token(Ident)@[0; 1)
      Token(Question)@[1; 2)
      Token(String)@[2; 5)
    Token(And)@[5; 7)
    Token(Ident)@[7; 11)
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
                (Token::Integer, "1"),
                (Token::Add, "+"),
                (Token::Integer, "1")
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
      Token(Integer)@[7; 8)
    Token(Add)@[8; 9)
    Token(Integer)@[9; 10)
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
                (Token::Integer, "1"),
                (Token::Semicolon, ";"),
                (Token::CurlyBClose, "}"),
                (Token::Merge, "//"),
                (Token::CurlyBOpen, "{"),
                (Token::Ident, "b"),
                (Token::Assign, "="),
                (Token::Integer, "2"),
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
        Token(Integer)@[3; 4)
        Token(Semicolon)@[4; 5)
      Token(CurlyBClose)@[5; 6)
    Token(Merge)@[6; 8)
    Marker(Set)@[8; 14)
      Token(CurlyBOpen)@[8; 9)
      Marker(SetEntry)@[9; 13)
        Marker(Attribute)@[9; 10)
          Token(Ident)@[9; 10)
        Token(Assign)@[10; 11)
        Token(Integer)@[11; 12)
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
    fn assert() {
        assert_eq!(
            [
                (Token::Assert, "assert"),
                (Token::Ident, "a"),
                (Token::Equal, "=="),
                (Token::Ident, "b"),
                (Token::Semicolon, ";"),
                (Token::String, "\"a == b\"")
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
    Token(String)@[11; 19)
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
                        (Token::Integer, "1"),
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
      Token(Integer)@[3; 4)
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
    #[test]
    fn ifs() {
        assert_eq!(
            [
                (Token::Ident, "false"),
                (Token::Implication, "->"),
                (Token::Invert, "!"),
                (Token::Ident, "false"),

                (Token::And, "&&"),

                (Token::Ident, "false"),
                (Token::Equal, "=="),
                (Token::Ident, "true"),

                (Token::Or, "||"),

                (Token::Ident, "true")
            ],
            "\
Marker(Root)@[0; 32)
  Marker(Operation)@[0; 32)
    Token(Ident)@[0; 5)
    Token(Implication)@[5; 7)
    Marker(Operation)@[7; 32)
      Marker(Operation)@[7; 26)
        Marker(Unary)@[7; 13)
          Token(Invert)@[7; 8)
          Token(Ident)@[8; 13)
        Token(And)@[13; 15)
        Marker(Operation)@[15; 26)
          Token(Ident)@[15; 20)
          Token(Equal)@[20; 22)
          Token(Ident)@[22; 26)
      Token(Or)@[26; 28)
      Token(Ident)@[28; 32)
"
        );
        assert_eq!(
            [
                (Token::Integer, "1"),
                (Token::Less, "<"),
                (Token::Integer, "2"),

                (Token::Or, "||"),

                (Token::Integer, "2"),
                (Token::LessOrEq, "<="),
                (Token::Integer, "2"),

                (Token::And, "&&"),

                (Token::Integer, "2"),
                (Token::More, ">"),
                (Token::Integer, "1"),

                (Token::And, "&&"),

                (Token::Integer, "2"),
                (Token::MoreOrEq, ">="),
                (Token::Integer, "2")
            ],
            "\
Marker(Root)@[0; 20)
  Marker(Operation)@[0; 20)
    Marker(Operation)@[0; 3)
      Token(Integer)@[0; 1)
      Token(Less)@[1; 2)
      Token(Integer)@[2; 3)
    Token(Or)@[3; 5)
    Marker(Operation)@[5; 20)
      Marker(Operation)@[5; 14)
        Marker(Operation)@[5; 9)
          Token(Integer)@[5; 6)
          Token(LessOrEq)@[6; 8)
          Token(Integer)@[8; 9)
        Token(And)@[9; 11)
        Marker(Operation)@[11; 14)
          Token(Integer)@[11; 12)
          Token(More)@[12; 13)
          Token(Integer)@[13; 14)
      Token(And)@[14; 16)
      Marker(Operation)@[16; 20)
        Token(Integer)@[16; 17)
        Token(MoreOrEq)@[17; 19)
        Token(Integer)@[19; 20)
"
        );
        assert_eq!(
            [
                (Token::Integer, "1"),
                (Token::Equal, "=="),
                (Token::Integer, "1"),

                (Token::And, "&&"),

                (Token::Integer, "2"),
                (Token::NotEqual, "!="),
                (Token::Integer, "3")
            ],
            "\
Marker(Root)@[0; 10)
  Marker(Operation)@[0; 10)
    Marker(Operation)@[0; 4)
      Token(Integer)@[0; 1)
      Token(Equal)@[1; 3)
      Token(Integer)@[3; 4)
    Token(And)@[4; 6)
    Marker(Operation)@[6; 10)
      Token(Integer)@[6; 7)
      Token(NotEqual)@[7; 9)
      Token(Integer)@[9; 10)
"
        );
        assert_eq!(
            [
                (Token::If, "if"),
                (Token::Ident, "false"),
                (Token::Then, "then"),
                    (Token::Integer, "1"),
                (Token::Else, "else"),
                    (Token::If, "if"),
                    (Token::Ident, "true"),
                    (Token::Then, "then"),
                        (Token::Ident, "two"),
                    (Token::Else, "else"),
                        (Token::Integer, "3")
            ],
            "\
Marker(Root)@[0; 34)
  Marker(IfElse)@[0; 34)
    Token(If)@[0; 2)
    Token(Ident)@[2; 7)
    Token(Then)@[7; 11)
    Token(Integer)@[11; 12)
    Token(Else)@[12; 16)
    Marker(IfElse)@[16; 34)
      Token(If)@[16; 18)
      Token(Ident)@[18; 22)
      Token(Then)@[22; 26)
      Token(Ident)@[26; 29)
      Token(Else)@[29; 33)
      Token(Integer)@[33; 34)
"
        );
    }
    #[test]
    fn list() {
        assert_eq!(
            [
                (Token::SquareBOpen, "["),
                (Token::Ident, "a"),
                (Token::Integer, "2"),
                (Token::Integer, "3"),
                (Token::String, "\"lol\""),
                (Token::SquareBClose, "]")
            ],
            "\
Marker(Root)@[0; 10)
  Marker(List)@[0; 10)
    Token(SquareBOpen)@[0; 1)
    Marker(ListItem)@[1; 2)
      Token(Ident)@[1; 2)
    Marker(ListItem)@[2; 3)
      Token(Integer)@[2; 3)
    Marker(ListItem)@[3; 4)
      Token(Integer)@[3; 4)
    Marker(ListItem)@[4; 9)
      Token(String)@[4; 9)
    Token(SquareBClose)@[9; 10)
"
        );
        assert_eq!(
            [
                (Token::SquareBOpen, "["), (Token::Integer, "1"), (Token::SquareBClose, "]"),
                (Token::Concat, "++"),
                (Token::SquareBOpen, "["), (Token::Ident, "two"), (Token::SquareBClose, "]"),
                (Token::Concat, "++"),
                (Token::SquareBOpen, "["), (Token::Integer, "3"), (Token::SquareBClose, "]")
            ],
            "\
Marker(Root)@[0; 15)
  Marker(Operation)@[0; 15)
    Marker(Operation)@[0; 10)
      Marker(List)@[0; 3)
        Token(SquareBOpen)@[0; 1)
        Marker(ListItem)@[1; 2)
          Token(Integer)@[1; 2)
        Token(SquareBClose)@[2; 3)
      Token(Concat)@[3; 5)
      Marker(List)@[5; 10)
        Token(SquareBOpen)@[5; 6)
        Marker(ListItem)@[6; 9)
          Token(Ident)@[6; 9)
        Token(SquareBClose)@[9; 10)
    Token(Concat)@[10; 12)
    Marker(List)@[12; 15)
      Token(SquareBOpen)@[12; 13)
      Marker(ListItem)@[13; 14)
        Token(Integer)@[13; 14)
      Token(SquareBClose)@[14; 15)
"
        );
    }
    #[test]
    fn lambda() {
        assert_eq!(
            [
                (Token::Ident, "import"),
                (Token::Path, "<nixpkgs>"),
                (Token::CurlyBOpen, "{"),
                (Token::CurlyBClose, "}")
            ],
            "\
Marker(Root)@[0; 17)
  Marker(Apply)@[0; 17)
    Marker(Apply)@[0; 15)
      Token(Ident)@[0; 6)
      Token(Path)@[6; 15)
    Marker(Set)@[15; 17)
      Token(CurlyBOpen)@[15; 16)
      Token(CurlyBClose)@[16; 17)
"
        );
        assert_eq!(
            [
                (Token::Ident, "a"),
                (Token::Colon, ":"),
                (Token::Whitespace, " "),
                (Token::Ident, "b"),
                (Token::Colon, ":"),
                (Token::Whitespace, " "),
                (Token::Ident, "a"),
                (Token::Whitespace, " "),
                (Token::Add, "+"),
                (Token::Whitespace, " "),
                (Token::Ident, "b")
            ],
            "\
Marker(Root)@[0; 11)
  Marker(Lambda)@[0; 11)
    Token(Ident)@[0; 1)
    Token(Colon)@[1; 2)
    Token(Whitespace)@[2; 3)
    Marker(Lambda)@[3; 11)
      Token(Ident)@[3; 4)
      Token(Colon)@[4; 5)
      Token(Whitespace)@[5; 6)
      Marker(Operation)@[6; 11)
        Token(Ident)@[6; 7)
        Token(Whitespace)@[7; 8)
        Token(Add)@[8; 9)
        Token(Whitespace)@[9; 10)
        Token(Ident)@[10; 11)
"
        );
        assert_eq!(
            [
                (Token::Ident, "a"),
                (Token::Whitespace, " "),
                (Token::Integer, "1"),
                (Token::Whitespace, " "),
                (Token::Integer, "2"),
                (Token::Whitespace, " "),
                (Token::Add, "+"),
                (Token::Whitespace, " "),
                (Token::Integer, "3")
            ],
            "\
Marker(Root)@[0; 9)
  Marker(Operation)@[0; 9)
    Marker(Apply)@[0; 6)
      Marker(Apply)@[0; 4)
        Token(Ident)@[0; 1)
        Token(Whitespace)@[1; 2)
        Token(Integer)@[2; 3)
        Token(Whitespace)@[3; 4)
      Token(Integer)@[4; 5)
      Token(Whitespace)@[5; 6)
    Token(Add)@[6; 7)
    Token(Whitespace)@[7; 8)
    Token(Integer)@[8; 9)
"
        );
   }
    #[test]
    fn patterns() {
        assert_eq!(
            [
                (Token::CurlyBOpen, "{"),
                (Token::Whitespace, " "),
                (Token::Ellipsis, "..."),
                (Token::Whitespace, " "),
                (Token::CurlyBClose, "}"),
                (Token::Colon, ":"),
                (Token::Whitespace, " "),
                (Token::Integer, "1")
            ],
            "\
Marker(Root)@[0; 10)
  Marker(Lambda)@[0; 10)
    Marker(Pattern)@[0; 7)
      Token(CurlyBOpen)@[0; 1)
      Token(Whitespace)@[1; 2)
      Token(Ellipsis)@[2; 5)
      Token(Whitespace)@[5; 6)
      Token(CurlyBClose)@[6; 7)
    Token(Colon)@[7; 8)
    Token(Whitespace)@[8; 9)
    Token(Integer)@[9; 10)
"
        );
        assert_eq!(
            [
                (Token::CurlyBOpen, "{"),
                (Token::CurlyBClose, "}"),
                (Token::Whitespace, " "),
                (Token::At, "@"),
                (Token::Whitespace, " "),
                (Token::Ident, "outer"),
                (Token::Colon, ":"),
                (Token::Whitespace, " "),
                (Token::Integer, "1")
            ],
            "\
Marker(Root)@[0; 13)
  Marker(Lambda)@[0; 13)
    Marker(Pattern)@[0; 10)
      Token(CurlyBOpen)@[0; 1)
      Token(CurlyBClose)@[1; 2)
      Token(Whitespace)@[2; 3)
      Marker(PatBind)@[3; 10)
        Token(At)@[3; 4)
        Token(Whitespace)@[4; 5)
        Token(Ident)@[5; 10)
    Token(Colon)@[10; 11)
    Token(Whitespace)@[11; 12)
    Token(Integer)@[12; 13)
"
        );
        assert_eq!(
            [
                (Token::CurlyBOpen, "{"), (Token::Whitespace, " "),

                (Token::Ident, "a"), (Token::Comma, ","), (Token::Whitespace, " "),
                (Token::Ident, "b"), (Token::Whitespace, " "),
                    (Token::Question, "?"), (Token::Whitespace, " "),
                    (Token::String, "\"default\""),

                (Token::CurlyBClose, "}"),
                (Token::Colon, ":"),
                (Token::Whitespace, " "),
                (Token::Ident, "a")
            ],
            "\
Marker(Root)@[0; 22)
  Marker(Lambda)@[0; 22)
    Marker(Pattern)@[0; 19)
      Token(CurlyBOpen)@[0; 1)
      Token(Whitespace)@[1; 2)
      Marker(PatEntry)@[2; 3)
        Token(Ident)@[2; 3)
      Token(Comma)@[3; 4)
      Token(Whitespace)@[4; 5)
      Marker(PatEntry)@[5; 18)
        Token(Ident)@[5; 6)
        Token(Whitespace)@[6; 7)
        Token(Question)@[7; 8)
        Token(Whitespace)@[8; 9)
        Token(String)@[9; 18)
      Token(CurlyBClose)@[18; 19)
    Token(Colon)@[19; 20)
    Token(Whitespace)@[20; 21)
    Token(Ident)@[21; 22)
"
        );
        assert_eq!(
            [
                (Token::CurlyBOpen, "{"), (Token::Whitespace, " "),

                (Token::Ident, "a"), (Token::Comma, ","), (Token::Whitespace, " "),
                (Token::Ident, "b"), (Token::Whitespace, " "),
                    (Token::Question, "?"), (Token::Whitespace, " "),
                    (Token::String, "\"default\""), (Token::Comma, ","), (Token::Whitespace, " "),
                (Token::Ellipsis, "..."), (Token::Whitespace, " "),

                (Token::CurlyBClose, "}"),
                (Token::Colon, ":"),
                (Token::Whitespace, " "),
                (Token::Ident, "a")
            ],
            "\
Marker(Root)@[0; 28)
  Marker(Lambda)@[0; 28)
    Marker(Pattern)@[0; 25)
      Token(CurlyBOpen)@[0; 1)
      Token(Whitespace)@[1; 2)
      Marker(PatEntry)@[2; 3)
        Token(Ident)@[2; 3)
      Token(Comma)@[3; 4)
      Token(Whitespace)@[4; 5)
      Marker(PatEntry)@[5; 18)
        Token(Ident)@[5; 6)
        Token(Whitespace)@[6; 7)
        Token(Question)@[7; 8)
        Token(Whitespace)@[8; 9)
        Token(String)@[9; 18)
      Token(Comma)@[18; 19)
      Token(Whitespace)@[19; 20)
      Token(Ellipsis)@[20; 23)
      Token(Whitespace)@[23; 24)
      Token(CurlyBClose)@[24; 25)
    Token(Colon)@[25; 26)
    Token(Whitespace)@[26; 27)
    Token(Ident)@[27; 28)
"
        );
        assert_eq!(
            [
                (Token::Ident, "outer"), (Token::Whitespace, " "),
                (Token::At, "@"), (Token::Whitespace, " "),
                (Token::CurlyBOpen, "{"), (Token::Whitespace, " "),
                (Token::Ident, "a"), (Token::Whitespace, " "),
                (Token::CurlyBClose, "}"),
                (Token::Colon, ":"), (Token::Whitespace, " "),
                (Token::Ident, "outer")
            ],
            "\
Marker(Root)@[0; 20)
  Marker(Lambda)@[0; 20)
    Marker(Pattern)@[0; 13)
      Marker(PatBind)@[0; 7)
        Token(Ident)@[0; 5)
        Token(Whitespace)@[5; 6)
        Token(At)@[6; 7)
      Token(Whitespace)@[7; 8)
      Token(CurlyBOpen)@[8; 9)
      Token(Whitespace)@[9; 10)
      Marker(PatEntry)@[10; 12)
        Token(Ident)@[10; 11)
        Token(Whitespace)@[11; 12)
      Token(CurlyBClose)@[12; 13)
    Token(Colon)@[13; 14)
    Token(Whitespace)@[14; 15)
    Token(Ident)@[15; 20)
"
        );
        assert_eq!(
            [
                (Token::CurlyBOpen, "{"),
                (Token::Ident, "a"),
                (Token::Question, "?"),
                (Token::CurlyBOpen, "{"),
                (Token::CurlyBClose, "}"),
                (Token::CurlyBClose, "}"),
                (Token::Colon, ":"),
                (Token::Ident, "a")
            ],
            "\
Marker(Root)@[0; 8)
  Marker(Lambda)@[0; 8)
    Marker(Pattern)@[0; 6)
      Token(CurlyBOpen)@[0; 1)
      Marker(PatEntry)@[1; 5)
        Token(Ident)@[1; 2)
        Token(Question)@[2; 3)
        Marker(Set)@[3; 5)
          Token(CurlyBOpen)@[3; 4)
          Token(CurlyBClose)@[4; 5)
      Token(CurlyBClose)@[5; 6)
    Token(Colon)@[6; 7)
    Token(Ident)@[7; 8)
"
        );
        assert_eq!(
            [
                (Token::CurlyBOpen, "{"),
                (Token::Ident, "a"),
                (Token::Comma, ","),
                (Token::CurlyBClose, "}"),
                (Token::Colon, ":"),
                (Token::Ident, "a")
            ],
            "\
Marker(Root)@[0; 6)
  Marker(Lambda)@[0; 6)
    Marker(Pattern)@[0; 4)
      Token(CurlyBOpen)@[0; 1)
      Marker(PatEntry)@[1; 2)
        Token(Ident)@[1; 2)
      Token(Comma)@[2; 3)
      Token(CurlyBClose)@[3; 4)
    Token(Colon)@[4; 5)
    Token(Ident)@[5; 6)
"
        );
    }
    #[test]
    fn dynamic() {
        assert_eq!(
            [
                (Token::DynamicStart, "${"),
                    (Token::Ident, "a"),
                (Token::DynamicEnd, "}")
            ],
            "\
Marker(Root)@[0; 4)
  Marker(Dynamic)@[0; 4)
    Token(DynamicStart)@[0; 2)
    Token(Ident)@[2; 3)
    Token(DynamicEnd)@[3; 4)
"
        );
    }
}
