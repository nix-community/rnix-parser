//! The parser: turns a series of tokens into an AST

use crate::types::{TypedNode, Root};

use rowan::{GreenNodeBuilder, SmolStr, SyntaxKind, SyntaxNode, TextRange, TreeArc};
use std::collections::VecDeque;

const OR: &'static str = "or";

/// An error that occured during parsing
#[derive(Clone, Debug, Fail, PartialEq)]
pub enum ParseError {
    #[fail(display = "unexpected input")]
    Unexpected(TextRange),
    #[fail(display = "unexpected eof")]
    UnexpectedEOF,
    #[fail(display = "unexpected eof, wanted {:?}", _0)]
    UnexpectedEOFWanted(SyntaxKind),
}

/// The type of a node in the AST
pub mod nodes {
    pub use crate::tokenizer::tokens::*;
    use rowan::SyntaxKind;

    // TODO BEFORE COMMITTING DON'T FORGET PLZ
    // recreate the numbers
    pub const NODE_APPLY: SyntaxKind = SyntaxKind(100);
    pub const NODE_ASSERT: SyntaxKind = SyntaxKind(101);
    pub const NODE_ATTRIBUTE: SyntaxKind = SyntaxKind(102);
    pub const NODE_DYNAMIC: SyntaxKind = SyntaxKind(103);
    pub const NODE_ERROR: SyntaxKind = SyntaxKind(104);
    pub const NODE_IDENT: SyntaxKind = SyntaxKind(105);
    pub const NODE_IF_ELSE: SyntaxKind = SyntaxKind(106);
    pub const NODE_INDEX_SET: SyntaxKind = SyntaxKind(107);
    pub const NODE_INHERIT: SyntaxKind = SyntaxKind(108);
    pub const NODE_INHERIT_FROM: SyntaxKind = SyntaxKind(109);
    pub const NODE_INTERPOL: SyntaxKind = SyntaxKind(110);
    pub const NODE_INTERPOL_LITERAL: SyntaxKind = SyntaxKind(111);
    pub const NODE_LAMBDA: SyntaxKind = SyntaxKind(112);
    pub const NODE_LET: SyntaxKind = SyntaxKind(113);
    pub const NODE_LET_IN: SyntaxKind = SyntaxKind(114);
    pub const NODE_LIST: SyntaxKind = SyntaxKind(115);
    pub const NODE_OPERATION: SyntaxKind = SyntaxKind(116);
    pub const NODE_OR_DEFAULT: SyntaxKind = SyntaxKind(117);
    pub const NODE_PAREN: SyntaxKind = SyntaxKind(118);
    pub const NODE_PATTERN: SyntaxKind = SyntaxKind(119);
    pub const NODE_PAT_BIND: SyntaxKind = SyntaxKind(120);
    pub const NODE_PAT_ENTRY: SyntaxKind = SyntaxKind(121);
    pub const NODE_ROOT: SyntaxKind = SyntaxKind(122);
    pub const NODE_SET: SyntaxKind = SyntaxKind(123);
    pub const NODE_SET_ENTRY: SyntaxKind = SyntaxKind(124);
    pub const NODE_UNARY: SyntaxKind = SyntaxKind(125);
    pub const NODE_VALUE: SyntaxKind = SyntaxKind(126);
    pub const NODE_WITH: SyntaxKind = SyntaxKind(127);
}
use self::nodes::*;

/// The result of a parse
#[derive(Clone)]
pub struct AST {
    node: TreeArc<SyntaxNode>,
    errors: Vec<ParseError>
}
impl AST {
    /// Return the root node
    pub fn into_node(self) -> TreeArc<SyntaxNode> {
        self.node
    }
    /// Return a reference to the root node
    pub fn node(&self) -> &SyntaxNode {
        &self.node
    }
    /// Return a borrowed typed root node
    pub fn root(&self) -> &Root {
        Root::cast(&self.node).unwrap()
    }
    /// Return an owned typed root node
    pub fn into_root(self) -> TreeArc<Root> {
        TreeArc::cast(self.node)
    }
    pub fn root_errors(&self) -> &[ParseError] {
        &self.errors
    }
    /// Return all errors in the tree, if any
    pub fn errors(&self) -> Vec<ParseError> {
        let mut errors = self.errors.clone();
        errors.extend(
            self.root().errors().into_iter()
                .map(|node| ParseError::Unexpected(node.range()))
        );

        errors
    }
    /// Either return the first error in the tree, or if there are none return self
    pub fn as_result(self) -> Result<Self, ParseError> {
        if let Some(err) = self.errors.first() {
            return Err(err.clone());
        }
        if let Some(node) = self.root().errors().first() {
            return Err(ParseError::Unexpected(node.range()));
        }
        Ok(self)
    }
}

struct Parser<I>
    where I: Iterator<Item = (SyntaxKind, SmolStr)>
{
    builder: GreenNodeBuilder,
    errors: Vec<ParseError>,

    buffer: VecDeque<I::Item>,
    iter: I
}
impl<I> Parser<I>
    where I: Iterator<Item = (SyntaxKind, SmolStr)>
{
    fn new(iter: I) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),

            buffer: VecDeque::with_capacity(1),
            iter
        }
    }

    fn peek_raw(&mut self) -> Option<&(SyntaxKind, SmolStr)> {
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
            Some((token, s)) => self.builder.token(token, s),
            None => self.errors.push(ParseError::UnexpectedEOF)
        }
    }
    fn peek_data(&mut self) -> Option<&(SyntaxKind, SmolStr)> {
        while self.peek_raw().map(|&(t, _)| token_helpers::is_trivia(t)).unwrap_or(false) {
            self.bump();
        }
        self.peek_raw()
    }
    fn peek(&mut self) -> Option<SyntaxKind> {
        self.peek_data().map(|&(t, _)| t)
    }
    fn expect(&mut self, expected: SyntaxKind) {
        if let Some(actual) = self.peek() {
            if actual != expected {
                self.builder.start_node(NODE_ERROR);
                while { self.bump(); self.peek().map(|actual| actual != expected).unwrap_or(false) } {}
                self.builder.finish_node();
            } else {
                self.bump();
            }
        } else {
            self.errors.push(ParseError::UnexpectedEOFWanted(expected));
        }
    }

    fn parse_dynamic(&mut self) {
        self.builder.start_node(NODE_DYNAMIC);
        self.bump();
        while self.peek().map(|t| t != TOKEN_DYNAMIC_END).unwrap_or(false) {
            self.parse_expr();
        }
        self.bump();
        self.builder.finish_node();
    }
    fn parse_interpol(&mut self) {
        self.builder.start_node(NODE_INTERPOL);

        self.builder.start_node(NODE_INTERPOL_LITERAL);
        self.bump();
        self.builder.finish_node();

        loop {
            match self.peek() {
                None | Some(TOKEN_INTERPOL_END) => {
                    self.builder.start_node(NODE_INTERPOL_LITERAL);
                    self.bump();
                    self.builder.finish_node();
                    break;
                },
                Some(TOKEN_INTERPOL_END_START) => {
                    self.builder.start_node(NODE_INTERPOL_LITERAL);
                    self.bump();
                    self.builder.finish_node();
                },
                Some(_) => self.parse_expr()
            }
        }

        self.builder.finish_node();
    }
    fn next_attr(&mut self) {
        match self.peek() {
            Some(TOKEN_DYNAMIC_START) => self.parse_dynamic(),
            Some(TOKEN_INTERPOL_START) => self.parse_interpol(),
            Some(TOKEN_STRING) => {
                self.builder.start_node(NODE_VALUE);
                self.bump();
                self.builder.finish_node();
            },
            _ => {
                self.builder.start_node(NODE_IDENT);
                self.expect(TOKEN_IDENT);
                self.builder.finish_node();
            }
        }
    }
    fn parse_attr(&mut self) {
        self.builder.start_node(NODE_ATTRIBUTE);
        loop {
            self.next_attr();

            if self.peek() == Some(TOKEN_DOT) {
                self.bump();
            } else {
                break;
            }
        }
        self.builder.finish_node();
    }
    fn parse_pattern(&mut self, bound: bool) {
        if self.peek().map(|t| t == TOKEN_CURLY_B_CLOSE).unwrap_or(true) {
            self.bump();
        } else {
            loop {
                match self.peek() {
                    Some(TOKEN_CURLY_B_CLOSE) => {
                        self.bump();
                        break;
                    },
                    Some(TOKEN_ELLIPSIS) => {
                        self.bump();
                        self.expect(TOKEN_CURLY_B_CLOSE);
                        break;
                    },
                    Some(TOKEN_IDENT) => {
                        self.builder.start_node(NODE_PAT_ENTRY);

                        self.builder.start_node(NODE_IDENT);
                        self.bump();
                        self.builder.finish_node();

                        if let Some(TOKEN_QUESTION) = self.peek() {
                            self.bump();
                            self.parse_expr();
                        }
                        self.builder.finish_node();

                        match self.peek() {
                            Some(TOKEN_COMMA) => self.bump(),
                            _ => {
                                self.expect(TOKEN_CURLY_B_CLOSE);
                                break;
                            },
                        }
                    },
                    None => {
                        self.errors.push(ParseError::UnexpectedEOFWanted(TOKEN_IDENT));
                        break;
                    },
                    Some(_) => {
                        self.builder.start_node(NODE_ERROR);
                        self.bump();
                        self.builder.finish_node();
                    }
                }
            }
        }

        if self.peek() == Some(TOKEN_AT) {
            let kind = if bound { NODE_ERROR } else { NODE_PAT_BIND };
            self.builder.start_node(kind);
            self.bump();
            self.builder.start_node(NODE_IDENT);
            self.expect(TOKEN_IDENT);
            self.builder.finish_node();
            self.builder.finish_node();
        }
    }
    fn parse_set(&mut self, until: SyntaxKind) {
        loop {
            match self.peek() {
                None => break,
                token if token == Some(until) => break,
                Some(TOKEN_INHERIT) => {
                    self.builder.start_node(NODE_INHERIT);
                    self.bump();

                    if self.peek() == Some(TOKEN_PAREN_OPEN) {
                        self.builder.start_node(NODE_INHERIT_FROM);
                        self.bump();
                        self.parse_expr();
                        self.expect(TOKEN_PAREN_CLOSE);
                        self.builder.finish_node();
                    }

                    while let Some(TOKEN_IDENT) = self.peek() {
                        self.builder.start_node(NODE_IDENT);
                        self.bump();
                        self.builder.finish_node();
                    }

                    self.expect(TOKEN_SEMICOLON);
                    self.builder.finish_node();
                },
                Some(_) => {
                    self.builder.start_node(NODE_SET_ENTRY);
                    self.parse_attr();
                    self.expect(TOKEN_ASSIGN);
                    self.parse_expr();
                    self.expect(TOKEN_SEMICOLON);
                    self.builder.finish_node();
                }
            }
        }
        self.bump(); // the final close, like '}'
    }
    fn parse_val(&mut self) {
        let checkpoint = self.builder.checkpoint();
        match self.peek() {
            Some(TOKEN_PAREN_OPEN) => {
                self.builder.start_node(NODE_PAREN);
                self.bump();
                self.parse_expr();
                self.bump();
                self.builder.finish_node();
            },
            Some(TOKEN_REC) => {
                self.builder.start_node(NODE_SET);
                self.bump();
                self.expect(TOKEN_CURLY_B_OPEN);
                self.parse_set(TOKEN_CURLY_B_CLOSE);
                self.builder.finish_node();
            },
            Some(TOKEN_CURLY_B_OPEN) => {
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
                        if kind.map(|t| !token_helpers::is_trivia(t)).unwrap_or(true) {
                            break kind;
                        }
                    };
                }

                match peek {
                    [Some(TOKEN_IDENT), Some(TOKEN_COMMA)]
                    | [Some(TOKEN_IDENT), Some(TOKEN_QUESTION)]
                    | [Some(TOKEN_IDENT), Some(TOKEN_CURLY_B_CLOSE)]
                    | [Some(TOKEN_ELLIPSIS), Some(TOKEN_CURLY_B_CLOSE)]
                    | [Some(TOKEN_CURLY_B_CLOSE), Some(TOKEN_COLON)]
                    | [Some(TOKEN_CURLY_B_CLOSE), Some(TOKEN_AT)] => {
                        // This looks like a pattern
                        self.builder.start_node(NODE_LAMBDA);

                        self.builder.start_node(NODE_PATTERN);
                        self.bump();
                        self.parse_pattern(false);
                        self.builder.finish_node();

                        self.expect(TOKEN_COLON);
                        self.parse_expr();

                        self.builder.finish_node();
                    },
                    _ => {
                        // This looks like a set
                        self.builder.start_node(NODE_SET);
                        self.bump();
                        self.parse_set(TOKEN_CURLY_B_CLOSE);
                        self.builder.finish_node();
                    }
                }
            },
            Some(TOKEN_SQUARE_B_OPEN) => {
                self.builder.start_node(NODE_LIST);
                self.bump();
                while self.peek().map(|t| t != TOKEN_SQUARE_B_CLOSE).unwrap_or(false) {
                    self.parse_val();
                }
                self.bump();
                self.builder.finish_node();
            },
            Some(TOKEN_DYNAMIC_START) => self.parse_dynamic(),
            Some(TOKEN_INTERPOL_START) => self.parse_interpol(),
            Some(t) if token_helpers::is_value(t) => {
                self.builder.start_node(NODE_VALUE);
                self.bump();
                self.builder.finish_node();
            },
            Some(TOKEN_IDENT) => {
                let checkpoint = self.builder.checkpoint();
                self.builder.start_node(NODE_IDENT);
                self.bump();
                self.builder.finish_node();

                match self.peek() {
                    Some(TOKEN_COLON) => {
                        self.builder.start_node_at(checkpoint, NODE_LAMBDA);
                        self.bump();
                        self.parse_expr();
                        self.builder.finish_node();
                    },
                    Some(TOKEN_AT) => {
                        self.builder.start_node_at(checkpoint, NODE_LAMBDA);
                        self.builder.start_node_at(checkpoint, NODE_PATTERN);
                        self.builder.start_node_at(checkpoint, NODE_PAT_BIND);
                        self.bump();
                        self.builder.finish_node(); // PatBind

                        self.expect(TOKEN_CURLY_B_OPEN);
                        self.parse_pattern(true);
                        self.builder.finish_node(); // Pattern

                        self.expect(TOKEN_COLON);
                        self.parse_expr();
                        self.builder.finish_node(); // Lambda
                    },
                    _ => ()
                }
            },
            _ => {
                self.builder.start_node(NODE_ERROR);
                self.bump();
                self.builder.finish_node();
            }
        }

        while self.peek() == Some(TOKEN_DOT) {
            self.builder.start_node_at(checkpoint, NODE_INDEX_SET);
            self.bump();
            self.next_attr();
            self.builder.finish_node();
        }
        if self.peek_data().map(|&(t, ref s)| t == TOKEN_IDENT && s == OR).unwrap_or(false) {
            self.builder.start_node_at(checkpoint, NODE_OR_DEFAULT);
            self.bump();
            self.parse_val();
            self.builder.finish_node();
        }
    }
    fn parse_fn(&mut self) {
        let checkpoint = self.builder.checkpoint();
        self.parse_val();

        while self.peek().map(|t| token_helpers::is_fn_arg(t)).unwrap_or(false) {
            self.builder.start_node_at(checkpoint, NODE_APPLY);
            self.parse_val();
            self.builder.finish_node();
        }
    }
    fn parse_negate(&mut self) {
        if self.peek() == Some(TOKEN_SUB) {
            self.builder.start_node(NODE_UNARY);
            self.bump();
            self.parse_negate();
            self.builder.finish_node();
        } else {
            self.parse_fn()
        }
    }
    fn handle_operation(&mut self, once: bool, next: fn(&mut Self), ops: &[SyntaxKind]) {
        let checkpoint = self.builder.checkpoint();
        next(self);
        while self.peek().map(|t| ops.contains(&t)).unwrap_or(false) {
            self.builder.start_node_at(checkpoint, NODE_OPERATION);
            self.bump();
            next(self);
            self.builder.finish_node();
            if once {
                break;
            }
        }
    }
    fn parse_isset(&mut self) {
        self.handle_operation(false, Self::parse_negate, &[TOKEN_QUESTION])
    }
    fn parse_concat(&mut self) {
        self.handle_operation(false, Self::parse_isset, &[TOKEN_CONCAT])
    }
    fn parse_mul(&mut self) {
        self.handle_operation(false, Self::parse_concat, &[TOKEN_MUL, TOKEN_DIV])
    }
    fn parse_add(&mut self) {
        self.handle_operation(false, Self::parse_mul, &[TOKEN_ADD, TOKEN_SUB])
    }
    fn parse_invert(&mut self) {
        if self.peek() == Some(TOKEN_INVERT) {
            self.builder.start_node(NODE_UNARY);
            self.bump();
            self.parse_invert();
            self.builder.finish_node();
        } else {
            self.parse_add()
        }
    }
    fn parse_merge(&mut self) {
        self.handle_operation(false, Self::parse_invert, &[TOKEN_MERGE])
    }
    fn parse_compare(&mut self) {
        self.handle_operation(true, Self::parse_merge, &[TOKEN_LESS, TOKEN_LESS_OR_EQ, TOKEN_MORE, TOKEN_MORE_OR_EQ])
    }
    fn parse_equal(&mut self) {
        self.handle_operation(true, Self::parse_compare, &[TOKEN_EQUAL, TOKEN_NOT_EQUAL])
    }
    fn parse_and(&mut self) {
        self.handle_operation(false, Self::parse_equal, &[TOKEN_AND])
    }
    fn parse_or(&mut self) {
        self.handle_operation(false, Self::parse_and, &[TOKEN_OR])
    }
    fn parse_implication(&mut self) {
        self.handle_operation(false, Self::parse_or, &[TOKEN_IMPLICATION])
    }
    #[inline(always)]
    fn parse_math(&mut self) {
        // Always point this to the lowest-level math function there is
        self.parse_implication()
    }
    /// Parse Nix code into an AST
    pub fn parse_expr(&mut self) {
        match self.peek() {
            Some(TOKEN_LET) => {
                let checkpoint = self.builder.checkpoint();
                self.bump();

                if self.peek() == Some(TOKEN_CURLY_B_OPEN) {
                    self.builder.start_node_at(checkpoint, NODE_LET);
                    self.bump();
                    self.parse_set(TOKEN_CURLY_B_CLOSE);
                    self.builder.finish_node();
                } else {
                    self.builder.start_node_at(checkpoint, NODE_LET_IN);
                    self.parse_set(TOKEN_IN);
                    self.parse_expr();
                    self.builder.finish_node();
                }
            },
            Some(TOKEN_WITH) => {
                self.builder.start_node(NODE_WITH);
                self.bump();
                self.parse_expr();
                self.expect(TOKEN_SEMICOLON);
                self.parse_expr();
                self.builder.finish_node();
            },
            Some(TOKEN_IF) => {
                self.builder.start_node(NODE_IF_ELSE);
                self.bump();
                self.parse_expr();
                self.expect(TOKEN_THEN);
                self.parse_expr();
                self.expect(TOKEN_ELSE);
                self.parse_expr();
                self.builder.finish_node();
            },
            Some(TOKEN_ASSERT) => {
                self.builder.start_node(NODE_ASSERT);
                self.bump();
                self.parse_expr();
                self.expect(TOKEN_SEMICOLON);
                self.parse_expr();
                self.builder.finish_node();
            },
            _ => self.parse_math()
        }
    }
}

/// Parse tokens into an AST
pub fn parse<I>(iter: I) -> AST
    where I: IntoIterator<Item = (SyntaxKind, SmolStr)>
{
    let mut parser = Parser::new(iter.into_iter());
    parser.builder.start_node(NODE_ROOT);
    parser.parse_expr();
    if parser.peek().is_some() {
        parser.builder.start_node(NODE_ERROR);
        while parser.peek().is_some() {
            parser.bump();
        }
        parser.builder.finish_node();
    }
    parser.builder.finish_node();
    AST {
        node: SyntaxNode::new(parser.builder.finish(), None),
        errors: parser.errors
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use rowan::WalkEvent;
    use std::fmt::Write;

    fn stringify(node: &SyntaxNode) -> String {
        let mut out = String::new();
        let mut indent = 0;
        for event in node.preorder_with_tokens() {
            match event {
                WalkEvent::Enter(node) => {
                    writeln!(out, "{:indent$}{:?}", "", node, indent = indent).unwrap();
                    indent += 2;
                },
                WalkEvent::Leave(_) =>
                    indent -= 2
            }
        }
        // Until rowan lets you map ID -> name
        // (I heard someone was working on that)
        out
            .replace(&format!("{:?}", TOKEN_COMMENT), "TOKEN_COMMENT")
            .replace(&format!("{:?}", TOKEN_ERROR), "TOKEN_ERROR")
            .replace(&format!("{:?}", TOKEN_WHITESPACE), "TOKEN_WHITESPACE")
            .replace(&format!("{:?}", TOKEN_ASSERT), "TOKEN_ASSERT")
            .replace(&format!("{:?}", TOKEN_ELSE), "TOKEN_ELSE")
            .replace(&format!("{:?}", TOKEN_IF), "TOKEN_IF")
            .replace(&format!("{:?}", TOKEN_IN), "TOKEN_IN")
            .replace(&format!("{:?}", TOKEN_INHERIT), "TOKEN_INHERIT")
            .replace(&format!("{:?}", TOKEN_LET), "TOKEN_LET")
            .replace(&format!("{:?}", TOKEN_REC), "TOKEN_REC")
            .replace(&format!("{:?}", TOKEN_THEN), "TOKEN_THEN")
            .replace(&format!("{:?}", TOKEN_WITH), "TOKEN_WITH")
            .replace(&format!("{:?}", TOKEN_CURLY_B_OPEN), "TOKEN_CURLY_B_OPEN")
            .replace(&format!("{:?}", TOKEN_CURLY_B_CLOSE), "TOKEN_CURLY_B_CLOSE")
            .replace(&format!("{:?}", TOKEN_SQUARE_B_OPEN), "TOKEN_SQUARE_B_OPEN")
            .replace(&format!("{:?}", TOKEN_SQUARE_B_CLOSE), "TOKEN_SQUARE_B_CLOSE")
            .replace(&format!("{:?}", TOKEN_ASSIGN), "TOKEN_ASSIGN")
            .replace(&format!("{:?}", TOKEN_AT), "TOKEN_AT")
            .replace(&format!("{:?}", TOKEN_COLON), "TOKEN_COLON")
            .replace(&format!("{:?}", TOKEN_COMMA), "TOKEN_COMMA")
            .replace(&format!("{:?}", TOKEN_DOT), "TOKEN_DOT")
            .replace(&format!("{:?}", TOKEN_ELLIPSIS), "TOKEN_ELLIPSIS")
            .replace(&format!("{:?}", TOKEN_QUESTION), "TOKEN_QUESTION")
            .replace(&format!("{:?}", TOKEN_SEMICOLON), "TOKEN_SEMICOLON")
            .replace(&format!("{:?}", TOKEN_PAREN_OPEN), "TOKEN_PAREN_OPEN")
            .replace(&format!("{:?}", TOKEN_PAREN_CLOSE), "TOKEN_PAREN_CLOSE")
            .replace(&format!("{:?}", TOKEN_CONCAT), "TOKEN_CONCAT")
            .replace(&format!("{:?}", TOKEN_INVERT), "TOKEN_INVERT")
            .replace(&format!("{:?}", TOKEN_MERGE), "TOKEN_MERGE")
            .replace(&format!("{:?}", TOKEN_ADD), "TOKEN_ADD")
            .replace(&format!("{:?}", TOKEN_SUB), "TOKEN_SUB")
            .replace(&format!("{:?}", TOKEN_MUL), "TOKEN_MUL")
            .replace(&format!("{:?}", TOKEN_DIV), "TOKEN_DIV")
            .replace(&format!("{:?}", TOKEN_AND), "TOKEN_AND")
            .replace(&format!("{:?}", TOKEN_EQUAL), "TOKEN_EQUAL")
            .replace(&format!("{:?}", TOKEN_IMPLICATION), "TOKEN_IMPLICATION")
            .replace(&format!("{:?}", TOKEN_LESS), "TOKEN_LESS")
            .replace(&format!("{:?}", TOKEN_LESS_OR_EQ), "TOKEN_LESS_OR_EQ")
            .replace(&format!("{:?}", TOKEN_MORE), "TOKEN_MORE")
            .replace(&format!("{:?}", TOKEN_MORE_OR_EQ), "TOKEN_MORE_OR_EQ")
            .replace(&format!("{:?}", TOKEN_NOT_EQUAL), "TOKEN_NOT_EQUAL")
            .replace(&format!("{:?}", TOKEN_OR), "TOKEN_OR")
            .replace(&format!("{:?}", TOKEN_DYNAMIC_END), "TOKEN_DYNAMIC_END")
            .replace(&format!("{:?}", TOKEN_DYNAMIC_START), "TOKEN_DYNAMIC_START")
            .replace(&format!("{:?}", TOKEN_FLOAT), "TOKEN_FLOAT")
            .replace(&format!("{:?}", TOKEN_IDENT), "TOKEN_IDENT")
            .replace(&format!("{:?}", TOKEN_INTEGER), "TOKEN_INTEGER")
            .replace(&format!("{:?}", TOKEN_INTERPOL_END), "TOKEN_INTERPOL_END")
            .replace(&format!("{:?}", TOKEN_INTERPOL_END_START), "TOKEN_INTERPOL_END_START")
            .replace(&format!("{:?}", TOKEN_INTERPOL_START), "TOKEN_INTERPOL_START")
            .replace(&format!("{:?}", TOKEN_PATH), "TOKEN_PATH")
            .replace(&format!("{:?}", TOKEN_STRING), "TOKEN_STRING")
            .replace(&format!("{:?}", NODE_APPLY), "NODE_APPLY")
            .replace(&format!("{:?}", NODE_ASSERT), "NODE_ASSERT")
            .replace(&format!("{:?}", NODE_ATTRIBUTE), "NODE_ATTRIBUTE")
            .replace(&format!("{:?}", NODE_DYNAMIC), "NODE_DYNAMIC")
            .replace(&format!("{:?}", NODE_ERROR), "NODE_ERROR")
            .replace(&format!("{:?}", NODE_IDENT), "NODE_IDENT")
            .replace(&format!("{:?}", NODE_IF_ELSE), "NODE_IF_ELSE")
            .replace(&format!("{:?}", NODE_INDEX_SET), "NODE_INDEX_SET")
            .replace(&format!("{:?}", NODE_INHERIT), "NODE_INHERIT")
            .replace(&format!("{:?}", NODE_INHERIT_FROM), "NODE_INHERIT_FROM")
            .replace(&format!("{:?}", NODE_INTERPOL), "NODE_INTERPOL")
            .replace(&format!("{:?}", NODE_INTERPOL_LITERAL), "NODE_INTERPOL_LITERAL")
            .replace(&format!("{:?}", NODE_LAMBDA), "NODE_LAMBDA")
            .replace(&format!("{:?}", NODE_LET), "NODE_LET")
            .replace(&format!("{:?}", NODE_LET_IN), "NODE_LET_IN")
            .replace(&format!("{:?}", NODE_LIST), "NODE_LIST")
            .replace(&format!("{:?}", NODE_OPERATION), "NODE_OPERATION")
            .replace(&format!("{:?}", NODE_OR_DEFAULT), "NODE_OR_DEFAULT")
            .replace(&format!("{:?}", NODE_PAREN), "NODE_PAREN")
            .replace(&format!("{:?}", NODE_PATTERN), "NODE_PATTERN")
            .replace(&format!("{:?}", NODE_PAT_BIND), "NODE_PAT_BIND")
            .replace(&format!("{:?}", NODE_PAT_ENTRY), "NODE_PAT_ENTRY")
            .replace(&format!("{:?}", NODE_ROOT), "NODE_ROOT")
            .replace(&format!("{:?}", NODE_SET), "NODE_SET")
            .replace(&format!("{:?}", NODE_SET_ENTRY), "NODE_SET_ENTRY")
            .replace(&format!("{:?}", NODE_UNARY), "NODE_UNARY")
            .replace(&format!("{:?}", NODE_VALUE), "NODE_VALUE")
            .replace(&format!("{:?}", NODE_WITH), "NODE_WITH")
    }

    macro_rules! test {
        ([$(($token:expr, $str:expr)),*], $expected:expr) => {
            let parsed = parse(vec![$(($token, $str.into())),*]);
            let actual = stringify(parsed.node());

            if let Err(err) = parsed.as_result() {
                eprintln!("--- Errnous AST ---");
                eprintln!("{}", actual);
                eprintln!("--- End ---");
                panic!(err);
            } else if actual != $expected {
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
        test!(
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_WHITESPACE, " "),

                (TOKEN_IDENT, "meaning_of_life"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ASSIGN, "="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "42"),
                (TOKEN_SEMICOLON, ";"),

                (TOKEN_IDENT, "H4X0RNUM83R"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ASSIGN, "="),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_FLOAT, "1.337"),
                (TOKEN_SEMICOLON, ";"),

                (TOKEN_WHITESPACE, " "),
                (TOKEN_CURLY_B_CLOSE, "}")
            ],
            "\
Node(NODE_ROOT@[0; 45))
  Node(NODE_SET@[0; 45))
    Token(TOKEN_CURLY_B_OPEN@[0; 1))
    Token(TOKEN_WHITESPACE@[1; 2))
    Node(NODE_SET_ENTRY@[2; 23))
      Node(NODE_ATTRIBUTE@[2; 18))
        Node(NODE_IDENT@[2; 17))
          Token(TOKEN_IDENT@[2; 17))
        Token(TOKEN_WHITESPACE@[17; 18))
      Token(TOKEN_ASSIGN@[18; 19))
      Token(TOKEN_WHITESPACE@[19; 20))
      Node(NODE_VALUE@[20; 22))
        Token(TOKEN_INTEGER@[20; 22))
      Token(TOKEN_SEMICOLON@[22; 23))
    Node(NODE_SET_ENTRY@[23; 43))
      Node(NODE_ATTRIBUTE@[23; 35))
        Node(NODE_IDENT@[23; 34))
          Token(TOKEN_IDENT@[23; 34))
        Token(TOKEN_WHITESPACE@[34; 35))
      Token(TOKEN_ASSIGN@[35; 36))
      Token(TOKEN_WHITESPACE@[36; 37))
      Node(NODE_VALUE@[37; 42))
        Token(TOKEN_FLOAT@[37; 42))
      Token(TOKEN_SEMICOLON@[42; 43))
    Token(TOKEN_WHITESPACE@[43; 44))
    Token(TOKEN_CURLY_B_CLOSE@[44; 45))
"
        );
        test!(
            [
                (TOKEN_REC, "rec"),
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_IDENT, "test"),
                (TOKEN_ASSIGN, "="),
                (TOKEN_INTEGER, "1"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_CURLY_B_CLOSE, "}")
            ],
            "\
Node(NODE_ROOT@[0; 12))
  Node(NODE_SET@[0; 12))
    Token(TOKEN_REC@[0; 3))
    Token(TOKEN_CURLY_B_OPEN@[3; 4))
    Node(NODE_SET_ENTRY@[4; 11))
      Node(NODE_ATTRIBUTE@[4; 8))
        Node(NODE_IDENT@[4; 8))
          Token(TOKEN_IDENT@[4; 8))
      Token(TOKEN_ASSIGN@[8; 9))
      Node(NODE_VALUE@[9; 10))
        Token(TOKEN_INTEGER@[9; 10))
      Token(TOKEN_SEMICOLON@[10; 11))
    Token(TOKEN_CURLY_B_CLOSE@[11; 12))
"
        );
        test!(
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_CURLY_B_CLOSE, "}")
            ],
            "\
Node(NODE_ROOT@[0; 2))
  Node(NODE_SET@[0; 2))
    Token(TOKEN_CURLY_B_OPEN@[0; 1))
    Token(TOKEN_CURLY_B_CLOSE@[1; 2))
"
        );
        test!(
            [
                (TOKEN_CURLY_B_OPEN, "{"),

                (TOKEN_IDENT, "a"),
                    (TOKEN_DOT, "."),
                    (TOKEN_IDENT, "b"),
                (TOKEN_ASSIGN, "="),
                (TOKEN_INTEGER, "2"),
                (TOKEN_SEMICOLON, ";"),

                (TOKEN_INTERPOL_START, "\"${"),
                    (TOKEN_IDENT, "c"),
                (TOKEN_INTERPOL_END, "}\""),
                    (TOKEN_DOT, "."),
                    (TOKEN_DYNAMIC_START, "${"),
                        (TOKEN_IDENT, "d"),
                    (TOKEN_DYNAMIC_END, "${"),
                (TOKEN_ASSIGN, "="),
                (TOKEN_INTEGER, "3"),
                (TOKEN_SEMICOLON, ";"),

                (TOKEN_CURLY_B_CLOSE, "}")
            ],
            "\
Node(NODE_ROOT@[0; 23))
  Node(NODE_SET@[0; 23))
    Token(TOKEN_CURLY_B_OPEN@[0; 1))
    Node(NODE_SET_ENTRY@[1; 7))
      Node(NODE_ATTRIBUTE@[1; 4))
        Node(NODE_IDENT@[1; 2))
          Token(TOKEN_IDENT@[1; 2))
        Token(TOKEN_DOT@[2; 3))
        Node(NODE_IDENT@[3; 4))
          Token(TOKEN_IDENT@[3; 4))
      Token(TOKEN_ASSIGN@[4; 5))
      Node(NODE_VALUE@[5; 6))
        Token(TOKEN_INTEGER@[5; 6))
      Token(TOKEN_SEMICOLON@[6; 7))
    Node(NODE_SET_ENTRY@[7; 22))
      Node(NODE_ATTRIBUTE@[7; 19))
        Node(NODE_INTERPOL@[7; 13))
          Node(NODE_INTERPOL_LITERAL@[7; 10))
            Token(TOKEN_INTERPOL_START@[7; 10))
          Node(NODE_IDENT@[10; 11))
            Token(TOKEN_IDENT@[10; 11))
          Node(NODE_INTERPOL_LITERAL@[11; 13))
            Token(TOKEN_INTERPOL_END@[11; 13))
        Token(TOKEN_DOT@[13; 14))
        Node(NODE_DYNAMIC@[14; 19))
          Token(TOKEN_DYNAMIC_START@[14; 16))
          Node(NODE_IDENT@[16; 17))
            Token(TOKEN_IDENT@[16; 17))
          Token(TOKEN_DYNAMIC_END@[17; 19))
      Token(TOKEN_ASSIGN@[19; 20))
      Node(NODE_VALUE@[20; 21))
        Token(TOKEN_INTEGER@[20; 21))
      Token(TOKEN_SEMICOLON@[21; 22))
    Token(TOKEN_CURLY_B_CLOSE@[22; 23))
"
        );
    }
    #[test]
    fn math() {
        test!(
            [
                (TOKEN_INTEGER, "1"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ADD, "+"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ADD, "+"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "3"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_MUL, "*"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "4")
            ],
            "\
Node(NODE_ROOT@[0; 13))
  Node(NODE_OPERATION@[0; 13))
    Node(NODE_OPERATION@[0; 6))
      Node(NODE_VALUE@[0; 1))
        Token(TOKEN_INTEGER@[0; 1))
      Token(TOKEN_WHITESPACE@[1; 2))
      Token(TOKEN_ADD@[2; 3))
      Token(TOKEN_WHITESPACE@[3; 4))
      Node(NODE_VALUE@[4; 5))
        Token(TOKEN_INTEGER@[4; 5))
      Token(TOKEN_WHITESPACE@[5; 6))
    Token(TOKEN_ADD@[6; 7))
    Node(NODE_OPERATION@[7; 13))
      Token(TOKEN_WHITESPACE@[7; 8))
      Node(NODE_VALUE@[8; 9))
        Token(TOKEN_INTEGER@[8; 9))
      Token(TOKEN_WHITESPACE@[9; 10))
      Token(TOKEN_MUL@[10; 11))
      Token(TOKEN_WHITESPACE@[11; 12))
      Node(NODE_VALUE@[12; 13))
        Token(TOKEN_INTEGER@[12; 13))
"
        );
        test!(
            [
                (TOKEN_INTEGER, "5"),
                (TOKEN_MUL, "*"),
                (TOKEN_SUB, "-"),
                (TOKEN_PAREN_OPEN, "("),
                (TOKEN_INTEGER, "3"),
                (TOKEN_SUB, "-"),
                (TOKEN_INTEGER, "2"),
                (TOKEN_PAREN_CLOSE, ")")
            ],
            "\
Node(NODE_ROOT@[0; 8))
  Node(NODE_OPERATION@[0; 8))
    Node(NODE_VALUE@[0; 1))
      Token(TOKEN_INTEGER@[0; 1))
    Token(TOKEN_MUL@[1; 2))
    Node(NODE_UNARY@[2; 8))
      Token(TOKEN_SUB@[2; 3))
      Node(NODE_PAREN@[3; 8))
        Token(TOKEN_PAREN_OPEN@[3; 4))
        Node(NODE_OPERATION@[4; 7))
          Node(NODE_VALUE@[4; 5))
            Token(TOKEN_INTEGER@[4; 5))
          Token(TOKEN_SUB@[5; 6))
          Node(NODE_VALUE@[6; 7))
            Token(TOKEN_INTEGER@[6; 7))
        Token(TOKEN_PAREN_CLOSE@[7; 8))
"
        );
    }
    #[test]
    fn let_in() {
        test!(
            [
                (TOKEN_LET, "let"),
                    (TOKEN_WHITESPACE, " "),
                    (TOKEN_IDENT, "a"),
                    (TOKEN_WHITESPACE, " "),
                    (TOKEN_ASSIGN, "="),
                    (TOKEN_WHITESPACE, " "),
                    (TOKEN_INTEGER, "42"),
                    (TOKEN_SEMICOLON, ";"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IN, "in"),
                    (TOKEN_WHITESPACE, " "),
                    (TOKEN_IDENT, "a")
            ],
            "\
Node(NODE_ROOT@[0; 16))
  Node(NODE_LET_IN@[0; 16))
    Token(TOKEN_LET@[0; 3))
    Token(TOKEN_WHITESPACE@[3; 4))
    Node(NODE_SET_ENTRY@[4; 11))
      Node(NODE_ATTRIBUTE@[4; 6))
        Node(NODE_IDENT@[4; 5))
          Token(TOKEN_IDENT@[4; 5))
        Token(TOKEN_WHITESPACE@[5; 6))
      Token(TOKEN_ASSIGN@[6; 7))
      Token(TOKEN_WHITESPACE@[7; 8))
      Node(NODE_VALUE@[8; 10))
        Token(TOKEN_INTEGER@[8; 10))
      Token(TOKEN_SEMICOLON@[10; 11))
    Token(TOKEN_WHITESPACE@[11; 12))
    Token(TOKEN_IN@[12; 14))
    Token(TOKEN_WHITESPACE@[14; 15))
    Node(NODE_IDENT@[15; 16))
      Token(TOKEN_IDENT@[15; 16))
"
        );
    }
    #[test]
    fn let_legacy_syntax() {
        test!(
            [
                (TOKEN_LET, "let"),
                (TOKEN_CURLY_B_OPEN, "{"),
                    (TOKEN_IDENT, "a"),
                        (TOKEN_ASSIGN, "="),
                        (TOKEN_INTEGER, "42"),
                        (TOKEN_SEMICOLON, ";"),
                    (TOKEN_IDENT, "body"),
                        (TOKEN_ASSIGN, "="),
                        (TOKEN_IDENT, "a"),
                        (TOKEN_SEMICOLON, ";"),
                (TOKEN_CURLY_B_CLOSE, "}")
            ],
            "\
Node(NODE_ROOT@[0; 17))
  Node(NODE_LET@[0; 17))
    Token(TOKEN_LET@[0; 3))
    Token(TOKEN_CURLY_B_OPEN@[3; 4))
    Node(NODE_SET_ENTRY@[4; 9))
      Node(NODE_ATTRIBUTE@[4; 5))
        Node(NODE_IDENT@[4; 5))
          Token(TOKEN_IDENT@[4; 5))
      Token(TOKEN_ASSIGN@[5; 6))
      Node(NODE_VALUE@[6; 8))
        Token(TOKEN_INTEGER@[6; 8))
      Token(TOKEN_SEMICOLON@[8; 9))
    Node(NODE_SET_ENTRY@[9; 16))
      Node(NODE_ATTRIBUTE@[9; 13))
        Node(NODE_IDENT@[9; 13))
          Token(TOKEN_IDENT@[9; 13))
      Token(TOKEN_ASSIGN@[13; 14))
      Node(NODE_IDENT@[14; 15))
        Token(TOKEN_IDENT@[14; 15))
      Token(TOKEN_SEMICOLON@[15; 16))
    Token(TOKEN_CURLY_B_CLOSE@[16; 17))
"
        );
    }
    #[test]
    fn interpolation() {
        test!(
            [
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTERPOL_START, r#""Hello, ${"#),
                    (TOKEN_WHITESPACE, " "),
                    (TOKEN_CURLY_B_OPEN, "{"),
                    (TOKEN_WHITESPACE, " "),
                    (TOKEN_IDENT, "world"),
                    (TOKEN_WHITESPACE, " "),
                    (TOKEN_ASSIGN, "="),
                    (TOKEN_WHITESPACE, " "),
                    (TOKEN_STRING, r#""World""#),
                    (TOKEN_SEMICOLON, ";"),
                    (TOKEN_WHITESPACE, " "),
                    (TOKEN_CURLY_B_CLOSE, "}"),
                    (TOKEN_DOT, "."),
                    (TOKEN_IDENT, "world"),
                    (TOKEN_WHITESPACE, " "),
                (TOKEN_INTERPOL_END, r#"}!""#),
                (TOKEN_WHITESPACE, " ")
            ],
            "\
Node(NODE_ROOT@[0; 43))
  Token(TOKEN_WHITESPACE@[0; 1))
  Node(NODE_INTERPOL@[1; 42))
    Node(NODE_INTERPOL_LITERAL@[1; 11))
      Token(TOKEN_INTERPOL_START@[1; 11))
    Token(TOKEN_WHITESPACE@[11; 12))
    Node(NODE_INDEX_SET@[12; 38))
      Node(NODE_SET@[12; 32))
        Token(TOKEN_CURLY_B_OPEN@[12; 13))
        Token(TOKEN_WHITESPACE@[13; 14))
        Node(NODE_SET_ENTRY@[14; 30))
          Node(NODE_ATTRIBUTE@[14; 20))
            Node(NODE_IDENT@[14; 19))
              Token(TOKEN_IDENT@[14; 19))
            Token(TOKEN_WHITESPACE@[19; 20))
          Token(TOKEN_ASSIGN@[20; 21))
          Token(TOKEN_WHITESPACE@[21; 22))
          Node(NODE_VALUE@[22; 29))
            Token(TOKEN_STRING@[22; 29))
          Token(TOKEN_SEMICOLON@[29; 30))
        Token(TOKEN_WHITESPACE@[30; 31))
        Token(TOKEN_CURLY_B_CLOSE@[31; 32))
      Token(TOKEN_DOT@[32; 33))
      Node(NODE_IDENT@[33; 38))
        Token(TOKEN_IDENT@[33; 38))
    Token(TOKEN_WHITESPACE@[38; 39))
    Node(NODE_INTERPOL_LITERAL@[39; 42))
      Token(TOKEN_INTERPOL_END@[39; 42))
  Token(TOKEN_WHITESPACE@[42; 43))
"
        );
      test!(
          [
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTERPOL_START, r#""${"#),
                    (TOKEN_IDENT, "hello"),
                (TOKEN_INTERPOL_END_START, r#"} ${"#),
                    (TOKEN_IDENT, "world"),
                (TOKEN_INTERPOL_END, r#"}""#),
                (TOKEN_WHITESPACE, " ")
          ],
          "\
Node(NODE_ROOT@[0; 21))
  Token(TOKEN_WHITESPACE@[0; 1))
  Node(NODE_INTERPOL@[1; 20))
    Node(NODE_INTERPOL_LITERAL@[1; 4))
      Token(TOKEN_INTERPOL_START@[1; 4))
    Node(NODE_IDENT@[4; 9))
      Token(TOKEN_IDENT@[4; 9))
    Node(NODE_INTERPOL_LITERAL@[9; 13))
      Token(TOKEN_INTERPOL_END_START@[9; 13))
    Node(NODE_IDENT@[13; 18))
      Token(TOKEN_IDENT@[13; 18))
    Node(NODE_INTERPOL_LITERAL@[18; 20))
      Token(TOKEN_INTERPOL_END@[18; 20))
  Token(TOKEN_WHITESPACE@[20; 21))
"
      );
      test!(
          [
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTERPOL_START, r#"''${"#),
                    (TOKEN_INTERPOL_START, r#""${"#),
                        (TOKEN_IDENT, "var"),
                    (TOKEN_INTERPOL_END, r#"}""#),
                (TOKEN_INTERPOL_END, r#"}''"#),
                (TOKEN_WHITESPACE, " ")
          ],
          "\
Node(NODE_ROOT@[0; 17))
  Token(TOKEN_WHITESPACE@[0; 1))
  Node(NODE_INTERPOL@[1; 16))
    Node(NODE_INTERPOL_LITERAL@[1; 5))
      Token(TOKEN_INTERPOL_START@[1; 5))
    Node(NODE_INTERPOL@[5; 13))
      Node(NODE_INTERPOL_LITERAL@[5; 8))
        Token(TOKEN_INTERPOL_START@[5; 8))
      Node(NODE_IDENT@[8; 11))
        Token(TOKEN_IDENT@[8; 11))
      Node(NODE_INTERPOL_LITERAL@[11; 13))
        Token(TOKEN_INTERPOL_END@[11; 13))
    Node(NODE_INTERPOL_LITERAL@[13; 16))
      Token(TOKEN_INTERPOL_END@[13; 16))
  Token(TOKEN_WHITESPACE@[16; 17))
"
      );
    }
    #[test]
    fn index_set() {
        test!(
            [
                (TOKEN_IDENT, "a"),
                (TOKEN_DOT, "."),
                (TOKEN_IDENT, "b"),
                (TOKEN_DOT, "."),
                (TOKEN_IDENT, "c")
            ],
            "\
Node(NODE_ROOT@[0; 5))
  Node(NODE_INDEX_SET@[0; 5))
    Node(NODE_INDEX_SET@[0; 3))
      Node(NODE_IDENT@[0; 1))
        Token(TOKEN_IDENT@[0; 1))
      Token(TOKEN_DOT@[1; 2))
      Node(NODE_IDENT@[2; 3))
        Token(TOKEN_IDENT@[2; 3))
    Token(TOKEN_DOT@[3; 4))
    Node(NODE_IDENT@[4; 5))
      Token(TOKEN_IDENT@[4; 5))
"
        );
        test!(
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                    (TOKEN_IDENT, "a"),
                        (TOKEN_DOT, "."),
                        (TOKEN_IDENT, "b"),
                        (TOKEN_DOT, "."),
                        (TOKEN_IDENT, "c"),
                    (TOKEN_ASSIGN, "="),
                    (TOKEN_INTEGER, "1"),
                    (TOKEN_SEMICOLON, ";"),
                (TOKEN_CURLY_B_CLOSE, "}")
            ],
            "\
Node(NODE_ROOT@[0; 10))
  Node(NODE_SET@[0; 10))
    Token(TOKEN_CURLY_B_OPEN@[0; 1))
    Node(NODE_SET_ENTRY@[1; 9))
      Node(NODE_ATTRIBUTE@[1; 6))
        Node(NODE_IDENT@[1; 2))
          Token(TOKEN_IDENT@[1; 2))
        Token(TOKEN_DOT@[2; 3))
        Node(NODE_IDENT@[3; 4))
          Token(TOKEN_IDENT@[3; 4))
        Token(TOKEN_DOT@[4; 5))
        Node(NODE_IDENT@[5; 6))
          Token(TOKEN_IDENT@[5; 6))
      Token(TOKEN_ASSIGN@[6; 7))
      Node(NODE_VALUE@[7; 8))
        Token(TOKEN_INTEGER@[7; 8))
      Token(TOKEN_SEMICOLON@[8; 9))
    Token(TOKEN_CURLY_B_CLOSE@[9; 10))
"
        );
        test!(
            [
                (TOKEN_IDENT, "test"),
                    (TOKEN_DOT, "."),
                    (TOKEN_STRING, "\"invalid ident\""),
                    (TOKEN_DOT, "."),
                    (TOKEN_INTERPOL_START, "\"${"),
                        (TOKEN_IDENT, "hi"),
                    (TOKEN_INTERPOL_END, "}\""),
                    (TOKEN_DOT, "."),
                    (TOKEN_DYNAMIC_START, "${"),
                        (TOKEN_IDENT, "a"),
                    (TOKEN_DYNAMIC_END, "}")
            ],
            "\
Node(NODE_ROOT@[0; 33))
  Node(NODE_INDEX_SET@[0; 33))
    Node(NODE_INDEX_SET@[0; 28))
      Node(NODE_INDEX_SET@[0; 20))
        Node(NODE_IDENT@[0; 4))
          Token(TOKEN_IDENT@[0; 4))
        Token(TOKEN_DOT@[4; 5))
        Node(NODE_VALUE@[5; 20))
          Token(TOKEN_STRING@[5; 20))
      Token(TOKEN_DOT@[20; 21))
      Node(NODE_INTERPOL@[21; 28))
        Node(NODE_INTERPOL_LITERAL@[21; 24))
          Token(TOKEN_INTERPOL_START@[21; 24))
        Node(NODE_IDENT@[24; 26))
          Token(TOKEN_IDENT@[24; 26))
        Node(NODE_INTERPOL_LITERAL@[26; 28))
          Token(TOKEN_INTERPOL_END@[26; 28))
    Token(TOKEN_DOT@[28; 29))
    Node(NODE_DYNAMIC@[29; 33))
      Token(TOKEN_DYNAMIC_START@[29; 31))
      Node(NODE_IDENT@[31; 32))
        Token(TOKEN_IDENT@[31; 32))
      Token(TOKEN_DYNAMIC_END@[32; 33))
"
        );
    }
    #[test]
    fn isset() {
        test!(
            [
                (TOKEN_IDENT, "a"),
                (TOKEN_QUESTION, "?"),
                (TOKEN_STRING, "\"b\""),
                (TOKEN_AND, "&&"),
                (TOKEN_IDENT, "true")
            ],
            "\
Node(NODE_ROOT@[0; 11))
  Node(NODE_OPERATION@[0; 11))
    Node(NODE_OPERATION@[0; 5))
      Node(NODE_IDENT@[0; 1))
        Token(TOKEN_IDENT@[0; 1))
      Token(TOKEN_QUESTION@[1; 2))
      Node(NODE_VALUE@[2; 5))
        Token(TOKEN_STRING@[2; 5))
    Token(TOKEN_AND@[5; 7))
    Node(NODE_IDENT@[7; 11))
      Token(TOKEN_IDENT@[7; 11))
"
        );
        test!(
            [
                (TOKEN_IDENT, "a"),
                    (TOKEN_DOT, "."),
                    (TOKEN_IDENT, "b"),
                    (TOKEN_DOT, "."),
                    (TOKEN_IDENT, "c"),
                (TOKEN_IDENT, OR),
                (TOKEN_INTEGER, "1"),
                (TOKEN_ADD, "+"),
                (TOKEN_INTEGER, "1")
            ],
            "\
Node(NODE_ROOT@[0; 10))
  Node(NODE_OPERATION@[0; 10))
    Node(NODE_OR_DEFAULT@[0; 8))
      Node(NODE_INDEX_SET@[0; 5))
        Node(NODE_INDEX_SET@[0; 3))
          Node(NODE_IDENT@[0; 1))
            Token(TOKEN_IDENT@[0; 1))
          Token(TOKEN_DOT@[1; 2))
          Node(NODE_IDENT@[2; 3))
            Token(TOKEN_IDENT@[2; 3))
        Token(TOKEN_DOT@[3; 4))
        Node(NODE_IDENT@[4; 5))
          Token(TOKEN_IDENT@[4; 5))
      Token(TOKEN_IDENT@[5; 7))
      Node(NODE_VALUE@[7; 8))
        Token(TOKEN_INTEGER@[7; 8))
    Token(TOKEN_ADD@[8; 9))
    Node(NODE_VALUE@[9; 10))
      Token(TOKEN_INTEGER@[9; 10))
"
        );
    }
    #[test]
    fn merge() {
        test!(
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_IDENT, "a"),
                (TOKEN_ASSIGN, "="),
                (TOKEN_INTEGER, "1"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_CURLY_B_CLOSE, "}"),
                (TOKEN_MERGE, "//"),
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_IDENT, "b"),
                (TOKEN_ASSIGN, "="),
                (TOKEN_INTEGER, "2"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_CURLY_B_CLOSE, "}")
            ],
            "\
Node(NODE_ROOT@[0; 14))
  Node(NODE_OPERATION@[0; 14))
    Node(NODE_SET@[0; 6))
      Token(TOKEN_CURLY_B_OPEN@[0; 1))
      Node(NODE_SET_ENTRY@[1; 5))
        Node(NODE_ATTRIBUTE@[1; 2))
          Node(NODE_IDENT@[1; 2))
            Token(TOKEN_IDENT@[1; 2))
        Token(TOKEN_ASSIGN@[2; 3))
        Node(NODE_VALUE@[3; 4))
          Token(TOKEN_INTEGER@[3; 4))
        Token(TOKEN_SEMICOLON@[4; 5))
      Token(TOKEN_CURLY_B_CLOSE@[5; 6))
    Token(TOKEN_MERGE@[6; 8))
    Node(NODE_SET@[8; 14))
      Token(TOKEN_CURLY_B_OPEN@[8; 9))
      Node(NODE_SET_ENTRY@[9; 13))
        Node(NODE_ATTRIBUTE@[9; 10))
          Node(NODE_IDENT@[9; 10))
            Token(TOKEN_IDENT@[9; 10))
        Token(TOKEN_ASSIGN@[10; 11))
        Node(NODE_VALUE@[11; 12))
          Token(TOKEN_INTEGER@[11; 12))
        Token(TOKEN_SEMICOLON@[12; 13))
      Token(TOKEN_CURLY_B_CLOSE@[13; 14))
"
        );
    }
    #[test]
    fn with() {
        test!(
            [
                (TOKEN_WITH, "with"),
                (TOKEN_IDENT, "namespace"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_IDENT, "expr")
            ],
            "\
Node(NODE_ROOT@[0; 18))
  Node(NODE_WITH@[0; 18))
    Token(TOKEN_WITH@[0; 4))
    Node(NODE_IDENT@[4; 13))
      Token(TOKEN_IDENT@[4; 13))
    Token(TOKEN_SEMICOLON@[13; 14))
    Node(NODE_IDENT@[14; 18))
      Token(TOKEN_IDENT@[14; 18))
"
        );
    }
    #[test]
    fn assert() {
        test!(
            [
                (TOKEN_ASSERT, "assert"),
                (TOKEN_IDENT, "a"),
                (TOKEN_EQUAL, "=="),
                (TOKEN_IDENT, "b"),
                (TOKEN_SEMICOLON, ";"),
                (TOKEN_STRING, "\"a == b\"")
            ],
            "\
Node(NODE_ROOT@[0; 19))
  Node(NODE_ASSERT@[0; 19))
    Token(TOKEN_ASSERT@[0; 6))
    Node(NODE_OPERATION@[6; 10))
      Node(NODE_IDENT@[6; 7))
        Token(TOKEN_IDENT@[6; 7))
      Token(TOKEN_EQUAL@[7; 9))
      Node(NODE_IDENT@[9; 10))
        Token(TOKEN_IDENT@[9; 10))
    Token(TOKEN_SEMICOLON@[10; 11))
    Node(NODE_VALUE@[11; 19))
      Token(TOKEN_STRING@[11; 19))
"
        );
    }
    #[test]
    fn inherit() {
        test!(
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                    (TOKEN_IDENT, "a"),
                        (TOKEN_ASSIGN, "="),
                        (TOKEN_INTEGER, "1"),
                        (TOKEN_SEMICOLON, ";"),
                    (TOKEN_INHERIT, "inherit"),
                        (TOKEN_WHITESPACE, " "),
                        (TOKEN_IDENT, "b"),
                        (TOKEN_WHITESPACE, " "),
                        (TOKEN_IDENT, "c"),
                        (TOKEN_SEMICOLON, ";"),
                    (TOKEN_INHERIT, "inherit"),
                        (TOKEN_WHITESPACE, " "),
                        (TOKEN_PAREN_OPEN, "("),
                        (TOKEN_IDENT, "set"),
                        (TOKEN_PAREN_CLOSE, ")"),
                        (TOKEN_WHITESPACE, " "),
                        (TOKEN_IDENT, "d"),
                        (TOKEN_WHITESPACE, " "),
                        (TOKEN_IDENT, "e"),
                        (TOKEN_SEMICOLON, ";"),
                (TOKEN_CURLY_B_CLOSE, "}")
            ],
            "\
Node(NODE_ROOT@[0; 36))
  Node(NODE_SET@[0; 36))
    Token(TOKEN_CURLY_B_OPEN@[0; 1))
    Node(NODE_SET_ENTRY@[1; 5))
      Node(NODE_ATTRIBUTE@[1; 2))
        Node(NODE_IDENT@[1; 2))
          Token(TOKEN_IDENT@[1; 2))
      Token(TOKEN_ASSIGN@[2; 3))
      Node(NODE_VALUE@[3; 4))
        Token(TOKEN_INTEGER@[3; 4))
      Token(TOKEN_SEMICOLON@[4; 5))
    Node(NODE_INHERIT@[5; 17))
      Token(TOKEN_INHERIT@[5; 12))
      Token(TOKEN_WHITESPACE@[12; 13))
      Node(NODE_IDENT@[13; 14))
        Token(TOKEN_IDENT@[13; 14))
      Token(TOKEN_WHITESPACE@[14; 15))
      Node(NODE_IDENT@[15; 16))
        Token(TOKEN_IDENT@[15; 16))
      Token(TOKEN_SEMICOLON@[16; 17))
    Node(NODE_INHERIT@[17; 35))
      Token(TOKEN_INHERIT@[17; 24))
      Token(TOKEN_WHITESPACE@[24; 25))
      Node(NODE_INHERIT_FROM@[25; 30))
        Token(TOKEN_PAREN_OPEN@[25; 26))
        Node(NODE_IDENT@[26; 29))
          Token(TOKEN_IDENT@[26; 29))
        Token(TOKEN_PAREN_CLOSE@[29; 30))
      Token(TOKEN_WHITESPACE@[30; 31))
      Node(NODE_IDENT@[31; 32))
        Token(TOKEN_IDENT@[31; 32))
      Token(TOKEN_WHITESPACE@[32; 33))
      Node(NODE_IDENT@[33; 34))
        Token(TOKEN_IDENT@[33; 34))
      Token(TOKEN_SEMICOLON@[34; 35))
    Token(TOKEN_CURLY_B_CLOSE@[35; 36))
"
        );
    }
    #[test]
    fn ifs() {
        test!(
            [
                (TOKEN_IDENT, "false"),
                (TOKEN_IMPLICATION, "->"),
                (TOKEN_INVERT, "!"),
                (TOKEN_IDENT, "false"),

                (TOKEN_AND, "&&"),

                (TOKEN_IDENT, "false"),
                (TOKEN_EQUAL, "=="),
                (TOKEN_IDENT, "true"),

                (TOKEN_OR, "||"),

                (TOKEN_IDENT, "true")
            ],
            "\
Node(NODE_ROOT@[0; 32))
  Node(NODE_OPERATION@[0; 32))
    Node(NODE_IDENT@[0; 5))
      Token(TOKEN_IDENT@[0; 5))
    Token(TOKEN_IMPLICATION@[5; 7))
    Node(NODE_OPERATION@[7; 32))
      Node(NODE_OPERATION@[7; 26))
        Node(NODE_UNARY@[7; 13))
          Token(TOKEN_INVERT@[7; 8))
          Node(NODE_IDENT@[8; 13))
            Token(TOKEN_IDENT@[8; 13))
        Token(TOKEN_AND@[13; 15))
        Node(NODE_OPERATION@[15; 26))
          Node(NODE_IDENT@[15; 20))
            Token(TOKEN_IDENT@[15; 20))
          Token(TOKEN_EQUAL@[20; 22))
          Node(NODE_IDENT@[22; 26))
            Token(TOKEN_IDENT@[22; 26))
      Token(TOKEN_OR@[26; 28))
      Node(NODE_IDENT@[28; 32))
        Token(TOKEN_IDENT@[28; 32))
"
        );
        test!(
            [
                (TOKEN_INTEGER, "1"),
                (TOKEN_LESS, "<"),
                (TOKEN_INTEGER, "2"),

                (TOKEN_OR, "||"),

                (TOKEN_INTEGER, "2"),
                (TOKEN_LESS_OR_EQ, "<="),
                (TOKEN_INTEGER, "2"),

                (TOKEN_AND, "&&"),

                (TOKEN_INTEGER, "2"),
                (TOKEN_MORE, ">"),
                (TOKEN_INTEGER, "1"),

                (TOKEN_AND, "&&"),

                (TOKEN_INTEGER, "2"),
                (TOKEN_MORE_OR_EQ, ">="),
                (TOKEN_INTEGER, "2")
            ],
            "\
Node(NODE_ROOT@[0; 20))
  Node(NODE_OPERATION@[0; 20))
    Node(NODE_OPERATION@[0; 3))
      Node(NODE_VALUE@[0; 1))
        Token(TOKEN_INTEGER@[0; 1))
      Token(TOKEN_LESS@[1; 2))
      Node(NODE_VALUE@[2; 3))
        Token(TOKEN_INTEGER@[2; 3))
    Token(TOKEN_OR@[3; 5))
    Node(NODE_OPERATION@[5; 20))
      Node(NODE_OPERATION@[5; 14))
        Node(NODE_OPERATION@[5; 9))
          Node(NODE_VALUE@[5; 6))
            Token(TOKEN_INTEGER@[5; 6))
          Token(TOKEN_LESS_OR_EQ@[6; 8))
          Node(NODE_VALUE@[8; 9))
            Token(TOKEN_INTEGER@[8; 9))
        Token(TOKEN_AND@[9; 11))
        Node(NODE_OPERATION@[11; 14))
          Node(NODE_VALUE@[11; 12))
            Token(TOKEN_INTEGER@[11; 12))
          Token(TOKEN_MORE@[12; 13))
          Node(NODE_VALUE@[13; 14))
            Token(TOKEN_INTEGER@[13; 14))
      Token(TOKEN_AND@[14; 16))
      Node(NODE_OPERATION@[16; 20))
        Node(NODE_VALUE@[16; 17))
          Token(TOKEN_INTEGER@[16; 17))
        Token(TOKEN_MORE_OR_EQ@[17; 19))
        Node(NODE_VALUE@[19; 20))
          Token(TOKEN_INTEGER@[19; 20))
"
        );
        test!(
            [
                (TOKEN_INTEGER, "1"),
                (TOKEN_EQUAL, "=="),
                (TOKEN_INTEGER, "1"),

                (TOKEN_AND, "&&"),

                (TOKEN_INTEGER, "2"),
                (TOKEN_NOT_EQUAL, "!="),
                (TOKEN_INTEGER, "3")
            ],
            "\
Node(NODE_ROOT@[0; 10))
  Node(NODE_OPERATION@[0; 10))
    Node(NODE_OPERATION@[0; 4))
      Node(NODE_VALUE@[0; 1))
        Token(TOKEN_INTEGER@[0; 1))
      Token(TOKEN_EQUAL@[1; 3))
      Node(NODE_VALUE@[3; 4))
        Token(TOKEN_INTEGER@[3; 4))
    Token(TOKEN_AND@[4; 6))
    Node(NODE_OPERATION@[6; 10))
      Node(NODE_VALUE@[6; 7))
        Token(TOKEN_INTEGER@[6; 7))
      Token(TOKEN_NOT_EQUAL@[7; 9))
      Node(NODE_VALUE@[9; 10))
        Token(TOKEN_INTEGER@[9; 10))
"
        );
        test!(
            [
                (TOKEN_IF, "if"),
                (TOKEN_IDENT, "false"),
                (TOKEN_THEN, "then"),
                    (TOKEN_INTEGER, "1"),
                (TOKEN_ELSE, "else"),
                    (TOKEN_IF, "if"),
                    (TOKEN_IDENT, "true"),
                    (TOKEN_THEN, "then"),
                        (TOKEN_IDENT, "two"),
                    (TOKEN_ELSE, "else"),
                        (TOKEN_INTEGER, "3")
            ],
            "\
Node(NODE_ROOT@[0; 34))
  Node(NODE_IF_ELSE@[0; 34))
    Token(TOKEN_IF@[0; 2))
    Node(NODE_IDENT@[2; 7))
      Token(TOKEN_IDENT@[2; 7))
    Token(TOKEN_THEN@[7; 11))
    Node(NODE_VALUE@[11; 12))
      Token(TOKEN_INTEGER@[11; 12))
    Token(TOKEN_ELSE@[12; 16))
    Node(NODE_IF_ELSE@[16; 34))
      Token(TOKEN_IF@[16; 18))
      Node(NODE_IDENT@[18; 22))
        Token(TOKEN_IDENT@[18; 22))
      Token(TOKEN_THEN@[22; 26))
      Node(NODE_IDENT@[26; 29))
        Token(TOKEN_IDENT@[26; 29))
      Token(TOKEN_ELSE@[29; 33))
      Node(NODE_VALUE@[33; 34))
        Token(TOKEN_INTEGER@[33; 34))
"
        );
    }
    #[test]
    fn list() {
        test!(
            [
                (TOKEN_SQUARE_B_OPEN, "["),
                (TOKEN_IDENT, "a"),
                (TOKEN_INTEGER, "2"),
                (TOKEN_INTEGER, "3"),
                (TOKEN_STRING, "\"lol\""),
                (TOKEN_SQUARE_B_CLOSE, "]")
            ],
            "\
Node(NODE_ROOT@[0; 10))
  Node(NODE_LIST@[0; 10))
    Token(TOKEN_SQUARE_B_OPEN@[0; 1))
    Node(NODE_IDENT@[1; 2))
      Token(TOKEN_IDENT@[1; 2))
    Node(NODE_VALUE@[2; 3))
      Token(TOKEN_INTEGER@[2; 3))
    Node(NODE_VALUE@[3; 4))
      Token(TOKEN_INTEGER@[3; 4))
    Node(NODE_VALUE@[4; 9))
      Token(TOKEN_STRING@[4; 9))
    Token(TOKEN_SQUARE_B_CLOSE@[9; 10))
"
        );
        test!(
            [
                (TOKEN_SQUARE_B_OPEN, "["), (TOKEN_INTEGER, "1"), (TOKEN_SQUARE_B_CLOSE, "]"),
                (TOKEN_CONCAT, "++"),
                (TOKEN_SQUARE_B_OPEN, "["), (TOKEN_IDENT, "two"), (TOKEN_SQUARE_B_CLOSE, "]"),
                (TOKEN_CONCAT, "++"),
                (TOKEN_SQUARE_B_OPEN, "["), (TOKEN_INTEGER, "3"), (TOKEN_SQUARE_B_CLOSE, "]")
            ],
            "\
Node(NODE_ROOT@[0; 15))
  Node(NODE_OPERATION@[0; 15))
    Node(NODE_OPERATION@[0; 10))
      Node(NODE_LIST@[0; 3))
        Token(TOKEN_SQUARE_B_OPEN@[0; 1))
        Node(NODE_VALUE@[1; 2))
          Token(TOKEN_INTEGER@[1; 2))
        Token(TOKEN_SQUARE_B_CLOSE@[2; 3))
      Token(TOKEN_CONCAT@[3; 5))
      Node(NODE_LIST@[5; 10))
        Token(TOKEN_SQUARE_B_OPEN@[5; 6))
        Node(NODE_IDENT@[6; 9))
          Token(TOKEN_IDENT@[6; 9))
        Token(TOKEN_SQUARE_B_CLOSE@[9; 10))
    Token(TOKEN_CONCAT@[10; 12))
    Node(NODE_LIST@[12; 15))
      Token(TOKEN_SQUARE_B_OPEN@[12; 13))
      Node(NODE_VALUE@[13; 14))
        Token(TOKEN_INTEGER@[13; 14))
      Token(TOKEN_SQUARE_B_CLOSE@[14; 15))
"
        );
    }
    #[test]
    fn lambda() {
        test!(
            [
                (TOKEN_IDENT, "import"),
                (TOKEN_PATH, "<nixpkgs>"),
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_CURLY_B_CLOSE, "}")
            ],
            "\
Node(NODE_ROOT@[0; 17))
  Node(NODE_APPLY@[0; 17))
    Node(NODE_APPLY@[0; 15))
      Node(NODE_IDENT@[0; 6))
        Token(TOKEN_IDENT@[0; 6))
      Node(NODE_VALUE@[6; 15))
        Token(TOKEN_PATH@[6; 15))
    Node(NODE_SET@[15; 17))
      Token(TOKEN_CURLY_B_OPEN@[15; 16))
      Token(TOKEN_CURLY_B_CLOSE@[16; 17))
"
        );
        test!(
            [
                (TOKEN_IDENT, "a"),
                (TOKEN_COLON, ":"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "b"),
                (TOKEN_COLON, ":"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "a"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ADD, "+"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "b")
            ],
            "\
Node(NODE_ROOT@[0; 11))
  Node(NODE_LAMBDA@[0; 11))
    Node(NODE_IDENT@[0; 1))
      Token(TOKEN_IDENT@[0; 1))
    Token(TOKEN_COLON@[1; 2))
    Token(TOKEN_WHITESPACE@[2; 3))
    Node(NODE_LAMBDA@[3; 11))
      Node(NODE_IDENT@[3; 4))
        Token(TOKEN_IDENT@[3; 4))
      Token(TOKEN_COLON@[4; 5))
      Token(TOKEN_WHITESPACE@[5; 6))
      Node(NODE_OPERATION@[6; 11))
        Node(NODE_IDENT@[6; 7))
          Token(TOKEN_IDENT@[6; 7))
        Token(TOKEN_WHITESPACE@[7; 8))
        Token(TOKEN_ADD@[8; 9))
        Token(TOKEN_WHITESPACE@[9; 10))
        Node(NODE_IDENT@[10; 11))
          Token(TOKEN_IDENT@[10; 11))
"
        );
        test!(
            [
                (TOKEN_IDENT, "a"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "1"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "2"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ADD, "+"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "3")
            ],
            "\
Node(NODE_ROOT@[0; 9))
  Node(NODE_OPERATION@[0; 9))
    Node(NODE_APPLY@[0; 6))
      Node(NODE_APPLY@[0; 4))
        Node(NODE_IDENT@[0; 1))
          Token(TOKEN_IDENT@[0; 1))
        Token(TOKEN_WHITESPACE@[1; 2))
        Node(NODE_VALUE@[2; 3))
          Token(TOKEN_INTEGER@[2; 3))
        Token(TOKEN_WHITESPACE@[3; 4))
      Node(NODE_VALUE@[4; 5))
        Token(TOKEN_INTEGER@[4; 5))
      Token(TOKEN_WHITESPACE@[5; 6))
    Token(TOKEN_ADD@[6; 7))
    Token(TOKEN_WHITESPACE@[7; 8))
    Node(NODE_VALUE@[8; 9))
      Token(TOKEN_INTEGER@[8; 9))
"
        );
   }
    #[test]
    fn patterns() {
        test!(
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_ELLIPSIS, "..."),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_CURLY_B_CLOSE, "}"),
                (TOKEN_COLON, ":"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "1")
            ],
            "\
Node(NODE_ROOT@[0; 10))
  Node(NODE_LAMBDA@[0; 10))
    Node(NODE_PATTERN@[0; 7))
      Token(TOKEN_CURLY_B_OPEN@[0; 1))
      Token(TOKEN_WHITESPACE@[1; 2))
      Token(TOKEN_ELLIPSIS@[2; 5))
      Token(TOKEN_WHITESPACE@[5; 6))
      Token(TOKEN_CURLY_B_CLOSE@[6; 7))
    Token(TOKEN_COLON@[7; 8))
    Token(TOKEN_WHITESPACE@[8; 9))
    Node(NODE_VALUE@[9; 10))
      Token(TOKEN_INTEGER@[9; 10))
"
        );
        test!(
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_CURLY_B_CLOSE, "}"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_AT, "@"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "outer"),
                (TOKEN_COLON, ":"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_INTEGER, "1")
            ],
            "\
Node(NODE_ROOT@[0; 13))
  Node(NODE_LAMBDA@[0; 13))
    Node(NODE_PATTERN@[0; 10))
      Token(TOKEN_CURLY_B_OPEN@[0; 1))
      Token(TOKEN_CURLY_B_CLOSE@[1; 2))
      Token(TOKEN_WHITESPACE@[2; 3))
      Node(NODE_PAT_BIND@[3; 10))
        Token(TOKEN_AT@[3; 4))
        Node(NODE_IDENT@[4; 10))
          Token(TOKEN_WHITESPACE@[4; 5))
          Token(TOKEN_IDENT@[5; 10))
    Token(TOKEN_COLON@[10; 11))
    Token(TOKEN_WHITESPACE@[11; 12))
    Node(NODE_VALUE@[12; 13))
      Token(TOKEN_INTEGER@[12; 13))
"
        );
        test!(
            [
                (TOKEN_CURLY_B_OPEN, "{"), (TOKEN_WHITESPACE, " "),

                (TOKEN_IDENT, "a"), (TOKEN_COMMA, ","), (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "b"), (TOKEN_WHITESPACE, " "),
                    (TOKEN_QUESTION, "?"), (TOKEN_WHITESPACE, " "),
                    (TOKEN_STRING, "\"default\""),

                (TOKEN_CURLY_B_CLOSE, "}"),
                (TOKEN_COLON, ":"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "a")
            ],
            "\
Node(NODE_ROOT@[0; 22))
  Node(NODE_LAMBDA@[0; 22))
    Node(NODE_PATTERN@[0; 19))
      Token(TOKEN_CURLY_B_OPEN@[0; 1))
      Token(TOKEN_WHITESPACE@[1; 2))
      Node(NODE_PAT_ENTRY@[2; 3))
        Node(NODE_IDENT@[2; 3))
          Token(TOKEN_IDENT@[2; 3))
      Token(TOKEN_COMMA@[3; 4))
      Token(TOKEN_WHITESPACE@[4; 5))
      Node(NODE_PAT_ENTRY@[5; 18))
        Node(NODE_IDENT@[5; 6))
          Token(TOKEN_IDENT@[5; 6))
        Token(TOKEN_WHITESPACE@[6; 7))
        Token(TOKEN_QUESTION@[7; 8))
        Token(TOKEN_WHITESPACE@[8; 9))
        Node(NODE_VALUE@[9; 18))
          Token(TOKEN_STRING@[9; 18))
      Token(TOKEN_CURLY_B_CLOSE@[18; 19))
    Token(TOKEN_COLON@[19; 20))
    Token(TOKEN_WHITESPACE@[20; 21))
    Node(NODE_IDENT@[21; 22))
      Token(TOKEN_IDENT@[21; 22))
"
        );
        test!(
            [
                (TOKEN_CURLY_B_OPEN, "{"), (TOKEN_WHITESPACE, " "),

                (TOKEN_IDENT, "a"), (TOKEN_COMMA, ","), (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "b"), (TOKEN_WHITESPACE, " "),
                    (TOKEN_QUESTION, "?"), (TOKEN_WHITESPACE, " "),
                    (TOKEN_STRING, "\"default\""), (TOKEN_COMMA, ","), (TOKEN_WHITESPACE, " "),
                (TOKEN_ELLIPSIS, "..."), (TOKEN_WHITESPACE, " "),

                (TOKEN_CURLY_B_CLOSE, "}"),
                (TOKEN_COLON, ":"),
                (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "a")
            ],
            "\
Node(NODE_ROOT@[0; 28))
  Node(NODE_LAMBDA@[0; 28))
    Node(NODE_PATTERN@[0; 25))
      Token(TOKEN_CURLY_B_OPEN@[0; 1))
      Token(TOKEN_WHITESPACE@[1; 2))
      Node(NODE_PAT_ENTRY@[2; 3))
        Node(NODE_IDENT@[2; 3))
          Token(TOKEN_IDENT@[2; 3))
      Token(TOKEN_COMMA@[3; 4))
      Token(TOKEN_WHITESPACE@[4; 5))
      Node(NODE_PAT_ENTRY@[5; 18))
        Node(NODE_IDENT@[5; 6))
          Token(TOKEN_IDENT@[5; 6))
        Token(TOKEN_WHITESPACE@[6; 7))
        Token(TOKEN_QUESTION@[7; 8))
        Token(TOKEN_WHITESPACE@[8; 9))
        Node(NODE_VALUE@[9; 18))
          Token(TOKEN_STRING@[9; 18))
      Token(TOKEN_COMMA@[18; 19))
      Token(TOKEN_WHITESPACE@[19; 20))
      Token(TOKEN_ELLIPSIS@[20; 23))
      Token(TOKEN_WHITESPACE@[23; 24))
      Token(TOKEN_CURLY_B_CLOSE@[24; 25))
    Token(TOKEN_COLON@[25; 26))
    Token(TOKEN_WHITESPACE@[26; 27))
    Node(NODE_IDENT@[27; 28))
      Token(TOKEN_IDENT@[27; 28))
"
        );
        test!(
            [
                (TOKEN_IDENT, "outer"), (TOKEN_WHITESPACE, " "),
                (TOKEN_AT, "@"), (TOKEN_WHITESPACE, " "),
                (TOKEN_CURLY_B_OPEN, "{"), (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "a"), (TOKEN_WHITESPACE, " "),
                (TOKEN_CURLY_B_CLOSE, "}"),
                (TOKEN_COLON, ":"), (TOKEN_WHITESPACE, " "),
                (TOKEN_IDENT, "outer")
            ],
            "\
Node(NODE_ROOT@[0; 20))
  Node(NODE_LAMBDA@[0; 20))
    Node(NODE_PATTERN@[0; 13))
      Node(NODE_PAT_BIND@[0; 7))
        Node(NODE_IDENT@[0; 5))
          Token(TOKEN_IDENT@[0; 5))
        Token(TOKEN_WHITESPACE@[5; 6))
        Token(TOKEN_AT@[6; 7))
      Token(TOKEN_WHITESPACE@[7; 8))
      Token(TOKEN_CURLY_B_OPEN@[8; 9))
      Token(TOKEN_WHITESPACE@[9; 10))
      Node(NODE_PAT_ENTRY@[10; 12))
        Node(NODE_IDENT@[10; 11))
          Token(TOKEN_IDENT@[10; 11))
        Token(TOKEN_WHITESPACE@[11; 12))
      Token(TOKEN_CURLY_B_CLOSE@[12; 13))
    Token(TOKEN_COLON@[13; 14))
    Token(TOKEN_WHITESPACE@[14; 15))
    Node(NODE_IDENT@[15; 20))
      Token(TOKEN_IDENT@[15; 20))
"
        );
        test!(
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_IDENT, "a"),
                (TOKEN_QUESTION, "?"),
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_CURLY_B_CLOSE, "}"),
                (TOKEN_CURLY_B_CLOSE, "}"),
                (TOKEN_COLON, ":"),
                (TOKEN_IDENT, "a")
            ],
            "\
Node(NODE_ROOT@[0; 8))
  Node(NODE_LAMBDA@[0; 8))
    Node(NODE_PATTERN@[0; 6))
      Token(TOKEN_CURLY_B_OPEN@[0; 1))
      Node(NODE_PAT_ENTRY@[1; 5))
        Node(NODE_IDENT@[1; 2))
          Token(TOKEN_IDENT@[1; 2))
        Token(TOKEN_QUESTION@[2; 3))
        Node(NODE_SET@[3; 5))
          Token(TOKEN_CURLY_B_OPEN@[3; 4))
          Token(TOKEN_CURLY_B_CLOSE@[4; 5))
      Token(TOKEN_CURLY_B_CLOSE@[5; 6))
    Token(TOKEN_COLON@[6; 7))
    Node(NODE_IDENT@[7; 8))
      Token(TOKEN_IDENT@[7; 8))
"
        );
        test!(
            [
                (TOKEN_CURLY_B_OPEN, "{"),
                (TOKEN_IDENT, "a"),
                (TOKEN_COMMA, ","),
                (TOKEN_CURLY_B_CLOSE, "}"),
                (TOKEN_COLON, ":"),
                (TOKEN_IDENT, "a")
            ],
            "\
Node(NODE_ROOT@[0; 6))
  Node(NODE_LAMBDA@[0; 6))
    Node(NODE_PATTERN@[0; 4))
      Token(TOKEN_CURLY_B_OPEN@[0; 1))
      Node(NODE_PAT_ENTRY@[1; 2))
        Node(NODE_IDENT@[1; 2))
          Token(TOKEN_IDENT@[1; 2))
      Token(TOKEN_COMMA@[2; 3))
      Token(TOKEN_CURLY_B_CLOSE@[3; 4))
    Token(TOKEN_COLON@[4; 5))
    Node(NODE_IDENT@[5; 6))
      Token(TOKEN_IDENT@[5; 6))
"
        );
    }
    #[test]
    fn dynamic() {
        test!(
            [
                (TOKEN_DYNAMIC_START, "${"),
                    (TOKEN_IDENT, "a"),
                (TOKEN_DYNAMIC_END, "}")
            ],
            "\
Node(NODE_ROOT@[0; 4))
  Node(NODE_DYNAMIC@[0; 4))
    Token(TOKEN_DYNAMIC_START@[0; 2))
    Node(NODE_IDENT@[2; 3))
      Token(TOKEN_IDENT@[2; 3))
    Token(TOKEN_DYNAMIC_END@[3; 4))
"
        );
    }
}
