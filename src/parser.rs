//! The parser: turns a series of tokens into an AST

use std::{collections::VecDeque, fmt};

use cbitset::BitSet256;
use rowan::{Checkpoint, GreenNodeBuilder, SmolStr, SyntaxKind, SyntaxNode, TextRange, TreeArc};

use crate::types::{Root, TypedNode};

const OR: &'static str = "or";

/// An error that occurred during parsing
#[derive(Clone, Debug, PartialEq)]
pub enum ParseError {
    Unexpected(TextRange),
    UnexpectedEOF,
    UnexpectedEOFWanted(Box<[SyntaxKind]>),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::Unexpected(_range) => write!(f, "unexpected input"),
            ParseError::UnexpectedEOF => write!(f, "unexpected eof"),
            ParseError::UnexpectedEOFWanted(kinds) => {
                write!(f, "unexpected eof, wanted any of {:?}", kinds)
            }
        }
    }
}

impl std::error::Error for ParseError {}

/// The type of a node in the AST
pub mod nodes {
    pub use crate::tokenizer::tokens::*;
    use rowan::SyntaxKind;

    pub fn syntax_name(val: SyntaxKind) -> Option<&'static str> {
        token_name(val).or_else(|| node_name(val))
    }

    // Generates the constants like an enum, see lib.rs
    magic! {
        start 100;
        lookup node_name;

        NODE_APPLY
        NODE_ASSERT
        NODE_ATTRIBUTE
        NODE_DYNAMIC
        NODE_ERROR
        NODE_IDENT
        NODE_IF_ELSE
        NODE_INDEX_SET
        NODE_INHERIT
        NODE_INHERIT_FROM
        NODE_STRING
        NODE_STRING_INTERPOL
        NODE_LAMBDA
        NODE_LET
        NODE_LET_IN
        NODE_LIST
        NODE_OPERATION
        NODE_OR_DEFAULT
        NODE_PAREN
        NODE_PATTERN
        NODE_PAT_BIND
        NODE_PAT_ENTRY
        NODE_ROOT
        NODE_SET
        NODE_SET_ENTRY
        NODE_UNARY
        NODE_VALUE
        NODE_WITH
    }
}
use self::nodes::*;

/// The result of a parse
#[derive(Clone)]
pub struct AST {
    node: TreeArc<SyntaxNode>,
    errors: Vec<ParseError>,
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
    /// Return all the errors that occured while parsing - NOT
    /// including invalid stuff in the AST
    pub fn root_errors(&self) -> &[ParseError] {
        &self.errors
    }
    /// Return all errors in the tree, if any
    pub fn errors(&self) -> Vec<ParseError> {
        let mut errors = self.errors.clone();
        errors.extend(
            self.root().errors().into_iter().map(|node| ParseError::Unexpected(node.range())),
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
where
    I: Iterator<Item = (SyntaxKind, SmolStr)>,
{
    builder: GreenNodeBuilder,
    errors: Vec<ParseError>,

    trivia_buffer: Vec<I::Item>,
    buffer: VecDeque<I::Item>,
    iter: I,
}
impl<I> Parser<I>
where
    I: Iterator<Item = (SyntaxKind, SmolStr)>,
{
    fn new(iter: I) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),

            trivia_buffer: Vec::with_capacity(1),
            buffer: VecDeque::with_capacity(1),
            iter,
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
    fn eat_trivia(&mut self) {
        self.peek();
        self.trivia_buffer.drain(..).for_each({
            let builder = &mut self.builder;
            move |(t, s)| builder.token(t, s)
        });
    }
    fn start_node(&mut self, kind: SyntaxKind) {
        self.eat_trivia();
        self.builder.start_node(kind);
    }
    fn checkpoint(&mut self) -> Checkpoint {
        self.eat_trivia();
        self.builder.checkpoint()
    }
    fn bump(&mut self) {
        let next = self.buffer.pop_front().or_else(|| self.iter.next());
        match next {
            Some((token, s)) => {
                if token_helpers::is_trivia(token) {
                    self.trivia_buffer.push((token, s))
                } else {
                    self.trivia_buffer.drain(..).for_each({
                        let builder = &mut self.builder;
                        move |(t, s)| builder.token(t, s)
                    });
                    self.builder.token(token, s)
                }
            }
            None => self.errors.push(ParseError::UnexpectedEOF),
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
    fn expect_peek_any(&mut self, allowed_slice: &[SyntaxKind]) -> Option<SyntaxKind> {
        let allowed: BitSet256 = allowed_slice.iter().map(|k| k.0).collect();

        let next = match self.peek() {
            None => None,
            Some(kind) if allowed.contains(kind.0 as usize) => Some(kind),
            Some(_) => {
                self.start_node(NODE_ERROR);
                loop {
                    self.bump();
                    if self.peek().map(|kind| allowed.contains(kind.0 as usize)).unwrap_or(true) {
                        break;
                    }
                }
                self.builder.finish_node();
                self.peek()
            }
        };
        if next.is_none() {
            self.errors
                .push(ParseError::UnexpectedEOFWanted(allowed_slice.to_vec().into_boxed_slice()));
        }
        next
    }
    fn expect(&mut self, expected: SyntaxKind) {
        if self.expect_peek_any(&[expected]).is_some() {
            self.bump();
        }
    }

    fn parse_dynamic(&mut self) {
        self.start_node(NODE_DYNAMIC);
        self.bump();
        while self.peek().map(|t| t != TOKEN_DYNAMIC_END).unwrap_or(false) {
            self.parse_expr();
        }
        self.bump();
        self.builder.finish_node();
    }
    fn parse_string(&mut self) {
        self.start_node(NODE_STRING);
        self.expect(TOKEN_STRING_START);

        loop {
            match self.expect_peek_any(&[
                TOKEN_STRING_END,
                TOKEN_STRING_CONTENT,
                TOKEN_INTERPOL_START,
            ]) {
                Some(TOKEN_STRING_CONTENT) => self.bump(),
                Some(TOKEN_INTERPOL_START) => {
                    self.start_node(NODE_STRING_INTERPOL);
                    self.bump();
                    self.parse_expr();
                    self.expect(TOKEN_INTERPOL_END);
                    self.builder.finish_node();
                }
                // handled by expect_peek_any
                _ => break,
            }
        }
        self.expect(TOKEN_STRING_END);

        self.builder.finish_node();
    }
    fn next_attr(&mut self) {
        match self.peek() {
            Some(TOKEN_DYNAMIC_START) => self.parse_dynamic(),
            Some(TOKEN_STRING_START) => self.parse_string(),
            _ => {
                self.start_node(NODE_IDENT);
                self.expect(TOKEN_IDENT);
                self.builder.finish_node();
            }
        }
    }
    fn parse_attr(&mut self) {
        self.start_node(NODE_ATTRIBUTE);
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
                match self.expect_peek_any(&[TOKEN_CURLY_B_CLOSE, TOKEN_ELLIPSIS, TOKEN_IDENT]) {
                    Some(TOKEN_CURLY_B_CLOSE) => {
                        self.bump();
                        break;
                    }
                    Some(TOKEN_ELLIPSIS) => {
                        self.bump();
                        self.expect(TOKEN_CURLY_B_CLOSE);
                        break;
                    }
                    Some(TOKEN_IDENT) => {
                        self.start_node(NODE_PAT_ENTRY);

                        self.start_node(NODE_IDENT);
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
                            }
                        }
                    }
                    // handled by expect_peek_any
                    _ => break,
                }
            }
        }

        if self.peek() == Some(TOKEN_AT) {
            let kind = if bound { NODE_ERROR } else { NODE_PAT_BIND };
            self.start_node(kind);
            self.bump();
            self.start_node(NODE_IDENT);
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
                    self.start_node(NODE_INHERIT);
                    self.bump();

                    if self.peek() == Some(TOKEN_PAREN_OPEN) {
                        self.start_node(NODE_INHERIT_FROM);
                        self.bump();
                        self.parse_expr();
                        self.expect(TOKEN_PAREN_CLOSE);
                        self.builder.finish_node();
                    }

                    while let Some(TOKEN_IDENT) = self.peek() {
                        self.start_node(NODE_IDENT);
                        self.bump();
                        self.builder.finish_node();
                    }

                    self.expect(TOKEN_SEMICOLON);
                    self.builder.finish_node();
                }
                Some(_) => {
                    self.start_node(NODE_SET_ENTRY);
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
        let checkpoint = self.checkpoint();
        match self.peek() {
            Some(TOKEN_PAREN_OPEN) => {
                self.start_node(NODE_PAREN);
                self.bump();
                self.parse_expr();
                self.bump();
                self.builder.finish_node();
            }
            Some(TOKEN_REC) => {
                self.start_node(NODE_SET);
                self.bump();
                self.expect(TOKEN_CURLY_B_OPEN);
                self.parse_set(TOKEN_CURLY_B_CLOSE);
                self.builder.finish_node();
            }
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
                        self.start_node(NODE_LAMBDA);

                        self.start_node(NODE_PATTERN);
                        self.bump();
                        self.parse_pattern(false);
                        self.builder.finish_node();

                        self.expect(TOKEN_COLON);
                        self.parse_expr();

                        self.builder.finish_node();
                    }
                    _ => {
                        // This looks like a set
                        self.start_node(NODE_SET);
                        self.bump();
                        self.parse_set(TOKEN_CURLY_B_CLOSE);
                        self.builder.finish_node();
                    }
                }
            }
            Some(TOKEN_SQUARE_B_OPEN) => {
                self.start_node(NODE_LIST);
                self.bump();
                while self.peek().map(|t| t != TOKEN_SQUARE_B_CLOSE).unwrap_or(false) {
                    self.parse_val();
                }
                self.bump();
                self.builder.finish_node();
            }
            Some(TOKEN_DYNAMIC_START) => self.parse_dynamic(),
            Some(TOKEN_STRING_START) => self.parse_string(),
            Some(t) if token_helpers::is_value(t) => {
                self.start_node(NODE_VALUE);
                self.bump();
                self.builder.finish_node();
            }
            Some(TOKEN_IDENT) => {
                let checkpoint = self.checkpoint();
                self.start_node(NODE_IDENT);
                self.bump();
                self.builder.finish_node();

                match self.peek() {
                    Some(TOKEN_COLON) => {
                        self.builder.start_node_at(checkpoint, NODE_LAMBDA);
                        self.bump();
                        self.parse_expr();
                        self.builder.finish_node();
                    }
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
                    }
                    _ => (),
                }
            }
            _ => {
                self.start_node(NODE_ERROR);
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
        let checkpoint = self.checkpoint();
        self.parse_val();

        while self.peek().map(|t| token_helpers::is_fn_arg(t)).unwrap_or(false) {
            self.builder.start_node_at(checkpoint, NODE_APPLY);
            self.parse_val();
            self.builder.finish_node();
        }
    }
    fn parse_negate(&mut self) {
        if self.peek() == Some(TOKEN_SUB) {
            self.start_node(NODE_UNARY);
            self.bump();
            self.parse_negate();
            self.builder.finish_node();
        } else {
            self.parse_fn()
        }
    }
    fn handle_operation(&mut self, once: bool, next: fn(&mut Self), ops: &[SyntaxKind]) {
        let checkpoint = self.checkpoint();
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
            self.start_node(NODE_UNARY);
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
        self.handle_operation(
            true,
            Self::parse_merge,
            &[TOKEN_LESS, TOKEN_LESS_OR_EQ, TOKEN_MORE, TOKEN_MORE_OR_EQ],
        )
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
                let checkpoint = self.checkpoint();
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
            }
            Some(TOKEN_WITH) => {
                self.start_node(NODE_WITH);
                self.bump();
                self.parse_expr();
                self.expect(TOKEN_SEMICOLON);
                self.parse_expr();
                self.builder.finish_node();
            }
            Some(TOKEN_IF) => {
                self.start_node(NODE_IF_ELSE);
                self.bump();
                self.parse_expr();
                self.expect(TOKEN_THEN);
                self.parse_expr();
                self.expect(TOKEN_ELSE);
                self.parse_expr();
                self.builder.finish_node();
            }
            Some(TOKEN_ASSERT) => {
                self.start_node(NODE_ASSERT);
                self.bump();
                self.parse_expr();
                self.expect(TOKEN_SEMICOLON);
                self.parse_expr();
                self.builder.finish_node();
            }
            _ => self.parse_math(),
        }
    }
}

/// Parse tokens into an AST
pub fn parse<I>(iter: I) -> AST
where
    I: IntoIterator<Item = (SyntaxKind, SmolStr)>,
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
    parser.eat_trivia();
    parser.builder.finish_node();
    AST { node: SyntaxNode::new(parser.builder.finish(), None), errors: parser.errors }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::{ffi::OsStr, fmt::Write, fs, path::PathBuf};

    fn test_dir(name: &str) {
        let dir: PathBuf = ["test_data", "parser", name].iter().collect();

        for entry in dir.read_dir().unwrap() {
            let entry = entry.unwrap();
            let mut path = entry.path();
            if path.extension() != Some(OsStr::new("nix")) {
                continue;
            }
            let mut code = fs::read_to_string(&path).unwrap();
            if code.ends_with('\n') {
                code.truncate(code.len() - 1);
            }
            let ast = crate::parse(&code);
            path.set_extension("expect");
            let expected = fs::read_to_string(&path).unwrap();

            let mut actual = String::new();
            for error in ast.root_errors() {
                writeln!(actual, "error: {}", error).unwrap();
            }
            writeln!(actual, "{}", ast.root().dump()).unwrap();

            if actual != expected {
                path.set_extension("nix");
                eprintln!("In {}:", path.display());
                eprintln!("--- Actual ---");
                eprintln!("{}", actual);
                eprintln!("-- Expected ---");
                eprintln!("{}", expected);
                eprintln!("--- End ---");
                panic!("Tests did not match");
            }
        }
    }

    #[test]
    fn set() {
        test_dir("set");
    }
    #[test]
    fn math() {
        test_dir("math");
    }
    #[test]
    fn let_in() {
        test_dir("let_in");
    }
    #[test]
    fn let_legacy_syntax() {
        test_dir("let_legacy_syntax");
    }
    #[test]
    fn interpolation() {
        test_dir("interpolation");
    }
    #[test]
    fn index_set() {
        test_dir("index_set");
    }
    #[test]
    fn isset() {
        test_dir("isset");
    }
    #[test]
    fn merge() {
        test_dir("merge");
    }
    #[test]
    fn with() {
        test_dir("with");
    }
    #[test]
    fn assert() {
        test_dir("assert");
    }
    #[test]
    fn inherit() {
        test_dir("inherit");
    }
    #[test]
    fn ifs() {
        test_dir("ifs");
    }
    #[test]
    fn list() {
        test_dir("list");
    }
    #[test]
    fn lambda() {
        test_dir("lambda");
    }
    #[test]
    fn patterns() {
        test_dir("patterns");
    }
    #[test]
    fn dynamic() {
        test_dir("dynamic");
    }
    #[test]
    fn paths() {
        test_dir("paths");
    }
    #[test]
    fn strings() {
        test_dir("strings");
    }
}
