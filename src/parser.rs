//! The parser: turns a series of tokens into an AST

use std::{
    collections::{HashMap, VecDeque},
    fmt,
};

use rowan::{Checkpoint, GreenNode, GreenNodeBuilder, Language, TextRange, TextSize};

use crate::{
    tokenizer::Token,
    NixLanguage,
    SyntaxKind::{self, *},
    TokenSet,
};

/// An error that occurred during parsing
#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum ParseError {
    /// Unexpected is used when the cause cannot be specified further
    Unexpected(TextRange),
    /// UnexpectedExtra is used when there are additional tokens to the root in the tree
    UnexpectedExtra(TextRange),
    /// UnexpectedWanted is used when specific tokens are expected, but different one is found
    UnexpectedWanted(SyntaxKind, TextRange, Box<[SyntaxKind]>),
    /// UnexpectedDoubleBind is used when a pattern is bound twice
    UnexpectedDoubleBind(TextRange),
    /// UnexpectedEOF is used when the end of file is reached, while tokens are still expected
    UnexpectedEOF,
    /// UnexpectedEOFWanted is used when specific tokens are expected, but the end of file is reached
    UnexpectedEOFWanted(Box<[SyntaxKind]>),
    /// DuplicatedArgs is used when formal arguments are duplicated, e.g. `{ a, a }`
    DuplicatedArgs(TextRange, String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::Unexpected(range) => {
                write!(
                    f,
                    "error node at {}..{}",
                    usize::from(range.start()),
                    usize::from(range.end())
                )
            }
            ParseError::UnexpectedExtra(range) => {
                write!(
                    f,
                    "unexpected token at {}..{}",
                    usize::from(range.start()),
                    usize::from(range.end())
                )
            }
            ParseError::UnexpectedWanted(got, range, kinds) => write!(
                f,
                "unexpected {:?} at {}..{}, wanted any of {:?}",
                got,
                usize::from(range.start()),
                usize::from(range.end()),
                kinds
            ),
            ParseError::UnexpectedDoubleBind(range) => {
                write!(
                    f,
                    "unexpected double bind at {}..{}",
                    usize::from(range.start()),
                    usize::from(range.end())
                )
            }
            ParseError::UnexpectedEOF => write!(f, "unexpected end of file"),
            ParseError::UnexpectedEOFWanted(kinds) => {
                write!(f, "unexpected end of file, wanted any of {:?}", kinds)
            }
            ParseError::DuplicatedArgs(range, ident) => {
                write!(
                    f,
                    "argument `{}` is duplicated in {}..{}",
                    ident,
                    usize::from(range.start()),
                    usize::from(range.end())
                )
            }
        }
    }
}

impl std::error::Error for ParseError {}

struct Parser<'s, I>
where
    I: Iterator<Item = Token<'s>>,
{
    builder: GreenNodeBuilder<'static>,
    errors: Vec<ParseError>,

    trivia_buffer: Vec<Token<'s>>,
    buffer: VecDeque<Token<'s>>,
    iter: I,
    consumed: TextSize,
}
impl<'s, I> Parser<'s, I>
where
    I: Iterator<Item = Token<'s>>,
{
    fn new(iter: I) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),

            trivia_buffer: Vec::with_capacity(1),
            buffer: VecDeque::with_capacity(1),
            iter,
            consumed: TextSize::from(0),
        }
    }

    fn get_text_position(&self) -> TextSize {
        self.consumed
    }

    fn peek_raw(&mut self) -> Option<&Token<'s>> {
        if self.buffer.is_empty() {
            if let Some(token) = self.iter.next() {
                self.buffer.push_back(token);
            }
        }
        self.buffer.front()
    }
    fn drain_trivia_buffer(&mut self) {
        for (t, s) in self.trivia_buffer.drain(..) {
            self.consumed += TextSize::of(s);
            self.builder.token(NixLanguage::kind_to_raw(t), s);
        }
    }
    fn eat_trivia(&mut self) {
        self.peek();
        self.drain_trivia_buffer();
    }
    fn start_node(&mut self, kind: SyntaxKind) {
        self.eat_trivia();
        self.builder.start_node(NixLanguage::kind_to_raw(kind));
    }
    fn checkpoint(&mut self) -> Checkpoint {
        self.eat_trivia();
        self.builder.checkpoint()
    }
    fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        self.builder.start_node_at(checkpoint, NixLanguage::kind_to_raw(kind));
    }
    fn finish_node(&mut self) {
        self.builder.finish_node();
    }
    fn start_error_node(&mut self) -> TextSize {
        self.start_node(NODE_ERROR);
        self.get_text_position()
    }
    fn finish_error_node(&mut self) -> TextSize {
        self.finish_node();
        self.get_text_position()
    }
    fn bump(&mut self) {
        match self.try_next() {
            Some((token, s)) => {
                if token.is_trivia() {
                    self.trivia_buffer.push((token, s))
                } else {
                    self.drain_trivia_buffer();
                    self.manual_bump(s, token);
                }
            }
            None => self.errors.push(ParseError::UnexpectedEOF),
        }
    }
    fn try_next(&mut self) -> Option<Token<'s>> {
        self.buffer.pop_front().or_else(|| self.iter.next())
    }
    fn manual_bump(&mut self, s: &str, token: SyntaxKind) {
        self.consumed += TextSize::of(s);
        self.builder.token(NixLanguage::kind_to_raw(token), s)
    }
    fn peek_data(&mut self) -> Option<&Token<'s>> {
        while self.peek_raw().map(|&(t, _)| t.is_trivia()).unwrap_or(false) {
            self.bump();
        }
        self.peek_raw()
    }
    fn peek(&mut self) -> Option<SyntaxKind> {
        self.peek_data().map(|&(t, _)| t)
    }
    fn expect_peek_any(&mut self, allowed_slice: &[SyntaxKind]) -> Option<SyntaxKind> {
        let allowed = TokenSet::from_slice(allowed_slice);

        let next = match self.peek() {
            None => None,
            Some(kind) if allowed.contains(kind) => Some(kind),
            Some(kind) => {
                let start = self.start_error_node();
                loop {
                    self.bump();
                    if self.peek().map(|kind| allowed.contains(kind)).unwrap_or(true) {
                        break;
                    }
                }
                let end = self.finish_error_node();
                self.errors.push(ParseError::UnexpectedWanted(
                    kind,
                    TextRange::new(start, end),
                    allowed_slice.to_vec().into_boxed_slice(),
                ));

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
    fn expect_ident(&mut self) {
        if self.expect_peek_any(&[TOKEN_IDENT]).is_some() {
            self.start_node(NODE_IDENT);
            self.bump();
            self.finish_node()
        }
    }

    fn parse_dynamic(&mut self) {
        self.start_node(NODE_DYNAMIC);
        self.bump();
        while self.peek().map(|t| t != TOKEN_INTERPOL_END).unwrap_or(false) {
            self.parse_expr();
        }
        self.bump();
        self.finish_node();
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
                    self.finish_node();
                }
                // handled by expect_peek_any
                _ => break,
            }
        }
        self.expect(TOKEN_STRING_END);

        self.finish_node();
    }
    fn parse_attr(&mut self) {
        match self.peek() {
            Some(TOKEN_INTERPOL_START) => self.parse_dynamic(),
            Some(TOKEN_STRING_START) => self.parse_string(),
            _ => self.expect_ident(),
        }
    }
    fn parse_attrpath(&mut self) {
        self.start_node(NODE_ATTRPATH);
        loop {
            self.parse_attr();

            if self.peek() == Some(TOKEN_DOT) {
                self.bump();
            } else {
                break;
            }
        }
        self.finish_node();
    }
    fn parse_pattern(&mut self, bound: bool) {
        if self.peek().map(|t| t == TOKEN_CURLY_B_CLOSE).unwrap_or(true) {
            self.bump();
        } else {
            let mut args = HashMap::<&str, TextSize>::new();
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
                        let tp = self.get_text_position();
                        let mut ident_done = false;
                        if let Some((_, ident_name)) = self.peek_raw() {
                            let contains = args.contains_key(*ident_name);
                            args.insert(ident_name, tp);
                            if contains {
                                let id_str = ident_name.to_string();
                                ident_done = true;
                                self.start_error_node();
                                self.expect_ident();
                                let fin = self.finish_error_node();
                                self.errors.push(ParseError::DuplicatedArgs(
                                    TextRange::new(tp, fin),
                                    id_str,
                                ));
                            }
                        }

                        if !ident_done {
                            self.expect_ident();
                        }

                        if let Some(TOKEN_QUESTION) = self.peek() {
                            self.bump();
                            self.parse_expr();
                        }
                        self.finish_node();

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
            let start = self.get_text_position();
            self.bump();
            self.expect_ident();
            let end = self.finish_error_node();
            if bound {
                self.errors.push(ParseError::UnexpectedDoubleBind(TextRange::new(start, end)));
            }
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
                        self.finish_node();
                    }

                    loop {
                        match self.peek() {
                            Some(t) if t != TOKEN_SEMICOLON => {
                                self.parse_attr();
                            }
                            Some(_) => {
                                break;
                            }
                            None => {
                                self.errors.push(ParseError::UnexpectedEOF);
                                break;
                            }
                        }
                    }

                    self.expect(TOKEN_SEMICOLON);
                    self.finish_node();
                }
                Some(_) => {
                    self.start_node(NODE_ATTRPATH_VALUE);
                    self.parse_attrpath();
                    self.expect(TOKEN_ASSIGN);
                    self.parse_expr();
                    self.expect(TOKEN_SEMICOLON);
                    self.finish_node();
                }
            }
        }
        self.bump(); // the final close, like '}'
    }
    fn parse_val(&mut self) -> Checkpoint {
        let peek = match self.peek() {
            Some(it) => it,
            None => {
                self.errors.push(ParseError::UnexpectedEOF);
                // NB: we don't use `self.checkpoint()` here in order to avoid
                // eating the whitespace. The actual checkpoint doesn't matter
                // in this case and, ideally, should be returning `None`, but
                // that makes code slightly more complex for little real
                // benefit.
                return self.builder.checkpoint();
            }
        };
        let checkpoint = self.checkpoint();
        match peek {
            TOKEN_PAREN_OPEN => {
                self.start_node(NODE_PAREN);
                self.bump();
                self.parse_expr();
                self.bump();
                self.finish_node();
            }
            TOKEN_REC => {
                self.start_node(NODE_ATTR_SET);
                self.bump();
                self.expect(TOKEN_CURLY_B_OPEN);
                self.parse_set(TOKEN_CURLY_B_CLOSE);
                self.finish_node();
            }
            TOKEN_CURLY_B_OPEN => {
                // Do a lookahead:
                let mut peek = [None, None];
                for i in &mut peek {
                    let mut token;
                    *i = loop {
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
                        self.finish_node();

                        self.expect(TOKEN_COLON);
                        self.parse_expr();

                        self.finish_node();
                    }
                    _ => {
                        // This looks like a set
                        self.start_node(NODE_ATTR_SET);
                        self.bump();
                        self.parse_set(TOKEN_CURLY_B_CLOSE);
                        self.finish_node();
                    }
                }
            }
            TOKEN_SQUARE_B_OPEN => {
                self.start_node(NODE_LIST);
                self.bump();
                while self.peek().map(|t| t != TOKEN_SQUARE_B_CLOSE).unwrap_or(false) {
                    self.parse_val();
                }
                self.bump();
                self.finish_node();
            }
            TOKEN_STRING_START => self.parse_string(),
            TOKEN_PATH => {
                let next = self.try_next();
                if let Some((token, s)) = next {
                    let is_complex_path = self.peek().map_or(false, |t| t == TOKEN_INTERPOL_START);
                    self.builder.start_node(NixLanguage::kind_to_raw(if is_complex_path {
                        NODE_PATH_WITH_INTERPOL
                    } else {
                        NODE_LITERAL
                    }));
                    self.manual_bump(s, token);
                    if is_complex_path {
                        loop {
                            match self.peek_raw().map(|(t, _)| t) {
                                Some(TOKEN_PATH) => self.bump(),
                                Some(TOKEN_INTERPOL_START) => {
                                    self.start_node(NODE_STRING_INTERPOL);
                                    self.bump();
                                    self.parse_expr();
                                    self.expect(TOKEN_INTERPOL_END);
                                    self.finish_node();
                                }
                                _ => break,
                            }
                        }
                    }
                    self.finish_node();
                } else {
                    self.errors.push(ParseError::UnexpectedEOF);
                }
            }
            t if t.is_literal() => {
                self.start_node(NODE_LITERAL);
                self.bump();
                self.finish_node();
            }
            TOKEN_IDENT => {
                self.expect_ident();

                match self.peek() {
                    Some(TOKEN_COLON) => {
                        self.start_node_at(checkpoint, NODE_LAMBDA);
                        self.bump();
                        self.parse_expr();
                        self.finish_node();
                    }
                    Some(TOKEN_AT) => {
                        self.start_node_at(checkpoint, NODE_LAMBDA);
                        self.start_node_at(checkpoint, NODE_PATTERN);
                        self.start_node_at(checkpoint, NODE_PAT_BIND);
                        self.bump();
                        self.finish_node(); // PatBind

                        self.expect(TOKEN_CURLY_B_OPEN);
                        self.parse_pattern(true);
                        self.finish_node(); // Pattern

                        self.expect(TOKEN_COLON);
                        self.parse_expr();
                        self.finish_node(); // Lambda
                    }
                    _ => (),
                }
            }
            kind => {
                let start = self.start_error_node();
                self.bump();
                let end = self.finish_error_node();
                self.errors.push(ParseError::UnexpectedWanted(
                    kind,
                    TextRange::new(start, end),
                    [
                        TOKEN_PAREN_OPEN,
                        TOKEN_REC,
                        TOKEN_CURLY_B_OPEN,
                        TOKEN_SQUARE_B_OPEN,
                        TOKEN_STRING_START,
                        TOKEN_IDENT,
                    ]
                    .to_vec()
                    .into_boxed_slice(),
                ));
            }
        };

        if self.peek() == Some(TOKEN_DOT) {
            self.start_node_at(checkpoint, NODE_SELECT);
            self.bump();
            self.parse_attrpath();
            if self.peek() == Some(TOKEN_OR) {
                self.bump();
                self.parse_val();
            }
            self.finish_node();

        // This seems weird, but it matches Nix's behavior.
        // If there is no "." but a "or" immediately followed a primary expression,
        // we construct a application node, with "or" parsed as an identifier,
        // ignoring the associativity of ancestor applications.
        // Eg.
        // "a b or c" => "((a (b or)) c)"
        // "or a" => fail
        } else if self.peek() == Some(TOKEN_OR) {
            self.start_node_at(checkpoint, NODE_APPLY);
            self.start_node(NODE_IDENT);
            let (_, s) = self.try_next().unwrap();
            self.manual_bump(s, TOKEN_IDENT);
            self.finish_node();
            self.finish_node();
        }

        checkpoint
    }
    fn parse_fn(&mut self) -> Checkpoint {
        let checkpoint = self.parse_val();

        while self.peek().map(|t| t.is_fn_arg()).unwrap_or(false) {
            self.start_node_at(checkpoint, NODE_APPLY);
            self.parse_val();
            self.finish_node();
        }
        checkpoint
    }
    fn parse_negate(&mut self) -> Checkpoint {
        if self.peek() == Some(TOKEN_SUB) {
            let checkpoint = self.checkpoint();
            self.start_node(NODE_UNARY_OP);
            self.bump();
            self.parse_negate();
            self.finish_node();
            checkpoint
        } else {
            self.parse_fn()
        }
    }
    fn handle_operation_left(
        &mut self,
        once: bool,
        next: fn(&mut Self) -> Checkpoint,
        ops: &[SyntaxKind],
    ) -> Checkpoint {
        let checkpoint = next(self);
        while self.peek().map(|t| ops.contains(&t)).unwrap_or(false) {
            self.start_node_at(checkpoint, NODE_BIN_OP);
            self.bump();
            next(self);
            self.finish_node();
            if once {
                break;
            }
        }
        checkpoint
    }
    fn handle_operation_right(
        &mut self,
        next: fn(&mut Self) -> Checkpoint,
        ops: &[SyntaxKind],
    ) -> Checkpoint {
        let checkpoint = next(self);
        if self.peek().map(|t| ops.contains(&t)).unwrap_or(false) {
            self.start_node_at(checkpoint, NODE_BIN_OP);
            self.bump();
            self.handle_operation_right(next, ops);
            self.finish_node();
        }
        checkpoint
    }
    fn parse_hasattr(&mut self) -> Checkpoint {
        let checkpoint = self.parse_negate();
        while self.peek().map(|t| t == TOKEN_QUESTION).unwrap_or(false) {
            self.start_node_at(checkpoint, NODE_HAS_ATTR);
            self.bump();
            self.parse_attrpath();
            self.finish_node();
        }
        checkpoint
    }
    fn parse_concat(&mut self) -> Checkpoint {
        self.handle_operation_right(Self::parse_hasattr, &[TOKEN_CONCAT])
    }
    fn parse_mul(&mut self) -> Checkpoint {
        self.handle_operation_left(false, Self::parse_concat, &[TOKEN_MUL, TOKEN_DIV])
    }
    fn parse_add(&mut self) -> Checkpoint {
        self.handle_operation_left(false, Self::parse_mul, &[TOKEN_ADD, TOKEN_SUB])
    }
    fn parse_invert(&mut self) -> Checkpoint {
        if self.peek() == Some(TOKEN_INVERT) {
            let checkpoint = self.checkpoint();
            self.start_node(NODE_UNARY_OP);
            self.bump();
            self.parse_invert();
            self.finish_node();
            checkpoint
        } else {
            self.parse_add()
        }
    }
    fn parse_merge(&mut self) -> Checkpoint {
        self.handle_operation_right(Self::parse_invert, &[TOKEN_UPDATE])
    }
    fn parse_compare(&mut self) -> Checkpoint {
        self.handle_operation_left(
            true,
            Self::parse_merge,
            &[TOKEN_LESS, TOKEN_LESS_OR_EQ, TOKEN_MORE, TOKEN_MORE_OR_EQ],
        )
    }
    fn parse_equal(&mut self) -> Checkpoint {
        self.handle_operation_left(true, Self::parse_compare, &[TOKEN_EQUAL, TOKEN_NOT_EQUAL])
    }
    fn parse_and(&mut self) -> Checkpoint {
        self.handle_operation_left(false, Self::parse_equal, &[TOKEN_AND_AND])
    }
    fn parse_or(&mut self) -> Checkpoint {
        self.handle_operation_left(false, Self::parse_and, &[TOKEN_OR_OR])
    }
    fn parse_implication(&mut self) -> Checkpoint {
        self.handle_operation_right(Self::parse_or, &[TOKEN_IMPLICATION])
    }
    #[inline(always)]
    fn parse_math(&mut self) -> Checkpoint {
        // Always point this to the lowest-level math function there is
        self.parse_implication()
    }
    /// Parse Nix code into an AST
    pub fn parse_expr(&mut self) -> Checkpoint {
        match self.peek() {
            Some(TOKEN_LET) => {
                let checkpoint = self.checkpoint();
                self.bump();

                if self.peek() == Some(TOKEN_CURLY_B_OPEN) {
                    self.start_node_at(checkpoint, NODE_LEGACY_LET);
                    self.bump();
                    self.parse_set(TOKEN_CURLY_B_CLOSE);
                    self.finish_node();
                } else {
                    self.start_node_at(checkpoint, NODE_LET_IN);
                    self.parse_set(TOKEN_IN);
                    self.parse_expr();
                    self.finish_node();
                }
                checkpoint
            }
            Some(TOKEN_WITH) => {
                let checkpoint = self.checkpoint();
                self.start_node(NODE_WITH);
                self.bump();
                self.parse_expr();
                self.expect(TOKEN_SEMICOLON);
                self.parse_expr();
                self.finish_node();
                checkpoint
            }
            Some(TOKEN_IF) => {
                let checkpoint = self.checkpoint();
                self.start_node(NODE_IF_ELSE);
                self.bump();
                self.parse_expr();
                self.expect(TOKEN_THEN);
                self.parse_expr();
                self.expect(TOKEN_ELSE);
                self.parse_expr();
                self.finish_node();
                checkpoint
            }
            Some(TOKEN_ASSERT) => {
                let checkpoint = self.checkpoint();
                self.start_node(NODE_ASSERT);
                self.bump();
                self.parse_expr();
                self.expect(TOKEN_SEMICOLON);
                self.parse_expr();
                self.finish_node();
                checkpoint
            }
            _ => self.parse_math(),
        }
    }
}

/// Parse tokens into an AST
pub fn parse<'s, I>(iter: I) -> (GreenNode, Vec<ParseError>)
where
    I: Iterator<Item = Token<'s>>,
{
    let mut parser = Parser::new(iter);
    parser.builder.start_node(NixLanguage::kind_to_raw(NODE_ROOT));
    parser.parse_expr();
    parser.eat_trivia();
    if parser.peek().is_some() {
        let start = parser.start_error_node();
        while parser.peek().is_some() {
            parser.bump();
        }
        let end = parser.finish_error_node();
        parser.errors.push(ParseError::UnexpectedExtra(TextRange::new(start, end)));
        parser.eat_trivia();
    }
    parser.builder.finish_node();
    (parser.builder.finish(), parser.errors)
}

#[cfg(test)]
mod tests {
    use crate::Root;

    use super::*;

    use std::{env, ffi::OsStr, fmt::Write, fs, io::Write as IoWrite, path::PathBuf};

    //     #[test]
    //     fn whitespace_attachment_for_incomplete_code1() {
    //         let code = "{
    //   traceIf =
    //     # predicate to check
    //     pred:

    // ";
    //         let ast = crate::parse(code);
    //         let actual = format!("{}", ast.root().dump());
    //         // The core thing we want to check here is that `\n\n` belongs to the
    //         // root node, and not to some incomplete inner node.
    //         assert_eq!(
    //             actual.trim(),
    //             r##"
    // NODE_ROOT 0..50 {
    //   NODE_ATTR_SET 0..48 {
    //     TOKEN_CURLY_B_OPEN("{") 0..1
    //     TOKEN_WHITESPACE("\n  ") 1..4
    //     NODE_KEY_VALUE 4..48 {
    //       NODE_KEY 4..11 {
    //         NODE_IDENT 4..11 {
    //           TOKEN_IDENT("traceIf") 4..11
    //         }
    //       }
    //       TOKEN_WHITESPACE(" ") 11..12
    //       TOKEN_ASSIGN("=") 12..13
    //       TOKEN_WHITESPACE("\n    ") 13..18
    //       TOKEN_COMMENT("# predicate to check") 18..38
    //       TOKEN_WHITESPACE("\n    ") 38..43
    //       NODE_LAMBDA 43..48 {
    //         NODE_IDENT 43..47 {
    //           TOKEN_IDENT("pred") 43..47
    //         }
    //         TOKEN_COLON(":") 47..48
    //       }
    //     }
    //   }
    //   TOKEN_WHITESPACE("\n\n") 48..50
    // }"##
    //             .trim()
    //         );
    //     }

    //     #[test]
    //     fn whitespace_attachment_for_incomplete_code2() {
    //         let code = "{} =
    // ";
    //         let ast = crate::parse(code);
    //         let actual = format!("{}", ast.root().dump());
    //         assert_eq!(
    //             actual.trim(),
    //             r##"
    // NODE_ROOT 0..5 {
    //   NODE_ATTR_SET 0..2 {
    //     TOKEN_CURLY_B_OPEN("{") 0..1
    //     TOKEN_CURLY_B_CLOSE("}") 1..2
    //   }
    //   TOKEN_WHITESPACE(" ") 2..3
    //   NODE_ERROR 3..4 {
    //     TOKEN_ASSIGN("=") 3..4
    //   }
    //   TOKEN_WHITESPACE("\n") 4..5
    // }"##
    //             .trim()
    //         );
    //     }

    fn test_dir(name: &str) {
        let should_update = env::var("UPDATE_TESTS").map(|s| s == "1").unwrap_or(false);
        let dir: PathBuf = ["test_data", name].iter().collect();

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
            let parse = Root::parse(&code);
            path.set_extension("expect");
            if !path.exists() {
                fs::File::create(&path).expect("Failed to create .expect file");
            }
            let expected = fs::read_to_string(&path).unwrap();

            let mut actual = String::new();
            for error in parse.errors() {
                writeln!(actual, "error: {}", error).unwrap();
            }
            writeln!(actual, "{:#?}", parse.syntax()).unwrap();
            if should_update {
                let mut file =
                    fs::OpenOptions::new().write(true).truncate(true).open(&path).unwrap();
                write!(file, "{}", actual).unwrap();
                continue;
            }

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

    #[rustfmt::skip]
    mod dir_tests {
        use super::test_dir;

        #[test]
        fn general() {
            test_dir("general");
        }

        #[test]
        fn parser() {
            for dir in std::fs::read_dir("test_data/parser").unwrap() {
                let dir = dir.unwrap();
                let file_name = dir.file_name().into_string().unwrap();
                println!("testing: {}", file_name);
                test_dir(&format!("parser/{}", file_name));
            }
        }
    }
}
