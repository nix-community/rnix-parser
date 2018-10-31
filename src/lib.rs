#![feature(try_blocks)]

#[macro_use]
extern crate failure;

//pub mod parser;
pub mod tokenizer;
//pub mod value;

// only `impl`s, no need to expose the module
//mod display;

//use self::{
//    parser::ParseError,
//    tokenizer::{Meta, Span, Token, TokenizeError}
//};
//
///// An error during either tokenizing or parsing
//#[derive(Clone, Debug, Fail, PartialEq)]
//pub enum Error {
//    #[fail(display = "parse error: {}", _1)]
//    ParseError(Option<Span>, #[cause] ParseError),
//    #[fail(display = "parse error: {}", _1)]
//    TokenizeError(Span, #[cause] TokenizeError)
//}
//impl From<(Span, TokenizeError)> for Error {
//    fn from(err: (Span, TokenizeError)) -> Self {
//        let (span, err) = err;
//        Error::TokenizeError(span, err)
//    }
//}
//impl From<(Option<Span>, ParseError)> for Error {
//    fn from(err: (Option<Span>, ParseError)) -> Self {
//        let (span, err) = err;
//        Error::ParseError(span, err)
//    }
//}
//
///// A convenience function for first tokenizing and then parsing.
/////
///// Note: This is not lazy. It tokenizes first and parses later.
///// It is perhaps more efficient to tokenize one at a time before parsing, but
///// by how much is unclear. If you need every ounce of speed you can get, rnix'
///// other functions allow you not only to tokenize/parse lazily, but also do it
///// in paralell.
//pub fn parse(input: &str) -> Result<parser::AST<'static>, Error> {
//    let tokens: Result<Vec<(Meta, Token)>, (Span, TokenizeError)> = tokenizer::tokenize(input).collect();
//    parser::parse(tokens?).map_err(Error::from)
//}
