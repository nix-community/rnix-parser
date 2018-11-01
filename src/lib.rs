#[macro_use]
extern crate failure;

pub mod parser;
pub mod tokenizer;
pub mod value;

// only `impl`s, no need to expose the module
//mod display;

use self::{
    parser::Node,
    tokenizer::Tokenizer
};

/// A convenience function for first tokenizing and then parsing given input
pub fn parse(input: &str) -> Node {
    parser::parse(Tokenizer::new(input))
}
