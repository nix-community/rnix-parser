use std::mem;

use rnix::{parser::*, tokenizer::{Meta, Span, Trivia}};

fn main() {
    // If these get too large, it's easier to get a stack overflow :-(
    println!("AST total size: {}", mem::size_of::<AST>());
    println!("AST node size: {}", mem::size_of::<ASTNode>());
    println!("AST enum size: {}", mem::size_of::<ASTType>());
    println!("Metadata size: {}", mem::size_of::<Meta>());
    println!("Trivia size: {}", mem::size_of::<Trivia>());
    println!("Span size: {}", mem::size_of::<Span>());
}
