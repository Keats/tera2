#![allow(dead_code)]

use std::ops::Range;

pub mod ast;
mod errors;
mod lexer;
pub mod parser;

#[derive(Clone, Debug, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Range<usize>,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Range<usize>) -> Self {
        Self { node, span }
    }
}
