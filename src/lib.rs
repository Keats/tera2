#![allow(dead_code)]

use std::ops::Range;

pub mod ast;
mod errors;
mod lexer;
pub mod parser;

#[derive(Clone, Debug, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub range: Range<usize>,
}

impl<T> Spanned<T> {
    pub fn new(node: T, range: Range<usize>) -> Self {
        Self { node, range }
    }
}
