use serde_derive::Serialize;

mod compiler;
mod inheritance;
mod lexer;
mod macros;
mod parser;
mod rendering;
mod whitespace;

#[allow(dead_code)]
#[derive(Debug, Serialize)]
pub struct NestedObject {
    pub label: String,
    pub parent: Option<Box<NestedObject>>,
    pub numbers: Vec<usize>,
}
