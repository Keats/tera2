pub mod ast;
pub mod compiler;
mod instructions;
pub mod lexer;
pub mod parser;

pub(crate) use compiler::Compiler;
