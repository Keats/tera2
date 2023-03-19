//! AST -> bytecode
use crate::parsing::ast::{Expression, Node};
use crate::vm::instructions::{Chunk, Instruction};

pub(crate) struct Compiler {
    chunk: Chunk,
    body: String,
    /// How many bytes of raw content we've seen
    bytes_size: usize,
}

impl Compiler {
    pub fn new(name: &str, body: &str) -> Self {
        Self {
            chunk: Chunk::new(name),
            body: body.to_string(),
            bytes_size: 0,
        }
    }

    fn compile_expr(&mut self, expr: Expression) {
        match expr {
            Expression::Const(_) => {}
            Expression::Array(_) => {}
            Expression::Var(_) => {}
            Expression::GetAttr(_) => {}
            Expression::GetItem(_) => {}
            Expression::Test(_) => {}
            Expression::MacroCall(_) => {}
            Expression::FunctionCall(_) => {}
            Expression::UnaryOperation(_) => {}
            Expression::BinaryOperation(_) => {}
        }
    }

    pub fn compile_node(&mut self, node: Node) {
        match node {
            Node::Content(text) => {
                self.chunk.add_instruction(Instruction::Content(text));
            }
            Node::Expression(_) => {}
            Node::Set(_) => {}
            Node::Include(_) => {}
            Node::Block(_) => {}
            Node::ForLoop(_) => {}
            Node::If(_) => {}
            Node::FilterSection(_) => {}
        }
    }
}
