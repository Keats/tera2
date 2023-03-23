//! AST -> bytecode
use crate::parsing::ast::{BinaryOperator, Expression, Node, UnaryOperator};
use crate::vm::instructions::{Chunk, Instruction};

// TODO: a bit weird to have the compiler in the vm part?
pub(crate) struct Compiler {
    pub(crate) chunk: Chunk,
    body: String,
    /// How many bytes of raw content we've seen
    bytes_size: usize,
}

impl Compiler {
    pub(crate) fn new(name: &str, body: &str) -> Self {
        Self {
            chunk: Chunk::new(name),
            body: body.to_string(),
            bytes_size: 0,
        }
    }

    fn compile_expr(&mut self, expr: Expression) {
        match expr {
            Expression::Const(e) => {
                let (val, _) = e.into_parts();
                self.chunk.add_instruction(Instruction::LoadConst(val));
            }
            Expression::Array(_) => {}
            Expression::Var(e) => {
                let (val, _) = e.into_parts();
                self.chunk.add_instruction(Instruction::LoadVar(val.name));
            }
            Expression::GetAttr(_) => {}
            Expression::GetItem(_) => {}
            Expression::Test(_) => {}
            Expression::MacroCall(_) => {}
            Expression::Filter(_) => {}
            Expression::FunctionCall(_) => {}
            Expression::UnaryOperation(e) => {
                let (op, _) = e.into_parts();
                self.compile_expr(op.expr);
                match op.op {
                    UnaryOperator::Not => self.chunk.add_instruction(Instruction::Not),
                    UnaryOperator::Minus => self.chunk.add_instruction(Instruction::Negative),
                };
            }
            Expression::BinaryOperation(e) => {
                let (op, span) = e.into_parts();
                // TODO: implement constant folding for arithmetics, string concat
                // Value::constant_fold(self, other) -> Option<Value>?
                // need to pass the op as well...^
                self.compile_expr(op.left);
                self.compile_expr(op.right);
                let instr = match op.op {
                    BinaryOperator::Mul => Instruction::Mul,
                    BinaryOperator::Div => Instruction::Div,
                    BinaryOperator::Mod => Instruction::Mod,
                    BinaryOperator::Plus => Instruction::Plus,
                    BinaryOperator::Minus => Instruction::Minus,
                    BinaryOperator::FloorDiv => Instruction::FloorDiv,
                    BinaryOperator::Power => Instruction::Power,
                    BinaryOperator::LessThan => Instruction::LessThan,
                    BinaryOperator::GreaterThan => Instruction::GreaterThan,
                    BinaryOperator::LessThanOrEqual => Instruction::LessThanOrEqual,
                    BinaryOperator::GreaterThanOrEqual => Instruction::GreaterThanOrEqual,
                    BinaryOperator::Equal => Instruction::Equal,
                    BinaryOperator::NotEqual => Instruction::NotEqual,
                    BinaryOperator::StrConcat => Instruction::StrConcat,
                    BinaryOperator::In => Instruction::In,
                    BinaryOperator::And => todo!("what do we generate"),
                    BinaryOperator::Or => todo!("what do we generate"),
                    // These are not really binops and we already switched them to separate AST
                    // nodes in the parser
                    BinaryOperator::Is | BinaryOperator::Pipe => unreachable!(),
                };
                self.chunk.add_instruction_with_span(instr, span);
            }
        }
    }

    pub fn compile_node(&mut self, node: Node) {
        match node {
            Node::Content(text) => {
                self.chunk.add_instruction(Instruction::WriteText(text));
            }
            Node::Expression(expr) => {
                self.compile_expr(expr);
                self.chunk.add_instruction(Instruction::WriteTop);
            }
            Node::Set(s) => {
                self.compile_expr(s.value);
                let instr = if s.global {
                    Instruction::SetGlobal(s.name)
                } else {
                    Instruction::Set(s.name)
                };
                self.chunk.add_instruction(instr);
            }
            Node::Include(i) => {
                self.chunk.add_instruction(Instruction::Include(i.name));
            }
            Node::Block(_) => {}
            Node::ForLoop(_) => {}
            Node::If(_) => {}
            Node::FilterSection(_) => {}
        }
    }

    pub(crate) fn test_compile_tmp(&mut self, nodes: Vec<Node>) {
        println!("{:?}", nodes);
        for node in nodes {
            self.compile_node(node);
        }
    }
}
