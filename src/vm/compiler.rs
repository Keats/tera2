//! AST -> bytecode
use crate::parsing::ast::{BinaryOperator, Expression, Node, UnaryOperator};
use crate::value::Value;
use crate::vm::instructions::{Chunk, Instruction};
use std::collections::HashMap;

// TODO: a bit weird to have the compiler in the vm part?
pub(crate) struct Compiler {
    pub(crate) chunk: Chunk,
    body: String,
    macro_namespaces: Vec<String>,
    macro_names: Vec<Vec<String>>,
    /// How many bytes of raw content we've seen
    bytes_size: usize,
}

impl Compiler {
    pub(crate) fn new(name: &str, body: &str) -> Self {
        Self {
            chunk: Chunk::new(name),
            macro_namespaces: Vec::with_capacity(8),
            macro_names: Vec::with_capacity(8),
            body: body.to_string(),
            bytes_size: 0,
        }
    }

    fn compile_kwargs(&mut self, kwargs: HashMap<String, Expression>) {
        let num_args = kwargs.len();
        // TODO: push a single instr for all keys?
        for (key, value) in kwargs {
            self.chunk.add(Instruction::LoadConst(Value::from(key)));
            self.compile_expr(value);
        }
        self.chunk.add(Instruction::BuildKwargs(num_args));
    }

    fn compile_expr(&mut self, expr: Expression) {
        match expr {
            Expression::Const(e) => {
                let (val, _) = e.into_parts();
                self.chunk.add(Instruction::LoadConst(val));
            }
            Expression::Array(e) => {
                let (array, _) = e.into_parts();
                let num_elems = array.items.len();
                for val in array.items {
                    self.compile_expr(val);
                }
                self.chunk.add(Instruction::BuildList(num_elems));
            }
            Expression::Var(e) => {
                let (val, _) = e.into_parts();
                self.chunk.add(Instruction::LoadName(val.name));
            }
            Expression::GetAttr(e) => {
                let (attr, _) = e.into_parts();
                self.compile_expr(attr.expr);
                self.chunk.add(Instruction::LoadAttr(attr.name));
            }
            Expression::GetItem(e) => {
                let (item, _) = e.into_parts();
                self.compile_expr(item.expr);
                self.compile_expr(item.sub_expr);
                self.chunk.add(Instruction::BinarySubscript);
            }
            Expression::Filter(e) => {
                let (filter, _) = e.into_parts();
                self.compile_expr(filter.expr);
                self.compile_kwargs(filter.kwargs);
                self.chunk.add(Instruction::ApplyFilter(filter.name));
            }
            Expression::Test(e) => {
                let (test, _) = e.into_parts();
                self.compile_expr(test.expr);
                // TODO: use kwargs instead of Vec<args>
            }
            Expression::MacroCall(e) => {
                let (macro_call, _) = e.into_parts();
                self.compile_kwargs(macro_call.kwargs);
                let namespace_idx = if let Some(idx) = self
                    .macro_namespaces
                    .iter()
                    .position(|x| x == &macro_call.namespace)
                {
                    idx
                } else {
                    let len = self.macro_namespaces.len();
                    self.macro_namespaces.push(macro_call.namespace);
                    self.macro_names.push(Vec::with_capacity(8));
                    len
                };
                let name_idx = if let Some(idx) = self.macro_names[namespace_idx]
                    .iter()
                    .position(|x| x == &macro_call.name)
                {
                    idx
                } else {
                    let len = self.macro_names[namespace_idx].len();
                    self.macro_names
                        .get_mut(namespace_idx)
                        .unwrap()
                        .push(macro_call.name);
                    len
                };
                self.chunk
                    .add(Instruction::CallMacro(namespace_idx, name_idx));
            }
            Expression::FunctionCall(e) => {
                let (func, _) = e.into_parts();
                self.compile_kwargs(func.kwargs);
                self.chunk.add(Instruction::CallFunction(func.name));
            }
            Expression::UnaryOperation(e) => {
                let (op, _) = e.into_parts();
                self.compile_expr(op.expr);
                match op.op {
                    UnaryOperator::Not => self.chunk.add(Instruction::Not),
                    UnaryOperator::Minus => self.chunk.add(Instruction::Negative),
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
                self.bytes_size += text.as_bytes().len();
                self.chunk.add(Instruction::WriteText(text));
            }
            Node::Expression(expr) => {
                self.compile_expr(expr);
                self.chunk.add(Instruction::WriteTop);
            }
            Node::Set(s) => {
                self.compile_expr(s.value);
                let instr = if s.global {
                    Instruction::SetGlobal(s.name)
                } else {
                    Instruction::Set(s.name)
                };
                self.chunk.add(instr);
            }
            Node::Include(i) => {
                self.chunk.add(Instruction::Include(i.name));
            }
            Node::Block(_) => {}
            Node::ForLoop(_) => {}
            Node::If(_) => {}
            Node::FilterSection(f) => {}
        }
    }

    pub(crate) fn test_compile_tmp(&mut self, nodes: Vec<Node>) {
        println!("{:?}", nodes);
        for node in nodes {
            self.compile_node(node);
        }
    }
}
