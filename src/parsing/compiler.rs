//! AST -> bytecode
use crate::parsing::ast::{BinaryOperator, Block, Expression, Node, UnaryOperator};
use crate::parsing::instructions::{Chunk, Instruction};
use crate::value::Value;
use std::collections::HashMap;

/// We need to handle some pc jumps but we only know to where after we are done processing it
#[derive(Debug)]
enum ProcessingBody {
    /// if/elif
    Branch(usize),
    /// and/or
    ShortCircuit(Vec<usize>),
    Loop(usize),
}

// TODO: a bit weird to have the compiler in the vm folder?
pub(crate) struct Compiler {
    // TODO: keep a reference to the body to avoid copying it for blocks?
    pub(crate) chunk: Chunk,
    body: String,
    processing_bodies: Vec<ProcessingBody>,
    pub(crate) blocks: HashMap<String, Chunk>,
    macro_namespaces: Vec<String>,
    macro_names: Vec<Vec<String>>,
    /// How many bytes of raw content we've seen
    bytes_size: usize,
}

impl Compiler {
    pub(crate) fn new(name: &str, body: &str) -> Self {
        Self {
            chunk: Chunk::new(name),
            processing_bodies: Vec::new(),
            macro_namespaces: Vec::new(),
            macro_names: Vec::new(),
            blocks: HashMap::new(),
            body: body.to_string(),
            bytes_size: 0,
        }
    }

    fn compile_kwargs(&mut self, kwargs: HashMap<String, Expression>) {
        let num_args = kwargs.len();
        // TODO: push a single instr for all keys as a Vec<String> like Python? bench first
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
                let num_args = test.args.len();
                for arg in test.args {
                    self.compile_expr(arg);
                }
                self.chunk.add(Instruction::BuildList(num_args));
                self.chunk.add(Instruction::RunTest(test.name));
            }
            Expression::MacroCall(e) => {
                let (macro_call, _) = e.into_parts();
                self.compile_kwargs(macro_call.kwargs);
                // TODO: it's because we can't pass anything to the enum to make it 32 bytes
                // eg (String, String), (String, u8) etc don't fit
                let namespace_idx = if let Some(idx) = self
                    .macro_namespaces
                    .iter()
                    .position(|x| x == &macro_call.namespace)
                {
                    idx
                } else {
                    let len = self.macro_namespaces.len();
                    self.macro_namespaces.push(macro_call.namespace);
                    self.macro_names.push(Vec::with_capacity(4));
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
                    BinaryOperator::And | BinaryOperator::Or => {
                        self.processing_bodies
                            .push(ProcessingBody::ShortCircuit(vec![]));
                        self.compile_expr(op.left);
                        if let Some(ProcessingBody::ShortCircuit(ref mut instr)) =
                            self.processing_bodies.last_mut()
                        {
                            instr.push(self.chunk.add(if op.op == BinaryOperator::And {
                                Instruction::JumpIfFalseOrPop(0)
                            } else {
                                Instruction::JumpIfTrueOrPop(0)
                            }) as usize);
                        } else {
                            unreachable!();
                        }
                        self.compile_expr(op.right);
                        let end = self.chunk.len();
                        if let Some(ProcessingBody::ShortCircuit(instr)) =
                            self.processing_bodies.pop()
                        {
                            for i in instr {
                                match self.chunk.get_mut(i) {
                                    Some(Instruction::JumpIfFalseOrPop(ref mut target))
                                    | Some(Instruction::JumpIfTrueOrPop(ref mut target)) => {
                                        *target = end;
                                    }
                                    _ => {}
                                }
                            }
                        } else {
                            unreachable!()
                        }
                        return;
                    }
                    // These are not really binops and we already switched them to separate AST
                    // nodes in the parser so we are not going to have them here
                    BinaryOperator::Is | BinaryOperator::Pipe => unreachable!(),
                };
                // TODO: implement constant folding for arithmetics, string concat
                // Value::constant_fold(self, other) -> Option<Value>?
                // need to pass the op as well...^
                self.compile_expr(op.left);
                self.compile_expr(op.right);
                self.chunk.add_instruction_with_span(instr, span);
            }
        }
    }

    fn compile_block(&mut self, block: Block) {
        let mut compiler = Compiler::new(&self.chunk.name, &self.body);
        for node in block.body {
            compiler.compile_node(node);
        }
        self.bytes_size += compiler.bytes_size;
        self.blocks.extend(compiler.blocks.into_iter());
        self.blocks.insert(block.name.clone(), compiler.chunk);
        self.chunk.add(Instruction::CallBlock(block.name));
    }

    fn end_branch(&mut self, idx: usize) {
        match self.processing_bodies.pop() {
            Some(ProcessingBody::Branch(instr)) => match self.chunk.get_mut(instr) {
                Some(Instruction::Jump(ref mut target))
                | Some(Instruction::PopJumpIfFalse(ref mut target)) => {
                    *target = idx;
                }
                _ => {}
            },
            _ => unreachable!(),
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
            Node::Block(b) => {
                self.compile_block(b);
            }
            Node::ForLoop(forloop) => {
                self.compile_expr(forloop.target);
                self.chunk
                    .add(Instruction::StartIterate(forloop.key.is_some()));
                let start_idx = self.chunk.add(Instruction::Iterate(0)) as usize;
                self.processing_bodies.push(ProcessingBody::Loop(start_idx));

                // TODO: use UNPACK_SEQUENCE like python instead?
                if let Some(key_var) = forloop.key {
                    self.chunk.add(Instruction::StoreLocal(key_var));
                }
                self.chunk.add(Instruction::StoreLocal(forloop.value));

                for node in forloop.body {
                    self.compile_node(node);
                }

                match self.processing_bodies.pop() {
                    Some(ProcessingBody::Loop(start_idx)) => {
                        self.chunk.add(Instruction::Jump(start_idx));
                        let loop_end = self.chunk.len();

                        self.chunk.add(Instruction::PopFrame);
                        // TODO: handle else by pushing something
                        if let Some(Instruction::Iterate(ref mut jump_target)) =
                            self.chunk.get_mut(start_idx)
                        {
                            *jump_target = loop_end;
                        } else {
                            unreachable!();
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Node::If(i) => {
                for (expr, body) in i.conditions {
                    self.compile_expr(expr);
                    let idx = self.chunk.add(Instruction::PopJumpIfFalse(0)) as usize;
                    self.processing_bodies.push(ProcessingBody::Branch(idx));
                    for node in body {
                        self.compile_node(node);
                    }
                    self.end_branch(self.chunk.len());
                }

                if let Some(else_body) = i.else_body {
                    let idx = self.chunk.add(Instruction::Jump(0)) as usize;
                    self.processing_bodies.push(ProcessingBody::Branch(idx));
                    for node in else_body {
                        self.compile_node(node);
                    }

                    self.end_branch(self.chunk.len());
                }
            }
            Node::FilterSection(f) => {
                self.chunk.add(Instruction::Capture);
                for node in f.body {
                    self.compile_node(node);
                }
                self.chunk.add(Instruction::EndCapture);
                self.compile_kwargs(f.kwargs);
                self.chunk
                    .add(Instruction::ApplyFilter(f.name.into_parts().0));
            }
        }
    }

    pub fn compile(&mut self, nodes: Vec<Node>) {
        for node in nodes {
            self.compile_node(node);
        }
    }
}
