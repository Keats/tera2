//! AST -> bytecode
use crate::parsing::ast::{BinaryOperator, Block, Expression, Node, UnaryOperator};
use crate::parsing::instructions::{Chunk, Instruction};
use crate::value::Value;
use crate::HashMap;

/// We need to handle some pc jumps but we only know to where after we are done processing it
#[derive(Debug)]
enum ProcessingBody {
    /// if/elif
    Branch(usize),
    /// and/or
    ShortCircuit(Vec<usize>),
    Loop(usize),
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompiledMacroDefinition {
    pub name: String,
    pub kwargs: HashMap<String, Option<Value>>,
    pub chunk: Chunk,
}

pub(crate) struct Compiler<'s> {
    pub(crate) chunk: Chunk,
    source: &'s str,
    processing_bodies: Vec<ProcessingBody>,
    pub(crate) blocks: HashMap<String, Chunk>,
    // (namespace, name)
    pub(crate) macro_calls: Vec<(String, String)>,
    pub(crate) raw_content_num_bytes: usize,
}

impl<'s> Compiler<'s> {
    pub(crate) fn new(name: &str, source: &'s str) -> Self {
        Self {
            chunk: Chunk::new(name),
            processing_bodies: Vec::new(),
            macro_calls: Vec::new(),
            blocks: HashMap::new(),
            source,
            raw_content_num_bytes: 0,
        }
    }

    fn compile_kwargs(&mut self, kwargs: HashMap<String, Expression>) {
        let num_args = kwargs.len();
        // TODO: push a single instr for all keys as a Vec<String> like Python? bench first
        for (key, value) in kwargs {
            self.chunk.add(Instruction::LoadConst(Value::from(key)));
            self.compile_expr(value);
        }
        self.chunk.add(Instruction::BuildMap(num_args));
    }

    fn compile_expr(&mut self, expr: Expression) {
        match expr {
            Expression::Const(e) => {
                let (val, _) = e.into_parts();
                self.chunk.add(Instruction::LoadConst(val));
            }
            Expression::Map(e) => {
                let (map, _) = e.into_parts();
                let num_items = map.items.len();
                for (key, value) in map.items {
                    self.chunk.add(Instruction::LoadConst(Value::from(key)));
                    self.compile_expr(value);
                }
                self.chunk.add(Instruction::BuildMap(num_items));
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
                let call_idx = if let Some(idx) = self
                    .macro_calls
                    .iter()
                    .position(|x| x.0 == macro_call.namespace && x.1 == macro_call.name)
                {
                    idx
                } else {
                    let len = self.macro_calls.len();
                    self.macro_calls
                        .push((macro_call.namespace, macro_call.name));
                    len
                };

                self.chunk.add(Instruction::RenderMacro(call_idx));
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
        let mut compiler = Compiler::new(&self.chunk.name, self.source);
        compiler.macro_calls = std::mem::take(&mut self.macro_calls);
        for node in block.body {
            compiler.compile_node(node);
        }
        self.macro_calls = compiler.macro_calls;
        self.raw_content_num_bytes += compiler.raw_content_num_bytes;
        self.blocks.extend(compiler.blocks.into_iter());
        self.blocks.insert(block.name.clone(), compiler.chunk);
        self.chunk.add(Instruction::RenderBlock(block.name));
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
                self.raw_content_num_bytes += text.as_bytes().len();
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
            Node::BlockSet(b) => {
                self.chunk.add(Instruction::Capture);
                for node in b.body {
                    self.compile_node(node);
                }
                self.chunk.add(Instruction::EndCapture);
                for expr in b.filters {
                    if let Expression::Filter(f) = expr {
                        let (filter, _) = f.into_parts();
                        self.compile_kwargs(filter.kwargs);
                        self.chunk.add(Instruction::ApplyFilter(filter.name));
                    }
                }
                let instr = if b.global {
                    Instruction::SetGlobal(b.name)
                } else {
                    Instruction::Set(b.name)
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
                // The value is sent before the key to be consistent with a value only loop
                self.chunk.add(Instruction::StoreLocal(forloop.value));
                if let Some(key_var) = forloop.key {
                    self.chunk.add(Instruction::StoreLocal(key_var));
                }
                let start_idx = self.chunk.add(Instruction::Iterate(0)) as usize;
                self.processing_bodies.push(ProcessingBody::Loop(start_idx));

                for node in forloop.body {
                    self.compile_node(node);
                }

                let has_else = !forloop.else_body.is_empty();

                match self.processing_bodies.pop() {
                    Some(ProcessingBody::Loop(start_idx)) => {
                        self.chunk.add(Instruction::Jump(start_idx));
                        let loop_end = self.chunk.len();

                        if has_else {
                            self.chunk.add(Instruction::StoreDidNotIterate);
                        }

                        self.chunk.add(Instruction::PopLoop);
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

                if has_else {
                    let idx = self.chunk.add(Instruction::PopJumpIfFalse(0)) as usize;
                    self.processing_bodies.push(ProcessingBody::Branch(idx));
                    for node in forloop.else_body {
                        self.compile_node(node);
                    }
                    self.end_branch(self.chunk.len());
                }
            }
            Node::Break => {
                self.chunk.add(Instruction::Break);
            }
            Node::Continue => {
                if let ProcessingBody::Loop(idx) = self
                    .processing_bodies
                    .iter()
                    .rev()
                    .find(|b| matches!(b, ProcessingBody::Loop(..)))
                    .unwrap()
                {
                    self.chunk.add(Instruction::Jump(*idx));
                }
            }
            Node::If(i) => {
                self.compile_expr(i.expr);

                let idx = self.chunk.add(Instruction::PopJumpIfFalse(0)) as usize;
                self.processing_bodies.push(ProcessingBody::Branch(idx));
                for node in i.body {
                    self.compile_node(node);
                }

                if !i.false_body.is_empty() {
                    let idx = self.chunk.add(Instruction::Jump(0)) as usize;
                    self.end_branch(self.chunk.len());
                    self.processing_bodies.push(ProcessingBody::Branch(idx));

                    for node in i.false_body {
                        self.compile_node(node);
                    }
                }
                self.end_branch(self.chunk.len());
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
