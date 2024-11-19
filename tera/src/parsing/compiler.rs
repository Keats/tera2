//! AST -> bytecode
use crate::parsing::ast::{BinaryOperator, Block, Expression, MacroCall, Node, UnaryOperator};
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
    // We will need the MacroCall later to check if the arguments called actually exist
    pub(crate) macro_calls: Vec<MacroCall>,
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
            self.chunk.add(
                Instruction::LoadConst(Value::from(key)),
                Some(value.span().clone()),
            );
            self.compile_expr(value);
        }
        self.chunk.add(Instruction::BuildMap(num_args), None);
    }

    fn compile_expr(&mut self, expr: Expression) {
        match expr {
            Expression::Const(e) => {
                let (val, span) = e.into_parts();
                self.chunk.add(Instruction::LoadConst(val), Some(span));
            }
            Expression::Map(e) => {
                let (map, span) = e.into_parts();
                let num_items = map.items.len();
                for (key, value) in map.items {
                    self.chunk.add(
                        Instruction::LoadConst(Value::from(key)),
                        Some(value.span().clone()),
                    );
                    self.compile_expr(value);
                }
                self.chunk.add(Instruction::BuildMap(num_items), Some(span));
            }
            Expression::Array(e) => {
                let (array, span) = e.into_parts();
                let num_elems = array.items.len();
                for val in array.items {
                    self.compile_expr(val);
                }
                self.chunk
                    .add(Instruction::BuildList(num_elems), Some(span));
            }
            Expression::Var(e) => {
                let (val, span) = e.into_parts();
                self.chunk.add(Instruction::LoadName(val.name), Some(span));
            }
            Expression::GetAttr(e) => {
                let (attr, span) = e.into_parts();
                self.compile_expr(attr.expr);
                self.chunk.add(Instruction::LoadAttr(attr.name), Some(span));
            }
            Expression::GetItem(e) => {
                let (item, span) = e.into_parts();
                self.compile_expr(item.expr);
                self.compile_expr(item.sub_expr);
                self.chunk.add(Instruction::BinarySubscript, Some(span));
            }
            Expression::Slice(e) => {
                let (slice, span) = e.into_parts();
                self.compile_expr(slice.expr);
                if let Some(start) = slice.start {
                    self.compile_expr(start);
                } else {
                    self.chunk.add(Instruction::LoadConst(0.into()), None);
                }

                if let Some(end) = slice.end {
                    self.compile_expr(end);
                } else {
                    self.chunk.add(Instruction::LoadConst(Value::Null), None);
                }

                if let Some(step) = slice.step {
                    self.compile_expr(step);
                } else {
                    self.chunk.add(Instruction::LoadConst(1.into()), None);
                }

                self.chunk.add(Instruction::Slice, Some(span));
            }
            Expression::Filter(e) => {
                let (filter, span) = e.into_parts();
                self.compile_expr(filter.expr);
                self.compile_kwargs(filter.kwargs);
                self.chunk
                    .add(Instruction::ApplyFilter(filter.name), Some(span));
            }
            Expression::Test(e) => {
                let (test, span) = e.into_parts();
                self.compile_expr(test.expr);
                self.compile_kwargs(test.kwargs);
                self.chunk.add(Instruction::RunTest(test.name), Some(span));
            }
            Expression::Ternary(e) => {
                let (ternary, _) = e.into_parts();
                self.compile_expr(ternary.expr);
                let idx = self.chunk.add(Instruction::PopJumpIfFalse(0), None) as usize;
                self.processing_bodies.push(ProcessingBody::Branch(idx));
                self.compile_expr(ternary.true_expr);
                let idx = self.chunk.add(Instruction::Jump(0), None) as usize;
                self.end_branch(self.chunk.len());
                self.processing_bodies.push(ProcessingBody::Branch(idx));
                self.compile_expr(ternary.false_expr);
                self.end_branch(self.chunk.len());
            }
            Expression::ComponentCall(e) => {
                todo!("Implement component calls")
            }
            Expression::MacroCall(e) => {
                let (macro_call, span) = e.into_parts();
                self.compile_kwargs(macro_call.kwargs.clone());
                let call_idx =
                    if let Some(idx) = self.macro_calls.iter().position(|x| {
                        x.namespace == macro_call.namespace && x.name == macro_call.name
                    }) {
                        idx
                    } else {
                        let len = self.macro_calls.len();
                        self.macro_calls.push(macro_call);
                        len
                    };

                self.chunk
                    .add(Instruction::RenderMacro(call_idx), Some(span));
            }
            Expression::FunctionCall(e) => {
                let (func, span) = e.into_parts();
                self.compile_kwargs(func.kwargs);
                self.chunk
                    .add(Instruction::CallFunction(func.name), Some(span));
            }
            Expression::UnaryOperation(e) => {
                let (op, span) = e.into_parts();
                self.compile_expr(op.expr);
                match op.op {
                    UnaryOperator::Not => self.chunk.add(Instruction::Not, Some(span)),
                    UnaryOperator::Minus => self.chunk.add(Instruction::Negative, Some(span)),
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
                            instr.push(self.chunk.add(
                                if op.op == BinaryOperator::And {
                                    Instruction::JumpIfFalseOrPop(0)
                                } else {
                                    Instruction::JumpIfTrueOrPop(0)
                                },
                                None,
                            ) as usize);
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
                                    Some((Instruction::JumpIfFalseOrPop(ref mut target), _))
                                    | Some((Instruction::JumpIfTrueOrPop(ref mut target), _)) => {
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
                self.chunk.add(instr, Some(span));
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
        self.blocks.extend(compiler.blocks);
        self.blocks.insert(block.name.clone(), compiler.chunk);
        self.chunk.add(Instruction::RenderBlock(block.name), None);
    }

    fn end_branch(&mut self, idx: usize) {
        match self.processing_bodies.pop() {
            Some(ProcessingBody::Branch(instr)) => match self.chunk.get_mut(instr) {
                Some((Instruction::Jump(ref mut target), _))
                | Some((Instruction::PopJumpIfFalse(ref mut target), _)) => {
                    *target = idx;
                }
                _ => {}
            },
            _ => unreachable!(),
        }
    }

    fn get_current_loop(&self) -> Option<&ProcessingBody> {
        self.processing_bodies
            .iter()
            .rev()
            .find(|b| matches!(b, ProcessingBody::Loop(..)))
    }

    pub fn compile_node(&mut self, node: Node) {
        match node {
            Node::Content(text) => {
                self.raw_content_num_bytes += text.as_bytes().len();
                self.chunk.add(Instruction::WriteText(text), None);
            }
            Node::Expression(expr) => {
                self.compile_expr(expr);
                self.chunk.add(Instruction::WriteTop, None);
            }
            Node::Set(s) => {
                self.compile_expr(s.value);
                let instr = if s.global {
                    Instruction::SetGlobal(s.name)
                } else {
                    Instruction::Set(s.name)
                };
                self.chunk.add(instr, None);
            }
            Node::BlockSet(b) => {
                self.chunk.add(Instruction::Capture, None);
                for node in b.body {
                    self.compile_node(node);
                }
                self.chunk.add(Instruction::EndCapture, None);
                for expr in b.filters {
                    if let Expression::Filter(f) = expr {
                        let (filter, _) = f.into_parts();
                        self.compile_kwargs(filter.kwargs);
                        self.chunk.add(Instruction::ApplyFilter(filter.name), None);
                    }
                }
                let instr = if b.global {
                    Instruction::SetGlobal(b.name)
                } else {
                    Instruction::Set(b.name)
                };
                self.chunk.add(instr, None);
            }
            Node::Include(i) => {
                self.chunk.add(Instruction::Include(i.name), None);
            }
            Node::Block(b) => {
                self.compile_block(b);
            }
            Node::ForLoop(forloop) => {
                self.compile_expr(forloop.target);
                self.chunk
                    .add(Instruction::StartIterate(forloop.key.is_some()), None);
                // The value is sent before the key to be consistent with a value only loop
                self.chunk.add(Instruction::StoreLocal(forloop.value), None);
                if let Some(key_var) = forloop.key {
                    self.chunk.add(Instruction::StoreLocal(key_var), None);
                }
                let start_idx = self.chunk.add(Instruction::Iterate(0), None) as usize;
                self.processing_bodies.push(ProcessingBody::Loop(start_idx));

                for node in forloop.body {
                    self.compile_node(node);
                }

                let has_else = !forloop.else_body.is_empty();

                match self.processing_bodies.pop() {
                    Some(ProcessingBody::Loop(start_idx)) => {
                        self.chunk.add(Instruction::Jump(start_idx), None);
                        let loop_end = self.chunk.len();

                        if has_else {
                            self.chunk.add(Instruction::StoreDidNotIterate, None);
                        }

                        self.chunk.add(Instruction::PopLoop, None);
                        if let Some((Instruction::Iterate(ref mut jump_target), _)) =
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
                    let idx = self.chunk.add(Instruction::PopJumpIfFalse(0), None) as usize;
                    self.processing_bodies.push(ProcessingBody::Branch(idx));
                    for node in forloop.else_body {
                        self.compile_node(node);
                    }
                    self.end_branch(self.chunk.len());
                }
            }
            Node::Break => {
                self.chunk.add(Instruction::Break, None);
            }
            Node::Continue => {
                if let ProcessingBody::Loop(idx) = self.get_current_loop().unwrap() {
                    self.chunk.add(Instruction::Jump(*idx), None);
                }
            }
            Node::If(i) => {
                self.compile_expr(i.expr);

                let idx = self.chunk.add(Instruction::PopJumpIfFalse(0), None) as usize;
                self.processing_bodies.push(ProcessingBody::Branch(idx));
                for node in i.body {
                    self.compile_node(node);
                }

                if !i.false_body.is_empty() {
                    let idx = self.chunk.add(Instruction::Jump(0), None) as usize;
                    self.end_branch(self.chunk.len());
                    self.processing_bodies.push(ProcessingBody::Branch(idx));

                    for node in i.false_body {
                        self.compile_node(node);
                    }
                }
                self.end_branch(self.chunk.len());
            }
            Node::FilterSection(f) => {
                self.chunk.add(Instruction::Capture, None);
                for node in f.body {
                    self.compile_node(node);
                }
                self.chunk.add(Instruction::EndCapture, None);
                self.compile_kwargs(f.kwargs);
                self.chunk
                    .add(Instruction::ApplyFilter(f.name.into_parts().0), None);
                self.chunk.add(Instruction::WriteTop, None);
            }
        }
    }

    pub fn compile(&mut self, nodes: Vec<Node>) {
        for node in nodes {
            self.compile_node(node);
        }
    }
}
