use std::collections::BTreeMap;
use std::io::Write;

use crate::errors::{Error, TeraResult};
use crate::parsing::Instruction;
use crate::template::Template;
use crate::value::Value;
use crate::vm::for_loop::ForLoop;
use crate::{Context, Tera};

#[derive(Debug, Clone)]
struct Stack {
    values: Vec<Value>,
}

impl Stack {
    fn new() -> Self {
        Self {
            // TODO: check the size of the stack of an average template
            values: Vec::with_capacity(64),
        }
    }

    fn push(&mut self, val: Value) {
        self.values.push(val);
    }

    fn pop(&mut self) -> Value {
        self.values.pop().expect("to have a value")
    }

    fn peek(&mut self) -> &Value {
        self.values.last().expect("to peek a value")
    }
}

// TODO: handle set and set_global + forloop local variables -> idea of a frame
// TODO: add a method to error that automatically adds span + template source
pub(crate) struct VirtualMachine<'t> {
    tera: &'t Tera,
    template: &'t Template,
    context: &'t Context,
    stack: Stack,
    for_loops: Vec<ForLoop>,
    set_locals: BTreeMap<String, Value>,
    /// Any variable set with set_global will be stored here
    set_globals: BTreeMap<String, Value>,
    ip: usize,
    context_fn: Option<Box<dyn Fn(&str) -> Value + 't>>,
    /// To handle the capture instructions
    capture_buffers: Vec<Vec<u8>>,
}

impl<'t> VirtualMachine<'t> {
    pub fn new(tera: &'t Tera, template: &'t Template, context: &'t Context) -> Self {
        Self {
            tera,
            template,
            context,
            stack: Stack::new(),
            for_loops: Vec::new(),
            set_globals: BTreeMap::new(),
            set_locals: BTreeMap::new(),
            ip: 0,
            context_fn: None,
            capture_buffers: Vec::with_capacity(2),
        }
    }

    pub fn sub_vm_include(&'t self, template: &'t Template) -> Self {
        Self {
            tera: self.tera,
            template,
            context: self.context,
            stack: Stack::new(),
            for_loops: Vec::new(),
            set_globals: BTreeMap::new(),
            set_locals: BTreeMap::new(),
            ip: 0,
            context_fn: Some(Box::new(|name| self.get_value(name))),
            capture_buffers: Vec::with_capacity(2),
        }
    }

    /// Loads the value with the current name on the stack
    /// It goes in the following order for scopes:
    /// 1. All loops from the last to the first
    /// 2. set_locals
    /// 3. set_globals
    /// 4. self.context
    /// 5. return undefined
    fn get_value(&self, name: &str) -> Value {
        for forloop in &self.for_loops {
            if let Some(v) = forloop.get_by_name(name) {
                return v;
            }
        }

        if let Some(val) = self.set_locals.get(name) {
            return val.clone();
        }

        if let Some(val) = self.set_globals.get(name) {
            return val.clone();
        }

        if let Some(val) = self.context.data.get(name) {
            return val.clone();
        }

        if let Some(ref func) = self.context_fn {
            return func(name);
        }

        // TODO: we do need undefined to differentiate from null do we
        // TODO: in practice we want to only return undefined if it's in a if/condition and error
        // otherwise like in Tera v1? To consider. In those case we could emit a different
        // instruction that will not error if not found and make this return an Option
        Value::Null
    }

    fn load_name(&mut self, name: &str) {
        self.stack.push(self.get_value(name));
    }

    fn store_local(&mut self, name: &str, value: Value) {
        if let Some(forloop) = self.for_loops.last_mut() {
            forloop.store(name, value);
        } else {
            self.set_locals.insert(name.to_string(), value);
        }
    }

    fn interpret(&mut self, output: &mut impl Write) -> TeraResult<()> {
        macro_rules! op_binop {
            ($op:tt) => {{
                let b = self.stack.pop();
                let a = self.stack.pop();
                self.stack.push(Value::from(a $op b));
            }};
        }

        macro_rules! math_binop {
            ($fn:ident) => {{
                let b = self.stack.pop();
                let a = self.stack.pop();
                self.stack.push(crate::value::number::$fn(&a, &b)?);
            }};
        }

        macro_rules! write_to_buffer {
            ($val:expr) => {{
                if let Some(captured) = self.capture_buffers.last_mut() {
                    write!(captured, "{}", $val)?;
                } else {
                    write!(output, "{}", $val)?;
                }
            }};
        }

        // TODO later: macros/blocks/tests/filters/fns
        // println!("{:?}", self.template.chunk);
        while let Some(instr) = self.template.chunk.get(self.ip) {
            // println!("{:?}", instr);
            match instr {
                Instruction::LoadConst(v) => self.stack.push(v.clone()),
                Instruction::LoadName(n) => self.load_name(n),
                Instruction::LoadAttr(attr) => {
                    let a = self.stack.pop();
                    self.stack
                        .push(a.get_attr(attr).expect("TODO: handle error"));
                }
                Instruction::BinarySubscript => {
                    let subscript = self.stack.pop();
                    let val = self.stack.pop();
                    self.stack
                        .push(val.get_item(subscript).expect("TODO: handle error"));
                }
                Instruction::WriteText(t) => {
                    write_to_buffer!(t);
                }
                Instruction::WriteTop => {
                    write_to_buffer!(self.stack.pop());
                }
                Instruction::Set(name) => {
                    let val = self.stack.pop();
                    self.store_local(name, val);
                }
                Instruction::SetGlobal(name) => {
                    self.set_globals.insert(name.to_string(), self.stack.pop());
                }
                Instruction::Include(name) => {
                    let val = self.render_include(name)?;
                    write_to_buffer!(val);
                }
                Instruction::BuildMap(num_elem) => {
                    let mut elems = Vec::with_capacity(*num_elem);
                    for _ in 0..*num_elem {
                        let val = self.stack.pop();
                        let key = self.stack.pop();
                        elems.push((key.as_key()?, val));
                    }
                    let map: BTreeMap<_, _> = elems.into_iter().collect();
                    self.stack.push(Value::from(map))
                }
                Instruction::BuildList(num_elem) => {
                    let mut elems = Vec::with_capacity(*num_elem);
                    for _ in 0..*num_elem {
                        elems.push(self.stack.pop());
                    }
                    elems.reverse();
                    self.stack.push(Value::from(elems));
                }
                Instruction::CallFunction(_) => {}
                Instruction::CallMacro(_) => {}
                Instruction::ApplyFilter(_) => {}
                Instruction::RunTest(_) => {}
                Instruction::CallBlock(_) => {}
                Instruction::Jump(target_ip) => {
                    self.ip = *target_ip;
                    continue;
                }
                Instruction::PopJumpIfFalse(target_ip) => {
                    let val = self.stack.pop();
                    if !val.is_truthy() {
                        self.ip = *target_ip;
                        continue;
                    }
                }
                Instruction::JumpIfFalseOrPop(target_ip) => {
                    let peeked = self.stack.peek();
                    if !peeked.is_truthy() {
                        self.ip = *target_ip;
                        continue;
                    } else {
                        self.stack.pop();
                    }
                }
                Instruction::JumpIfTrueOrPop(target_ip) => {
                    let peeked = self.stack.peek();
                    if peeked.is_truthy() {
                        self.ip = *target_ip;
                        continue;
                    } else {
                        self.stack.pop();
                    }
                }
                Instruction::Capture => {
                    self.capture_buffers.push(Vec::with_capacity(128));
                }
                Instruction::EndCapture => {
                    let captured = self.capture_buffers.pop().unwrap();
                    let val = Value::from(String::from_utf8(captured)?);
                    self.stack.push(val);
                }
                Instruction::StartIterate(is_key_value) => {
                    let container = self.stack.pop();
                    self.for_loops.push(ForLoop::new(*is_key_value, container));
                }
                Instruction::Iterate(end_ip) => {
                    // TODO: something very very very slow happening with forloop
                    if let Some(for_loop) = self.for_loops.last_mut() {
                        if for_loop.is_over() {
                            self.ip = *end_ip;
                            continue;
                        }
                        for_loop.advance();
                        for_loop.end_ip = *end_ip;
                    } else {
                        unreachable!()
                    }
                }
                Instruction::StoreLocal(name) => {
                    if let Some(for_loop) = self.for_loops.last_mut() {
                        for_loop.store_local(name.as_str());
                    } else {
                        unreachable!()
                    }
                }
                Instruction::PopLoop => {
                    self.for_loops.pop();
                }
                Instruction::Mul => math_binop!(mul),
                Instruction::Div => math_binop!(div),
                Instruction::FloorDiv => math_binop!(floor_div),
                Instruction::Mod => math_binop!(rem),
                Instruction::Plus => math_binop!(add),
                Instruction::Minus => math_binop!(sub),
                Instruction::Power => math_binop!(pow),
                Instruction::LessThan => op_binop!(<),
                Instruction::GreaterThan => op_binop!(>),
                Instruction::LessThanOrEqual => op_binop!(<=),
                Instruction::GreaterThanOrEqual => op_binop!(>=),
                Instruction::Equal => op_binop!(==),
                Instruction::NotEqual => op_binop!(!=),
                Instruction::StrConcat => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    // TODO: we could push_str if `a` is a string
                    self.stack.push(Value::from(format!("{a}{b}")));
                }
                Instruction::In => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(Value::Bool(b.contains(&a)?));
                }
                Instruction::Not => {
                    let a = self.stack.pop();
                    self.stack.push(Value::from(!a.is_truthy()));
                }
                Instruction::Negative => {
                    let a = self.stack.pop();
                    self.stack.push(crate::value::number::negate(&a)?);
                }
            }

            self.ip += 1;
        }

        Ok(())
    }

    fn render_include(&mut self, name: &str) -> TeraResult<String> {
        let tpl = self.tera.get_template(name)?;
        // TODO: do we pass the buffer rather than creating one and concatenating?
        let mut include_vm = self.sub_vm_include(tpl);
        include_vm.render()
    }

    pub(crate) fn render(&mut self) -> TeraResult<String> {
        let mut output = Vec::with_capacity(self.template.size_hint());
        self.interpret(&mut output)?;
        Ok(String::from_utf8(output)?)
    }
}
