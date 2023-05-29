use std::io::Write;

use crate::errors::{Error, TeraResult};
use crate::parsing::Instruction;
use crate::template::Template;
use crate::value::Value;
use crate::Tera;

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
}

// TODO: handle set and set_global
pub(crate) struct VirtualMachine<'t> {
    tera: &'t Tera,
    template: &'t Template,
    ip: usize,
}

impl<'t> VirtualMachine<'t> {
    // TODO: add a buffer to write to
    pub fn new(tera: &'t Tera, template: &'t Template) -> Self {
        Self {
            tera,
            template,
            ip: 0,
        }
    }

    fn interpret(&mut self, output: &mut impl Write) -> TeraResult<()> {
        let mut stack = Stack::new();

        macro_rules! op_binop {
            ($op:tt) => {{
                let b = stack.pop();
                let a = stack.pop();
                stack.push(Value::from(a $op b));
            }};
        }

        macro_rules! math_binop {
            ($fn:ident) => {{
                let b = stack.pop();
                let a = stack.pop();
                stack.push(crate::value::number::$fn(&a, &b)?);
            }};
        }

        // TODO: load from context, loops, set
        while let Some(instr) = self.template.chunk.get(self.ip) {
            // println!("{:?}", instr);
            match instr {
                Instruction::LoadConst(v) => stack.push(v.clone()),
                Instruction::LoadName(_) => {}
                Instruction::StoreLocal(_) => {}
                Instruction::WriteText(t) => {
                    write!(output, "{t}")?;
                }
                Instruction::WriteTop => {
                    write!(output, "{}", stack.pop())?;
                }
                Instruction::Set(_) => {}
                Instruction::SetGlobal(_) => {}
                Instruction::Include(_) => {}
                Instruction::LoadAttr(_) => {}
                Instruction::BinarySubscript => {}
                Instruction::BuildMap(_) => {}
                Instruction::BuildList(_) => {}
                Instruction::CallFunction(_) => {}
                Instruction::CallMacro(_) => {}
                Instruction::ApplyFilter(_) => {}
                Instruction::RunTest(_) => {}
                Instruction::CallBlock(_) => {}
                Instruction::JumpIfFalse(_) => {}
                Instruction::PopJumpIfFalse(_) => {}
                Instruction::PopJumpIfTrue(_) => {}
                Instruction::Jump(_) => {}
                Instruction::JumpIfFalseOrPop(_) => {}
                Instruction::JumpIfTrueOrPop(_) => {}
                Instruction::Capture => {}
                Instruction::EndCapture => {}
                Instruction::StartIterate(_) => {}
                Instruction::Iterate(_) => {}
                Instruction::PopFrame => {}
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
                    let b = stack.pop();
                    let a = stack.pop();
                    // TODO: we could push_str if `a` is a string
                    stack.push(Value::from(format!("{a}{b}")));
                }
                Instruction::In => {
                    let b = stack.pop();
                    let a = stack.pop();
                    stack.push(Value::Bool(b.contains(&a)?));
                }
                Instruction::Not => {
                    let a = stack.pop();
                    stack.push(Value::from(!a.is_truthy()));
                }
                Instruction::Negative => {
                    let a = stack.pop();
                    stack.push(crate::value::number::negate(&a)?);
                }
            }

            self.ip += 1;
        }

        Ok(())
    }

    pub(crate) fn render(&mut self) -> TeraResult<String> {
        let mut output = Vec::with_capacity(self.template.size_hint());
        self.interpret(&mut output)?;
        Ok(String::from_utf8(output)?)
    }
}
