use std::borrow::Cow;
use std::collections::BTreeMap;
use std::io::Write;

use crate::errors::{Error, ErrorKind, ReportError, TeraResult};
use crate::parsing::compiler::CompiledMacroDefinition;
use crate::parsing::{Chunk, Instruction};
use crate::template::Template;
use crate::value::{Key, Value};
use crate::vm::for_loop::ForLoop;

use crate::vm::state::State;
use crate::{Context, Tera};

pub(crate) struct VirtualMachine<'tera> {
    tera: &'tera Tera,
    template: &'tera Template,
}

impl<'tera> VirtualMachine<'tera> {
    pub fn new(tera: &'tera Tera, template: &'tera Template) -> Self {
        Self { tera, template }
    }

    // TODO: do that at compile time so we don't need to do it at runtime
    fn get_block_lineage(&self, block_name: &str) -> TeraResult<Vec<&'tera Chunk>> {
        // We first get all the chunks we might need to render
        let mut blocks = Vec::with_capacity(10);
        // The block is present in the template we are rendering
        if let Some(bl) = self.template.block_lineage.get(block_name) {
            for c in bl {
                blocks.push(c);
            }
        } else {
            // the block is not present, we go up the lineage by 1 until we find a template that has it
            for parent_tpl_name in self.template.parents.iter().rev() {
                let parent_tpl = self.tera.get_template(parent_tpl_name)?;
                if let Some(bl) = parent_tpl.block_lineage.get(block_name) {
                    for c in bl {
                        blocks.push(c);
                    }
                    break;
                }
            }
        }

        Ok(blocks)
    }

    fn interpret(&self, state: &mut State<'tera>, output: &mut impl Write) -> TeraResult<()> {
        let mut ip = 0;

        macro_rules! rendering_error {
            ($msg:expr,$span:expr) => {{
                let mut err = ReportError::new($msg, &$span.as_ref().clone().unwrap());
                err.generate_report(
                    &self.template.name,
                    &self.template.source,
                    "Rendering error",
                );
                return Err(Error::new(ErrorKind::RenderingError(err)));
            }};
        }

        macro_rules! expand_span {
            ($first:expr,$second:expr) => {{
                let mut c_span = $first.clone().unwrap().into_owned();
                if let Some(ref second_span) = $second.as_ref() {
                    c_span.expand(&second_span);
                }
                Cow::Owned(c_span)
            }};
        }

        macro_rules! op_binop {
            ($op:tt) => {{
                let (b, b_span) = state.stack.pop();
                let (a, a_span) = state.stack.pop();
                state.stack.push(Value::from(a $op b), Some(expand_span!(a_span, b_span)));
            }};
        }

        macro_rules! math_binop {
            ($fn:ident) => {{
                let (b, b_span) = state.stack.pop();
                let (a, a_span) = state.stack.pop();

                if !a.is_number() {
                    rendering_error!(
                        format!(
                            "Math operations can only be done on numbers, found `{}`",
                            a.name()
                        ),
                        a_span
                    );
                }

                if !b.is_number() {
                    rendering_error!(
                        format!(
                            "Math operations can only be done on numbers, found `{}`",
                            b.name()
                        ),
                        b_span
                    );
                }

                let c_span = expand_span!(a_span, b_span);
                match crate::value::number::$fn(&a, &b) {
                    Ok(c) => state.stack.push(c, Some(c_span)),
                    Err(e) => {
                        let err_msg = e.to_string();
                        // yucky
                        if err_msg.contains("divide by 0") {
                            rendering_error!(err_msg, b_span);
                        } else {
                            rendering_error!(err_msg, Some(c_span));
                        }
                    }
                }
            }};
        }

        macro_rules! write_to_buffer {
            ($val:expr) => {{
                if let Some(captured) = state.capture_buffers.last_mut() {
                    write!(captured, "{}", $val)?;
                } else {
                    write!(output, "{}", $val)?;
                }
            }};
        }

        // TODO later: tests/filters/fns
        // println!("{:?}", self.template.macro_calls_def);
        while let Some((instr, span)) = state.chunk.get(ip) {
            // println!("{}. {:?}", state.chunk.name, instr);
            match instr {
                Instruction::LoadConst(v) => {
                    state.stack.push_borrowed(v.clone(), span.as_ref().unwrap())
                }
                Instruction::LoadName(n) => state.load_name(n, span),
                Instruction::LoadAttr(attr) => {
                    let (a, a_span) = state.stack.pop();
                    if a == Value::Undefined {
                        rendering_error!(format!("Container is not defined"), a_span);
                    }
                    state
                        .stack
                        .push_borrowed(a.get_attr(attr), span.as_ref().unwrap());
                }
                Instruction::BinarySubscript => {
                    let (subscript, subscript_span) = state.stack.pop();
                    let (val, val_span) = state.stack.pop();
                    if val == Value::Undefined {
                        rendering_error!(format!("Container is not defined"), val_span);
                    }

                    let c_span = expand_span!(val_span, subscript_span);
                    match val.get_item(subscript) {
                        Ok(v) => {
                            state.stack.push(v, Some(c_span));
                        }
                        Err(e) => {
                            rendering_error!(e.to_string(), subscript_span);
                        }
                    }
                }
                Instruction::WriteText(t) => {
                    write_to_buffer!(t);
                }
                Instruction::WriteTop => {
                    let (top, top_span) = state.stack.pop();
                    if top == Value::Undefined {
                        rendering_error!(
                            format!("Tried to render a variable that is not defined"),
                            top_span
                        );
                    }
                    write_to_buffer!(top);
                }
                Instruction::Set(name) => {
                    // TODO: do we need to keep those spans?
                    let (val, _) = state.stack.pop();
                    state.store_local(name, val);
                }
                Instruction::SetGlobal(name) => {
                    let (val, _) = state.stack.pop();
                    state.store_global(name, val);
                }
                Instruction::Include(name) => {
                    self.render_include(name, state, output)?;
                }
                Instruction::BuildMap(num_elem) => {
                    let mut elems = Vec::with_capacity(*num_elem);
                    for _ in 0..*num_elem {
                        let (val, _) = state.stack.pop();
                        let (key, _) = state.stack.pop();
                        elems.push((key.as_key()?, val));
                    }
                    let map: BTreeMap<_, _> = elems.into_iter().collect();
                    // TODO: do we need to keep track of the full span?
                    state.stack.push(Value::from(map), None)
                }
                Instruction::BuildList(num_elem) => {
                    let mut elems = Vec::with_capacity(*num_elem);
                    for _ in 0..*num_elem {
                        elems.push(state.stack.pop().0);
                    }
                    elems.reverse();
                    state.stack.push(Value::from(elems), None);
                }
                Instruction::CallFunction(fn_name) => {
                    let (kwargs, _) = state.stack.pop();
                    if fn_name == "super" {
                        let current_block_name =
                            state.current_block_name.expect("no current block");
                        let (blocks, level) = state
                            .blocks
                            .remove(current_block_name)
                            .expect("no lineage found");
                        if blocks.len() == 1 {
                            rendering_error!(
                                format!("Tried to use super() in the top level block"),
                                span
                            );
                        }

                        let block_chunk = blocks[level + 1];
                        let old_chunk = std::mem::replace(&mut state.chunk, block_chunk);
                        state.blocks.insert(current_block_name, (blocks, level + 1));
                        let res = self.interpret(state, output);
                        state.chunk = old_chunk;
                        res?;
                        state.stack.push(Value::Null, None);
                    } else {
                        println!("Calling {fn_name} with {kwargs:?}");
                    }
                }
                Instruction::ApplyFilter(_) => {}
                Instruction::RunTest(_) => {}
                Instruction::RenderMacro(idx) => {
                    let kwargs = state.stack.pop().0.into_map().expect("to have kwargs");
                    let mut context = Context::new();
                    // first need to make sure the data in the template makes sense
                    let curr_template = if self.template.parents.is_empty() {
                        self.template
                    } else {
                        self.tera.get_template(state.current_tpl_name())?
                    };

                    let compiled_macro_def = &curr_template.macro_calls_def[*idx];
                    for (key, value) in &compiled_macro_def.kwargs {
                        match kwargs.get(&Key::from(key.as_str())) {
                            Some(kwarg_val) => {
                                context.insert(key, kwarg_val);
                            }
                            None => match value {
                                Some(kwarg_val) => {
                                    context.insert(key, kwarg_val);
                                }
                                None => todo!("Missing arg macro error"),
                            },
                        }
                    }

                    let val = self.render_macro(
                        &curr_template.macro_calls[*idx],
                        compiled_macro_def,
                        context,
                    )?;
                    state
                        .stack
                        .push_borrowed(Value::from(val), span.as_ref().unwrap());
                }
                Instruction::RenderBlock(block_name) => {
                    let block_lineage = self.get_block_lineage(block_name)?;
                    let block_chunk = block_lineage[0];
                    let old_chunk = std::mem::replace(&mut state.chunk, block_chunk);
                    state.blocks.insert(block_name, (block_lineage, 0));
                    let old_block_name =
                        std::mem::replace(&mut state.current_block_name, Some(block_name));
                    let res = self.interpret(state, output);
                    state.chunk = old_chunk;
                    state.current_block_name = old_block_name;
                    res?;
                }
                Instruction::Jump(target_ip) => {
                    ip = *target_ip;
                    continue;
                }
                Instruction::PopJumpIfFalse(target_ip) => {
                    let (val, _) = state.stack.pop();
                    if !val.is_truthy() {
                        ip = *target_ip;
                        continue;
                    }
                }
                Instruction::JumpIfFalseOrPop(target_ip) => {
                    let (peeked, _) = state.stack.peek();
                    if !peeked.is_truthy() {
                        ip = *target_ip;
                        continue;
                    } else {
                        state.stack.pop();
                    }
                }
                Instruction::JumpIfTrueOrPop(target_ip) => {
                    let (peeked, _) = state.stack.peek();
                    if peeked.is_truthy() {
                        ip = *target_ip;
                        continue;
                    } else {
                        state.stack.pop();
                    }
                }
                Instruction::Capture => {
                    state.capture_buffers.push(Vec::with_capacity(128));
                }
                Instruction::EndCapture => {
                    // TODO: we should keep track of the buffer spans?
                    let captured = state.capture_buffers.pop().unwrap();
                    let val = Value::from(String::from_utf8(captured)?);
                    state.stack.push(val, None);
                }
                Instruction::StartIterate(is_key_value) => {
                    let (container, container_span) = state.stack.pop();
                    if !container.can_be_iterated_on() {
                        rendering_error!(
                            format!("Iteration not possible on type `{}`", container.name()),
                            container_span
                        );
                    }

                    if *is_key_value && !matches!(container, Value::Map(..)) {
                        rendering_error!(
                            format!(
                                "Key/value iteration is not possible on type `{}`, only on maps.",
                                container.name()
                            ),
                            container_span
                        );
                    }

                    state.for_loops.push(ForLoop::new(container));
                }
                Instruction::StoreLocal(name) => {
                    if let Some(for_loop) = state.for_loops.last_mut() {
                        for_loop.store_local(name.as_str());
                    }
                }
                Instruction::Iterate(end_ip) => {
                    if let Some(for_loop) = state.for_loops.last_mut() {
                        if for_loop.is_over() {
                            ip = *end_ip;
                            continue;
                        }
                        for_loop.advance();
                        for_loop.end_ip = *end_ip;
                    }
                }
                Instruction::StoreDidNotIterate => {
                    if let Some(for_loop) = state.for_loops.last() {
                        state.stack.push(Value::Bool(!for_loop.iterated()), None);
                    }
                }
                Instruction::Break => {
                    if let Some(for_loop) = state.for_loops.last_mut() {
                        ip = for_loop.end_ip;
                        continue;
                    }
                }
                Instruction::PopLoop => {
                    state.for_loops.pop();
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
                    let (b, b_span) = state.stack.pop();
                    let (a, a_span) = state.stack.pop();
                    let c_span = expand_span!(a_span, b_span);
                    // TODO: we could push_str if `a` is a string
                    state
                        .stack
                        .push(Value::from(format!("{a}{b}")), Some(c_span));
                }
                Instruction::In => {
                    let (container, container_span) = state.stack.pop();
                    let (needle, _) = state.stack.pop();
                    match container.contains(&needle) {
                        Ok(b) => {
                            state.stack.push(Value::Bool(b), None);
                        }
                        Err(e) => {
                            rendering_error!(e.to_string(), container_span);
                        }
                    };
                }
                Instruction::Not => {
                    let (a, a_span) = state.stack.pop();
                    state.stack.push(Value::from(!a.is_truthy()), a_span);
                }
                Instruction::Negative => {
                    let (a, a_span) = state.stack.pop();
                    match crate::value::number::negate(&a) {
                        Ok(b) => {
                            state.stack.push(b, a_span);
                        }
                        Err(e) => {
                            rendering_error!(e.to_string(), a_span);
                        }
                    }
                }
            }

            ip += 1;
        }

        Ok(())
    }

    fn render_macro(
        &self,
        info: &(String, String),
        macro_def: &CompiledMacroDefinition,
        context: Context,
    ) -> TeraResult<String> {
        let tpl = self.tera.get_template(&info.0)?;
        let vm = Self {
            tera: self.tera,
            template: tpl,
        };

        let mut state = State::new(&context, &macro_def.chunk);
        let mut output = Vec::with_capacity(1024);
        vm.interpret(&mut state, &mut output)?;

        Ok(String::from_utf8(output)?)
    }

    fn render_include(
        &self,
        name: &str,
        state: &State<'tera>,
        output: &mut impl Write,
    ) -> TeraResult<()> {
        let tpl = self.tera.get_template(name)?;
        let vm = Self {
            tera: self.tera,
            template: tpl,
        };

        // We create a dummy state for variables to be written to, but we don't keep it around
        let mut include_state = State::new(state.context, &tpl.chunk);
        include_state.include_parent = Some(state);
        vm.interpret(&mut include_state, output)?;
        Ok(())
    }

    pub(crate) fn render(&mut self, context: &Context) -> TeraResult<String> {
        let mut output = Vec::with_capacity(self.template.size_hint());
        // TODO(perf): can we optimize this at the bytecode level to avoid hashmap lookups?
        let chunk = if let Some(base_tpl_name) = self.template.parents.first() {
            let tpl = self.tera.get_template(base_tpl_name)?;
            &tpl.chunk
        } else {
            &self.template.chunk
        };
        let mut state = State::new(context, chunk);

        self.interpret(&mut state, &mut output)?;
        Ok(String::from_utf8(output)?)
    }
}
