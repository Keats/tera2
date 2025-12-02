use std::collections::BTreeMap;
use std::io::Write;

use crate::errors::{Error, ErrorKind, ReportError, TeraResult};
use crate::parsing::{Chunk, Instruction};
use crate::template::Template;
use crate::value::{Key, Value};
use crate::vm::for_loop::ForLoop;
use crate::vm::stack::{combine_spans, SpanRange};

use crate::args::Kwargs;
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
            ($msg:expr,$span_range:expr) => {{
                let chunk = state.chunk.expect("to have a chunk");
                let span = $span_range
                    .as_ref()
                    .and_then(|r| chunk.expand_span(r))
                    .expect("to have a span for error");
                let mut err = ReportError::new($msg, &span);
                let (name, source) = if self.template.name != chunk.name {
                    let tpl = &self.tera.templates[&chunk.name];
                    (&tpl.name, &tpl.source)
                } else {
                    (&self.template.name, &self.template.source)
                };
                err.generate_report(name, source, "Rendering error");
                return Err(Error::new(ErrorKind::RenderingError(err)));
            }};
            // Variant for fused instructions that takes a direct span
            ($msg:expr, span: $span:expr) => {{
                let chunk = state.chunk.expect("to have a chunk");
                let span = $span.expect("to have a span for error");
                let mut err = ReportError::new($msg, span);
                let (name, source) = if self.template.name != chunk.name {
                    let tpl = &self.tera.templates[&chunk.name];
                    (&tpl.name, &tpl.source)
                } else {
                    (&self.template.name, &self.template.source)
                };
                err.generate_report(name, source, "Rendering error");
                return Err(Error::new(ErrorKind::RenderingError(err)));
            }};
        }

        macro_rules! op_binop {
            ($op:tt) => {{
                let (b, b_span) = state.stack.pop();
                let (a, a_span) = state.stack.pop();
                state.stack.push(Value::from(a $op b), combine_spans(&a_span, &b_span));
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

                let c_span = combine_spans(&a_span, &b_span);
                match crate::value::number::$fn(&a, &b) {
                    Ok(c) => state.stack.push(c, c_span),
                    Err(e) => {
                        let err_msg = e.to_string();
                        // yucky
                        if err_msg.contains("divide by 0") {
                            rendering_error!(err_msg, b_span);
                        } else {
                            rendering_error!(err_msg, c_span);
                        }
                    }
                }
            }};
        }

        macro_rules! component {
            ($name:expr, $span_idx:expr, $has_body:expr) => {{
                let (kwargs, _) = state.stack.pop();
                let kwargs = kwargs.into_map().expect("to have kwargs");
                let mut context = Context::new();
                let (component_def, component_chunk) = &self.tera.components[$name];
                let current_span: SpanRange = Some($span_idx..=$span_idx);

                for key in kwargs.keys() {
                    if !component_def.kwargs.contains_key(key.as_str().unwrap()) {
                        let kwargs_list = component_def.kwargs_list();
                        let kwargs_msg = if kwargs_list.is_empty() {
                            String::new()
                        } else {
                            format!(" Possible argument(s) are: {}",
                                kwargs_list.iter()
                                .map(|s| format!("`{}`", s))
                                .collect::<Vec<_>>()
                                .join(", "))
                        };
                        rendering_error!(format!("Argument `{key}` not found in definition.{kwargs_msg}"), current_span)
                    }
                }

                for (key, value) in &component_def.kwargs {
                    match kwargs.get(&Key::Str(key)) {
                        Some(kwarg_val) => {
                            if value.type_matches(&kwarg_val) {
                                context.insert_value(key.clone(), kwarg_val.clone());
                            } else {
                                // TODO: we need to pass the span of each element in the map somehow
                                // so we can point exactly where the issue is
                                rendering_error!(format!("Component argument `{key}` (type: `{}`) does not match expected type: `{}`", kwarg_val.name(), value.typ.unwrap().as_str()), current_span);
                            }
                        }
                        None => match &value.default {
                            Some(kwarg_val) => {
                                context.insert_value(key.clone(), kwarg_val.clone());
                            }
                            None => {
                                // Missing argument that doesn't have a default
                                let typ_msg = if let Some(t) = value.typ.and_then(|t| Some(t.as_str())) {
                                    format!("(type: `{t}`)")
                                } else {
                                    String::new()
                                };
                                rendering_error!(format!("Argument `{key}` {typ_msg} missing."), current_span)
                            }
                        },
                    }
                }

                if $has_body {
                    context.insert_value("body", state.stack.pop().0);
                }
                let val = self.render_component(&component_chunk, context)?;
                state
                    .stack
                    .push(Value::safe_string(&val), current_span);
            }};
        }

        while let Some((instr, _)) = state.chunk.expect("To have a chunk").get(ip) {
            // Current instruction index as span reference
            let current_ip = ip as u32;

            match instr {
                Instruction::LoadConst(v) => {
                    state.stack.push(v.clone(), Some(current_ip..=current_ip));
                }
                Instruction::LoadName(n) => state.load_name(n, current_ip),
                Instruction::LoadAttr(attr) => {
                    let (a, a_span) = state.stack.pop();
                    if a.is_undefined() {
                        rendering_error!(format!("Container is not defined"), a_span);
                    }
                    state
                        .stack
                        .push(a.get_attr(attr), Some(current_ip..=current_ip));
                }
                Instruction::LoadAttrOpt(attr) => {
                    let (a, _a_span) = state.stack.pop();
                    if a.is_undefined() || a.is_null() {
                        state
                            .stack
                            .push(Value::undefined(), Some(current_ip..=current_ip));
                    } else {
                        state
                            .stack
                            .push(a.get_attr(attr), Some(current_ip..=current_ip));
                    }
                }
                Instruction::BinarySubscript => {
                    let (subscript, subscript_span) = state.stack.pop();
                    let (val, val_span) = state.stack.pop();
                    if val.is_undefined() {
                        rendering_error!(format!("Container is not defined"), val_span);
                    }

                    let c_span = combine_spans(&val_span, &subscript_span);
                    match val.get_item(subscript) {
                        Ok(v) => {
                            state.stack.push(v, c_span);
                        }
                        Err(e) => {
                            rendering_error!(e.to_string(), subscript_span);
                        }
                    }
                }
                Instruction::BinarySubscriptOpt => {
                    let (subscript, subscript_span) = state.stack.pop();
                    let (val, val_span) = state.stack.pop();
                    if val.is_undefined() || val.is_null() {
                        state
                            .stack
                            .push(Value::undefined(), Some(current_ip..=current_ip));
                    } else {
                        let c_span = combine_spans(&val_span, &subscript_span);
                        match val.get_item(subscript) {
                            Ok(v) => {
                                state.stack.push(v, c_span);
                            }
                            Err(e) => {
                                rendering_error!(e.to_string(), subscript_span);
                            }
                        }
                    }
                }
                Instruction::Slice => {
                    let (step, _) = state.stack.pop();
                    let (end, _) = state.stack.pop();
                    let (start, _) = state.stack.pop();
                    let (val, val_span) = state.stack.pop();
                    if val.is_undefined() {
                        rendering_error!(format!("Container is not defined"), val_span);
                    }

                    // This returns an error if the value is not an array/string so we don't need to
                    // expand the span.
                    match val.slice(start.as_i128(), end.as_i128(), step.as_i128()) {
                        Ok(v) => {
                            state.stack.push(v, val_span);
                        }
                        Err(e) => {
                            rendering_error!(e.to_string(), val_span);
                        }
                    }
                }
                Instruction::SliceOpt => {
                    let (step, _) = state.stack.pop();
                    let (end, _) = state.stack.pop();
                    let (start, _) = state.stack.pop();
                    let (val, val_span) = state.stack.pop();
                    if val.is_undefined() {
                        state
                            .stack
                            .push(Value::undefined(), Some(current_ip..=current_ip));
                    } else {
                        // This returns an error if the value is not an array/string so we don't need to
                        // expand the span.
                        match val.slice(start.as_i128(), end.as_i128(), step.as_i128()) {
                            Ok(v) => {
                                state.stack.push(v, val_span);
                            }
                            Err(e) => {
                                rendering_error!(e.to_string(), val_span);
                            }
                        }
                    }
                }
                Instruction::WriteText(t) => {
                    if let Some(captured) = state.capture_buffers.last_mut() {
                        captured.write_all(t.as_bytes())?;
                    } else {
                        output.write_all(t.as_bytes())?;
                    }
                }
                Instruction::WriteTop => {
                    let (top, top_span) = state.stack.pop();
                    if top.is_undefined() {
                        rendering_error!(
                            format!("Tried to render a variable that is not defined"),
                            top_span
                        );
                    }

                    if !self.template.autoescape_enabled || top.is_safe() {
                        if let Some(captured) = state.capture_buffers.last_mut() {
                            top.format(captured)?;
                        } else {
                            top.format(output)?;
                        }
                    } else {
                        // Avoiding String as much as possible
                        // TODO: Add more benchmarks
                        let mut out: Vec<u8> = Vec::new();
                        top.format(&mut out)?;
                        if let Some(captured) = state.capture_buffers.last_mut() {
                            (self.tera.escape_fn)(&out, captured)?;
                        } else {
                            (self.tera.escape_fn)(&out, output)?;
                        }
                    }
                }
                Instruction::Set(name) => {
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
                Instruction::CallFunction(name) => {
                    let (kwargs, _) = state.stack.pop();
                    if name == "super" {
                        let current_block_name =
                            state.current_block_name.expect("no current block");
                        let (blocks, level) = state
                            .blocks
                            .remove(current_block_name)
                            .expect("no lineage found");
                        if blocks.len() == 1 {
                            rendering_error!(
                                format!("Tried to use super() in the top level block"),
                                Some(current_ip..=current_ip)
                            );
                        }

                        let block_chunk = blocks[level + 1];
                        let old_chunk = std::mem::replace(&mut state.chunk, Some(block_chunk));
                        state.blocks.insert(current_block_name, (blocks, level + 1));
                        let res = self.interpret(state, output);
                        state.chunk = old_chunk;
                        res?;
                        state.stack.push(Value::null(), None);
                    } else if let Some(f) = self.tera.functions.get(name.as_str()) {
                        let val = match f.call(Kwargs::new(kwargs.into_map().unwrap()), state) {
                            Ok(v) => v,
                            Err(err) => {
                                rendering_error!(format!("{err}"), Some(current_ip..=current_ip))
                            }
                        };

                        state.stack.push(val, Some(current_ip..=current_ip));
                    } else {
                        // TODO: we _should_ be able to track that at compile time
                        rendering_error!(
                            format!("This function is not registered in Tera"),
                            Some(current_ip..=current_ip)
                        )
                    }
                }
                Instruction::ApplyFilter(name) => {
                    if let Some(f) = self.tera.filters.get(name.as_str()) {
                        let (kwargs, _) = state.stack.pop();
                        let (value, value_span) = state.stack.pop();
                        let val =
                            match f.call(&value, Kwargs::new(kwargs.into_map().unwrap()), state) {
                                Ok(v) => v,
                                Err(err) => match err.kind {
                                    ErrorKind::InvalidArgument { .. } => {
                                        rendering_error!(format!("{err}"), value_span)
                                    }
                                    _ => rendering_error!(
                                        format!("{err}"),
                                        Some(current_ip..=current_ip)
                                    ),
                                },
                            };
                        state.stack.push(val, Some(current_ip..=current_ip));
                    } else {
                        // TODO: we _should_ be able to track that at compile time
                        rendering_error!(
                            format!("This filter is not registered in Tera"),
                            Some(current_ip..=current_ip)
                        )
                    }
                }
                Instruction::RunTest(name) => {
                    if let Some(f) = self.tera.tests.get(name.as_str()) {
                        let (kwargs, _) = state.stack.pop();
                        let (value, value_span) = state.stack.pop();
                        let val =
                            match f.call(&value, Kwargs::new(kwargs.into_map().unwrap()), state) {
                                Ok(v) => v,
                                Err(err) => match err.kind {
                                    ErrorKind::InvalidArgument { .. } => {
                                        rendering_error!(format!("{err}"), value_span)
                                    }
                                    _ => rendering_error!(
                                        format!("{err}"),
                                        Some(current_ip..=current_ip)
                                    ),
                                },
                            };

                        state.stack.push(val.into(), Some(current_ip..=current_ip));
                    } else {
                        // TODO: we _should_ be able to track that at compile time
                        rendering_error!(
                            format!("This test is not registered in Tera"),
                            Some(current_ip..=current_ip)
                        )
                    }
                }
                Instruction::RenderBodyComponent(name) => {
                    component!(name, current_ip, true);
                }
                Instruction::RenderInlineComponent(name) => {
                    component!(name, current_ip, false);
                }
                Instruction::RenderBlock(block_name) => {
                    let block_lineage = self.get_block_lineage(block_name)?;
                    let block_chunk = block_lineage[0];
                    let old_chunk = std::mem::replace(&mut state.chunk, Some(block_chunk));
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

                    if *is_key_value && !container.is_map() {
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
                        state.stack.push(Value::from(!for_loop.iterated()), None);
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
                    let c_span = combine_spans(&a_span, &b_span);
                    // TODO: we could push_str if `a` is a string
                    state.stack.push(Value::from(format!("{a}{b}")), c_span);
                }
                Instruction::In => {
                    let (container, container_span) = state.stack.pop();
                    let (needle, _) = state.stack.pop();
                    match container.contains(&needle) {
                        Ok(b) => {
                            state.stack.push(Value::from(b), None);
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
                // Combined instructions
                Instruction::LoadPath(path) => {
                    let chunk = state.chunk.expect("to have a chunk");
                    let mut val = state.get(&path[0]);
                    let num_attrs = path.len() - 1;
                    for (k, attr) in path[1..].iter().enumerate() {
                        if val.is_undefined() {
                            rendering_error!(
                                format!("Container is not defined"),
                                span: chunk.get_span_at(current_ip, k)
                            );
                        }
                        val = val.get_attr(attr);
                        // Only error on intermediate undefined, not the final result
                        if val.is_undefined() && k + 1 < num_attrs {
                            rendering_error!(
                                format!("Field `{}` is not defined", attr),
                                span: chunk.get_span_at(current_ip, k + 1)
                            );
                        }
                    }
                    state.stack.push(val, Some(current_ip..=current_ip));
                }
                Instruction::WritePath(path) => {
                    let mut val = state.get(&path[0]);
                    if val.is_undefined() {
                        let chunk = state.chunk.expect("to have a chunk");
                        rendering_error!(
                            format!("Field `{}` is not defined", path[0]),
                            span: chunk.get_span_at(current_ip, 0)
                        );
                    }
                    for (k, attr) in path[1..].iter().enumerate() {
                        val = val.get_attr(attr);
                        // Check if attribute access failed (returned undefined)
                        if val.is_undefined() {
                            let chunk = state.chunk.expect("to have a chunk");
                            rendering_error!(
                                format!("Field `{}` is not defined", attr),
                                span: chunk.get_span_at(current_ip, k + 1)
                            );
                        }
                    }

                    if !self.template.autoescape_enabled || val.is_safe() {
                        if let Some(captured) = state.capture_buffers.last_mut() {
                            val.format(captured)?;
                        } else {
                            val.format(output)?;
                        }
                    } else {
                        let mut out: Vec<u8> = Vec::new();
                        val.format(&mut out)?;
                        if let Some(captured) = state.capture_buffers.last_mut() {
                            (self.tera.escape_fn)(&out, captured)?;
                        } else {
                            (self.tera.escape_fn)(&out, output)?;
                        }
                    }
                }
            }

            ip += 1;
        }

        Ok(())
    }

    fn render_component(&self, chunk: &Chunk, context: Context) -> TeraResult<String> {
        // TODO: need to keep around the filename the component is defined in to fetch it for errors
        let vm = Self {
            tera: self.tera,
            template: self.template,
        };

        let mut state = State::new_with_chunk(&context, chunk);
        state.filters = Some(&self.tera.filters);
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
        let mut include_state = State::new_with_chunk(state.context, &tpl.chunk);
        include_state.include_parent = Some(state);
        include_state.filters = Some(&self.tera.filters);
        vm.interpret(&mut include_state, output)?;
        Ok(())
    }

    pub(crate) fn render(&mut self, context: &Context) -> TeraResult<String> {
        let mut output = Vec::with_capacity(self.template.size_hint());
        self.render_to(context, &mut output)?;
        Ok(String::from_utf8(output)?)
    }

    pub(crate) fn render_to(
        &mut self,
        context: &Context,
        mut output: impl Write,
    ) -> TeraResult<()> {
        // TODO(perf): can we optimize this at the bytecode level to avoid hashmap lookups?
        let chunk = if let Some(base_tpl_name) = self.template.parents.first() {
            let tpl = self.tera.get_template(base_tpl_name)?;
            &tpl.chunk
        } else {
            &self.template.chunk
        };
        let mut state = State::new_with_chunk(context, chunk);
        state.filters = Some(&self.tera.filters);
        self.interpret(&mut state, &mut output)
    }
}
