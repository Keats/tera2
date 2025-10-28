use crate::parsing::Chunk;
use crate::vm::for_loop::ForLoop;
use crate::vm::stack::Stack;
use crate::{Context, Value};

use crate::utils::Span;
use std::collections::BTreeMap;

/// Special string indicating request to dump context
static MAGICAL_DUMP_VAR: &str = "__tera_context";

/// The state of the interpreter.
/// We pass it around rather than put it on the VM to avoid multiple borrow issues
/// when dealing with inheritance.
#[derive(Debug)]
pub struct State<'tera> {
    pub(crate) stack: Stack<'tera>,
    /// It can be None for things like tests as we don't expose Chunk outside of the crate
    pub(crate) chunk: Option<&'tera Chunk>,
    pub(crate) for_loops: Vec<ForLoop>,
    /// Any variables with {% set %} outside a for loop or {% set_global %} will be stored here
    /// Locals set in a for loop are set in `for_loops`
    pub(crate) set_variables: BTreeMap<String, Value>,
    pub(crate) context: &'tera Context,
    /// To handle the capture instructions
    pub(crate) capture_buffers: Vec<Vec<u8>>,
    /// Used in includes only
    pub(crate) include_parent: Option<&'tera State<'tera>>,

    /// (block name, (all_chunks, level))
    pub(crate) blocks: BTreeMap<&'tera str, (Vec<&'tera Chunk>, usize)>,
    pub(crate) current_block_name: Option<&'tera str>,
    /// Body chunk index for lazy component body rendering
    pub(crate) body_chunk_idx: Option<usize>,
    /// Parent context for body rendering (to access parent template variables)
    pub(crate) body_parent_context: Option<&'tera Context>,
    /// Body chunks where the body to be rendered is stored (from caller)
    pub(crate) caller_body_chunks: Option<&'tera Vec<Chunk>>,
    /// Component's own body chunks (for nested component calls within this component)
    pub(crate) component_body_chunks: Option<&'tera Vec<Chunk>>,
}

impl<'t> State<'t> {
    pub(crate) fn new_with_chunk(context: &'t Context, chunk: &'t Chunk) -> Self {
        let mut s = Self::new(context);
        s.chunk = Some(chunk);
        s
    }

    pub fn new(context: &'t Context) -> Self {
        Self {
            stack: Stack::new(),
            for_loops: Vec::with_capacity(4),
            set_variables: BTreeMap::new(),
            context,
            chunk: None,
            capture_buffers: Vec::with_capacity(4),
            include_parent: None,
            blocks: BTreeMap::new(),
            current_block_name: None,
            body_chunk_idx: None,
            body_parent_context: None,
            caller_body_chunks: None,
            component_body_chunks: None,
        }
    }

    pub(crate) fn current_tpl_name(&self) -> &str {
        &self.chunk.expect("to have a chunk").name
    }

    pub(crate) fn store_local(&mut self, name: &str, value: Value) {
        if let Some(forloop) = self.for_loops.last_mut() {
            forloop.store(name, value);
        } else {
            self.store_global(name, value);
        }
    }

    pub(crate) fn store_global(&mut self, name: &str, value: Value) {
        self.set_variables.insert(name.to_string(), value);
    }

    /// Loads the value with the current name on the stack
    /// It goes in the following order for scopes:
    /// 1. All loops from the inner to the outer
    /// 2. set_variables
    /// 3. self.context
    /// 4. return Value::Undefined
    pub(crate) fn get(&self, name: &str) -> Value {
        for forloop in &self.for_loops {
            if let Some(v) = forloop.get(name) {
                return v;
            }
        }

        if let Some(val) = self.set_variables.get(name) {
            return val.clone();
        }

        if let Some(val) = self.context.data.get(name) {
            return val.clone();
        }

        if let Some(parent) = self.include_parent {
            parent.get(name)
        } else {
            Value::undefined()
        }
    }

    pub fn get_from_path(&self, path: &str) -> Value {
        if let Some((start, rest)) = path.split_once('.') {
            let base_value = self.get(start);
            base_value.get_from_path(rest)
        } else {
            self.get(path)
        }
    }

    fn dump_context(&self) -> Value {
        let mut context = crate::HashMap::new();
        for (k, v) in &self.context.data {
            context.insert(k.to_string(), v.clone());
        }
        context.extend(self.set_variables.clone());

        for forloop in &self.for_loops {
            context.extend(forloop.context.clone());
        }

        context.into()
    }

    pub(crate) fn load_name<'tera: 't>(&mut self, name: &str, span: &'tera Option<Span>) {
        if name == MAGICAL_DUMP_VAR {
            self.stack.push(self.dump_context(), None);
        } else {
            self.stack
                .push_borrowed(self.get(name), span.as_ref().unwrap());
        }
    }
}
