use crate::parsing::Chunk;
use crate::vm::for_loop::ForLoop;
use crate::vm::stack::Stack;
use crate::{Context, Value};

use crate::template::Template;
use std::collections::BTreeMap;

/// The state of the interpreter.
/// We pass it around rather than put it on the VM to avoid multiple borrow issues
/// when dealing with inheritance.
#[derive(Debug, PartialEq)]
pub(crate) struct State<'tera> {
    pub(crate) stack: Stack,
    pub(crate) chunk: &'tera Chunk,
    pub(crate) for_loops: Vec<ForLoop>,
    /// Any variables with {% set %} outside a for loop or {% set_global %} will be stored here
    /// Locals set in a for loop are set in `for_loops`
    set_variables: BTreeMap<String, Value>,
    pub(crate) context: &'tera Context,
    /// To handle the capture instructions
    pub(crate) capture_buffers: Vec<Vec<u8>>,
    /// Used in includes only
    pub(crate) include_parent: Option<&'tera State<'tera>>,

    /// (block name, (all_chunks, level))
    pub(crate) blocks: BTreeMap<&'tera str, (Vec<&'tera Chunk>, usize)>,
    pub(crate) current_block_name: Option<&'tera str>,
}

impl<'t> State<'t> {
    pub(crate) fn new(context: &'t Context, chunk: &'t Chunk) -> Self {
        Self {
            stack: Stack::new(),
            for_loops: Vec::with_capacity(4),
            set_variables: BTreeMap::new(),
            context,
            chunk,
            capture_buffers: Vec::with_capacity(4),
            include_parent: None,
            blocks: BTreeMap::new(),
            current_block_name: None,
        }
    }

    pub(crate) fn current_tpl_name(&self) -> &str {
        &self.chunk.name
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
    /// 4. return undefined
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

        // TODO: we do need undefined to differentiate from null do we
        // TODO: in practice we want to only return undefined if it's in a if/condition and error
        // TODO: otherwise like in Tera v1? To consider. In those case we could emit a different
        // TODO: instruction that will not error if not found and make this return an Option
        if let Some(parent) = self.include_parent {
            parent.get(name)
        } else {
            Value::Null
        }
    }

    pub(crate) fn load_name(&mut self, name: &str) {
        self.stack.push(self.get(name));
    }
}
