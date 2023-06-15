use crate::Value;

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Stack {
    values: Vec<Value>,
}

impl Stack {
    pub(crate) fn new() -> Self {
        Self {
            // TODO: check the size of the stack of an average template
            values: Vec::with_capacity(64),
        }
    }

    pub(crate) fn push(&mut self, val: Value) {
        self.values.push(val);
    }

    pub(crate) fn pop(&mut self) -> Value {
        self.values.pop().expect("to have a value")
    }

    pub(crate) fn peek(&mut self) -> &Value {
        self.values.last().expect("to peek a value")
    }
}
