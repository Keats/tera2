use crate::utils::Span;
use crate::Value;

#[derive(Debug, Eq, PartialEq)]
pub(crate) struct Stack<'tera> {
    values: Vec<(Value, &'tera Option<Span>)>,
}

impl<'t> Stack<'t> {
    pub(crate) fn new() -> Self {
        Self {
            // TODO: check the size of the stack of an average template
            values: Vec::with_capacity(64),
        }
    }

    pub(crate) fn push(&mut self, val: Value, span: &'t Option<Span>) {
        self.values.push((val, span));
    }

    pub(crate) fn pop(&mut self) -> (Value, &'t Option<Span>) {
        self.values.pop().expect("to have a value")
    }

    pub(crate) fn peek(&mut self) -> &(Value, &'t Option<Span>) {
        self.values.last().expect("to peek a value")
    }
}
