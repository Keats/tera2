use crate::utils::Span;
use crate::Value;
use std::borrow::Cow;

#[derive(Debug, Eq, PartialEq, Default)]
pub(crate) struct Stack<'t> {
    values: Vec<(Value, Option<Cow<'t, Span>>)>,
}

impl<'t> Stack<'t> {
    pub(crate) fn new() -> Self {
        Self {
            // TODO: check the size of the stack of an average template
            values: Vec::with_capacity(64),
        }
    }

    pub(crate) fn push(&mut self, val: Value, span: Option<Cow<'t, Span>>) {
        self.values.push((val, span));
    }

    pub(crate) fn push_borrowed(&mut self, val: Value, span: &'t Span) {
        self.values.push((val, Some(Cow::Borrowed(span))));
    }

    pub(crate) fn pop(&mut self) -> (Value, Option<Cow<'t, Span>>) {
        self.values.pop().expect("to have a value")
    }

    pub(crate) fn peek(&mut self) -> &(Value, Option<Cow<'t, Span>>) {
        self.values.last().expect("to peek a value")
    }
}
