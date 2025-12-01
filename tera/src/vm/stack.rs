use crate::Value;
use std::ops::RangeInclusive;

/// A range of span indices (inclusive) for error reporting.
/// When an error occurs, spans can be looked up from the chunk and expanded.
pub(crate) type SpanRange = Option<RangeInclusive<u32>>;

/// Combine two span ranges into one that covers both
#[inline]
pub(crate) fn combine_spans(first: &SpanRange, second: &SpanRange) -> SpanRange {
    match (first, second) {
        (Some(a), Some(b)) => Some(*a.start().min(b.start())..=*a.end().max(b.end())),
        (Some(a), None) => Some(a.clone()),
        (None, Some(b)) => Some(b.clone()),
        (None, None) => None,
    }
}

#[derive(Debug, Eq, PartialEq, Default)]
pub(crate) struct Stack {
    values: Vec<(Value, SpanRange)>,
}

impl Stack {
    pub(crate) fn new() -> Self {
        Self {
            // TODO: check the size of the stack of an average template
            values: Vec::with_capacity(64),
        }
    }

    pub(crate) fn push(&mut self, val: Value, span: SpanRange) {
        self.values.push((val, span));
    }

    pub(crate) fn pop(&mut self) -> (Value, SpanRange) {
        self.values.pop().expect("to have a value")
    }

    pub(crate) fn peek(&mut self) -> &(Value, SpanRange) {
        self.values.last().expect("to peek a value")
    }
}
