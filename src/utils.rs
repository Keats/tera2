use std::fmt;
use std::fmt::Formatter;
use std::ops::{Deref, Range};

#[derive(Clone, PartialEq)]
pub struct Spanned<T: fmt::Debug> {
    node: Box<T>,
    span: Span,
}

impl<T: fmt::Debug> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self {
            node: Box::new(node),
            span,
        }
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub(crate) fn node(&self) -> &T {
        &self.node
    }
}

impl<T: fmt::Debug> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", &self.node)
    }
}

#[derive(Clone, PartialEq, Default)]
pub struct Span {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
    pub range: Range<usize>,
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            " @ {}:{}-{}:{} ({:?})",
            self.start_line, self.start_col, self.end_line, self.end_col, self.range,
        )
    }
}

impl Span {
    pub fn expand(&mut self, other: &Span) {
        self.end_line = other.end_line;
        self.end_col = other.end_col;
        self.range = self.range.start..other.range.end;
    }
}