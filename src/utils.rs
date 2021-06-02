use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub range: Range<usize>,
}

impl<T> Spanned<T> {
    pub fn new(node: T, range: Range<usize>) -> Self {
        Self { node, range }
    }
}
