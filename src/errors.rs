//! The Tera error type, with optional nice terminal error reporting.

use std::fmt::{self};
use std::ops::Range;

use crate::utils::Span;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    // Both lexer and parser errors
    SyntaxError,
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    message: String,
    filename: Option<String>,
    span: Option<Span>,
    // If the error comes from some third party libs, do we need that?
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
    // We need the source of each file involved in the error as well
    labels: Vec<(String, Range<usize>)>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!("Display for error")
    }
}
impl Error {
    pub fn new(kind: ErrorKind, message: &str) -> Self {
        Self {
            kind,
            message: message.to_owned(),
            filename: None,
            span: None,
            source: None,
            labels: Vec::new(),
        }
    }

    pub(crate) fn set_filename(&mut self, filename: &str) {
        self.filename = Some(filename.to_owned());
    }

    pub(crate) fn set_span(&mut self, span: Span) {
        self.span = Some(span);
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source.as_ref().map(|e| e.as_ref() as _)
    }
}

pub type TeraResult<T> = Result<T, Error>;
