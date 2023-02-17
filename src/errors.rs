//! The Tera error type, with optional nice terminal error reporting.
use std::fmt::{self};

use crate::utils::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxError {
    message: String,
    pub(crate) filename: String,
    span: Span,
}

impl SyntaxError {
    pub fn new(message: String, span: &Span) -> Self {
        Self {
            message,
            filename: String::new(),
            span: span.clone(),
        }
    }

    pub fn unexpected_end_of_input(span: &Span) -> Self {
        Self::new("Unexpected end of input".to_string(), span)
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error: {}", &self.message)?;
        // TODO: handle cases without filenames
        write!(f, "\n  --> {} {}", &self.filename, &self.span)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    // Both lexer and parser errors
    SyntaxError(SyntaxError),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::SyntaxError(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    // If the error comes from some third party libs, todo we need that?
    pub(crate) source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
        Self { kind, source: None }
    }

    pub fn new_syntax_error(message: String, span: &Span) -> Self {
        Self {
            kind: ErrorKind::SyntaxError(SyntaxError::new(message, span)),
            source: None,
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source.as_ref().map(|e| e.as_ref() as _)
    }
}

pub type TeraResult<T> = Result<T, Error>;
