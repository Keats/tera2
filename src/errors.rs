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
    /// Both lexer and parser errors
    SyntaxError(SyntaxError),
    /// A loop was found while looking up the inheritance chain
    CircularExtend {
        /// Name of the template with the loop
        tpl: String,
        /// All the parents templates we found so far
        inheritance_chain: Vec<String>,
    },
    /// A template is extending a template that wasn't found in the Tera instance
    MissingParent {
        /// The template we are currently looking at
        current: String,
        /// The missing template
        parent: String,
    },
    /// A template is calling a macro namespace that is not loaded
    NamespaceNotLoaded {
        /// Name of the template with the issue
        tpl: String,
        /// The namespace causing problems
        namespace: String,
    },
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::SyntaxError(s) => write!(f, "{s}"),
            ErrorKind::CircularExtend {
                ref tpl,
                ref inheritance_chain,
            } => write!(
                f,
                "Circular extend detected for template '{}'. Inheritance chain: `{:?}`",
                tpl, inheritance_chain
            ),
            ErrorKind::MissingParent {
                ref current,
                ref parent,
            } => write!(
                f,
                "Template '{}' is inheriting from '{}', which doesn't exist or isn't loaded.",
                current, parent
            ),
            ErrorKind::NamespaceNotLoaded {
                ref tpl,
                ref namespace,
            } => write!(
                f,
                "Template '{}' is trying to use namespace `{}` which is not loaded",
                tpl, namespace
            ),
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

    pub(crate) fn syntax_error(message: String, span: &Span) -> Self {
        Self {
            kind: ErrorKind::SyntaxError(SyntaxError::new(message, span)),
            source: None,
        }
    }

    pub(crate) fn circular_extend(tpl: impl ToString, inheritance_chain: Vec<String>) -> Self {
        Self {
            kind: ErrorKind::CircularExtend {
                tpl: tpl.to_string(),
                inheritance_chain,
            },
            source: None,
        }
    }

    pub(crate) fn missing_parent(current: impl ToString, parent: impl ToString) -> Self {
        Self {
            kind: ErrorKind::MissingParent {
                current: current.to_string(),
                parent: parent.to_string(),
            },
            source: None,
        }
    }

    pub(crate) fn namespace_not_loaded(tpl: impl ToString, namespace: impl ToString) -> Self {
        Self {
            kind: ErrorKind::NamespaceNotLoaded {
                tpl: tpl.to_string(),
                namespace: namespace.to_string(),
            },
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
