use std::error::Error;
use std::fmt;

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::lexer::Token;
use crate::Spanned;

#[derive(Clone, Debug, PartialEq)]
pub enum ParsingError {
    UnexpectedToken(Token, Token),
}

pub type SpannedParsingError = Spanned<ParsingError>;
pub type ParsingResult<T> = Result<T, SpannedParsingError>;

impl SpannedParsingError {
    pub fn report(&self) -> Diagnostic<()> {
        match self.node {
            ParsingError::UnexpectedToken(expected, actual) => Diagnostic::error()
                .with_message("Unexpected token found")
                .with_labels(vec![Label::primary((), self.span.start..self.span.end)
                    .with_message(format!(
                        "expected `{}`, found `{}`",
                        expected, actual
                    ))]),
        }
    }
}

// TODO
impl fmt::Display for SpannedParsingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SpannedParsingError is here!")
    }
}

impl Error for SpannedParsingError {}
