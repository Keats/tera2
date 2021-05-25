use std::error::Error;
use std::fmt;

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::lexer::{Operator, Token};
use crate::Spanned;

#[derive(Clone, Debug, PartialEq)]
pub enum ParsingError {
    // token we got, token we were supposed to get
    UnexpectedToken(Token, Option<Token>),
    // operator found, list of usable operators
    UnexpectedOperator(Operator, Vec<Operator>),
    UnexpectedEof,
}

pub type SpannedParsingError = Spanned<ParsingError>;
pub type ParsingResult<T> = Result<T, SpannedParsingError>;

impl SpannedParsingError {
    pub fn report(&self) -> Diagnostic<()> {
        match self.node {
            ParsingError::UnexpectedToken(actual, expected) => {
                let msg = if let Some(e) = expected {
                    format!("expected `{}`, found `{}`", e, actual)
                } else {
                    format!("found `{}`", actual)
                };
                Diagnostic::error()
                    .with_message("Unexpected token found")
                    .with_labels(vec![
                        Label::primary((), self.range.start..self.range.end).with_message(msg)
                    ])
            }
            ParsingError::UnexpectedOperator(found, ref available) => {
                let mut ops = String::new();
                for op in available {
                    ops.push_str(&format!("`{}`, ", op));
                }
                let msg = format!(
                    "found `{}` but only {} can be used here",
                    found,
                    ops.trim().trim_end_matches(',')
                );
                Diagnostic::error()
                    .with_message("Unexpected operator found")
                    .with_labels(vec![
                        Label::primary((), self.range.start..self.range.end).with_message(msg)
                    ])
            }
            ParsingError::UnexpectedEof => Diagnostic::error()
                .with_message("Unexpected end of template")
                .with_labels(vec![Label::primary((), self.range.start..self.range.end)]),
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
