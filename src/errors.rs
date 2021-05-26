use std::error::Error;
use std::fmt;

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::lexer::{Operator, Token};
use crate::Spanned;

#[derive(Clone, Debug, PartialEq)]
pub enum ParsingError {
    // token we got, list of token we were supposed to get if known
    UnexpectedToken(Token, Vec<Token>),
    // operator found, list of usable operators
    UnexpectedOperator(Operator, Vec<Operator>),
    UnexpectedEof,
    // TODO
    // InvalidExpression,
}

pub type SpannedParsingError = Spanned<ParsingError>;
pub type ParsingResult<T> = Result<T, SpannedParsingError>;

impl SpannedParsingError {
    pub fn report(&self) -> Diagnostic<()> {
        let get_token_formatted = |t: &Token| -> String {
            match t {
                Token::Error => "unexpected characters".to_owned(),
                Token::Ident => format!("{}, ", t),
                Token::String => format!("{}, ", t),
                t => format!("`{}`, ", t),
            }
        };

        match self.node {
            ParsingError::UnexpectedToken(actual, ref expected) => {
                let actual_fmt = get_token_formatted(&actual)
                    .trim()
                    .trim_end_matches(',')
                    .to_owned();
                let msg = if expected.is_empty() {
                    format!("found {}", actual_fmt)
                } else if expected.len() == 1 {
                    format!("expected `{}` but found {}", expected[0], actual_fmt)
                } else {
                    let mut options = String::new();
                    for t in expected {
                        options.push_str(&get_token_formatted(t));
                    }
                    format!(
                        "expected one of: {} but found {}",
                        options.trim().trim_end_matches(','),
                        actual_fmt,
                    )
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
