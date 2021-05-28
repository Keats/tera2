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
    InvalidInclude,
    DuplicateExtend(String),
    InvalidExpression(String),
    UnexpectedEof,
}

impl ParsingError {
    pub(crate) fn message(&self) -> &str {
        use ParsingError::*;

        match self {
            UnexpectedToken(_, _) => "Unexpected token found",
            UnexpectedOperator(_, _) => "Unexpected operator found",
            InvalidExpression(_) => "Invalid expression",
            InvalidInclude => "InvalidInclude",
            DuplicateExtend(_) => "Several extend tags found",
            UnexpectedEof => "Unexpected end of template",
        }
    }
}

pub type SpannedParsingError = Spanned<ParsingError>;
pub type ParsingResult<T> = Result<T, SpannedParsingError>;

impl SpannedParsingError {
    pub fn report(&self) -> Diagnostic<()> {
        let get_token_formatted = |t: &Token| -> String {
            match t {
                Token::Error => "unexpected characters".to_owned(),
                Token::Integer(_) => "an integer".to_owned(),
                Token::Float(_) => "a float".to_owned(),
                Token::Bool(_) => "a boolean".to_owned(),
                Token::Ident => format!("{}, ", t),
                Token::String => format!("{}, ", t),
                t => format!("`{}`, ", t),
            }
        };

        let msg = match &self.node {
            ParsingError::UnexpectedToken(actual, ref expected) => {
                let actual_fmt = get_token_formatted(&actual)
                    .trim()
                    .trim_end_matches(',')
                    .to_owned();

                let mut options = String::new();
                for t in expected {
                    options.push_str(&get_token_formatted(t));
                }
                if expected.is_empty() {
                    format!("found {}", actual_fmt)
                } else if expected.len() == 1 {
                    format!(
                        "expected {} but found {}",
                        options.trim().trim_end_matches(','),
                        actual_fmt
                    )
                } else {
                    format!(
                        "expected one of: {} but found {}",
                        options.trim().trim_end_matches(','),
                        actual_fmt,
                    )
                }
            }
            ParsingError::UnexpectedOperator(found, ref available) => {
                let mut ops = String::new();
                for op in available {
                    ops.push_str(&format!("`{}`, ", op));
                }
                format!(
                    "found `{}` but only {} can be used here",
                    found,
                    ops.trim().trim_end_matches(',')
                )
            }
            ParsingError::InvalidInclude => "values in an include array must be strings".to_owned(),
            ParsingError::InvalidExpression(ref msg) => msg.to_owned(),
            ParsingError::DuplicateExtend(ref msg) => msg.to_owned(),
            ParsingError::UnexpectedEof => String::new(),
        };

        let mut label = Label::primary((), self.range.start..self.range.end);
        if !msg.is_empty() {
            label = label.with_message(msg);
        }
        Diagnostic::error()
            .with_message(self.node.message())
            .with_labels(vec![label])
    }
}

// TODO
impl fmt::Display for SpannedParsingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SpannedParsingError is here!")
    }
}

impl Error for SpannedParsingError {}
