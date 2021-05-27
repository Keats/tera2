use std::collections::HashMap;
use std::fmt;

use crate::lexer::Operator;

/// An expression is the node found in variable block, kwargs and conditions.
#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs)]
pub enum Expression {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Ident(String),
    Array(Vec<Expression>),
    // name, args
    Test(String, Vec<Expression>),
    // namespace, name, kwargs
    MacroCall(String, String, HashMap<String, Expression>),
    // name, kwargs
    Function(String, HashMap<String, Expression>),
    Expr(Operator, Vec<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expression::*;

        match self {
            String(i) => write!(f, "'{}'", i),
            Int(i) => write!(f, "{}", i),
            Float(i) => write!(f, "{}", i),
            Ident(i) => write!(f, "{}", i),
            Bool(i) => write!(f, "{}", i),
            Array(vals) => {
                write!(f, "[")?;
                for (i, s) in vals.iter().enumerate() {
                    if i == vals.len() - 1 {
                        write!(f, "{}", s)?
                    } else {
                        write!(f, "{}, ", s)?
                    }
                }
                write!(f, "]")
            }
            Test(name, rest) => {
                write!(f, "{}", name)?;

                if !rest.is_empty() {
                    write!(f, "{{",)?;
                    for (i, s) in rest.iter().enumerate() {
                        if i == rest.len() - 1 {
                            write!(f, "{}", s)?
                        } else {
                            write!(f, "{}, ", s)?
                        }
                    }
                    write!(f, "}}",)?;
                }
                Ok(())
            }
            MacroCall(namespace, name, kwargs) => {
                write!(f, "{}::{}", namespace, name)?;
                write!(f, "{{",)?;
                let mut keys = kwargs.keys().collect::<Vec<_>>();
                keys.sort();
                for (i, k) in keys.iter().enumerate() {
                    if i == kwargs.len() - 1 {
                        write!(f, "{}={}", k, kwargs[*k])?
                    } else {
                        write!(f, "{}={}, ", k, kwargs[*k])?
                    }
                }
                write!(f, "}}",)?;
                Ok(())
            }
            Function(name, kwargs) => {
                write!(f, "{}", name)?;
                write!(f, "{{",)?;
                let mut keys = kwargs.keys().collect::<Vec<_>>();
                keys.sort();
                for (i, k) in keys.iter().enumerate() {
                    if i == kwargs.len() - 1 {
                        write!(f, "{}={}", k, kwargs[*k])?
                    } else {
                        write!(f, "{}={}, ", k, kwargs[*k])?
                    }
                }
                write!(f, "}}",)?;
                Ok(())
            }
            Expr(op, rest) => {
                write!(f, "({}", op)?;
                for s in rest {
                    write!(f, " {}", s)?
                }
                write!(f, ")")
            }
        }
    }
}

/// All Tera nodes that can be encountered
#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Text(String),
    Expression(Expression),
}
