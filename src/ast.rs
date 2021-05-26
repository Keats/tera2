use std::collections::HashMap;
use std::fmt;
use std::string::String as StdString;

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

impl Expression {
    pub fn constant_fold_and_validate(self) -> Self {
        use Expression::*;

        let fold_expr_array = |exprs: Vec<Expression>| -> Vec<Expression> {
            let mut folded_vals = Vec::with_capacity(exprs.len());
            for e in exprs {
                folded_vals.push(e.constant_fold_and_validate());
            }
            folded_vals
        };

        let fold_expr_map =
            |exprs: HashMap<StdString, Expression>| -> HashMap<StdString, Expression> {
                let mut folded_vals = HashMap::with_capacity(exprs.len());
                for (name, e) in exprs {
                    folded_vals.insert(name, e.constant_fold_and_validate());
                }
                folded_vals
            };

        match self {
            String(_) | Int(_) | Float(_) | Bool(_) | Ident(_) => self,
            Array(exprs) => Array(fold_expr_array(exprs)),
            Test(name, exprs) => Test(name, fold_expr_array(exprs)),
            MacroCall(namespace, name, kwargs) => MacroCall(namespace, name, fold_expr_map(kwargs)),
            Function(name, kwargs) => Function(name, fold_expr_map(kwargs)),
            Expr(op, exprs) => {
                // match op {
                //     Operator::StrConcat => {
                //
                //     }
                // }
                Expr(op, exprs)
            }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expression::*;

        match self {
            String(i) => write!(f, "{}", i),
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
