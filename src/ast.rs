use std::collections::HashMap;
use std::fmt;

use crate::lexer::Operator;

/// An expression is the node found in variable block, kwargs and conditions.
#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs)]
pub enum Expression {
    Str(String),
    Integer(i64),
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
    // TODO: probably remove that and inline it
    pub(crate) fn into_array(self) -> Vec<Expression> {
        match self {
            Expression::Array(vals) => vals,
            _ => panic!("Called as_array on a non array value"),
        }
    }

    pub(crate) fn can_be_iterated_on(&self) -> bool {
        use Expression::*;
        matches!(
            self,
            Str(_) | Ident(_) | Array(_) | Function(_, _) | Expr(_, _)
        )
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expression::*;

        match self {
            Str(i) => write!(f, "'{}'", i),
            Integer(i) => write!(f, "{}", i),
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

/// Set a variable in the context `{% set val = "hey" %}`
#[derive(Clone, Debug, PartialEq)]
pub struct Set {
    /// The name for that value in the context
    pub key: String,
    /// The value to assign
    pub value: Expression,
    /// Whether we want to set the variable globally or locally
    /// global_set is only useful in loops
    pub global: bool,
}

/// A block definition
#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    /// The block name
    pub name: String,
    /// The block content
    pub body: Vec<Node>,
}

/// An if/elif/else condition with their respective body
#[derive(Clone, Debug, PartialEq)]
pub struct If {
    /// First item if the if, all the ones after are elif
    pub conditions: Vec<(Expression, Vec<Node>)>,
    /// The optional `else` block
    pub otherwise: Vec<Node>,
}

/// A filter section node `{{ filter name(param="value") }} content {{ endfilter }}`
#[derive(Clone, Debug, PartialEq)]
pub struct FilterSection {
    pub name: String,
    pub kwargs: HashMap<String, Expression>,
    /// The filter body
    pub body: Vec<Node>,
}

/// A Macro definition `{% macro hello() %}...{% endmacro %}`
#[derive(Clone, Debug, PartialEq)]
pub struct MacroDefinition {
    pub name: String,
    /// The args for that macro: name -> optional default value
    pub kwargs: HashMap<String, Option<Expression>>,
    pub body: Vec<Node>,
}

/// A forloop: can be over values or key/values
#[derive(Clone, Debug, PartialEq)]
pub struct ForLoop {
    /// Name of the key in the loop (only when iterating on map-like objects)
    pub key: Option<String>,
    /// Name of the local variable for the value in the loop
    pub value: String,
    /// Expression being iterated on
    pub container: Expression,
    /// What's in the forloop itself
    pub body: Vec<Node>,
    /// The body to execute in case of an empty object in the `{% for .. %}{% else %}{% endfor %}` construct
    pub otherwise: Vec<Node>,
}

/// All Tera nodes that can be encountered
#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Text(String),
    VariableBlock(Expression),
    Set(Set),
    Raw(String),
    Include {
        files: Vec<String>,
        ignore_missing: bool,
    },
    Block(Block),
    Super,
    If(If),
    ForLoop(ForLoop),
    Continue,
    Break,
    FilterSection(FilterSection),
}
