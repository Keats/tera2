use std::collections::HashMap;
use std::fmt;

use crate::parser::lexer::Operator;
use crate::utils::Spanned;

/// The various possible literals in variable blocks/squared brackets
#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs)]
pub enum Literal {
    Str(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
}

/// An expression is the node found in variable block, kwargs and conditions.
#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs)]
pub enum Expression {
    Str(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
    Ident(String),
    Array(Vec<SpannedExpression>),
    Test(Test),
    MacroCall(MacroCall),
    FunctionCall(FunctionCall),
    Expr(Operator, Vec<SpannedExpression>),
}

pub type SpannedExpression = Spanned<Expression>;

#[derive(Clone, Debug, PartialEq)]
pub struct Test {
    pub name: String,
    pub args: Vec<SpannedExpression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MacroCall {
    pub namespace: String,
    pub name: String,
    pub kwargs: HashMap<String, SpannedExpression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    pub name: String,
    pub kwargs: HashMap<String, SpannedExpression>,
}

impl SpannedExpression {
    pub(crate) fn can_be_iterated_on(&self) -> bool {
        use Expression::*;
        matches!(
            self.node,
            Str(_) | Ident(_) | Array(_) | FunctionCall(_) | Expr(_, _)
        )
    }
}

impl fmt::Display for SpannedExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expression::*;

        match &self.node {
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
            Test(t) => {
                write!(f, "{}", t.name)?;

                if !t.args.is_empty() {
                    write!(f, "{{",)?;
                    for (i, s) in t.args.iter().enumerate() {
                        if i == t.args.len() - 1 {
                            write!(f, "{}", s)?
                        } else {
                            write!(f, "{}, ", s)?
                        }
                    }
                    write!(f, "}}",)?;
                }
                Ok(())
            }
            MacroCall(mc) => {
                write!(f, "{}::{}", mc.namespace, mc.name)?;
                write!(f, "{{",)?;
                let mut keys = mc.kwargs.keys().collect::<Vec<_>>();
                keys.sort();
                for (i, k) in keys.iter().enumerate() {
                    if i == mc.kwargs.len() - 1 {
                        write!(f, "{}={}", k, mc.kwargs[*k])?
                    } else {
                        write!(f, "{}={}, ", k, mc.kwargs[*k])?
                    }
                }
                write!(f, "}}",)?;
                Ok(())
            }
            FunctionCall(fc) => {
                write!(f, "{}", fc.name)?;
                write!(f, "{{",)?;
                let mut keys = fc.kwargs.keys().collect::<Vec<_>>();
                keys.sort();
                for (i, k) in keys.iter().enumerate() {
                    if i == fc.kwargs.len() - 1 {
                        write!(f, "{}={}", k, fc.kwargs[*k])?
                    } else {
                        write!(f, "{}={}, ", k, fc.kwargs[*k])?
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
    pub value: SpannedExpression,
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
    pub conditions: Vec<(SpannedExpression, Vec<Node>)>,
    /// The optional `else` block
    pub otherwise: Vec<Node>,
}

/// A filter section node `{{ filter name(param="value") }} content {{ endfilter }}`
#[derive(Clone, Debug, PartialEq)]
pub struct FilterSection {
    pub name: String,
    pub kwargs: HashMap<String, SpannedExpression>,
    /// The filter body
    pub body: Vec<Node>,
}

/// A Macro definition `{% macro hello() %}...{% endmacro %}`
#[derive(Clone, Debug, PartialEq)]
pub struct MacroDefinition {
    pub name: String,
    /// The args for that macro: name -> optional default value
    pub kwargs: HashMap<String, Option<SpannedExpression>>,
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
    pub container: SpannedExpression,
    /// What's in the forloop itself
    pub body: Vec<Node>,
    /// The body to execute in case of an empty object in the `{% for .. %}{% else %}{% endfor %}` construct
    pub otherwise: Vec<Node>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Include {
    pub files: Vec<String>,
    pub ignore_missing: bool,
}

/// All Tera nodes that can be encountered
#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Text(String),
    VariableBlock(SpannedExpression),
    Set(Set),
    Raw(String),
    Include(Include),
    Block(Block),
    Super,
    If(If),
    ForLoop(ForLoop),
    Continue,
    Break,
    FilterSection(FilterSection),
}
