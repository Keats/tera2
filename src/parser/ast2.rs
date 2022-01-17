use std::collections::HashMap;
use std::fmt;

use crate::utils::Spanned;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    Not,
    Minus,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use UnaryOperator::*;

        let val = match self {
            Minus => "-",
            Not => "not",
        };
        write!(f, "{}", val)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    // math
    Mul,
    Div,
    Mod,
    Plus,
    Minus,
    FloorDiv,
    Power,

    // comparison
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,

    // rest
    And,
    Or,
    StrConcat,
    In,
    Is,
    Pipe,
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BinaryOperator::*;

        let val = match self {
            Mul => "*",
            Power => "**",
            Div => "/",
            FloorDiv => "//",
            Mod => "%",
            Plus => "+",
            Minus => "-",
            LessThan => "<",
            GreaterThan => ">",
            LessThanOrEqual => "<=",
            GreaterThanOrEqual => ">=",
            Equal => "==",
            NotEqual => "!=",
            And => "and",
            Or => "or",
            In => "in",
            Is => "is",
            StrConcat => "~",
            Pipe => "|",
        };
        write!(f, "{}", val)
    }
}

// TODO: handle my_data.blob[val].hey at compile time -> Eg no ident

/// An expression is the node found in variable block, kwargs and conditions.
#[derive(Clone, PartialEq)]
#[allow(missing_docs)]
pub enum Expression {
    Str(Spanned<String>),
    Integer(Spanned<i64>),
    Float(Spanned<f64>),
    Bool(Spanned<bool>),
    Ident(Spanned<String>),
    Array(Spanned<Array>),
    Test(Spanned<Test>),
    MacroCall(Spanned<MacroCall>),
    FunctionCall(Spanned<FunctionCall>),
    UnaryOperation(Spanned<UnaryOperation>),
    BinaryOperation(Spanned<BinaryOperation>),
}

#[derive(Clone, PartialEq)]
pub struct UnaryOperation {
    pub op: UnaryOperator,
    pub expr: Expression,
}

impl fmt::Debug for UnaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {:?})", self.op, self.expr)
    }
}

#[derive(Clone, PartialEq)]
pub struct BinaryOperation {
    pub op: BinaryOperator,
    pub left: Expression,
    pub right: Expression,
}

impl fmt::Debug for BinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {:?} {:?})", self.op, self.left, self.right)
    }
}

#[derive(Clone, PartialEq)]
pub struct Array {
    pub items: Vec<Expression>,
}

impl fmt::Debug for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for (i, s) in self.items.iter().enumerate() {
            if i == self.items.len() - 1 {
                write!(f, "{:?}", s)?
            } else {
                write!(f, "{:?}, ", s)?
            }
        }
        write!(f, "]")
    }
}

#[derive(Clone, PartialEq)]
pub struct Test {
    pub name: String,
    pub args: Vec<Expression>,
}

impl fmt::Debug for Test {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;

        if !self.args.is_empty() {
            write!(f, "{{",)?;
            for (i, s) in self.args.iter().enumerate() {
                if i == self.args.len() - 1 {
                    write!(f, "{:?}", s)?
                } else {
                    write!(f, "{:?}, ", s)?
                }
            }
            write!(f, "}}",)?;
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq)]
pub struct MacroCall {
    pub namespace: String,
    pub name: String,
    pub kwargs: HashMap<String, Expression>,
}

impl fmt::Debug for MacroCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}::{}", self.namespace, self.name)?;
        write!(f, "{{",)?;
        let mut keys = self.kwargs.keys().collect::<Vec<_>>();
        keys.sort();
        for (i, k) in keys.iter().enumerate() {
            if i == self.kwargs.len() - 1 {
                write!(f, "{}={:?}", k, self.kwargs[*k])?
            } else {
                write!(f, "{}={:?}, ", k, self.kwargs[*k])?
            }
        }
        write!(f, "}}",)
    }
}

#[derive(Clone, PartialEq)]
pub struct FunctionCall {
    pub name: String,
    pub kwargs: HashMap<String, Expression>,
}

impl fmt::Debug for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        write!(f, "{{",)?;
        let mut keys = self.kwargs.keys().collect::<Vec<_>>();
        keys.sort();
        for (i, k) in keys.iter().enumerate() {
            if i == self.kwargs.len() - 1 {
                write!(f, "{}={:?}", k, self.kwargs[*k])?
            } else {
                write!(f, "{}={:?}, ", k, self.kwargs[*k])?
            }
        }
        write!(f, "}}",)
    }
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expression::*;

        match self {
            Str(i) => write!(f, "'{}'", **i),
            Integer(i) => write!(f, "{}", **i),
            Float(i) => write!(f, "{}", **i),
            Ident(i) => write!(f, "{}", **i),
            Bool(i) => write!(f, "{}", **i),
            Array(i) => write!(f, "{:?}", **i),
            Test(i) => write!(f, "{:?}", **i),
            MacroCall(i) => write!(f, "{:?}", **i),
            FunctionCall(i) => write!(f, "{:?}", **i),
            UnaryOperation(i) => write!(f, "{:?}", **i),
            BinaryOperation(i) => write!(f, "{:?}", **i),
        }
    }
}

// /// Set a variable in the context `{% set val = "hey" %}`
// #[derive(Clone, Debug, PartialEq)]
// pub struct Set {
//     /// The name for that value in the context
//     pub key: String,
//     /// The value to assign
//     pub value: SpannedExpression,
//     /// Whether we want to set the variable globally or locally
//     /// global_set is only useful in loops
//     pub global: bool,
// }
//
// /// A block definition
// #[derive(Clone, Debug, PartialEq)]
// pub struct Block {
//     /// The block name
//     pub name: String,
//     /// The block content
//     pub body: Vec<Node>,
// }
//
// /// An if/elif/else condition with their respective body
// #[derive(Clone, Debug, PartialEq)]
// pub struct If {
//     /// First item if the if, all the ones after are elif
//     pub conditions: Vec<(SpannedExpression, Vec<Node>)>,
//     /// The optional `else` block
//     pub otherwise: Vec<Node>,
// }
//
// /// A filter section node `{{ filter name(param="value") }} content {{ endfilter }}`
// #[derive(Clone, Debug, PartialEq)]
// pub struct FilterSection {
//     pub name: String,
//     pub kwargs: HashMap<String, SpannedExpression>,
//     /// The filter body
//     pub body: Vec<Node>,
// }
//
// /// A Macro definition `{% macro hello() %}...{% endmacro %}`
// #[derive(Clone, Debug, PartialEq)]
// pub struct MacroDefinition {
//     pub name: String,
//     /// The args for that macro: name -> optional default value
//     pub kwargs: HashMap<String, Option<SpannedExpression>>,
//     pub body: Vec<Node>,
// }
//
// /// A forloop: can be over values or key/values
// #[derive(Clone, Debug, PartialEq)]
// pub struct ForLoop {
//     /// Name of the key in the loop (only when iterating on map-like objects)
//     pub key: Option<String>,
//     /// Name of the local variable for the value in the loop
//     pub value: String,
//     /// Expression being iterated on
//     pub container: SpannedExpression,
//     /// What's in the forloop itself
//     pub body: Vec<Node>,
//     /// The body to execute in case of an empty object in the `{% for .. %}{% else %}{% endfor %}` construct
//     pub otherwise: Vec<Node>,
// }
//
// #[derive(Clone, Debug, PartialEq)]
// pub struct Include {
//     pub files: Vec<String>,
//     pub ignore_missing: bool,
// }
//
// /// All Tera nodes that can be encountered
// #[derive(Clone, Debug, PartialEq)]
// pub enum Node {
//     Content(String),
//     VariableBlock(SpannedExpression),
//     Set(Set),
//     Raw(String),
//     Include(Include),
//     Block(Block),
//     Super,
//     If(If),
//     ForLoop(ForLoop),
//     // Do those need to be in the AST?
//     Continue,
//     Break,
//     FilterSection(FilterSection),
// }

#[derive(Clone, PartialEq)]
pub enum Node {
    Content(String),
    Expression(Expression),
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Node::*;

        match self {
            Node::Content(s) =>  write!(f, "{:?}", s),
            Node::Expression(e) => write!(f, "{:?}", e),
        }
    }
}