use std::collections::HashMap;
use std::fmt;

use crate::utils::{Span, Spanned};

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

/// An expression is the node found in variable block, kwargs and conditions.
#[derive(Clone, PartialEq)]
#[allow(missing_docs)]
pub enum Expression {
    Str(Spanned<String>),
    Integer(Spanned<i64>),
    Float(Spanned<f64>),
    Bool(Spanned<bool>),
    Array(Spanned<Array>),
    Var(Spanned<Var>),
    GetAttr(Spanned<GetAttr>),
    GetItem(Spanned<GetItem>),
    Test(Spanned<Test>),
    MacroCall(Spanned<MacroCall>),
    FunctionCall(Spanned<FunctionCall>),
    UnaryOperation(Spanned<UnaryOperation>),
    BinaryOperation(Spanned<BinaryOperation>),
}

impl Expression {
    pub(crate) fn is_literal(&self) -> bool {
        matches!(
            self,
            Expression::Str(..)
                | Expression::Integer(..)
                | Expression::Float(..)
                | Expression::Bool(..)
        )
    }

    /// Whether those nodes can be used in for loops
    pub(crate) fn can_be_iterated_on(&self) -> bool {
        matches!(
            self,
            Expression::Str(..)
                | Expression::Var(..)
                | Expression::GetItem(..)
                | Expression::GetAttr(..)
                | Expression::Array(..)
                | Expression::FunctionCall(..)
                | Expression::BinaryOperation(..)
        )
    }

    pub fn expand_span(&mut self, span: &Span) {
        match self {
            Expression::Str(s) => s.span_mut().expand(&span),
            Expression::Integer(s) => s.span_mut().expand(&span),
            Expression::Float(s) => s.span_mut().expand(&span),
            Expression::Bool(s) => s.span_mut().expand(&span),
            Expression::Array(s) => s.span_mut().expand(&span),
            Expression::Test(s) => s.span_mut().expand(&span),
            Expression::MacroCall(s) => s.span_mut().expand(&span),
            Expression::FunctionCall(s) => s.span_mut().expand(&span),
            Expression::UnaryOperation(s) => s.span_mut().expand(&span),
            Expression::BinaryOperation(s) => s.span_mut().expand(&span),
            Expression::Var(s) => s.span_mut().expand(&span),
            Expression::GetAttr(s) => s.span_mut().expand(&span),
            Expression::GetItem(s) => s.span_mut().expand(&span),
        }
    }
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expression::*;

        match self {
            Str(i) => write!(f, "'{}'", **i),
            Integer(i) => write!(f, "{:?}", **i),
            Float(i) => write!(f, "{:?}", **i),
            Bool(i) => write!(f, "{:?}", **i),
            Array(i) => write!(f, "{:?}", **i),
            Test(i) => write!(f, "{:?}", **i),
            MacroCall(i) => write!(f, "{:?}", **i),
            FunctionCall(i) => write!(f, "{:?}", **i),
            UnaryOperation(i) => write!(f, "{:?}", **i),
            BinaryOperation(i) => write!(f, "{:?}", **i),
            Var(i) => write!(f, "{:?}", **i),
            GetAttr(i) => write!(f, "{:?}", **i),
            GetItem(i) => write!(f, "{:?}", **i),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expression::*;

        match self {
            Str(i) => write!(f, "'{}'", **i),
            Integer(i) => write!(f, "{}", **i),
            Float(i) => write!(f, "{}", **i),
            Bool(i) => write!(f, "{}", **i),
            Array(i) => write!(f, "{}", **i),
            Test(i) => write!(f, "{}", **i),
            MacroCall(i) => write!(f, "{}", **i),
            FunctionCall(i) => write!(f, "{}", **i),
            UnaryOperation(i) => write!(f, "{}", **i),
            BinaryOperation(i) => write!(f, "{}", **i),
            Var(i) => write!(f, "{}", **i),
            GetAttr(i) => write!(f, "{}", **i),
            GetItem(i) => write!(f, "{}", **i),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryOperation {
    pub op: UnaryOperator,
    pub expr: Expression,
}

impl fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {})", self.op, self.expr)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryOperation {
    pub op: BinaryOperator,
    pub left: Expression,
    pub right: Expression,
}

impl fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.op, self.left, self.right)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Array {
    pub items: Vec<Expression>,
}

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for (i, s) in self.items.iter().enumerate() {
            if i == self.items.len() - 1 {
                write!(f, "{}", s)?
            } else {
                write!(f, "{}, ", s)?
            }
        }
        write!(f, "]")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Test {
    pub name: String,
    pub args: Vec<Expression>,
}

impl fmt::Display for Test {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;

        if !self.args.is_empty() {
            write!(f, "{{",)?;
            for (i, s) in self.args.iter().enumerate() {
                if i == self.args.len() - 1 {
                    write!(f, "{}", s)?
                } else {
                    write!(f, "{}, ", s)?
                }
            }
            write!(f, "}}",)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MacroCall {
    pub namespace: String,
    pub name: String,
    pub kwargs: HashMap<String, Expression>,
}

impl fmt::Display for MacroCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}::{}", self.namespace, self.name)?;
        write!(f, "{{",)?;
        let mut keys = self.kwargs.keys().collect::<Vec<_>>();
        keys.sort();
        for (i, k) in keys.iter().enumerate() {
            if i == self.kwargs.len() - 1 {
                write!(f, "{}={}", k, self.kwargs[*k])?
            } else {
                write!(f, "{}={}, ", k, self.kwargs[*k])?
            }
        }
        write!(f, "}}",)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    pub expr: Expression,
    pub kwargs: HashMap<String, Expression>,
}

impl fmt::Display for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expr)?;
        write!(f, "{{",)?;
        let mut keys = self.kwargs.keys().collect::<Vec<_>>();
        keys.sort();
        for (i, k) in keys.iter().enumerate() {
            if i == self.kwargs.len() - 1 {
                write!(f, "{}={}", k, self.kwargs[*k])?
            } else {
                write!(f, "{}={}, ", k, self.kwargs[*k])?
            }
        }
        write!(f, "}}",)
    }
}

/// A variable lookup
#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    pub name: String,
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// An attribute lookup expression.
#[derive(Clone, Debug, PartialEq)]
pub struct GetAttr {
    pub expr: Expression,
    pub name: String,
}

impl fmt::Display for GetAttr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.expr, self.name)
    }
}

/// An item lookup expression.
#[derive(Clone, Debug, PartialEq)]
pub struct GetItem {
    pub expr: Expression,
    pub sub_expr: Expression,
}

impl fmt::Display for GetItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]", self.expr, self.sub_expr)
    }
}

/// Set a variable in the context `{% set val = "hey" %}`
#[derive(Clone, Debug, PartialEq)]
pub struct Set {
    /// The name for that value in the context
    pub name: String,
    /// The value to assign
    pub value: Expression,
    /// Whether we want to set the variable globally or locally
    /// set_global is only useful in loops
    pub global: bool,
}

/// A template to include
#[derive(Clone, Debug, PartialEq)]
pub struct Include {
    pub name: String,
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
    pub else_body: Vec<Node>,
}

/// A filter section node `{{ filter name(param="value") }} content {{ endfilter }}`
#[derive(Clone, Debug, PartialEq)]
pub struct FilterSection {
    pub filter: Expression,
    /// The filter body
    pub body: Vec<Node>,
}

// /// A Macro definition `{% macro hello() %}...{% endmacro %}`
// #[derive(Clone, Debug, PartialEq)]
// pub struct MacroDefinition {
//     pub name: String,
//     /// The args for that macro: name -> optional default value
//     pub kwargs: HashMap<String, Option<SpannedExpression>>,
//     pub body: Vec<Node>,
// }
//

/// A forloop: can be over values or key/values
#[derive(Clone, Debug, PartialEq)]
pub struct ForLoop {
    /// Name of the key in the loop (only when iterating on map-like objects)
    pub key: Option<String>,
    /// Name of the local variable for the value in the loop
    pub value: String,
    /// Expression being iterated on
    pub target: Expression,
    /// What's in the forloop itself
    pub body: Vec<Node>,
    /// The body to execute in case of an empty object in the `{% for .. %}{% else %}{% endfor %}` construct
    pub else_body: Vec<Node>,
}

// TODO: use spanned as well? here?
#[derive(Clone, PartialEq)]
pub enum Node {
    Content(String),
    Expression(Expression),
    Set(Set),
    Include(Include),
    Block(Block),
    ForLoop(ForLoop),
    If(If),
    FilterSection(FilterSection),
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Node::*;

        match self {
            Content(s) => fmt::Debug::fmt(s, f),
            Expression(s) => fmt::Debug::fmt(s, f),
            Set(s) => fmt::Debug::fmt(s, f),
            Include(s) => fmt::Debug::fmt(s, f),
            Block(s) => fmt::Debug::fmt(s, f),
            ForLoop(s) => fmt::Debug::fmt(s, f),
            If(s) => fmt::Debug::fmt(s, f),
            FilterSection(s) => fmt::Debug::fmt(s, f),
        }
    }
}
