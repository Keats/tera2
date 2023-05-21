use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

use crate::utils::{Span, Spanned};
use crate::value::Value;

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
        write!(f, "{val}")
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

    // Not binary operators, only there simplicity for precedence in the parser.
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
            StrConcat => "~",
            In => "in",
            Is => "is",
            Pipe => "|",
        };
        write!(f, "{val}")
    }
}

/// An expression is the node found in variable block, kwargs and conditions.
#[derive(Clone, PartialEq)]
#[allow(missing_docs)]
pub enum Expression {
    /// A constant: string, number, boolean, array or null
    Const(Spanned<Value>),
    /// An array that contains things not
    Array(Spanned<Array>),
    /// A variable to look up in the context.
    /// Note that
    Var(Spanned<Var>),
    /// The `.` getter, as in item.field
    GetAttr(Spanned<GetAttr>),
    /// The in brackets getter as in item[hello * 10]
    GetItem(Spanned<GetItem>),
    /// my_value | safe(potential="argument") filter
    Filter(Spanned<Filter>),
    /// my_value is defined
    Test(Spanned<Test>),
    MacroCall(Spanned<MacroCall>),
    FunctionCall(Spanned<FunctionCall>),
    UnaryOperation(Spanned<UnaryOperation>),
    BinaryOperation(Spanned<BinaryOperation>),
}

impl Expression {
    pub fn is_literal(&self) -> bool {
        matches!(self, Expression::Const(..))
    }

    pub fn span(&self) -> &Span {
        match self {
            Expression::Const(s) => s.span(),
            Expression::Array(s) => s.span(),
            Expression::Test(s) => s.span(),
            Expression::MacroCall(s) => s.span(),
            Expression::FunctionCall(s) => s.span(),
            Expression::UnaryOperation(s) => s.span(),
            Expression::BinaryOperation(s) => s.span(),
            Expression::Var(s) => s.span(),
            Expression::GetAttr(s) => s.span(),
            Expression::GetItem(s) => s.span(),
            Expression::Filter(s) => s.span(),
        }
    }

    pub fn expand_span(&mut self, span: &Span) {
        match self {
            Expression::Const(s) => s.span_mut().expand(span),
            Expression::Array(s) => s.span_mut().expand(span),
            Expression::Test(s) => s.span_mut().expand(span),
            Expression::MacroCall(s) => s.span_mut().expand(span),
            Expression::FunctionCall(s) => s.span_mut().expand(span),
            Expression::UnaryOperation(s) => s.span_mut().expand(span),
            Expression::BinaryOperation(s) => s.span_mut().expand(span),
            Expression::Var(s) => s.span_mut().expand(span),
            Expression::GetAttr(s) => s.span_mut().expand(span),
            Expression::GetItem(s) => s.span_mut().expand(span),
            Expression::Filter(s) => s.span_mut().expand(span),
        }
    }
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expression::*;

        match self {
            Const(i) => match i.node() {
                Value::Bool(j) => fmt::Debug::fmt(&Spanned::new(*j, i.span().clone()), f),
                Value::I64(j) => fmt::Debug::fmt(&Spanned::new(*j, i.span().clone()), f),
                Value::F64(j) => fmt::Debug::fmt(&Spanned::new(*j, i.span().clone()), f),
                Value::String(j) => fmt::Debug::fmt(&Spanned::new(j, i.span().clone()), f),
                Value::Array(j) => fmt::Debug::fmt(&Spanned::new(j, i.span().clone()), f),
                Value::Null => fmt::Debug::fmt(&Spanned::new((), i.span().clone()), f),
                _ => unreachable!("{self} is not implemented"),
            },
            Array(i) => fmt::Debug::fmt(i, f),
            Test(i) => fmt::Debug::fmt(i, f),
            MacroCall(i) => fmt::Debug::fmt(i, f),
            Filter(i) => fmt::Debug::fmt(i, f),
            FunctionCall(i) => fmt::Debug::fmt(i, f),
            UnaryOperation(i) => fmt::Debug::fmt(i, f),
            BinaryOperation(i) => fmt::Debug::fmt(i, f),
            Var(i) => fmt::Debug::fmt(i, f),
            GetAttr(i) => fmt::Debug::fmt(i, f),
            GetItem(i) => fmt::Debug::fmt(i, f),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expression::*;

        match self {
            Const(i) => match i.node() {
                Value::String(s) => write!(f, "'{}'", *s),
                Value::I64(s) => write!(f, "{}", *s),
                Value::F64(s) => write!(f, "{}", *s),
                Value::Bool(s) => write!(f, "{}", *s),
                Value::Array(s) => {
                    write!(f, "[")?;
                    for (i, elem) in s.iter().enumerate() {
                        if i > 0 && i != s.len() {
                            write!(f, ", ")?;
                        }
                        match elem {
                            Value::Char(t) => write!(f, "'{t}'"),
                            Value::String(t) => write!(f, r#""{t}""#),
                            _ => write!(f, "{elem}"),
                        }?;
                    }
                    write!(f, "]")
                }
                Value::Null => write!(f, "null"),
                _ => unreachable!(),
            },
            Array(i) => write!(f, "{}", **i),
            Test(i) => write!(f, "{}", **i),
            MacroCall(i) => write!(f, "{}", **i),
            Filter(i) => write!(f, "{}", **i),
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
pub struct Filter {
    pub expr: Expression,
    pub name: String,
    pub kwargs: HashMap<String, Expression>,
}

impl fmt::Display for Filter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(| {}", self.expr)?;
        write!(f, " {}", self.name)?;
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
        write!(f, "}})",)
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
                write!(f, "{s}")?
            } else {
                write!(f, "{s}, ")?
            }
        }
        write!(f, "]")
    }
}

impl Array {
    pub(crate) fn as_const(&self) -> Option<Value> {
        let mut res = Vec::with_capacity(self.items.len());
        for v in &self.items {
            match v {
                Expression::Const(v) => res.push(v.node().clone()),
                _ => return None,
            }
        }
        Some(Value::Array(Arc::new(res)))
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct Test {
    pub expr: Expression,
    pub name: String,
    pub args: Vec<Expression>,
}

impl fmt::Display for Test {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(is {}", self.expr)?;
        write!(f, " {}", self.name)?;
        write!(f, "{{",)?;

        if !self.args.is_empty() {
            for (i, s) in self.args.iter().enumerate() {
                if i == self.args.len() - 1 {
                    write!(f, "{s}")?
                } else {
                    write!(f, "{s}, ")?
                }
            }
        }

        write!(f, "}})",)?;
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
    pub name: String,
    pub kwargs: HashMap<String, Expression>,
}

impl fmt::Display for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
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

/// Set a variable in the context from a block `{% set val %}Hello {{world}}{% endset %}`
#[derive(Clone, Debug, PartialEq)]
pub struct BlockSet {
    /// The name for that value in the context
    pub name: String,
    /// The filters to apply to the block, with a dummy source set to null
    pub filters: Vec<Expression>,
    /// The content of the block
    pub body: Vec<Node>,
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
    pub else_body: Option<Vec<Node>>,
}

/// A filter section node `{{ filter name(param="value") }} content {{ endfilter }}`
#[derive(Clone, Debug, PartialEq)]
pub struct FilterSection {
    pub name: Spanned<String>,
    pub kwargs: HashMap<String, Expression>,
    /// The filter body
    pub body: Vec<Node>,
}

/// A Macro definition `{% macro hello() %}...{% endmacro %}`
/// Not present in the AST, we extract them during parsing
#[derive(Clone, Debug, PartialEq)]
pub struct MacroDefinition {
    pub name: String,
    /// The args for that macro: name -> optional default value
    /// Expression for default args can only be literals
    pub kwargs: HashMap<String, Option<Value>>,
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
    pub target: Expression,
    /// What's in the forloop itself
    pub body: Vec<Node>,
    /// The body to execute in case of an empty object in the `{% for .. %}{% else %}{% endfor %}` construct
    pub else_body: Vec<Node>,
}

#[derive(Clone, PartialEq)]
pub enum Node {
    Content(String),
    Expression(Expression),
    Set(Set),
    BlockSet(BlockSet),
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
            BlockSet(s) => fmt::Debug::fmt(s, f),
            Include(s) => fmt::Debug::fmt(s, f),
            Block(s) => fmt::Debug::fmt(s, f),
            ForLoop(s) => fmt::Debug::fmt(s, f),
            If(s) => fmt::Debug::fmt(s, f),
            FilterSection(s) => fmt::Debug::fmt(s, f),
        }
    }
}
