use crate::errors::{Error, TeraResult};
use std::collections::BTreeMap;
use std::fmt;
use std::str::FromStr;
use std::sync::Arc;

use crate::utils::{Span, Spanned};
use crate::value::{format_map, Key, Value};
use crate::HashMap;

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
    /// An array that contains things that we need to look up in the context
    Array(Spanned<Array>),
    /// A hashmap defined in the template where we need to look up in the context
    Map(Spanned<Map>),
    /// A variable to look up in the context.
    Var(Spanned<Var>),
    /// The `.` getter, as in item.field
    GetAttr(Spanned<GetAttr>),
    /// The in brackets getter as in `item[hello * 10]`
    GetItem(Spanned<GetItem>),
    /// A python like slice indexing pattern, like `[1:5:2]`
    Slice(Spanned<Slice>),
    /// my_value | safe(potential="argument") filter
    Filter(Spanned<Filter>),
    /// my_value is defined
    Test(Spanned<Test>),
    /// 'a' if truthy else 'b'
    Ternary(Spanned<Ternary>),
    MacroCall(Spanned<MacroCall>),
    ComponentCall(Spanned<ComponentCall>),
    FunctionCall(Spanned<FunctionCall>),
    UnaryOperation(Spanned<UnaryOperation>),
    BinaryOperation(Spanned<BinaryOperation>),
}

impl Expression {
    pub fn is_literal(&self) -> bool {
        matches!(self, Expression::Const(..))
    }

    pub fn is_array_or_map_literal(&self) -> bool {
        match self {
            Expression::Const(c) => c.as_map().is_some() || c.as_vec().is_some(),
            Expression::Map(_) | Expression::Array(_) => true,
            _ => false,
        }
    }

    pub(crate) fn as_value(&self) -> Option<Value> {
        match self {
            Expression::Const(c) => Some(c.node().clone()),
            _ => None,
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            Expression::Const(s) => s.span(),
            Expression::Map(s) => s.span(),
            Expression::Array(s) => s.span(),
            Expression::Test(s) => s.span(),
            Expression::ComponentCall(s) => s.span(),
            Expression::MacroCall(s) => s.span(),
            Expression::FunctionCall(s) => s.span(),
            Expression::UnaryOperation(s) => s.span(),
            Expression::BinaryOperation(s) => s.span(),
            Expression::Var(s) => s.span(),
            Expression::GetAttr(s) => s.span(),
            Expression::GetItem(s) => s.span(),
            Expression::Slice(s) => s.span(),
            Expression::Filter(s) => s.span(),
            Expression::Ternary(s) => s.span(),
        }
    }

    pub fn expand_span(&mut self, span: &Span) {
        match self {
            Expression::Const(s) => s.span_mut().expand(span),
            Expression::Map(s) => s.span_mut().expand(span),
            Expression::Array(s) => s.span_mut().expand(span),
            Expression::Test(s) => s.span_mut().expand(span),
            Expression::ComponentCall(s) => s.span_mut().expand(span),
            Expression::MacroCall(s) => s.span_mut().expand(span),
            Expression::FunctionCall(s) => s.span_mut().expand(span),
            Expression::UnaryOperation(s) => s.span_mut().expand(span),
            Expression::BinaryOperation(s) => s.span_mut().expand(span),
            Expression::Var(s) => s.span_mut().expand(span),
            Expression::GetAttr(s) => s.span_mut().expand(span),
            Expression::GetItem(s) => s.span_mut().expand(span),
            Expression::Slice(s) => s.span_mut().expand(span),
            Expression::Filter(s) => s.span_mut().expand(span),
            Expression::Ternary(s) => s.span_mut().expand(span),
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
                Value::String(j, _) => fmt::Debug::fmt(&Spanned::new(j, i.span().clone()), f),
                Value::Array(j) => fmt::Debug::fmt(&Spanned::new(j, i.span().clone()), f),
                Value::Map(j) => fmt::Debug::fmt(&Spanned::new(j, i.span().clone()), f),
                Value::Null => fmt::Debug::fmt(&Spanned::new((), i.span().clone()), f),
                _ => unreachable!("{self} is not implemented"),
            },
            Map(i) => fmt::Debug::fmt(i, f),
            Array(i) => fmt::Debug::fmt(i, f),
            Test(i) => fmt::Debug::fmt(i, f),
            ComponentCall(i) => fmt::Debug::fmt(i, f),
            MacroCall(i) => fmt::Debug::fmt(i, f),
            Filter(i) => fmt::Debug::fmt(i, f),
            FunctionCall(i) => fmt::Debug::fmt(i, f),
            UnaryOperation(i) => fmt::Debug::fmt(i, f),
            BinaryOperation(i) => fmt::Debug::fmt(i, f),
            Var(i) => fmt::Debug::fmt(i, f),
            GetAttr(i) => fmt::Debug::fmt(i, f),
            GetItem(i) => fmt::Debug::fmt(i, f),
            Slice(i) => fmt::Debug::fmt(i, f),
            Ternary(i) => fmt::Debug::fmt(i, f),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expression::*;

        match self {
            Const(i) => match i.node() {
                Value::String(s, _) => write!(f, "'{}'", *s),
                Value::I64(s) => write!(f, "{}", *s),
                Value::F64(s) => write!(f, "{}", *s),
                Value::U64(s) => write!(f, "{}", *s),
                Value::U128(s) => write!(f, "{}", *s),
                Value::I128(s) => write!(f, "{}", *s),
                Value::Bool(s) => write!(f, "{}", *s),
                Value::Array(s) => {
                    write!(f, "[")?;
                    for (i, elem) in s.iter().enumerate() {
                        if i > 0 && i != s.len() {
                            write!(f, ", ")?;
                        }
                        match elem {
                            Value::String(t, _) => write!(f, r#""{t}""#),
                            _ => write!(f, "{elem}"),
                        }?;
                    }
                    write!(f, "]")
                }
                Value::Null => write!(f, "null"),
                Value::Undefined => write!(f, "undefined"),
                Value::Bytes(_) => write!(f, "<bytes>"),
                Value::Map(s) => {
                    let mut buf: Vec<u8> = Vec::new();
                    format_map(s, &mut buf).expect("failed to write map to vec");
                    write!(
                        f,
                        "{}",
                        std::str::from_utf8(&buf).expect("valid utf-8 in display")
                    )
                }
            },
            Map(i) => write!(f, "{}", **i),
            Array(i) => write!(f, "{}", **i),
            Test(i) => write!(f, "{}", **i),
            MacroCall(i) => write!(f, "{}", **i),
            ComponentCall(i) => write!(f, "{}", **i),
            Filter(i) => write!(f, "{}", **i),
            FunctionCall(i) => write!(f, "{}", **i),
            UnaryOperation(i) => write!(f, "{}", **i),
            BinaryOperation(i) => write!(f, "{}", **i),
            Var(i) => write!(f, "{}", **i),
            GetAttr(i) => write!(f, "{}", **i),
            GetItem(i) => write!(f, "{}", **i),
            Slice(i) => write!(f, "{}", **i),
            Ternary(i) => write!(f, "{}", **i),
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

#[cfg(not(feature = "preserve_order"))]
pub type ExpressionMap = HashMap<Key<'static>, Expression>;

#[cfg(feature = "preserve_order")]
pub type ExpressionMap = indexmap::IndexMap<Key<'static>, Expression>;

#[derive(Clone, Debug, PartialEq)]
pub struct Map {
    pub items: ExpressionMap,
}

impl fmt::Display for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        for (i, (key, value)) in self.items.iter().enumerate() {
            if i == self.items.len() - 1 {
                write!(f, "{key}: {value}")?
            } else {
                write!(f, "{key}: {value}, ")?
            }
        }
        write!(f, "}}")
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
    pub kwargs: HashMap<String, Expression>,
}

impl fmt::Display for Test {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(is {}", self.expr)?;
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

        write!(f, "}})",)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ComponentCall {
    pub name: String,
    pub kwargs: HashMap<String, Expression>,
    pub body: Vec<Node>,
}

impl fmt::Display for ComponentCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ":{}", self.name)?;
        // TODO: print body
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
        write!(f, "}}",)?;

        if !self.body.is_empty() {
            write!(f, "[",)?;
            for node in &self.body {
                write!(f, "{:?}", node)?;
            }
            write!(f, "]",)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MacroCall {
    pub namespace: String,
    /// The filename it's defined in
    pub filename: Option<String>,
    pub name: String,
    pub kwargs: HashMap<String, Expression>,
}

impl MacroCall {
    pub fn validate_args_names(
        &self,
        tpl_name: &str,
        kwargs_allowed: &[&String],
    ) -> TeraResult<()> {
        let mut kwargs_names = self.kwargs.keys().collect::<Vec<_>>();
        kwargs_names.sort();
        let mut allowed = kwargs_allowed
            .iter()
            .map(|x| (*x).as_str())
            .collect::<Vec<_>>();
        allowed.sort();

        for arg_name in &kwargs_names {
            if !allowed.contains(&arg_name.as_str()) {
                return Err(Error::message(format!(
                    "Template {tpl_name} is calling macro {} \
                                    with an argument {arg_name} which isn't present in its definition. \
                                    Only the following are allowed: {}.",
                    self.name,
                    allowed.join(", ")
                )));
            }
        }

        Ok(())
    }

    pub fn validate(&self, tpl_name: &str, defs: &[MacroDefinition]) -> TeraResult<()> {
        if let Some(macro_def) = defs.iter().find(|x| x.name == self.name) {
            self.validate_args_names(tpl_name, &macro_def.kwargs.keys().collect::<Vec<_>>())
        } else {
            Err(Error::macro_not_found(
                tpl_name,
                &self.namespace,
                &self.name,
            ))
        }
    }
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

#[derive(Clone, Debug, PartialEq)]
pub struct Ternary {
    pub expr: Expression,
    pub true_expr: Expression,
    pub false_expr: Expression,
}

impl fmt::Display for Ternary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} if {} else {}",
            self.true_expr, self.expr, self.false_expr
        )
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

/// A slicing expression (eg [-1], [1:], [:2] etc)
#[derive(Clone, Debug, PartialEq)]
pub struct Slice {
    pub expr: Expression,
    pub start: Option<Expression>,
    pub end: Option<Expression>,
    pub step: Option<Expression>,
}

impl fmt::Display for Slice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[", self.expr)?;
        if let Some(ref expr) = self.start {
            write!(f, "{}", expr)?;
        }
        if let Some(ref expr) = self.end {
            write!(f, ":{}", expr)?;
        }
        if let Some(ref expr) = self.step {
            write!(f, ":{}", expr)?;
        }
        write!(f, "]")
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
    pub expr: Expression,
    /// The body to render in if the expr is truthy
    pub body: Vec<Node>,
    /// The body to render in if the expr is not truthy.
    /// Will also contain the elifs
    pub false_body: Vec<Node>,
}

/// A filter section node `{% filter name(param="value") %} content {% endfilter %}`
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Type {
    String,
    Bool,
    Integer,
    Float,
    Number,
    Array,
    Map,
}

impl FromStr for Type {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "string" => Ok(Type::String),
            "bool" => Ok(Type::Bool),
            "integer" => Ok(Type::Integer),
            "float" => Ok(Type::Float),
            "number" => Ok(Type::Number),
            "array" => Ok(Type::Array),
            "map" => Ok(Type::Map),
            _ => Err(Error::message(
                format!("Found {s} but the only types allowed are: string, bool, integer, float, number, array and map"),
            )),
        }
    }
}

impl Type {
    pub fn as_str(&self) -> &'static str {
        match self {
            Type::String => "string",
            Type::Bool => "bool",
            Type::Integer => "integer",
            Type::Float => "float",
            Type::Number => "number",
            Type::Array => "array",
            Type::Map => "map",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct ComponentArgument {
    pub default: Option<Value>,
    pub typ: Option<Type>,
}

/// A component definition `{% component hello() %}...{% endcomponent %}`
/// Not present in the AST, we extract them during parsing
#[derive(Clone, Debug, PartialEq, Default)]
pub struct ComponentDefinition {
    pub name: String,
    /// The args for that macro: name -> optional default value
    /// Expression for default args can only be literals
    pub kwargs: BTreeMap<String, ComponentArgument>,
    /// Component metadata that you might need at compile time
    pub metadata: BTreeMap<String, Value>,
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
    Break,
    Continue,
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
            Break => fmt::Debug::fmt("{% break %}", f),
            Continue => fmt::Debug::fmt("{% continue %}", f),
        }
    }
}
