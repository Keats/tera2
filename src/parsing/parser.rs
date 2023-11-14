use std::borrow::Cow;
use std::collections::BTreeMap;
use std::iter::Peekable;

use crate::errors::{Error, ErrorKind, ReportError, TeraResult};
use crate::parsing::ast::{
    Array, BinaryOperation, Block, BlockSet, Expression, Filter, FilterSection, ForLoop,
    FunctionCall, GetAttr, GetItem, If, Include, MacroCall, MacroDefinition, Map, Set, Test,
    UnaryOperation, Var,
};
use crate::parsing::ast::{BinaryOperator, Node, UnaryOperator};
use crate::parsing::lexer::{tokenize, Token};
use crate::utils::{Span, Spanned};
use crate::value::{Key, Value};
use crate::{HashMap, HashSet};

/// parse_expression can call itself max 100 times, after that it's an error
const MAX_EXPR_RECURSION: usize = 100;
/// We only allow that many dimensions in an array literal
const MAX_DIMENSION_ARRAY: usize = 2;
/// How many nesting of brackets can we have in an variable, eg `a[b[e]]` counts as 2
const MAX_NUM_LEFT_BRACKETS: usize = 4;

// From https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

fn unary_binding_power(op: UnaryOperator) -> ((), u8) {
    use UnaryOperator::*;

    match op {
        Not => ((), 3),
        Minus => ((), 20),
    }
}

fn binary_binding_power(op: BinaryOperator) -> (u8, u8) {
    use BinaryOperator::*;

    match op {
        And | Or => (1, 2),
        In | Is => (3, 4),
        Pipe => (5, 6),
        Equal | NotEqual | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual => (7, 8),
        Plus | Minus => (11, 12),
        Mul | Div | Mod | StrConcat | FloorDiv | Power => (13, 14),
    }
}

macro_rules! expect_token {
    ($parser:expr, $match:pat, $expectation:expr) => {{
        match $parser.next_or_error()? {
            (token, span) if matches!(token, $match) => Ok((token, span)),
            (token, _) => Err(Error::syntax_error(
                format!("Found {} but expected {}.", token, $expectation),
                &$parser.current_span,
            )),
        }
    }};
    ($parser:expr, $match:pat => $target:expr, $expectation:expr) => {{
        match $parser.next_or_error()? {
            ($match, span) => Ok(($target, span)),
            (token, _) => Err(Error::syntax_error(
                format!("Found {} but expected {}.", token, $expectation),
                &$parser.current_span,
            )),
        }
    }};
}

const RESERVED_NAMES: [&str; 13] = [
    "true", "True", "false", "False", "loop", "self", "and", "or", "not", "is", "in", "continue",
    "break",
];

/// This enum is only used to error when some tags are used in places they are not allowed
/// For example super() outside of a block or a macro definition inside a macro definition
/// or continue/break in for
#[derive(Copy, Clone, Debug, PartialEq)]
enum BodyContext {
    ForLoop,
    Block,
    If,
    MacroDefinition,
    FilterSection,
}

impl BodyContext {
    fn can_contain_blocks(&self) -> bool {
        matches!(self, BodyContext::Block | BodyContext::FilterSection)
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ParserOutput {
    // filled when we encounter a {% extends %}
    pub(crate) parent: Option<String>,
    // The AST for the body
    pub(crate) nodes: Vec<Node>,
    pub(crate) macro_definitions: Vec<MacroDefinition>,
    // (file, namespace)
    pub(crate) macro_imports: Vec<(String, String)>,
}

pub struct Parser<'a> {
    source: &'a str,
    lexer: Peekable<Box<dyn Iterator<Item = Result<(Token<'a>, Span), Error>> + 'a>>,
    // The next token/span tuple.
    next: Option<Result<(Token<'a>, Span), Error>>,
    // We keep track of the current span
    current_span: Span,
    // A stack of our body context to know where we are
    body_contexts: Vec<BodyContext>,
    // The current array dimension, to avoid stack overflows with too many of them
    array_dimension: usize,
    // We limit the length of an expression to avoid stack overflows with crazy expressions like
    // 100 `(`
    num_expr_calls: usize,
    // We limit the number of nesting for brackets in idents
    num_left_brackets: usize,
    blocks_seen: HashSet<String>,
    output: ParserOutput,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let iter = Box::new(tokenize(source)) as Box<dyn Iterator<Item = _>>;
        Self {
            source,
            lexer: iter.peekable(),
            next: None,
            current_span: Span::default(),
            body_contexts: Vec::new(),
            num_expr_calls: 0,
            array_dimension: 0,
            num_left_brackets: 0,
            blocks_seen: HashSet::with_capacity(10),
            output: ParserOutput::default(),
        }
    }

    fn next(&mut self) -> TeraResult<Option<(Token<'a>, Span)>> {
        let cur = self.next.take();
        self.next = self.lexer.next();
        if let Some(Ok((_, ref span))) = cur {
            self.current_span = span.clone();
        }

        cur.transpose()
    }

    fn eoi(&self) -> Error {
        // The EOI is after the current span
        let mut span = self.current_span.clone();
        span.start_col = span.end_col;
        span.start_line = span.end_line;
        Error::new(ErrorKind::SyntaxError(
            ReportError::unexpected_end_of_input(&span),
        ))
    }

    fn different_name_end_tag(&self, start_name: &str, end_name: &str, kind: &str) -> Error {
        Error::syntax_error(
            format!(
                "{kind} was named `{start_name}` in the opening tag, found `{end_name}` as name in the end tag"
            ),
            &self.current_span,
        )
    }

    fn next_or_error(&mut self) -> TeraResult<(Token<'a>, Span)> {
        match self.next()? {
            None => Err(self.eoi()),
            Some(c) => Ok(c),
        }
    }

    /// Can be just an ident or a macro call/fn
    fn parse_ident(&mut self, ident: &str) -> TeraResult<Expression> {
        let mut start_span = self.current_span.clone();
        // We might not end up using that one if it's a macro or a fn call
        let mut expr = Expression::Var(Spanned::new(
            Var {
                name: ident.to_string(),
            },
            start_span.clone(),
        ));

        loop {
            match self.next {
                Some(Ok((Token::Dot, _))) => {
                    expect_token!(self, Token::Dot, ".")?;
                    let (attr, span) = expect_token!(self, Token::Ident(id) => id, "identifier")?;
                    expr = Expression::GetAttr(Spanned::new(
                        GetAttr {
                            expr,
                            name: attr.to_string(),
                        },
                        span.clone(),
                    ));
                }
                Some(Ok((Token::LeftBracket, _))) => {
                    expect_token!(self, Token::LeftBracket, "[")?;
                    self.num_left_brackets += 1;
                    if self.num_left_brackets > MAX_NUM_LEFT_BRACKETS {
                        return Err(Error::syntax_error(
                            format!("Identifiers can only have up to {MAX_NUM_LEFT_BRACKETS} nested brackets."),
                            &self.current_span,
                        ));
                    }

                    let sub_expr = self.parse_expression(0)?;
                    start_span.expand(&self.current_span);

                    expr = Expression::GetItem(Spanned::new(
                        GetItem { expr, sub_expr },
                        start_span.clone(),
                    ));

                    expect_token!(self, Token::RightBracket, "]")?;
                    self.num_left_brackets -= 1;
                }
                // Function
                Some(Ok((Token::LeftParen, _))) => {
                    let kwargs = self.parse_kwargs()?;
                    start_span.expand(&self.current_span);
                    expr = Expression::FunctionCall(Spanned::new(
                        FunctionCall {
                            name: ident.to_owned(),
                            kwargs,
                        },
                        start_span,
                    ));
                    break;
                }
                // Macro calls
                Some(Ok((Token::Colon, _))) => {
                    // we expect 2 colons, eg macros::bla()
                    expect_token!(self, Token::Colon, ":")?;
                    expect_token!(self, Token::Colon, ":")?;
                    // Then the macro name
                    let (macro_name, _) =
                        expect_token!(self, Token::Ident(id) => id, "identifier")?;
                    let kwargs = self.parse_kwargs()?;
                    start_span.expand(&self.current_span);
                    expr = Expression::MacroCall(Spanned::new(
                        MacroCall {
                            name: macro_name.to_string(),
                            namespace: ident.to_string(),
                            kwargs,
                        },
                        start_span,
                    ));
                    break;
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_kwargs(&mut self) -> TeraResult<HashMap<String, Expression>> {
        let mut kwargs = HashMap::new();
        expect_token!(self, Token::LeftParen, "(")?;

        loop {
            if let Some(Ok((Token::RightParen, _))) = self.next {
                break;
            }

            let (arg_name, _) = expect_token!(self, Token::Ident(id) => id, "identifier")?;
            expect_token!(self, Token::Assign, "=")?;
            let value = self.parse_expression(0)?;
            kwargs.insert(arg_name.to_string(), value);

            if let Some(Ok((Token::Comma, _))) = self.next {
                self.next_or_error()?;
            }
        }

        expect_token!(self, Token::RightParen, ")")?;

        Ok(kwargs)
    }

    fn parse_filter(&mut self, expr: Expression) -> TeraResult<Expression> {
        let (name, mut span) = expect_token!(self, Token::Ident(id) => id, "identifier")?;
        let mut kwargs = HashMap::new();

        // We have potentially args to handle
        if matches!(self.next, Some(Ok((Token::LeftParen, _)))) {
            kwargs = self.parse_kwargs()?;
        }
        span.expand(&self.current_span);

        Ok(Expression::Filter(Spanned::new(
            Filter {
                expr,
                name: name.to_string(),
                kwargs,
            },
            span,
        )))
    }

    fn parse_test(&mut self, expr: Expression) -> TeraResult<Expression> {
        let (name, mut span) = expect_token!(self, Token::Ident(id) => id, "identifier")?;
        let mut args = Vec::new();

        // We have potentially args to handle
        if matches!(self.next, Some(Ok((Token::LeftParen, _)))) {
            self.next_or_error()?;

            if !matches!(self.next, Some(Ok((Token::RightParen, _)))) {
                loop {
                    args.push(self.parse_expression(0)?);

                    // after an arg we have either a `,` or a `)`
                    match self.next {
                        Some(Ok((Token::RightParen, _))) => {
                            break;
                        }
                        Some(Ok((Token::Comma, _))) => {
                            self.next_or_error()?;

                            // trailing comma
                            if let Some(Ok((Token::RightParen, _))) = self.next {
                                break;
                            }
                        }
                        Some(Ok((ref token, ref span))) => {
                            return Err(Error::syntax_error(
                                format!("Found {token} but expected `)` or `,`."),
                                span,
                            ));
                        }
                        Some(Err(ref e)) => {
                            return Err(Error {
                                kind: e.kind.clone(),
                                source: None,
                            });
                        }
                        None => return Err(self.eoi()),
                    };
                }
            }

            expect_token!(self, Token::RightParen, ")")?;
        }

        span.expand(&self.current_span);

        Ok(Expression::Test(Spanned::new(
            Test {
                expr,
                name: name.to_string(),
                args,
            },
            span,
        )))
    }

    fn parse_map(&mut self) -> TeraResult<Expression> {
        let mut literal_only = true;
        let mut items = BTreeMap::new();
        let mut span = self.current_span.clone();

        loop {
            if matches!(self.next, Some(Ok((Token::RightBrace, _)))) {
                break;
            }

            // trailing commas
            if !items.is_empty() {
                expect_token!(self, Token::Comma, ",")?;
            }

            if matches!(self.next, Some(Ok((Token::RightBrace, _)))) {
                break;
            }

            let key = match self.next_or_error()? {
                // TODO: can we borrow there?
                (Token::String(key), _) => Key::String(Cow::Owned(key.to_string())),
                (Token::Integer(key), _) => Key::I64(key),
                (Token::Bool(key), _) => Key::Bool(key),
                (token, span) => {
                    return Err(Error::syntax_error(
                        format!(
                            "Found {} but expected a string, an integer or a bool.",
                            token
                        ),
                        &span,
                    ));
                }
            };

            expect_token!(self, Token::Colon, ":")?;
            let value = self.inner_parse_expression(0)?;
            if !value.is_literal() {
                literal_only = false;
            }
            items.insert(key, value);
        }
        expect_token!(self, Token::RightBrace, "}")?;
        span.expand(&self.current_span);

        if literal_only {
            let mut out = crate::value::Map::with_capacity(items.len());
            for (k, v) in items {
                if let Expression::Const(val) = v {
                    out.insert(k, val.into_parts().0);
                }
            }
            Ok(Expression::Const(Spanned::new(Value::from(out), span)))
        } else {
            Ok(Expression::Map(Spanned::new(Map { items }, span)))
        }
    }

    fn parse_array(&mut self) -> TeraResult<Vec<Expression>> {
        let mut vals = Vec::new();

        loop {
            if matches!(self.next, Some(Ok((Token::RightBracket, _)))) {
                break;
            }

            // trailing commas
            if !vals.is_empty() {
                expect_token!(self, Token::Comma, ",")?;
            }

            if matches!(self.next, Some(Ok((Token::RightBracket, _)))) {
                break;
            }

            // parse_array is always called from inner_parse_expression
            vals.push(self.inner_parse_expression(0)?);
        }

        Ok(vals)
    }

    /// This is called recursively so we do put a limit as to how many times it can call itself
    /// to avoid stack overflow. In practice, normal users will not run into the limit at all.
    /// We're talking 100 parentheses for example
    fn inner_parse_expression(&mut self, min_bp: u8) -> TeraResult<Expression> {
        self.num_expr_calls += 1;
        if self.num_expr_calls > MAX_EXPR_RECURSION {
            return Err(Error::syntax_error(
                "The expression is too complex".to_string(),
                &self.current_span,
            ));
        }

        let (token, mut span) = self.next_or_error()?;

        let mut lhs = match token {
            Token::Integer(i) => Expression::Const(Spanned::new(Value::from(i), span.clone())),
            Token::Float(f) => Expression::Const(Spanned::new(Value::from(f), span.clone())),
            Token::String(s) => Expression::Const(Spanned::new(Value::from(s), span.clone())),
            Token::Bool(b) => Expression::Const(Spanned::new(Value::from(b), span.clone())),
            Token::Minus | Token::Ident("not") => {
                let op = match token {
                    Token::Minus => UnaryOperator::Minus,
                    Token::Ident("not") => UnaryOperator::Not,
                    _ => unreachable!(),
                };
                match &self.next {
                    Some(Ok((Token::Minus, next_span)))
                    | Some(Ok((Token::Ident("not"), next_span))) => {
                        // Can't have unary with unary (eg - - - - - 1) otherwise we will quickly
                        // stack overflow. It doesn't make much sense anyway in practice.
                        // Alternatively, limit the number to 2?
                        return Err(Error::syntax_error(
                            "`-` and `not` cannot be used consecutively.".to_string(),
                            next_span,
                        ));
                    }
                    _ => (),
                }

                let (_, r_bp) = unary_binding_power(op);
                let expr = self.inner_parse_expression(r_bp)?;
                span.expand(&self.current_span);
                Expression::UnaryOperation(Spanned::new(UnaryOperation { op, expr }, span.clone()))
            }
            Token::Ident(ident) => self.parse_ident(ident)?,
            Token::LeftBrace => self.parse_map()?,
            Token::LeftBracket => {
                self.array_dimension += 1;
                if self.array_dimension > MAX_DIMENSION_ARRAY {
                    return Err(Error::syntax_error(
                        format!("Arrays can have a maximum of {MAX_DIMENSION_ARRAY} dimensions."),
                        &self.current_span,
                    ));
                }
                let items = self.parse_array()?;
                self.array_dimension -= 1;
                expect_token!(self, Token::RightBracket, "]")?;
                span.expand(&self.current_span);
                let array = Array { items };
                if let Some(const_array) = array.as_const() {
                    Expression::Const(Spanned::new(const_array, span.clone()))
                } else {
                    Expression::Array(Spanned::new(array, span.clone()))
                }
            }
            Token::LeftParen => {
                let mut lhs = self.inner_parse_expression(0)?;
                expect_token!(self, Token::RightParen, ")")?;
                lhs.expand_span(&self.current_span);
                lhs
            }
            _ => {
                return Err(Error::syntax_error(
                    format!("Found {token} but expected one of: integer, float, string, bool, ident, `-`, `not`, `{{`, `[` or `(`"),
                    &self.current_span,
                ));
            }
        };

        let mut negated = false;

        while let Some(Ok((ref token, _))) = self.next {
            let op = match token {
                Token::Mul => BinaryOperator::Mul,
                Token::Div => BinaryOperator::Div,
                Token::FloorDiv => BinaryOperator::FloorDiv,
                Token::Mod => BinaryOperator::Mod,
                Token::Plus => BinaryOperator::Plus,
                Token::Minus => BinaryOperator::Minus,
                Token::Power => BinaryOperator::Power,
                Token::LessThan => BinaryOperator::LessThan,
                Token::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
                Token::GreaterThan => BinaryOperator::GreaterThan,
                Token::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
                Token::Equal => BinaryOperator::Equal,
                Token::NotEqual => BinaryOperator::NotEqual,
                Token::Tilde => BinaryOperator::StrConcat,
                Token::Ident("not") => {
                    negated = true;
                    // eat it and continue
                    self.next_or_error()?;
                    continue;
                }
                Token::Ident("in") => BinaryOperator::In,
                Token::Ident("and") => BinaryOperator::And,
                Token::Ident("or") => BinaryOperator::Or,
                Token::Ident("is") => BinaryOperator::Is,
                Token::Pipe => BinaryOperator::Pipe,
                _ => break,
            };

            let (l_bp, r_bp) = binary_binding_power(op);
            if l_bp < min_bp {
                break;
            }

            // Advance past the op
            self.next_or_error()?;

            // Whether we get is not/and not/or not
            if matches!(
                op,
                BinaryOperator::Is | BinaryOperator::And | BinaryOperator::Or
            ) && matches!(self.next, Some(Ok((Token::Ident("not"), _))))
            {
                // eat the "not"
                self.next_or_error()?;
                negated = true;
            }

            lhs = match op {
                BinaryOperator::Is => self.parse_test(lhs)?,
                BinaryOperator::Pipe => self.parse_filter(lhs)?,
                _ => {
                    let rhs = self.inner_parse_expression(r_bp)?;
                    span.expand(&self.current_span);

                    // unary operators are not allowed after a ~
                    if op == BinaryOperator::StrConcat {
                        if let Expression::UnaryOperation(uop) = rhs {
                            return Err(Error::syntax_error(
                                format!("`{}` is not allowed after `~`", uop.op),
                                &self.current_span,
                            ));
                        }
                    }

                    Expression::BinaryOperation(Spanned::new(
                        BinaryOperation {
                            op,
                            left: lhs,
                            right: rhs,
                        },
                        span.clone(),
                    ))
                }
            };
            if negated {
                lhs = Expression::UnaryOperation(Spanned::new(
                    UnaryOperation {
                        op: UnaryOperator::Not,
                        expr: lhs,
                    },
                    span.clone(),
                ));
                negated = false;
            }
        }

        Ok(lhs)
    }

    fn parse_expression(&mut self, min_bp: u8) -> TeraResult<Expression> {
        self.num_expr_calls = 0;
        self.inner_parse_expression(min_bp)
    }

    fn parse_for_loop(&mut self) -> TeraResult<ForLoop> {
        self.body_contexts.push(BodyContext::ForLoop);
        let (mut name, _) = expect_token!(self, Token::Ident(id) => id, "identifier")?;
        // Do we have a key?
        let mut key = None;
        if matches!(self.next, Some(Ok((Token::Comma, _)))) {
            self.next_or_error()?;
            let (val, _) = expect_token!(self, Token::Ident(id) => id, "identifier")?;
            key = Some(name.to_string());
            name = val;
        }
        expect_token!(self, Token::Ident("in"), "in")?;
        let target = self.parse_expression(0)?;
        expect_token!(self, Token::TagEnd(..), "%}")?;
        let body =
            self.parse_until(|tok| matches!(tok, Token::Ident("endfor") | Token::Ident("else")))?;
        let mut else_body = None;
        if matches!(self.next, Some(Ok((Token::Ident("else"), _)))) {
            self.next_or_error()?;
            expect_token!(self, Token::TagEnd(..), "%}")?;
            else_body = Some(self.parse_until(|tok| matches!(tok, Token::Ident("endfor")))?);
        }
        // eat the endfor
        self.next_or_error()?;
        self.body_contexts.pop();

        Ok(ForLoop {
            key,
            value: name.to_string(),
            target,
            body,
            else_body: else_body.unwrap_or_default(),
        })
    }

    fn parse_if(&mut self) -> TeraResult<If> {
        self.body_contexts.push(BodyContext::If);
        let expr = self.parse_expression(0)?;
        expect_token!(self, Token::TagEnd(..), "%}")?;
        let body = self.parse_until(|tok| {
            matches!(
                tok,
                Token::Ident("endif") | Token::Ident("else") | Token::Ident("elif")
            )
        })?;

        let mut false_body = Vec::new();

        let mut elif = None;
        loop {
            match &self.next {
                Some(Ok((Token::Ident("elif"), _))) => {
                    if let Some(existing_elif) = elif {
                        false_body.push(Node::If(existing_elif));
                    }
                    self.next_or_error()?;
                    let expr = self.parse_expression(0)?;
                    expect_token!(self, Token::TagEnd(..), "%}")?;
                    let elif_body = self.parse_until(|tok| {
                        matches!(
                            tok,
                            Token::Ident("endif") | Token::Ident("else") | Token::Ident("elif")
                        )
                    })?;
                    elif = Some(If {
                        expr,
                        body: elif_body,
                        false_body: Vec::new(),
                    });
                }
                Some(Ok((Token::Ident("else"), _))) => {
                    self.next_or_error()?;
                    expect_token!(self, Token::TagEnd(..), "%}")?;
                    let else_body = self.parse_until(|tok| matches!(tok, Token::Ident("endif")))?;
                    if let Some(ref mut current_elif) = elif {
                        current_elif.false_body = else_body;
                    } else {
                        false_body = else_body;
                    }
                }
                Some(Ok((Token::Ident("endif"), _))) => {
                    if let Some(el) = elif {
                        false_body.push(Node::If(el));
                    }
                    self.next_or_error()?;
                    break;
                }
                Some(Ok((token, _))) => {
                    return Err(Error::syntax_error(
                        format!("Found {token} but was expecting `elif`, `else` or `endif`."),
                        &self.current_span,
                    ));
                }
                Some(Err(e)) => {
                    return Err(Error {
                        kind: e.kind.clone(),
                        source: None,
                    });
                }
                None => return Err(self.eoi()),
            }
        }

        self.body_contexts.pop();
        Ok(If {
            expr,
            body,
            false_body,
        })
    }

    fn parse_macro_definition(&mut self) -> TeraResult<MacroDefinition> {
        if !self.body_contexts.is_empty() {
            return Err(Error::syntax_error(
                "Macro definitions cannot be written in another tag.".to_string(),
                &self.current_span,
            ));
        }

        self.body_contexts.push(BodyContext::MacroDefinition);
        let (name, _) = expect_token!(self, Token::Ident(id) => id, "identifier")?;
        expect_token!(self, Token::LeftParen, "(")?;
        let mut kwargs = HashMap::new();

        loop {
            if matches!(self.next, Some(Ok((Token::RightParen, _)))) {
                self.next_or_error()?;
                break;
            }

            let (arg_name, _) = expect_token!(self, Token::Ident(id) => id, "identifier")?;
            kwargs.insert(arg_name.to_string(), None);
            match self.next {
                Some(Ok((Token::Assign, _))) => {
                    self.next_or_error()?;

                    let val = match &self.next {
                        Some(Ok((Token::Bool(b), _))) => Value::from(*b),
                        Some(Ok((Token::String(b), _))) => Value::from(*b),
                        Some(Ok((Token::Integer(b), _))) => Value::from(*b),
                        Some(Ok((Token::Float(b), _))) => Value::from(*b),
                        Some(Ok((token, span))) => {
                            return Err(Error::syntax_error(
                                format!("Found {token} but macro default arguments can only be one of: string, bool, integer, float"),
                                span,
                            ));
                        }
                        Some(Err(e)) => {
                            return Err(Error {
                                kind: e.kind.clone(),
                                source: None,
                            });
                        }
                        None => return Err(self.eoi()),
                    };
                    self.next_or_error()?;

                    kwargs.insert(arg_name.to_string(), Some(val));

                    if matches!(self.next, Some(Ok((Token::Comma, _)))) {
                        self.next_or_error()?;
                        continue;
                    }
                }
                Some(Ok((Token::Comma, _))) => {
                    self.next_or_error()?;
                }
                _ => continue,
            }
        }
        expect_token!(self, Token::TagEnd(..), "%}")?;
        let body = self.parse_until(|tok| matches!(tok, Token::Ident("endmacro")))?;
        self.next_or_error()?;

        if matches!(self.next, Some(Ok((Token::Ident(..), _)))) {
            let (end_name, _) = expect_token!(self, Token::Ident(id) => id, "identifier")?;
            if name != end_name {
                return Err(self.different_name_end_tag(name, end_name, "macro"));
            }
        }

        self.body_contexts.pop();

        Ok(MacroDefinition {
            name: name.to_string(),
            kwargs,
            body,
        })
    }

    fn parse_set(&mut self, global: bool) -> TeraResult<Node> {
        let (name, _) = expect_token!(self, Token::Ident(id) => id, "identifier")?;
        if RESERVED_NAMES.contains(&name) {
            return Err(Error::syntax_error(
                format!("{name} is a reserved keyword of Tera, it cannot be assigned to."),
                &self.current_span,
            ));
        }

        // From where we will diverge whether it's a basic set or a block set
        let node = match self.next {
            Some(Ok((Token::Assign, _))) => {
                expect_token!(self, Token::Assign, "=")?;
                let value = self.parse_expression(0)?;
                Node::Set(Set {
                    name: name.to_string(),
                    value,
                    global,
                })
            }
            Some(Ok((Token::Pipe, _))) | Some(Ok((Token::TagEnd(..), _))) => {
                let mut filters = vec![];

                loop {
                    if let Some(Ok((Token::Pipe, _))) = self.next {
                        expect_token!(self, Token::Pipe, "|")?;
                        filters.push(self.parse_filter(Expression::Const(Spanned::new(
                            Value::Null,
                            self.current_span.clone(),
                        )))?);
                    } else {
                        expect_token!(self, Token::TagEnd(..), "%}")?;
                        break;
                    }
                }

                let body = self.parse_until(|tok| matches!(tok, Token::Ident("endset")))?;
                self.next_or_error()?;
                Node::BlockSet(BlockSet {
                    name: name.to_string(),
                    filters,
                    body,
                    global,
                })
            }
            _ => {
                return Err(Error::syntax_error(
                    "Invalid syntax for `set`: expecting an `=` followed by an expression or a `%}}`".to_string(),
                    &self.current_span,
                ));
            }
        };

        Ok(node)
    }

    // We need to know whether this is the first node we encounter to error if the tag is an extend
    // but not the first content node
    fn parse_tag(&mut self, is_first_node: bool) -> TeraResult<Option<Node>> {
        let (tag_token, _) = self.next_or_error()?;
        match tag_token {
            Token::Ident("set") | Token::Ident("set_global") => Ok(Some(
                self.parse_set(tag_token == Token::Ident("set_global"))?,
            )),
            Token::Ident("include") => {
                let (name, _) = expect_token!(self, Token::String(s) => s, "identifier")?;
                Ok(Some(Node::Include(Include {
                    name: name.to_string(),
                })))
            }
            Token::Ident("extends") => {
                let (name, _) = expect_token!(self, Token::String(s) => s, "identifier")?;
                if let Some(ref parent) = self.output.parent {
                    return Err(Error::syntax_error(
                        format!("Template is already extending `{parent}`"),
                        &self.current_span,
                    ));
                }
                if !is_first_node {
                    return Err(Error::syntax_error(
                        "`extends` needs to be the first tag of the template, with the exception of macro imports that are allowed before.".to_string(),
                        &self.current_span,
                    ));
                }
                if !self.body_contexts.is_empty() {
                    return Err(Error::syntax_error(
                        "`extends` cannot be nested in other tags.".to_string(),
                        &self.current_span,
                    ));
                }
                self.output.parent = Some(name.to_string());
                Ok(None)
            }
            Token::Ident("block") => {
                if self.body_contexts.iter().any(|b| !b.can_contain_blocks()) {
                    return Err(Error::syntax_error(
                        "Blocks cannot be written in a tag other than block or filter section."
                            .to_string(),
                        &self.current_span,
                    ));
                }
                self.body_contexts.push(BodyContext::Block);
                let (name, _) = expect_token!(self, Token::Ident(s) => s, "identifier")?;
                if self.blocks_seen.contains(name) {
                    return Err(Error::syntax_error(
                        format!("Template already contains a block named `{name}`"),
                        &self.current_span,
                    ));
                } else {
                    self.blocks_seen.insert(name.to_string());
                }

                expect_token!(self, Token::TagEnd(..), "%}")?;
                let body = self.parse_until(|tok| matches!(tok, Token::Ident("endblock")))?;
                self.next_or_error()?;
                if let Some(Ok((Token::Ident(end_name), _))) = self.next {
                    self.next_or_error()?;
                    if end_name != name {
                        return Err(self.different_name_end_tag(name, end_name, "block"));
                    }
                }

                self.body_contexts.pop();

                self.blocks_seen.insert(name.to_string());
                Ok(Some(Node::Block(Block {
                    name: name.to_string(),
                    body,
                })))
            }
            Token::Ident("for") => {
                let node = self.parse_for_loop()?;
                Ok(Some(Node::ForLoop(node)))
            }
            Token::Ident("if") => {
                let node = self.parse_if()?;
                Ok(Some(Node::If(node)))
            }
            Token::Ident("filter") => {
                self.body_contexts.push(BodyContext::FilterSection);
                let (name, ident_span) = expect_token!(self, Token::Ident(s) => s, "identifier")?;

                let kwargs = if matches!(self.next, Some(Ok((Token::LeftParen, _)))) {
                    self.parse_kwargs()?
                } else {
                    HashMap::new()
                };
                let mut fn_span = ident_span.clone();
                fn_span.expand(&self.current_span);
                expect_token!(self, Token::TagEnd(..), "%}")?;
                let body = self.parse_until(|tok| matches!(tok, Token::Ident("endfilter")))?;
                self.next_or_error()?;
                self.body_contexts.pop();
                Ok(Some(Node::FilterSection(FilterSection {
                    name: Spanned::new(name.to_owned(), ident_span),
                    kwargs,
                    body,
                })))
            }
            Token::Ident("macro") => {
                let macro_def = self.parse_macro_definition()?;
                self.output.macro_definitions.push(macro_def);
                Ok(None)
            }
            Token::Ident("import") => {
                // {% import 'macros.html' as macros %}
                let (filename, _) = expect_token!(self, Token::String(s) => s, "string")?;
                expect_token!(self, Token::Ident("as"), "as")?;
                let (namespace, _) = expect_token!(self, Token::Ident(s) => s, "identifier")?;

                for (_, namespace2) in &self.output.macro_imports {
                    if namespace == namespace2 {
                        return Err(Error::syntax_error(
                            format!("Multiple macros imports using the `{namespace}` namespace"),
                            &self.current_span,
                        ));
                    }
                }
                self.output
                    .macro_imports
                    .push((filename.to_string(), namespace.to_string()));

                Ok(None)
            }
            Token::Ident("break") | Token::Ident("continue") => {
                let is_break = tag_token == Token::Ident("break");
                if !self
                    .body_contexts
                    .iter()
                    .any(|b| *b == BodyContext::ForLoop)
                {
                    return Err(Error::syntax_error(
                        format!(
                            "{} can only be used in a for loop",
                            if is_break { "break" } else { "continue" }
                        ),
                        &self.current_span,
                    ));
                }
                // TODO: add a Node::Keyword if we have more than one like that
                Ok(Some(if is_break {
                    Node::Break
                } else {
                    Node::Continue
                }))
            }
            _ => Err(Error::syntax_error(
                "Unknown tag".to_string(),
                &self.current_span,
            )),
        }
    }

    fn parse_until<F: Fn(&Token) -> bool>(&mut self, end_check_fn: F) -> TeraResult<Vec<Node>> {
        let mut nodes = Vec::new();

        while let Some((token, _)) = self.next()? {
            match token {
                Token::Content(c) => {
                    // We have pushed an empty content to replace comment so we ignore those
                    if !c.is_empty() {
                        nodes.push(Node::Content(c.to_owned()));
                    }
                }
                Token::VariableStart(_) => {
                    let expr = self.parse_expression(0)?;
                    expect_token!(self, Token::VariableEnd(..), "}}")?;
                    nodes.push(Node::Expression(expr));
                }
                Token::TagStart(_) => {
                    let tok = match &self.next {
                        None => return Err(self.eoi()),
                        Some(Ok((tok, _))) => tok,
                        Some(Err(ref e)) => {
                            return Err(Error {
                                kind: e.kind.clone(),
                                source: None,
                            });
                        }
                    };
                    if end_check_fn(tok) {
                        return Ok(nodes);
                    }
                    let node = self.parse_tag(nodes.is_empty())?;
                    expect_token!(self, Token::TagEnd(..), "%}")?;
                    if let Some(n) = node {
                        nodes.push(n);
                    }
                }
                t => unreachable!("Unexpected token when parsing: {:?}", t),
            }
        }

        Ok(nodes)
    }

    pub(crate) fn parse(mut self) -> TeraResult<ParserOutput> {
        // get the first token
        self.next()?;
        self.output.nodes = self.parse_until(|_| false)?;
        Ok(self.output)
    }
}
