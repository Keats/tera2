use std::collections::HashMap;
use std::iter::Peekable;

use crate::errors::{Error, ErrorKind, TeraResult};
use crate::parser::ast2::{Array, BinaryOperation, Block, Expression, FunctionCall, GetAttr, GetItem, Include, MacroCall, Set, Test, UnaryOperation, Var};
use crate::parser::ast2::{BinaryOperator, Node, UnaryOperator};
use crate::parser::lexer2::{tokenize, Token};
use crate::utils::{Span, Spanned};

// From https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

fn unary_binding_power(op: UnaryOperator) -> ((), u8) {
    use UnaryOperator::*;

    match op {
        Not => ((), 3),
        Minus => ((), 7),
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
    ($parser:expr, $expectation:expr) => {{
        match $parser.next_or_error()? {
            Some(rv) => Ok(rv),
            None => Err(Error::new(ErrorKind::SyntaxError, "todo simple expect")),
        }
    }};
    ($parser:expr, $match:pat, $expectation:expr) => {{
        match $parser.next_or_error()? {
            (token, span) if matches!(token, $match) => Ok((token, span)),
            (token, _) => Err(Error::new(
                ErrorKind::SyntaxError,
                &format!("todo match got: {:?}, expected {}", token, $expectation),
            )),
        }
    }};
    ($parser:expr, $match:pat => $target:expr, $expectation:expr) => {{
        match $parser.next_or_error()? {
            ($match, span) => Ok(($target, span)),
            (token, _) => Err(Error::new(
                ErrorKind::SyntaxError,
                &format!("todo match got {:?}, expected {}", token, $expectation),
            )),
        }
    }};
}

// TODO: double check what is actually reserved so we can't re-assign them
const RESERVED_NAMES: [&str; 13] = [
    "true", "True", "false", "False", "loop", "self", "and", "or", "not", "is", "in", "continue", "break",
];

pub struct Parser<'a> {
    source: &'a str,
    lexer: Peekable<Box<dyn Iterator<Item = Result<(Token<'a>, Span), Error>> + 'a>>,
    // The next token/span tuple.
    next: Option<Result<(Token<'a>, Span), Error>>,
    // We keep track of the current span TODO
    current_span: Span,
    // filled when we encounter a {% extends %}, we don't need to keep the extends node in the AST
    pub parent: Option<String>,
    // Only filled if `parent.is_some()`. In that case, we will ignore all the other nodes found
    // while parsing
    pub blocks: HashMap<String, Vec<Node>>,
    // (file, namespace)
    pub macro_imports: Vec<(String, String)>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let iter = Box::new(tokenize(source)) as Box<dyn Iterator<Item = _>>;
        Self {
            source,
            lexer: iter.peekable(),
            next: None,
            current_span: Span::default(),
            parent: None,
            blocks: HashMap::new(),
            macro_imports: Vec::new(),
        }
    }

    fn current(&mut self) -> Result<Option<(&Token<'a>, &Span)>, Error> {
        if self.next.is_none() {
            self.next()?;
        }

        match self.next {
            Some(Ok(ref tok)) => Ok(Some((&tok.0, &tok.1))),
            Some(Err(_)) => Err(self.next.take().unwrap().unwrap_err()),
            None => Ok(None),
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

    fn next_or_error(&mut self) -> TeraResult<(Token<'a>, Span)> {
        match self.next()? {
            None => {
                todo!("handle error message")
            }
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
                    start_span.expand(&span);
                    expr = Expression::GetAttr(Spanned::new(
                        GetAttr {
                            expr,
                            name: attr.to_string(),
                        },
                        start_span.clone(),
                    ));
                }
                Some(Ok((Token::LeftBracket, _))) => {
                    expect_token!(self, Token::LeftBracket, "[")?;
                    let sub_expr = self.parse_expression(0)?;
                    start_span.expand(&self.current_span);

                    expr = Expression::GetItem(Spanned::new(
                        GetItem { expr, sub_expr },
                        start_span.clone(),
                    ));

                    expect_token!(self, Token::RightBracket, "]")?;
                }
                // Function
                Some(Ok((Token::LeftParen, _))) => {
                    let kwargs = self.parse_kwargs()?;
                    expr = Expression::FunctionCall(Spanned::new(
                        FunctionCall { expr, kwargs },
                        start_span.clone(),
                    ));
                    break;
                }
                // Macro calls
                Some(Ok((Token::Colon, _))) => {
                    // we expect 2 colons, eg macros::bla()
                    expect_token!(self, Token::Colon, ":")?;
                    expect_token!(self, Token::Colon, ":")?;
                    // Then the macro name
                    let (macro_name, span) =
                        expect_token!(self, Token::Ident(id) => id, "identifier")?;
                    let kwargs = self.parse_kwargs()?;
                    expr = Expression::MacroCall(Spanned::new(
                        MacroCall {
                            name: macro_name.to_string(),
                            namespace: ident.to_string(),
                            kwargs,
                        },
                        start_span.clone(),
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
            // TODO: make it optional for macro default kwargs
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

    fn parse_test(&mut self) -> TeraResult<Expression> {
        let (name, span) = expect_token!(self, Token::Ident(id) => id, "identifier")?;
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
                        _ => todo!("handle unexpected tokens in test"),
                    };
                }
            }

            expect_token!(self, Token::RightParen, ")")?;
        }

        Ok(Expression::Test(Spanned::new(
            Test {
                name: name.to_string(),
                args,
            },
            span,
        )))
    }

    fn parse_array(&mut self) -> TeraResult<Vec<Expression>> {
        let mut vals = Vec::new();

        loop {
            if matches!(self.current()?, Some((Token::RightBracket, _))) {
                break;
            }

            if !vals.is_empty() {
                expect_token!(self, Token::Comma, ",")?;
            }

            // trailing commas
            if matches!(self.current()?, Some((Token::RightBracket, _))) {
                break;
            }

            vals.push(self.parse_expression(0)?);
        }

        Ok(vals)
    }

    fn parse_expression(&mut self, min_bp: u8) -> TeraResult<Expression> {
        let (token, mut span) = self.next_or_error()?;

        let mut lhs = match token {
            Token::Integer(i) => Expression::Integer(Spanned::new(i, span.clone())),
            Token::Float(f) => Expression::Float(Spanned::new(f, span.clone())),
            Token::String(s) => Expression::Str(Spanned::new(s.to_owned(), span.clone())),
            Token::Ident("true") | Token::Ident("True") => {
                Expression::Bool(Spanned::new(true, span.clone()))
            }
            Token::Ident("false") | Token::Ident("False") => {
                Expression::Bool(Spanned::new(false, span.clone()))
            }
            Token::Minus | Token::Ident("not") => {
                let op = match token {
                    Token::Minus => UnaryOperator::Minus,
                    Token::Ident("not") => UnaryOperator::Not,
                    _ => unreachable!(),
                };
                let (_, r_bp) = unary_binding_power(op);
                let expr = self.parse_expression(r_bp)?;
                span.expand(&self.current_span);
                Expression::UnaryOperation(Spanned::new(UnaryOperation { op, expr }, span.clone()))
            }
            Token::Ident(ident) => self.parse_ident(ident)?,
            Token::LeftBracket => {
                let items = self.parse_array()?;
                expect_token!(self, Token::RightBracket, "]")?;
                span.expand(&self.current_span);
                Expression::Array(Spanned::new(Array { items }, span.clone()))
            }
            Token::LeftParen => {
                let mut lhs = self.parse_expression(0)?;
                expect_token!(self, Token::RightParen, ")")?;
                lhs.expand_span(&self.current_span);
                lhs
            }
            _ => todo!("finish expression: {:?}", token),
        };

        let mut negated = false;
        loop {
            let op = match &self.next {
                Some(Ok((token, _))) => {
                    match token {
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
                        Token::Pipe => BinaryOperator::Pipe,
                        Token::Ident("not") => {
                            negated = true;
                            // eat it and continue
                            self.next_or_error()?;
                            continue;
                            expect_token!(self, Token::Ident("in"), "in")?;
                            BinaryOperator::In
                        }
                        Token::Ident("in") => BinaryOperator::In,
                        Token::Ident("and") => BinaryOperator::And,
                        Token::Ident("or") => BinaryOperator::Or,
                        Token::Ident("is") => BinaryOperator::Is,
                        _ => break,
                    }
                }
                _ => break,
            };

            let (l_bp, r_bp) = binary_binding_power(op);
            if l_bp < min_bp {
                break;
            }

            // Advance past the op
            self.next_or_error()?;

            if matches!(
                op,
                BinaryOperator::Is | BinaryOperator::And | BinaryOperator::Or
            ) {
                if matches!(self.next, Some(Ok((Token::Ident("not"), _)))) {
                    // eat the "not"
                    self.next_or_error()?;
                    negated = true;
                }
            }

            let mut rhs = match op {
                BinaryOperator::Is => self.parse_test()?,
                _ => self.parse_expression(r_bp)?,
            };

            // We can have filters that look like ident (eg hello | upper) so we need to make sure
            // we have them as fn in the AST
            if op == BinaryOperator::Pipe {
                rhs = match rhs {
                    Expression::Var(s) => {
                        let node_span = s.span().clone();
                        Expression::FunctionCall(Spanned::new(
                            FunctionCall {
                                expr: Expression::Var(s),
                                kwargs: HashMap::new(),
                            },
                            node_span,
                        ))
                    }
                    _ => rhs,
                };
            }

            // TODO: expand span with rhs
            lhs = Expression::BinaryOperation(Spanned::new(
                BinaryOperation {
                    op,
                    left: lhs,
                    right: rhs,
                },
                span.clone(),
            ));

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

    fn parse_tag(&mut self) -> TeraResult<Option<Node>> {
        let (tag_token, start_span) = self.next_or_error()?;
        match tag_token {
            Token::Ident("set") | Token::Ident("set_global") => {
                let (name, _) = expect_token!(self, Token::Ident(id) => id, "identifier")?;
                expect_token!(self, Token::Assign, "=")?;
                let value = self.parse_expression(0)?;
                Ok(Some(Node::Set(Set {
                    name: name.to_string(),
                    value,
                    global: tag_token == Token::Ident("set_global"),
                })))
            }
            Token::Ident("include") => {
                let (name, _) = expect_token!(self, Token::String(s) => s, "identifier")?;
                Ok(Some(Node::Include(Include {
                    name: name.to_string(),
                })))
            }
            Token::Ident("extends") => {
                let (name, _) = expect_token!(self, Token::String(s) => s, "identifier")?;
                if self.parent.is_some() {
                    todo!("error: can't extend multiple template");
                }
                // TODO: make it so extends has to be the first tag, maybe in parse_until if nodes
                // is not empty?
                self.parent = Some(name.to_string());
                Ok(None)
            }
            Token::Ident("block") => {
                let (name, _) = expect_token!(self, Token::Ident(s) => s, "identifier")?;
                expect_token!(self, Token::TagEnd(..), "%}")?;
                // TODO: would it matches {{ endblock }}
                let body = self.parse_until(|tok| matches!(tok, Token::Ident("endblock")))?;
                self.next_or_error()?;
                if let Some(Ok((Token::Ident(end_name), _))) = self.next {
                    self.next_or_error()?;
                    if end_name != name {
                        todo!("not matching block names");
                    }
                }

                if self.parent.is_some() {
                    // TODO: error on duplicate blocks, maybe insert an empty vec to keep
                    // track of which ones has been inserted in all cases?
                    self.blocks.insert(name.to_string(), body);
                    Ok(None)
                } else {
                    Ok(Some(Node::Block(Block {
                        name: name.to_string(),
                        body
                    })))
                }


            }
            _ => todo!("handle all cases"),
        }
    }

    fn parse_until<F: Fn(&Token) -> bool>(&mut self, end_check_fn: F) -> TeraResult<Vec<Node>> {
        let mut nodes = Vec::new();

        while let Some((token, ref span)) = self.next()? {
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
                        None => todo!("unexpected eof"),
                        Some(Ok((tok, _))) => tok,
                        e => panic!("got {:?}", e)
                    };
                    if end_check_fn(tok) {
                        return Ok(nodes);
                    }
                    let node = self.parse_tag()?;
                    expect_token!(self, Token::TagEnd(..), "%}")?;
                    if let Some(n) = node {
                        nodes.push(n);
                    }
                }
                _ => unreachable!("Something wrong happened while lexing/parsing"),
            }
        }

        Ok(nodes)
    }

    pub fn parse(&mut self) -> TeraResult<Vec<Node>> {
        // get the first token
        self.next()?;
        let nodes = self.parse_until(|_| false)?;
        Ok(nodes)
    }
}
