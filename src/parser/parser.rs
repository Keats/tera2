use std::iter::Peekable;

use crate::errors::{Error, ErrorKind, TeraResult};
use crate::parser::ast2::{Array, Expression, GetAttr, GetItem, UnaryOperation, Var};
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
                &format!("todo match expect: {:?} {}", token, $expectation),
            )),
        }
    }};
    ($parser:expr, $match:pat => $target:expr, $expectation:expr) => {{
        match $parser.next_or_error()? {
            ($match, span) => Ok(($target, span)),
            (token, _) => Err(Error::new(
                ErrorKind::SyntaxError,
                &format!("todo match expect {:?}, expected {}", token, $expectation),
            )),
        }
    }};
}

// TODO: double check what is actually reserved so we can't re-assign them
const RESERVED_NAMES: [&str; 6] = ["true", "True", "false", "False", "loop", "self"];

pub struct Parser<'a> {
    source: &'a str,
    lexer: Peekable<Box<dyn Iterator<Item = Result<(Token<'a>, Span), Error>> + 'a>>,
    current: Option<Result<(Token<'a>, Span), Error>>,
    // We keep track of the current span TODO
    current_span: Span,
    // filled when we encounter a {% extends %}, we don't need to keep the extends node in the AST
    pub parent: Option<String>,
    // (file, namespace)
    pub macro_imports: Vec<(String, String)>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let iter = Box::new(tokenize(source)) as Box<dyn Iterator<Item = _>>;
        Self {
            source,
            lexer: iter.peekable(),
            current: None,
            current_span: Span::default(),
            parent: None,
            macro_imports: Vec::new(),
        }
    }

    fn current(&mut self) -> Result<Option<(&Token<'a>, &Span)>, Error> {
        if self.current.is_none() {
            self.next()?;
        }

        match self.current {
            Some(Ok(ref tok)) => Ok(Some((&tok.0, &tok.1))),
            Some(Err(_)) => Err(self.current.take().unwrap().unwrap_err()),
            None => Ok(None),
        }
    }

    fn next(&mut self) -> TeraResult<Option<(Token<'a>, Span)>> {
        let cur = self.current.take();
        self.current = self.lexer.next();
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

    fn expect(&mut self, expected_token: Token, message: &str) -> TeraResult<()> {
        let (current_token, _) = self.next_or_error()?;
        if current_token != expected_token {
            return Err(Error::new(
                ErrorKind::SyntaxError,
                &format!("Expected {} but got {:?}", message, current_token),
            ));
        }

        Ok(())
    }
    //
    // fn peek(&mut self) -> TeraResult<Token<'a>> {
    //     match self.lexer.peek()? {
    //
    //     }
    // }

    /// Can be just an ident or a macro call/fn
    fn parse_ident(&mut self, ident: &str) -> TeraResult<Expression> {
        // TODO: handle things not allowed in various context (eg hello["hey"](ho))
        let mut start_span = self.current_span.clone();
        // We might not end up using that one if it's a macro or a fn call
        let mut expr = Expression::Var(Spanned::new(
            Var {
                name: ident.to_string(),
            },
            start_span.clone(),
        ));

        loop {
            match self.current {
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
                    expect_token!(self, Token::LeftParen, "(")?;
                    // TODO: parse kwargs
                    expect_token!(self, Token::RightParen, ")")?;
                }
                // Macro calls
                Some(Ok((Token::Colon, _))) => {
                    // we expect 2, eg macros::bla
                    expect_token!(self, Token::Colon, ":")?;
                    expect_token!(self, Token::Colon, ":")?;
                    // Then the macro name
                    let (macro_name, span) =
                        expect_token!(self, Token::Ident(id) => id, "identifier")?;
                    expect_token!(self, Token::LeftParen, "(")?;
                    // TODO: parse kwargs
                    expect_token!(self, Token::RightParen, ")")?;
                }
                _ => break,
            }
        }

        println!("Expr: {:?}", expr);
        println!("current: {:?}", self.current);

        Ok(expr)
    }

    fn parse_array(&mut self) -> TeraResult<Vec<Expression>> {
        let mut vals = Vec::new();

        loop {
            if matches!(self.current()?, Some((Token::RightBracket, _))) {
                break;
            }

            if !vals.is_empty() {
                self.expect(Token::Comma, ",")?;
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

        let lhs = match token {
            Token::Integer(i) => Expression::Integer(Spanned::new(i, span)),
            Token::Float(f) => Expression::Float(Spanned::new(f, span)),
            Token::String(s) => Expression::Str(Spanned::new(s.to_owned(), span)),
            Token::Ident("true") | Token::Ident("True") => {
                Expression::Bool(Spanned::new(true, span))
            }
            Token::Ident("false") | Token::Ident("False") => {
                Expression::Bool(Spanned::new(false, span))
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
                Expression::UnaryOperation(Spanned::new(UnaryOperation { op, expr }, span))
            }
            Token::Ident(ident) => self.parse_ident(ident)?,
            Token::LeftBracket => {
                let items = self.parse_array()?;
                expect_token!(self, Token::RightBracket, "]")?;
                span.expand(&self.current_span);
                Expression::Array(Spanned::new(Array { items }, span))
            }
            Token::LeftParen => {
                let mut lhs = self.parse_expression(0)?;
                expect_token!(self, Token::RightParen, ")")?;
                lhs.expand_span(&self.current_span);
                lhs
            }
            _ => todo!("finish expression: {:?}", token),
        };

        Ok(lhs)
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
                Token::TagStart(_) => {}
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
