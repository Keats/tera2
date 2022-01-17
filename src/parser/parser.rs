use crate::errors::{Error, ErrorKind, TeraResult};
use crate::parser::ast2::{Array, Expression, UnaryOperation};
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
            _ => Err(Error::new(ErrorKind::SyntaxError, "todo match expect")),
        }
    }};
}

// TODO: double check what is actually reserved so we can't re-assign them
const RESERVED_NAMES: [&str; 6] = ["true", "True", "false", "False", "loop", "self"];

pub struct Parser<'a> {
    source: &'a str,
    lexer: Box<dyn Iterator<Item = Result<(Token<'a>, Span), Error>> + 'a>,
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
        Self {
            source,
            lexer: Box::new(tokenize(source).peekable()),
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
            return Err(Error::new(ErrorKind::SyntaxError, &format!("Expected {} but got {:?}", message, current_token)));
        }

        Ok(())
    }

    fn peek(&mut self) {}

    /// An ident can have multiple forms:
    /// `hey`
    /// `hey.ho`
    /// `hey['ho']`
    /// `hey[ho]`
    /// `hey[ho].name`
    /// `hey[1]`
    fn parse_ident(&mut self) -> TeraResult<String> {

        Ok(String::new())
    }


    // TODO: make sure it handles trailing commas
    fn parse_array(&mut self) -> TeraResult<Vec<Expression>> {
        let mut vals = Vec::new();

        loop {
            if matches!(self.current()?, Some((Token::RightBracket, _))) {
                break;
            }

            if !vals.is_empty() {
                self.expect(Token::Comma, ",")?;
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
            Token::Ident("true") | Token::Ident("True") => Expression::Bool(Spanned::new(true, span)),
            Token::Ident("false") | Token::Ident("False") => Expression::Bool(Spanned::new(false, span)),
            Token::Minus | Token::Ident("not") => {
                let op = match token {
                    Token::Minus => UnaryOperator::Minus,
                    Token::Ident("not") => UnaryOperator::Not,
                    _ => unreachable!(),
                };
                let (_, r_bp) = unary_binding_power(op);
                let expr = self.parse_expression(r_bp)?;
                span.expand(&self.current_span);
                Expression::UnaryOperation(Spanned::new(UnaryOperation {op, expr}, span))
            }
            Token::Ident(ident) => {

            }
            Token::LeftBracket => {
                let items = self.parse_array()?;
                span.expand(&self.current_span);
                Expression::Array(Spanned::new(Array { items }, span))
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
                    // We have pushed an empty content to replace comment
                    if !c.is_empty() {
                        nodes.push(Node::Content(c.to_owned()));
                    }
                }
                Token::VariableStart(_) => {
                    let expr = self.parse_expression(0)?;
                    expect_token!(self, Token::VariableEnd(..), "%}")?;
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
