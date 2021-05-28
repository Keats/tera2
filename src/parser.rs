use std::collections::HashMap;

use crate::ast::{Expression, Node, Set};
use crate::errors::{ParsingError, ParsingResult, SpannedParsingError};
use crate::lexer::{Keyword, Operator, PeekableLexer, Symbol, Token};

// From https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

fn prefix_binding_power(op: Operator) -> ParsingResult<((), u8)> {
    use Operator::*;

    match op {
        Not => Ok(((), 3)),
        Add | Sub => Ok(((), 7)),
        _ => Err(SpannedParsingError::new(
            ParsingError::UnexpectedOperator(op, vec![Not, Add, Sub]),
            0..0,
        )),
    }
}

// Some of those could be postfix but if it works like that...
fn infix_binding_power(op: Operator) -> (u8, u8) {
    use Operator::*;

    match op {
        And | Or => (1, 2),
        In | Is => (3, 4),
        Pipe => (5, 6),
        Equal | NotEqual | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual => (7, 8),
        Add | Sub => (11, 12),
        Mul | Div | Mod | StrConcat => (13, 14),
        _ => unreachable!("bad op: {:?}", op),
    }
}

/// Strings are delimited by double quotes, single quotes and backticks
/// We need to remove those before putting them in the AST
fn replace_string_markers(input: &str) -> String {
    match input.chars().next().unwrap() {
        '"' => input.replace('"', ""),
        '\'' => input.replace('\'', ""),
        '`' => input.replace('`', ""),
        _ => unreachable!("How did you even get there"),
    }
}

#[derive(Clone, Debug, PartialEq)]
enum ParsingContext {
    Paren,
    Array,
    TestArgs,
    Kwargs,
    If,
    Elif,
    For,
    Block,
    Set,
}

fn eof_error(last_idx: usize) -> SpannedParsingError {
    SpannedParsingError::new(ParsingError::UnexpectedEof, last_idx..last_idx)
}

pub struct Parser<'a> {
    source: &'a str,
    lexer: PeekableLexer<'a>,
    pub nodes: Vec<Node>,
    contexts: Vec<ParsingContext>,
    trim_left: bool,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let lexer = PeekableLexer::new(source);

        Self {
            source,
            lexer,
            nodes: Vec::new(),
            contexts: Vec::new(),
            trim_left: false,
        }
    }

    pub(crate) fn parse(&mut self) -> ParsingResult<()> {
        self.parse_content()
    }

    pub(crate) fn parse_content(&mut self) -> ParsingResult<()> {
        match self.lexer.next() {
            Some(Token::VariableStart(_)) => {
                let expr = self.parse_expression(0)?;
                self.nodes.push(Node::Expression(expr));
            }
            Some(Token::TagStart(_)) => {
                let node = self.parse_tags()?;
                self.nodes.push(node);
            }
            None => (),
            _ => todo!("Not implemented yet"),
        }

        Ok(())
    }

    fn parse_tags(&mut self) -> ParsingResult<Node> {
        match self.next_or_error()? {
            Token::Keyword(k) => match k {
                Keyword::Set | Keyword::SetGlobal => {
                    self.contexts.push(ParsingContext::Set);
                    let global = k == Keyword::SetGlobal;
                    self.expect(Token::Ident)?;
                    let key = self.parse_ident()?;
                    self.expect(Token::Symbol(Symbol::Assign))?;
                    let value = self.parse_expression(0)?;
                    self.contexts.pop();
                    self.expect_tag_end()?;

                    Ok(Node::Set(Set { key, value, global }))
                }
                Keyword::Include => {
                    let files = match self
                        .expect_one_of(vec![Token::String, Token::Symbol(Symbol::LeftBracket)])?
                    {
                        Token::String => {
                            let val = replace_string_markers(self.lexer.slice());
                            vec![val]
                        }
                        Token::Symbol(Symbol::LeftBracket) => {
                            let start = self.lexer.span().start;
                            let vals = self.parse_array()?.as_array();
                            let end = self.lexer.span().end;
                            let mut files = Vec::with_capacity(vals.len());

                            for v in vals {
                                match v {
                                    Expression::String(s) => files.push(s),
                                    _ => {
                                        return Err(SpannedParsingError::new(
                                            ParsingError::InvalidInclude,
                                            start..end,
                                        ));
                                    }
                                }
                            }
                            files
                        }
                        _ => unreachable!(),
                    };

                    let ignore_missing = match self.peek_or_error()? {
                        Token::Keyword(Keyword::IgnoreMissing) => {
                            self.lexer.next();
                            true
                        }
                        Token::TagEnd(_) => false,
                        t => {
                            return Err(SpannedParsingError::new(
                                ParsingError::UnexpectedToken(
                                    t,
                                    vec![
                                        Token::Keyword(Keyword::IgnoreMissing),
                                        Token::TagEnd(true),
                                        Token::TagEnd(false),
                                    ],
                                ),
                                self.lexer.span(),
                            ))
                        }
                    };
                    self.expect_tag_end()?;
                    Ok(Node::Include {
                        files,
                        ignore_missing,
                    })
                }
                Keyword::Extends => {
                    self.expect(Token::String)?;
                    let val = replace_string_markers(self.lexer.slice());
                    Ok(Node::Extends(val))
                }
                _ => panic!("hey"),
            },
            t => Err(SpannedParsingError::new(
                ParsingError::UnexpectedToken(t, vec![]),
                self.lexer.span(),
            )),
        }
    }

    fn parse_kwargs(&mut self) -> ParsingResult<HashMap<String, Expression>> {
        let mut kwargs = HashMap::new();
        self.contexts.push(ParsingContext::Kwargs);

        self.expect(Token::Symbol(Symbol::LeftParen))?;

        loop {
            let name = match self.next_or_error()? {
                Token::Ident => self.lexer.slice().to_owned(),
                Token::Symbol(Symbol::RightParen) => break,
                t => {
                    return Err(SpannedParsingError::new(
                        ParsingError::UnexpectedToken(
                            t,
                            vec![Token::Ident, Token::Symbol(Symbol::RightParen)],
                        ),
                        self.lexer.span(),
                    ));
                }
            };

            self.expect(Token::Symbol(Symbol::Assign))?;
            let value = self.parse_expression(0)?;
            kwargs.insert(name, value);

            match self.next_or_error()? {
                Token::Symbol(Symbol::RightParen) => break,
                _ => continue,
            }
        }

        self.contexts.pop();
        Ok(kwargs)
    }

    fn parse_ident(&mut self) -> ParsingResult<String> {
        // We are already at the ident token when we start
        let mut base_ident = self.lexer.slice().to_owned();
        let mut after_dot = false;
        let mut in_brackets = Vec::new();

        loop {
            let token = self.peek_or_error()?;

            // After a dot, only an ident or an integer is allowed
            if after_dot {
                after_dot = false;
                self.lexer.next();

                match token {
                    Token::Ident => {
                        base_ident.push_str(&self.lexer.slice());
                    }
                    Token::Integer(i) => {
                        base_ident.push_str(&i.to_string());
                    }
                    t => {
                        return Err(SpannedParsingError::new(
                            ParsingError::UnexpectedToken(t, vec![Token::Ident, Token::Integer(0)]),
                            self.lexer.span(),
                        ));
                    }
                }
                continue;
            }

            // After a left brackets: ident, integer, string
            if !in_brackets.is_empty() {
                self.lexer.next();

                match token {
                    Token::Ident => {
                        base_ident.push_str(&self.lexer.slice());
                    }
                    Token::Integer(i) => {
                        base_ident.push_str(&i.to_string());
                    }
                    Token::String => {
                        base_ident.push_str(self.lexer.slice());
                    }
                    Token::Symbol(Symbol::LeftBracket) => {
                        in_brackets.push(true);
                        base_ident.push_str(self.lexer.slice());
                    }
                    _ => {
                        // Need to disallow a[], base_ident is never an empty string
                        if token == Token::Symbol(Symbol::RightBracket)
                            && !base_ident.ends_with('[')
                        {
                            in_brackets.pop();
                            base_ident.push_str(self.lexer.slice());
                            continue;
                        }

                        return Err(SpannedParsingError::new(
                            ParsingError::UnexpectedToken(
                                token,
                                vec![
                                    Token::Ident,
                                    Token::Integer(0),
                                    Token::String,
                                    Token::Symbol(Symbol::RightBracket),
                                ],
                            ),
                            self.lexer.span(),
                        ));
                    }
                }
                continue;
            }

            // After an ident, only dot, left bracket
            // In array it can be followed by a `,` and in functions by `=` or `,`
            let mut allow_comma = false;
            let mut allow_assign = false;
            if let Some(c) = self.contexts.last() {
                allow_comma = *c == ParsingContext::Array
                    || *c == ParsingContext::TestArgs
                    || *c == ParsingContext::Kwargs;
                allow_assign = *c == ParsingContext::Kwargs || *c == ParsingContext::Set;
            }

            match token {
                Token::Symbol(Symbol::Dot) => {
                    after_dot = true;
                    self.lexer.next();
                    base_ident.push_str(self.lexer.slice());
                }
                Token::Symbol(Symbol::LeftBracket) => {
                    in_brackets.push(true);
                    self.lexer.next();
                    base_ident.push_str(self.lexer.slice());
                }
                Token::Op(_)
                | Token::VariableEnd(_)
                | Token::TagEnd(_)
                | Token::Symbol(Symbol::LeftParen)
                | Token::Symbol(Symbol::DoubleColumn) => break,
                _ => {
                    if token == Token::Symbol(Symbol::Comma) && allow_comma {
                        break;
                    }
                    if token == Token::Symbol(Symbol::Assign) && allow_assign {
                        break;
                    }

                    self.lexer.next();
                    return Err(SpannedParsingError::new(
                        ParsingError::UnexpectedToken(
                            token,
                            vec![
                                Token::Symbol(Symbol::Dot),
                                Token::Symbol(Symbol::LeftBracket),
                            ],
                        ),
                        self.lexer.span(),
                    ));
                }
            }
        }

        Ok(base_ident)
    }

    fn parse_array(&mut self) -> ParsingResult<Expression> {
        let mut vals = Vec::new();
        self.contexts.push(ParsingContext::Array);

        loop {
            match self.lexer.peek() {
                Some(Token::Symbol(Symbol::Comma)) => {
                    self.lexer.next();
                }
                Some(Token::Symbol(Symbol::RightBracket)) => {
                    self.lexer.next();
                    break;
                }
                _ => vals.push(self.parse_expression(0)?),
            };
        }

        self.contexts.pop();
        Ok(Expression::Array(vals))
    }

    pub(crate) fn parse_test(&mut self) -> ParsingResult<Expression> {
        self.expect(Token::Ident)?;
        let name = self.lexer.slice().to_owned();
        let mut args = vec![];
        self.contexts.push(ParsingContext::TestArgs);

        // Do we have arguments?
        if let Some(Token::Symbol(Symbol::LeftParen)) = self.lexer.peek() {
            self.lexer.next();

            loop {
                let expr = self.parse_expression(0)?;
                args.push(expr);

                match self.next_or_error()? {
                    Token::Symbol(Symbol::Comma) => {
                        if let Some(Token::Symbol(Symbol::RightParen)) = self.lexer.peek() {
                            // it was a trailing comma
                            break;
                        }
                    }
                    Token::Symbol(Symbol::RightParen) => {
                        break;
                    }
                    t => {
                        return Err(SpannedParsingError::new(
                            ParsingError::UnexpectedToken(
                                t,
                                vec![
                                    Token::Symbol(Symbol::Comma),
                                    Token::Symbol(Symbol::RightParen),
                                ],
                            ),
                            self.lexer.span(),
                        ));
                    }
                }
            }
        }

        self.contexts.pop();
        Ok(Expression::Test(name, args))
    }

    pub fn parse_expression(&mut self, min_bp: u8) -> ParsingResult<Expression> {
        let mut lhs = match self.next_or_error()? {
            Token::Integer(i) => Expression::Int(i),
            Token::Float(i) => Expression::Float(i),
            Token::Bool(i) => Expression::Bool(i),
            Token::Ident => {
                // Need to parse it first in case it's actually an ident since we will move
                // past it otherwise
                let ident = self.parse_ident()?;
                match self.lexer.peek() {
                    // a function
                    Some(Token::Symbol(Symbol::LeftParen)) => {
                        let kwargs = self.parse_kwargs()?;
                        Expression::Function(ident, kwargs)
                    }
                    // a macro call
                    Some(Token::Symbol(Symbol::DoubleColumn)) => {
                        self.lexer.next();
                        // Should be followed by macro name
                        self.expect(Token::Ident)?;
                        let macro_name = self.lexer.slice().to_owned();
                        // and left paren
                        self.peek_and_expect(Token::Symbol(Symbol::LeftParen))?;
                        let kwargs = self.parse_kwargs()?;
                        Expression::MacroCall(ident, macro_name, kwargs)
                    }
                    _ => Expression::Ident(ident),
                }
            }
            Token::String => Expression::String(replace_string_markers(self.lexer.slice())),
            Token::Symbol(Symbol::LeftBracket) => self.parse_array()?,
            Token::Symbol(Symbol::LeftParen) => {
                self.contexts.push(ParsingContext::Paren);
                let lhs = self.parse_expression(0)?;
                self.expect(Token::Symbol(Symbol::RightParen))?;
                self.contexts.pop();
                lhs
            }
            Token::Op(op) => {
                let (_, r_bp) = prefix_binding_power(op).map_err(|mut e| {
                    e.range = self.lexer.span();
                    e
                })?;

                let rhs = self.parse_expression(r_bp)?;
                Expression::Expr(op, vec![rhs])
            }
            t => {
                return Err(SpannedParsingError::new(
                    ParsingError::UnexpectedToken(t, vec![]),
                    self.lexer.span(),
                ))
            }
        };

        let mut negated = false;
        loop {
            let op = match self.lexer.peek() {
                Some(Token::Op(op)) => op,
                Some(t @ Token::Symbol(_)) => {
                    if let Some(c) = self.contexts.last() {
                        match c {
                            ParsingContext::Array => {
                                let tokens = vec![
                                    Token::Symbol(Symbol::Comma),
                                    Token::Symbol(Symbol::RightBracket),
                                ];
                                if !tokens.contains(&t) {
                                    self.lexer.next();
                                    return Err(SpannedParsingError::new(
                                        ParsingError::UnexpectedToken(t, tokens),
                                        self.lexer.span(),
                                    ));
                                }
                                break;
                            }
                            ParsingContext::TestArgs => {
                                let tokens = vec![
                                    Token::Symbol(Symbol::Comma),
                                    Token::Symbol(Symbol::RightParen),
                                ];
                                if !tokens.contains(&t) {
                                    self.lexer.next();
                                    return Err(SpannedParsingError::new(
                                        ParsingError::UnexpectedToken(t, tokens),
                                        self.lexer.span(),
                                    ));
                                }
                                break;
                            }
                            ParsingContext::Kwargs => {
                                let tokens = vec![
                                    Token::Symbol(Symbol::Comma),
                                    Token::Symbol(Symbol::RightParen),
                                ];
                                if !tokens.contains(&t) {
                                    self.lexer.next();
                                    return Err(SpannedParsingError::new(
                                        ParsingError::UnexpectedToken(t, tokens),
                                        self.lexer.span(),
                                    ));
                                }
                                break;
                            }
                            _ => break,
                        }
                    } else {
                        self.lexer.next();
                        return Err(SpannedParsingError::new(
                            ParsingError::UnexpectedToken(t, vec![]),
                            self.lexer.span(),
                        ));
                    }
                }
                Some(_) => break,
                None => {
                    break;
                    // TODO?
                    // self.lexer.next();
                    // return Err(eof_error(self.lexer.last_idx()));
                }
            };

            // Special case for `not in` which is 2 operators in a row
            if op == Operator::Not {
                self.lexer.next();
                self.peek_and_expect(Token::Op(Operator::In))?;
                negated = true;
                continue;
            }

            let (l_bp, r_bp) = infix_binding_power(op);
            if l_bp < min_bp {
                break;
            }
            // Advance past the op
            self.lexer.next();

            let mut rhs = if op == Operator::Is {
                // Special-case `is not`
                if let Some(Token::Op(Operator::Not)) = self.lexer.peek() {
                    negated = true;
                    self.lexer.next();
                }
                self.parse_test()?
            } else {
                match self.lexer.peek() {
                    Some(t @ Token::Op(_)) => {
                        // Only `is`, `and` and `or` can have an operator after and it should always be `not`
                        if t == Token::Op(Operator::Not)
                            && (op == Operator::And || op == Operator::Or)
                        {
                            self.parse_expression(r_bp)?
                        } else {
                            self.lexer.next();
                            return Err(SpannedParsingError::new(
                                ParsingError::UnexpectedToken(t, vec![]),
                                self.lexer.span(),
                            ));
                        }
                    }
                    _ => self.parse_expression(r_bp)?,
                }
            };

            // We can have filters that look like ident, without parentheses so we need to convert
            // them to a function
            if op == Operator::Pipe {
                rhs = match rhs {
                    Expression::Ident(s) => Expression::Function(s, HashMap::new()),
                    _ => rhs,
                };
            }

            lhs = Expression::Expr(op, vec![lhs, rhs]);
            if negated {
                lhs = Expression::Expr(Operator::Not, vec![lhs]);
                negated = false;
            }
            continue;
        }

        Ok(lhs)
    }

    fn peek_or_error(&mut self) -> ParsingResult<Token> {
        match self.lexer.peek() {
            Some(t) => Ok(t),
            None => {
                self.lexer.next();
                Err(eof_error(self.lexer.last_idx()))
            }
        }
    }

    fn next_or_error(&mut self) -> ParsingResult<Token> {
        match self.lexer.next() {
            Some(t) => Ok(t),
            None => Err(eof_error(self.lexer.last_idx())),
        }
    }

    fn peek_and_expect(&mut self, token: Token) -> ParsingResult<()> {
        match self.lexer.peek() {
            Some(t) => {
                if t != token {
                    Err(SpannedParsingError::new(
                        ParsingError::UnexpectedToken(t, vec![token]),
                        self.lexer.span(),
                    ))
                } else {
                    Ok(())
                }
            }
            None => Err(eof_error(self.lexer.last_idx())),
        }
    }

    fn expect(&mut self, token: Token) -> ParsingResult<()> {
        match self.lexer.next() {
            Some(t) => {
                if t != token {
                    Err(SpannedParsingError::new(
                        ParsingError::UnexpectedToken(t, vec![token]),
                        self.lexer.span(),
                    ))
                } else {
                    Ok(())
                }
            }
            None => Err(eof_error(self.lexer.last_idx())),
        }
    }

    fn expect_one_of(&mut self, tokens: Vec<Token>) -> ParsingResult<Token> {
        match self.lexer.next() {
            Some(t) => {
                if !tokens.contains(&t) {
                    Err(SpannedParsingError::new(
                        ParsingError::UnexpectedToken(t, tokens),
                        self.lexer.span(),
                    ))
                } else {
                    Ok(t)
                }
            }
            None => Err(eof_error(self.lexer.last_idx())),
        }
    }

    fn expect_tag_end(&mut self) -> ParsingResult<()> {
        if let Token::TagEnd(b) =
            self.expect_one_of(vec![Token::TagEnd(false), Token::TagEnd(true)])?
        {
            self.trim_left = b;
        }
        Ok(())
    }
}
