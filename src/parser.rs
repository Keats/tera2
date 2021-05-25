use std::collections::HashMap;

use crate::ast::Expression;
use crate::errors::{ParsingError, ParsingResult, SpannedParsingError};
use crate::lexer::{Operator, PeekableLexer, Symbol, Token};

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

fn eof_error(last_idx: usize) -> SpannedParsingError {
    SpannedParsingError::new(ParsingError::UnexpectedEof, last_idx..last_idx)
}

pub struct Parser<'a> {
    source: &'a str,
    lexer: PeekableLexer<'a>,
    nodes: Vec<usize>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let lexer = PeekableLexer::new(source);

        Self {
            source,
            lexer,
            nodes: Vec::new(),
        }
    }

    pub fn new_in_tag(source: &'a str) -> Self {
        let lexer = PeekableLexer::new_in_tag(source);

        Self {
            source,
            lexer,
            nodes: Vec::new(),
        }
    }

    pub(crate) fn parse(&mut self) {}

    fn parse_template(&mut self) {}

    fn parse_content(&mut self) {}

    fn parse_kwargs(&mut self) -> ParsingResult<HashMap<String, Expression>> {
        let mut kwargs = HashMap::new();

        self.expect(Token::Symbol(Symbol::LeftParen))?;

        loop {
            let name = if let Some(Token::Ident) = self.lexer.peek() {
                self.lexer.next();
                self.lexer.slice().to_owned()
            } else {
                break;
            };
            self.expect(Token::Symbol(Symbol::Assign))?;
            let value = self.parse_expression(0)?;
            kwargs.insert(name, value);

            match self.lexer.peek() {
                Some(Token::Symbol(Symbol::Comma)) => {
                    self.lexer.next();
                    continue;
                }
                Some(Token::Symbol(Symbol::RightParen)) => break,
                _ => {
                    // TODO: error
                    break;
                }
            }
        }

        self.expect(Token::Symbol(Symbol::RightParen))?;

        Ok(kwargs)
    }

    fn parse_ident(&mut self) -> Expression {
        let mut base_ident = self.lexer.slice().to_owned();

        loop {
            match self.lexer.peek() {
                Some(Token::Symbol(Symbol::Dot)) => {
                    self.lexer.next();
                    base_ident.push_str(self.lexer.slice());
                }
                Some(Token::Ident) => {
                    self.lexer.next();
                    let ident = self.parse_ident();
                    base_ident.push_str(&ident.to_string());
                }
                Some(Token::Integer(i)) => {
                    // This is the in 0 for example in an ident like hey.0
                    self.lexer.next();
                    base_ident.push_str(&i.to_string());
                }
                Some(Token::Symbol(Symbol::LeftBracket)) => {
                    self.lexer.next();
                    let in_bracket = self.parse_ident();
                    base_ident.push_str(&in_bracket.to_string());
                }
                Some(Token::Symbol(Symbol::RightBracket)) => {
                    self.lexer.next();
                    base_ident.push_str(self.lexer.slice());
                }
                Some(Token::String) => {
                    self.lexer.next();
                    base_ident.push_str(self.lexer.slice());
                }
                _ => break,
            }
        }

        Expression::Ident(base_ident)
    }

    fn parse_array(&mut self) -> ParsingResult<Expression> {
        let mut vals = Vec::new();

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

        Ok(Expression::Array(vals))
    }

    pub(crate) fn parse_test(&mut self) -> ParsingResult<Expression> {
        self.expect(Token::Ident)?;
        let name = self.lexer.slice().to_owned();
        let mut args = vec![];

        // Do we have arguments?
        if let Some(Token::Symbol(Symbol::LeftParen)) = self.lexer.peek() {
            self.lexer.next();

            loop {
                let expr = self.parse_expression(0)?;
                args.push(expr);

                match self.lexer.peek() {
                    Some(Token::Symbol(Symbol::Comma)) => {
                        self.lexer.next();
                        if let Some(Token::Symbol(Symbol::RightParen)) = self.lexer.peek() {
                            // it was a trailing comma
                            break;
                        }
                    }
                    Some(Token::Symbol(Symbol::RightParen)) => {
                        self.lexer.next();
                        break;
                    }
                    Some(t) => {
                        self.lexer.next();
                        return Err(SpannedParsingError::new(
                            ParsingError::UnexpectedToken(t, None),
                            self.lexer.span(),
                        ));
                    }
                    None => {
                        self.lexer.next();
                        return Err(eof_error(self.lexer.last_idx()));
                    }
                }
            }
        }

        Ok(Expression::Test(name, args))
    }

    pub fn parse_expression(&mut self, min_bp: u8) -> ParsingResult<Expression> {
        let mut lhs = match self.lexer.next() {
            Some(Token::Integer(i)) => Expression::Int(i),
            Some(Token::Float(i)) => Expression::Float(i),
            Some(Token::Bool(i)) => Expression::Bool(i),
            Some(Token::Ident) => {
                // Need to parse it first in case it's actually an ident since we will move
                // past it otherwise
                let ident = self.parse_ident();
                match self.lexer.peek() {
                    // a filter
                    Some(Token::Symbol(Symbol::LeftParen)) => {
                        let kwargs = self.parse_kwargs()?;
                        match ident {
                            Expression::Ident(s) => Expression::Function(s, kwargs),
                            _ => unreachable!("got an ident that is not an ident"),
                        }
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
                        match ident {
                            Expression::Ident(s) => Expression::MacroCall(s, macro_name, kwargs),
                            _ => unreachable!("got an ident that is not an ident"),
                        }
                    }
                    _ => ident,
                }
            }
            Some(Token::String) => Expression::String(self.lexer.slice().to_owned()),
            Some(Token::Symbol(Symbol::LeftBracket)) => self.parse_array()?,
            Some(Token::Symbol(Symbol::LeftParen)) => {
                let lhs = self.parse_expression(0)?;
                self.expect(Token::Symbol(Symbol::RightParen))?;
                lhs
            }
            Some(Token::Op(op)) => {
                let (_, r_bp) = prefix_binding_power(op).map_err(|mut e| {
                    e.range = self.lexer.span();
                    e
                })?;

                let rhs = self.parse_expression(r_bp)?;
                Expression::Expr(op, vec![rhs])
            }
            Some(t) => {
                return Err(SpannedParsingError::new(
                    ParsingError::UnexpectedToken(t, None),
                    self.lexer.span(),
                ))
            }
            None => return Err(eof_error(self.lexer.last_idx())),
        };

        let mut negated = false;
        loop {
            let op = match self.lexer.peek() {
                Some(Token::Op(op)) => op,
                _ => break,
            };

            // Special case for `not in` which is 2 operators in a row
            if op == Operator::Not {
                self.lexer.next();
                self.peek_and_expect(Token::Op(Operator::In))?;
                negated = true;
                continue;
            }

            let (l_bp, r_bp) = infix_binding_power(op);
            // println!("op {}: l-{} r-{} (min bp {})", op, l_bp, r_bp, min_bp);
            if l_bp < min_bp {
                break;
            }

            // Advance past the op
            self.lexer.next();

            let mut rhs = if op == Operator::Is {
                // Special-case `is not`
                match self.lexer.peek() {
                    Some(Token::Op(Operator::Not)) => {
                        negated = true;
                        self.lexer.next();
                    }
                    _ => (),
                }
                self.parse_test()?
            } else {
                self.parse_expression(r_bp)?
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

        // TODO: validate/fold the expression before returning it

        Ok(lhs)
    }

    fn peek_and_expect(&mut self, token: Token) -> ParsingResult<()> {
        match self.lexer.peek() {
            Some(t) => {
                if t != token {
                    Err(SpannedParsingError::new(
                        ParsingError::UnexpectedToken(t, Some(token)),
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
                        ParsingError::UnexpectedToken(t, Some(token)),
                        self.lexer.span(),
                    ))
                } else {
                    Ok(())
                }
            }
            None => Err(eof_error(self.lexer.last_idx())),
        }
    }
}

#[cfg(test)]
mod tests {
    use codespan_reporting::diagnostic::Severity;

    use super::*;

    #[test]
    fn can_parse_ident() {
        let tests = vec![
            "hello",
            "hello_",
            "hello_1",
            "HELLO",
            "_1",
            "hey.ho",
            "h",
            "ho",
            "hey.ho.hu",
            "hey.0",
            "h.u",
            "hey.ho.hu",
            "hey.0",
            "h.u.x.0",
            "hey[0]",
            "hey[a[0]]",
            "hey['ho'][\"hu\"]",
            "h['u'].x[0]",
        ];

        for t in tests {
            let mut parser = Parser::new_in_tag(t);
            assert_eq!(parser.parse_ident().to_string(), t);
        }
    }

    #[test]
    fn can_parse_expression() {
        let tests = vec![
            // literals + basic types
            ("-1", "-1"),
            ("1", "1"),
            ("'hello'", "'hello'"),
            ("true", "true"),
            ("-1.2", "-1.2"),
            ("1.2", "1.2"),
            ("a", "a"),
            ("-a", "(- a)"),
            ("+a", "(+ a)"),
            ("- a * 2", "(- (* a 2))"),
            ("[1, 1.2, a, 'b', true]", "[1, 1.2, a, 'b', true]"),
            ("[1, 1.2, a, 'b', true,]", "[1, 1.2, a, 'b', true]"), // Allows trailing `,`
            // Actual expressions
            ("1 + 2 + 3", "(+ (+ 1 2) 3)"),
            ("1 + count", "(+ 1 count)"),
            ("1 + 2 * 3", "(+ 1 (* 2 3))"),
            ("a + b * c * d + e", "(+ (+ a (* (* b c) d)) e)"),
            // https://github.com/pallets/jinja/issues/119
            ("2 * 4 % 8", "(% (* 2 4) 8)"),
            ("[1 + 1, 2, 3 * 2,]", "[(+ 1 1), 2, (* 3 2)]"),
            // string concat
            ("hey ~ ho", "(~ hey ho)"),
            ("1 ~ ho", "(~ 1 ho)"),
            ("-1.2 ~ ho", "(~ -1.2 ho)"),
            ("[] ~ ho", "(~ [] ho)"),
            ("'hey' ~ ho", "(~ 'hey' ho)"),
            ("`hello` ~ ident ~ 'ho'", "(~ (~ `hello` ident) 'ho')"),
            // Comparisons
            ("a == b", "(== a b)"),
            ("a != b", "(!= a b)"),
            ("a <= b", "(<= a b)"),
            ("a >= b", "(>= a b)"),
            ("a < b", "(< a b)"),
            ("a > b", "(> a b)"),
            ("1 + a > b", "(> (+ 1 a) b)"),
            ("1 + a > b * 8", "(> (+ 1 a) (* b 8))"),
            // and/or
            ("a and b", "(and a b)"),
            ("a or b", "(or a b)"),
            (
                "a + 1 == 2 or b * 3 > 10",
                "(or (== (+ a 1) 2) (> (* b 3) 10))",
            ),
            // in
            ("a in b", "(in a b)"),
            ("a in b and b in c", "(and (in a b) (in b c))"),
            // https://github.com/mozilla/nunjucks/pull/336
            (
                "msg.status in ['pending', 'confirmed'] and msg.body",
                "(and (in msg.status ['pending', 'confirmed']) msg.body)",
            ),
            // test
            ("a is defined", "(is a defined)"),
            ("a is not defined", "(not (is a defined))"),
            ("a + 1 is odd", "(is (+ a 1) odd)"),
            ("a + 1 is not odd", "(not (is (+ a 1) odd))"),
            ("a is ending_with('s')", "(is a ending_with{'s'})"),
            // function calls
            (
                "get_url(path=page.path, in_content=true)",
                "get_url{in_content=true, path=page.path}",
            ),
            ("get_url()", "get_url{}"),
            // filters
            ("a | round", "(| a round{})"),
            ("a | round()", "(| a round{})"),
            ("1 + 2.1 | round", "(| (+ 1 2.1) round{})"),
            ("[1] + [3, 2] | sort", "(| (+ [1] [3, 2]) sort{})"),
            ("(1 + 2.1) | round", "(| (+ 1 2.1) round{})"),
            (
                "value | json_encode | safe",
                "(| (| value json_encode{}) safe{})",
            ),
            (
                "value | truncate(length=10)",
                "(| value truncate{length=10})",
            ),
            (
                "get_content() | upper | safe",
                "(| (| get_content{} upper{}) safe{})",
            ),
            (
                "admin | default or user == current_user",
                "(or (| admin default{}) (== user current_user))",
            ),
            (
                "user == current_user or admin | default",
                "(or (== user current_user) (| admin default{}))",
            ),
            (
                "members in interfaces | groupby(attribute='vlan')",
                "(in members (| interfaces groupby{attribute='vlan'}))",
            ),
            ("a ~ b | upper", "(| (~ a b) upper{})"),
            (
                "status == 'needs_restart' | ternary(truthy='restart', falsy='continue')",
                "(| (== status 'needs_restart') ternary{falsy='continue', truthy='restart'})",
            ),
            (
                "(status == 'needs_restart') | ternary(truthy='restart', falsy='continue')",
                "(| (== status 'needs_restart') ternary{falsy='continue', truthy='restart'})",
            ),
            // Macro calls
            (
                "macros::input(label='Name', type='text')",
                "macros::input{label='Name', type='text'}",
            ),
            ("macros::input() | safe", "(| macros::input{} safe{})"),
            // Parentheses
            ("((1))", "1"),
            ("(2 * 3) / 10", "(/ (* 2 3) 10)"),
            ("(2 * 3) / 10", "(/ (* 2 3) 10)"),
            // not
            ("not a", "(not a)"),
            ("not b * 1", "(not (* b 1))"),
            ("not a and 1 + b > 3", "(and (not a) (> (+ 1 b) 3))"),
            (
                "not id and not true and not 1 + c",
                "(and (and (not id) (not true)) (not (+ 1 c)))",
            ),
            ("a not in b", "(not (in a b))"),
            (
                "a is defined and b is not defined(1, 2)",
                "(and (is a defined) (not (is b defined{1, 2})))",
            ),
            (
                "a is defined and not b is defined(1, 2)",
                "(and (is a defined) (not (is b defined{1, 2})))",
            ),
            (
                "not admin | default(val=true)",
                "(not (| admin default{val=true}))",
            ),
        ];

        for (input, expected) in tests {
            println!("{:?}", input);
            let mut parser = Parser::new_in_tag(input);
            assert_eq!(parser.parse_expression(0).unwrap().to_string(), expected);
        }
    }

    #[test]
    fn can_get_eof_error() {
        let mut parser = Parser::new_in_tag("1 +");
        let err = parser.parse_expression(0).unwrap_err();
        assert_eq!(err.range, 3..3);
        let diag = err.report();
        assert_eq!(diag.severity, Severity::Error);
        assert!(diag.message.contains("Unexpected end of template"));
    }

    #[test]
    fn can_get_unexpected_token_error() {
        let mut parser = Parser::new_in_tag("hello(]");
        let err = parser.parse_expression(0).unwrap_err();
        assert_eq!(err.range, 6..7);
        let diag = err.report();
        assert_eq!(diag.severity, Severity::Error);
        assert!(diag.message.contains("Unexpected token found"));
        assert_eq!(diag.labels[0].range, err.range);
        assert_eq!(diag.labels[0].message, "expected `)`, found `]`");
    }

    #[test]
    fn can_get_unexpected_prefix_operator() {
        let mut parser = Parser::new_in_tag("and");
        let err = parser.parse_expression(0).unwrap_err();
        assert_eq!(err.range, 0..3);
        let diag = err.report();
        assert_eq!(diag.severity, Severity::Error);
        assert!(diag.message.contains("Unexpected operator found"));
        assert_eq!(diag.labels[0].range, err.range);
        assert_eq!(
            diag.labels[0].message,
            "found `and` but only `not`, `+`, `-` can be used here"
        );
    }

    #[test]
    fn can_get_unexpected_token_generic_error() {
        let mut parser = Parser::new_in_tag("=");
        let err = parser.parse_expression(0).unwrap_err();
        assert_eq!(err.range, 0..1);
        let diag = err.report();
        assert_eq!(diag.severity, Severity::Error);
        assert!(diag.message.contains("Unexpected token found"));
        assert_eq!(diag.labels[0].range, err.range);
        assert_eq!(diag.labels[0].message, "found `=`");
    }

    #[test]
    fn can_get_unexpected_token_in_test_args_error() {
        let mut parser = Parser::new_in_tag("1 is odd(1=)");
        let err = parser.parse_expression(0).unwrap_err();
        assert_eq!(err.range, 10..11);
        let diag = err.report();
        assert_eq!(diag.severity, Severity::Error);
        assert!(diag.message.contains("Unexpected token found"));
        assert_eq!(diag.labels[0].range, err.range);
        assert_eq!(diag.labels[0].message, "found `=`");
    }

    // TODO
    // #[test]
    // fn can_parse_expression_constant_folding() {
    //     // TODO
    //
    //     let tests = vec![
    //         // TODO
    //         // https://github.com/Keats/tera/blob/master/src/parser/tests/parser.rs#L1074
    //         // ("`hello` ~ 'hey'", "'hellohey'"),
    //         // ("1 ~ 'ho'", "'1ho'"),
    //         // comparisons
    //         // ("1 == 1", "true"),
    //         // ("1 == '1'", "false"),
    //         // ("1 == 0", "false"),
    //     ];
    //
    //     for (input, expected) in tests {
    //         let mut parser = Parser::new(input);
    //         assert_eq!(parser.parse_expression(0).to_string(), expected);
    //     }
    // }
}
