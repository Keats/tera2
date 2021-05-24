use crate::ast::Expression;
use crate::lexer::{Operator, PeekableLexer, Symbol, Token};
use std::collections::HashMap;

// From https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

fn prefix_binding_power(op: Operator) -> ((), u8) {
    use Operator::*;

    match op {
        Not => ((), 3),
        Add | Sub => ((), 7),
        _ => panic!("bad op: {:?}", op),
    }
}

fn infix_binding_power(op: Operator) -> (u8, u8) {
    use Operator::*;

    match op {
        And | Or => (1, 2),
        Equal | NotEqual | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual => (5, 6),
        Add | Sub | In | Is => (7, 8),
        Mul | Div | Mod | StrConcat => (11, 12),
        _ => panic!("bad op: {:?}", op),
    }
}

pub(crate) struct Parser<'a> {
    source: &'a str,
    lexer: PeekableLexer<'a>,
    nodes: Vec<usize>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        let lexer = PeekableLexer::new(source);

        Self {
            source,
            lexer,
            nodes: Vec::new(),
        }
    }

    pub(crate) fn new_in_tag(source: &'a str) -> Self {
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

    fn parse_kwargs(&mut self) -> HashMap<String, Expression> {
        let mut kwargs = HashMap::new();

        self.expect(Token::Symbol(Symbol::LeftParen));

        loop {
            let name = if let Some(Token::Ident) = self.lexer.peek() {
                self.lexer.next();
                self.lexer.slice().to_owned()
            } else {
                break;
            };
            self.expect(Token::Symbol(Symbol::Assign));
            let value = self.parse_expression(0);
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

        self.expect(Token::Symbol(Symbol::RightParen));

        kwargs
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

    fn parse_array(&mut self) -> Expression {
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
                _ => vals.push(self.parse_expression(0)),
            };
        }

        Expression::Array(vals)
    }

    pub(crate) fn parse_test(&mut self) -> Expression {
        self.expect(Token::Ident);
        let name = self.lexer.slice().to_owned();
        let mut args = vec![];

        // Do we have arguments?
        if let Some(Token::Symbol(Symbol::LeftParen)) = self.lexer.peek() {
            self.lexer.next();

            loop {
                let expr = self.parse_expression(0);
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
                    _ => panic!(
                        "unexpected token while parsing test args: {:?}",
                        self.lexer.peek()
                    ),
                }
            }
        }

        Expression::Test(name, args)
    }

    pub(crate) fn parse_expression(&mut self, min_bp: u8) -> Expression {
        let mut lhs = match self.lexer.next() {
            Some(Token::Integer(i)) => Expression::Int(i),
            Some(Token::Float(i)) => Expression::Float(i),
            Some(Token::Bool(i)) => Expression::Bool(i),
            Some(Token::Ident) => {
                let ident = self.parse_ident();
                if let Some(Token::Symbol(Symbol::LeftParen)) = self.lexer.peek() {
                    let kwargs = self.parse_kwargs();
                    println!("kwargs {:?}", kwargs);
                    match ident {
                        Expression::Ident(s) => Expression::Function(s.clone(), kwargs),
                        _ => unreachable!("got an ident that is not an ident"),
                    }
                } else {
                    ident
                }
            }
            Some(Token::String) => Expression::String(self.lexer.slice().to_owned()),
            Some(Token::Symbol(Symbol::LeftBracket)) => self.parse_array(),
            Some(Token::Symbol(Symbol::LeftParen)) => {
                let lhs = self.parse_expression(0);
                self.expect(Token::Symbol(Symbol::RightParen));
                lhs
            }
            Some(Token::Op(op)) => {
                let (_, r_bp) = prefix_binding_power(op);
                let rhs = self.parse_expression(r_bp);
                Expression::Expr(op, vec![rhs])
            }
            Some(t) => panic!("wrong token found: {:?}", t),
            None => panic!("no token found"),
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
                // TODO: create `peek_and_expect` if needed more than once
                match self.lexer.peek() {
                    Some(Token::Op(Operator::In)) => (),
                    _ => panic!("Unexpected not token"),
                }
                negated = true;
                continue;
            }

            let (l_bp, r_bp) = infix_binding_power(op);
            if l_bp < min_bp {
                break;
            }

            // Advance past the op
            self.lexer.next();

            let rhs = if op == Operator::Is {
                // Special-case `is not`
                match self.lexer.peek() {
                    Some(Token::Op(Operator::Not)) => {
                        negated = true;
                        self.lexer.next();
                    }
                    _ => (),
                }
                self.parse_test()
            } else {
                self.parse_expression(r_bp)
            };

            lhs = Expression::Expr(op, vec![lhs, rhs]);
            if negated {
                lhs = Expression::Expr(Operator::Not, vec![lhs]);
                negated = false;
            }
            continue;
        }

        // TODO: validate/fold the expression before returning it

        lhs
    }

    fn expect(&mut self, token: Token) {
        match self.lexer.next() {
            Some(t) => {
                if t != token {
                    panic!("Unexpected token found: {:?}, expected {:?}", t, token);
                }
            }
            None => panic!("Reached EOF"),
        }
    }
}

#[cfg(test)]
mod tests {
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
        ];

        for (input, expected) in tests {
            let mut parser = Parser::new_in_tag(input);
            assert_eq!(parser.parse_expression(0).to_string(), expected);
        }
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
