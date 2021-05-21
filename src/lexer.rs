use std::fmt;

use logos::{Lexer, Logos};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    // math
    Mul,
    Div,
    Mod,
    Add,
    Sub,

    // comparison
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,

    // rest
    Not,
    And,
    Or,
    StrConcat,
    In,
}

impl Operator {
    fn from_str(s: &str) -> Operator {
        match s {
            "*" => Operator::Mul,
            "/" => Operator::Div,
            "%" => Operator::Mod,
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "<" => Operator::LessThan,
            ">" => Operator::GreaterThan,
            "<=" => Operator::LessThanOrEqual,
            ">=" => Operator::GreaterThanOrEqual,
            "==" => Operator::Equal,
            "!=" => Operator::NotEqual,
            "and" => Operator::And,
            "or" => Operator::Or,
            "not" => Operator::Not,
            "in" => Operator::In,
            "~" => Operator::StrConcat,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let val = match self {
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::Mod => "%",
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::LessThan => "<",
            Operator::GreaterThan => ">",
            Operator::LessThanOrEqual => "<=",
            Operator::GreaterThanOrEqual => ">=",
            Operator::Equal => "==",
            Operator::NotEqual => "!=",
            Operator::And => "and",
            Operator::Or => "or",
            Operator::Not => "not",
            Operator::In => "in",
            Operator::StrConcat => "~",
        };
        write!(f, "{}", val)
    }
}

#[derive(Debug, Default, PartialEq, Copy, Clone)]
pub(crate) struct LexerExtras {
    line_number: usize,
}

fn lex_operator(lex: &mut logos::Lexer<Token>) -> Operator {
    Operator::from_str(lex.slice())
}

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(extras = LexerExtras)]
pub(crate) enum Token {
    #[token("\n", |lex| {
        lex.extras.line_number += 1;
    })]
    #[regex(r"[ \t\r]+")]
    Whitespace,

    #[token("true", |_| true)]
    #[token("True", |_| true)]
    #[token("false", |_| false)]
    #[token("False", |_| false)]
    Bool(bool),

    // maths
    #[token("+", lex_operator)]
    #[token("-", lex_operator)]
    #[token("/", lex_operator)]
    #[token("*", lex_operator)]
    #[token("%", lex_operator)]
    // comparison
    #[token("==", lex_operator)]
    #[token("!=", lex_operator)]
    #[token(">=", lex_operator)]
    #[token("<=", lex_operator)]
    #[token(">", lex_operator)]
    #[token("<", lex_operator)]
    // and the rest
    #[token("or", lex_operator)]
    #[token("and", lex_operator)]
    #[token("not", lex_operator)]
    #[token("in", lex_operator)]
    #[token("~", lex_operator)]
    Op(Operator),

    #[token("=")]
    Assign,
    #[token("|")]
    Pipe,

    #[token("{{", |_| false)]
    #[token("{{-", |_| true)]
    VariableStart(bool),
    #[token("}}", |_| false)]
    #[token("-}}", |_| true)]
    VariableEnd(bool),

    #[token("{%", |_| false)]
    #[token("{%-", |_| true)]
    TagStart(bool),
    #[token("%}", |_| false)]
    #[token("-%}", |_| true)]
    TagEnd(bool),

    #[token("{#", |_| false)]
    #[token("{#-", |_| true)]
    CommentStart(bool),
    #[token("#}", |_| false)]
    #[token("-#}", |_| true)]
    CommentEnd(bool),

    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,

    #[regex("\"(?s:[^\"\\\\]|\\\\.)*\"")]
    StringDoubleQuoted,
    #[regex("'(?s:[^'\\\\]|\\\\.)*'")]
    StringSingleQuoted,
    #[regex("`(?s:[^`\\\\]|\\\\.)*`")]
    StringBacktickQuoted,

    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*")]
    Ident,

    #[regex("-?[0-9]+", |lex| lex.slice().parse())]
    Integer(i64),

    #[regex("-?[0-9]+\\.[0-9]+", |lex| lex.slice().parse())]
    Float(f64),

    // All the keywords
    #[token("for")]
    For,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("endfor")]
    EndFor,
    #[token("if")]
    If,
    #[token("elif")]
    Elif,
    #[token("else")]
    Else,
    #[token("endif")]
    EndIf,
    #[token("block")]
    Block,
    #[token("super()")]
    Super,
    #[token("endblock")]
    EndBlock,
    #[token("macro")]
    Macro,
    #[token("endmacro")]
    EndMacro,
    #[token("raw")]
    Raw,
    #[token("endraw")]
    EndRaw,
    #[token("include")]
    Include,
    #[token("filter")]
    Filter,
    #[token("endfilter")]
    EndFilter,
    #[token("set")]
    Set,
    #[token("set_global")]
    SetGlobal,
    #[token("is")]
    Is,
    #[token("ignore")]
    Ignore,
    #[token("missing")]
    Missing,
    #[token("extends")]
    Extends,
    #[token("import")]
    Import,
    #[token("As")]
    As,

    #[error]
    Error,
}

pub(crate) struct PeekableLexer<'source> {
    lexer: Lexer<'source, Token>,
    peeked: Option<Option<Token>>,
}

impl<'source> PeekableLexer<'source> {
    pub(crate) fn new(source: &'source str) -> Self {
        Self {
            lexer: Token::lexer(source),
            peeked: None,
        }
    }

    pub(crate) fn peek(&mut self) -> Option<Token> {
        if self.peeked.is_none() {
            self.peeked = Some(self.lexer.next());
        }
        self.peeked.unwrap()
    }

    pub(crate) fn slice(&self) -> &str {
        self.lexer.slice()
    }

    pub(crate) fn extras(&self) -> &LexerExtras {
        &self.lexer.extras
    }
}

impl<'source> Iterator for PeekableLexer<'source> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(peeked) = self.peeked.take() {
            peeked
        } else {
            self.lexer.next()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::Range;

    fn assert_lex(source: &str, tokens: &[(Token, &str, Range<usize>)]) {
        let mut lex = Token::lexer(source);

        for tuple in tokens {
            assert_eq!(
                &(lex.next().expect("Unexpected end"), lex.slice(), lex.span()),
                tuple
            );
        }

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn can_lex_int() {
        let tests = vec![
            ("0", 0),
            ("10", 10),
            ("10000", 10000),
            ("010000", 10000),
            ("-100", -100),
        ];
        for (t, val) in tests {
            let mut lex = Token::lexer(t);
            assert_eq!(lex.next().unwrap(), Token::Integer(val));
            assert!(lex.next().is_none());
        }
    }

    #[test]
    fn can_lex_float() {
        let tests = vec![
            ("0.0", 0.0),
            ("10.1", 10.1),
            ("10000.09", 10000.09),
            ("010000.12", 10000.12),
            ("-100.200", -100.2),
        ];
        for (t, val) in tests {
            let mut lex = Token::lexer(t);
            assert_eq!(lex.next().unwrap(), Token::Float(val));
            assert!(lex.next().is_none());
        }
    }

    #[test]
    fn can_lex_ident() {
        let tests = vec!["hello", "hello_", "hello_1", "HELLO", "_1"];
        for t in tests {
            let mut lex = Token::lexer(t);
            assert_eq!(lex.next().unwrap(), Token::Ident);
            assert!(lex.next().is_none());
        }
    }

    #[test]
    fn can_lex_double_quoted_strings() {
        let tests = vec![
            r#""a string12345""#,
            r#""a 'string""#,
            r#""a `string""#,
            r#""a \"string""#,
        ];
        for t in tests {
            let mut lex = Token::lexer(t);
            assert_eq!(lex.next().unwrap(), Token::StringDoubleQuoted);
            assert!(lex.next().is_none());
        }
    }

    #[test]
    fn can_lex_single_quoted_strings() {
        let tests = vec![
            r#"'a string12345'"#,
            r#"'a \'string'"#,
            r#"'a `string'"#,
            r#"'a "string'"#,
        ];
        for t in tests {
            let mut lex = Token::lexer(t);
            assert_eq!(lex.next().unwrap(), Token::StringSingleQuoted);
            assert!(lex.next().is_none());
        }
    }

    #[test]
    fn can_lex_backtick_quoted_strings() {
        let tests = vec![
            r#"`a string12345`"#,
            r#"`a 'string`"#,
            r#"`a \`string`"#,
            r#"`a "string`"#,
        ];
        for t in tests {
            let mut lex = Token::lexer(t);
            assert_eq!(lex.next().unwrap(), Token::StringBacktickQuoted);
            assert!(lex.next().is_none());
        }
    }

    #[test]
    fn can_lex_an_expression() {
        assert_lex(
            "{{- name*10+1.0 }}",
            &[
                (Token::VariableStart(true), "{{-", 0..3),
                (Token::Whitespace, " ", 3..4),
                (Token::Ident, "name", 4..8),
                (Token::Op(Operator::Mul), "*", 8..9),
                (Token::Integer(10), "10", 9..11),
                (Token::Op(Operator::Add), "+", 11..12),
                (Token::Float(1.0), "1.0", 12..15),
                (Token::Whitespace, " ", 15..16),
                (Token::VariableEnd(false), "}}", 16..18),
            ],
        );
    }

    #[test]
    fn can_keep_count_of_line_number() {
        let mut lex = Token::lexer("hello\nworld\nhi");
        assert_eq!(lex.extras.line_number, 0);
        assert_eq!(lex.next().unwrap(), Token::Ident);
        assert_eq!(lex.next().unwrap(), Token::Whitespace);
        assert_eq!(lex.extras.line_number, 1);
        assert_eq!(lex.next().unwrap(), Token::Ident);
        assert_eq!(lex.next().unwrap(), Token::Whitespace);
        assert_eq!(lex.extras.line_number, 2);
        assert_eq!(lex.next().unwrap(), Token::Ident);
    }
}
