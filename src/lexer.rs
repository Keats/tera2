use std::fmt;

use logos::{Lexer, Logos};
use std::ops::Range;

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
    Is,
    Pipe,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Operator::*;

        let val = match self {
            Mul => "*",
            Div => "/",
            Mod => "%",
            Add => "+",
            Sub => "-",
            LessThan => "<",
            GreaterThan => ">",
            LessThanOrEqual => "<=",
            GreaterThanOrEqual => ">=",
            Equal => "==",
            NotEqual => "!=",
            And => "and",
            Or => "or",
            Not => "not",
            In => "in",
            Is => "is",
            StrConcat => "~",
            Pipe => "|",
        };
        write!(f, "{}", val)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Keyword {
    For,
    Break,
    Continue,
    EndFor,
    If,
    Elif,
    Else,
    EndIf,
    Block,
    Super,
    EndBlock,
    Macro,
    EndMacro,
    Raw,
    EndRaw,
    Include,
    Filter,
    EndFilter,
    Set,
    SetGlobal,
    IgnoreMissing,
    Extends,
    Import,
    As,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Keyword::*;
        let val = match self {
            For => "for",
            Break => "break",
            Continue => "continue",
            EndFor => "endfor",
            If => "if",
            Elif => "elif",
            Else => "else",
            EndIf => "endif",
            Block => "block",
            Super => "super()",
            EndBlock => "endblock",
            Macro => "macro",
            EndMacro => "endmacro",
            Raw => "raw",
            EndRaw => "endraw",
            Include => "include",
            Filter => "filter",
            EndFilter => "endfilter",
            Set => "set",
            SetGlobal => "set_global",
            IgnoreMissing => "ignore missing",
            Extends => "extends",
            Import => "import",
            As => "as",
        };
        write!(f, "{}", val)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Symbol {
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    LeftParen,
    RightParen,
    Assign,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token {
    VariableStart(bool),
    VariableEnd(bool),
    TagStart(bool),
    TagEnd(bool),
    Comment,

    Bool(bool),
    Op(Operator),
    String,
    Ident,
    Integer(i64),
    Float(f64),
    Keyword(Keyword),
    Symbol(Symbol),

    Error,
}

// TODO: inline those?
impl Token {
    fn from_content(tok: Content) -> Self {
        match tok {
            Content::VariableStart(b) => Self::VariableStart(b),
            Content::TagStart(b) => Self::TagStart(b),
            Content::Comment => Self::Comment,
            Content::Error => Self::Error,
        }
    }

    fn from_in_tag(tok: InTag) -> Self {
        match tok {
            InTag::Symbol(s) => Self::Symbol(s),
            InTag::Keyword(k) => Self::Keyword(k),
            InTag::Op(op) => Self::Op(op),
            InTag::Integer(i) => Self::Integer(i),
            InTag::Float(f) => Self::Float(f),
            InTag::Bool(b) => Self::Bool(b),
            InTag::Ident => Self::Ident,
            InTag::String => Self::String,
            InTag::Error => Self::Error,
            InTag::VariableEnd(b) => Self::VariableEnd(b),
            InTag::TagEnd(b) => Self::TagEnd(b),
        }
    }
}

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
pub(crate) enum Content {
    #[token("{{", |_| false)]
    #[token("{{-", |_| true)]
    VariableStart(bool),
    #[token("{%", |_| false)]
    #[token("{%-", |_| true)]
    TagStart(bool),

    #[token("{#", |lex| {
        let len = lex.remainder().find("#}")?;
        lex.bump(len + 2); // include len of `#}`

        Some(())
    })]
    Comment,

    #[regex(r"[^{]+", logos::skip)]
    #[token("{", logos::skip)]
    #[error]
    Error,
}

impl Content {
    fn is_tag_start(&self) -> bool {
        matches!(self, Self::TagStart(_) | Self::VariableStart(_))
    }
}

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
pub(crate) enum InTag {
    #[token("}}", |_| false)]
    #[token("-}}", |_| true)]
    VariableEnd(bool),
    #[token("%}", |_| false)]
    #[token("-%}", |_| true)]
    TagEnd(bool),

    #[token("true", |_| true)]
    #[token("True", |_| true)]
    #[token("false", |_| false)]
    #[token("False", |_| false)]
    Bool(bool),

    // maths
    #[token("+", |_| Operator::Add)]
    #[token("-", |_| Operator::Sub)]
    #[token("/", |_| Operator::Div)]
    #[token("*", |_| Operator::Mul)]
    #[token("%", |_| Operator::Mod)]
    // comparison
    #[token("==", |_| Operator::Equal)]
    #[token("!=", |_| Operator::NotEqual)]
    #[token(">=", |_| Operator::GreaterThanOrEqual)]
    #[token("<=", |_| Operator::LessThanOrEqual)]
    #[token(">", |_| Operator::GreaterThan)]
    #[token("<", |_| Operator::LessThan)]
    // and the rest
    #[token("or", |_| Operator::Or)]
    #[token("and", |_| Operator::And)]
    #[token("not", |_| Operator::Not)]
    #[token("in", |_| Operator::In)]
    #[token("is", |_| Operator::In)]
    #[token("~", |_| Operator::StrConcat)]
    #[token("|", |_| Operator::Pipe)]
    Op(Operator),

    #[token("=", |_| Symbol::Assign)]
    #[token("[", |_| Symbol::LeftBracket)]
    #[token("]", |_| Symbol::RightBracket)]
    #[token(",", |_| Symbol::Comma)]
    #[token(".", |_| Symbol::Dot)]
    #[token("(", |_| Symbol::LeftParen)]
    #[token(")", |_| Symbol::RightParen)]
    Symbol(Symbol),

    #[regex("\"(?s:[^\"\\\\]|\\\\.)*\"")]
    #[regex("'(?s:[^'\\\\]|\\\\.)*'")]
    #[regex("`(?s:[^`\\\\]|\\\\.)*`")]
    String,
    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*")]
    Ident,
    #[regex("-?[0-9]+", |lex| lex.slice().parse())]
    Integer(i64),
    #[regex("-?[0-9]+\\.[0-9]+", |lex| lex.slice().parse())]
    Float(f64),

    #[token("for", |_| Keyword::For)]
    #[token("break", |_| Keyword::Break)]
    #[token("continue", |_| Keyword::Continue)]
    #[token("endfor", |_| Keyword::EndFor)]
    #[token("if", |_| Keyword::If)]
    #[token("elif", |_| Keyword::Elif)]
    #[token("else", |_| Keyword::Else)]
    #[token("endif", |_| Keyword::EndIf)]
    #[token("block", |_| Keyword::Block)]
    #[token("super()", |_| Keyword::Super)]
    #[token("endblock", |_| Keyword::EndBlock)]
    #[token("macro", |_| Keyword::Macro)]
    #[token("endmacro", |_| Keyword::EndMacro)]
    #[token("raw", |_| Keyword::Raw)]
    #[token("endraw", |_| Keyword::EndRaw)]
    #[token("include", |_| Keyword::Include)]
    #[token("filter", |_| Keyword::Filter)]
    #[token("endfilter", |_| Keyword::EndFilter)]
    #[token("set", |_| Keyword::Set)]
    #[token("set_global", |_| Keyword::SetGlobal)]
    #[token("ignore missing", |_| Keyword::IgnoreMissing)]
    #[token("extends", |_| Keyword::Extends)]
    #[token("import", |_| Keyword::Import)]
    #[token("as", |_| Keyword::As)]
    Keyword(Keyword),

    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    #[error]
    Error,
}

impl InTag {
    fn is_tag_end(&self) -> bool {
        matches!(self, Self::TagEnd(_) | Self::VariableEnd(_))
    }
}

pub(crate) enum LexerKind<'source> {
    Content(logos::Lexer<'source, Content>),
    InTag(logos::Lexer<'source, InTag>),
    Done,
}

impl<'source> Default for LexerKind<'source> {
    fn default() -> Self {
        Self::Done
    }
}

pub(crate) struct PeekableLexer<'source> {
    source: &'source str,
    lexer: LexerKind<'source>,
    last: usize,
    peeked: Option<Option<Token>>,
}

impl<'source> PeekableLexer<'source> {
    pub(crate) fn new(source: &'source str) -> Self {
        Self {
            source,
            lexer: LexerKind::Content(Lexer::new(source)),
            last: 0,
            peeked: None,
        }
    }

    /// Only used in tests
    pub(crate) fn new_in_tag(source: &'source str) -> Self {
        Self {
            source,
            lexer: LexerKind::InTag(Lexer::new(source)),
            last: 0,
            peeked: None,
        }
    }

    pub(crate) fn peek(&mut self) -> Option<Token> {
        if self.peeked.is_none() {
            self.peeked = Some(self.next());
        }
        self.peeked.unwrap()
    }

    pub(crate) fn span(&self) -> Range<usize> {
        match &self.lexer {
            LexerKind::Content(l) => l.span(),
            LexerKind::InTag(l) => l.span(),
            LexerKind::Done => unreachable!(),
        }
    }

    pub(crate) fn slice(&self) -> &str {
        match &self.lexer {
            LexerKind::Content(l) => l.slice(),
            LexerKind::InTag(l) => l.slice(),
            LexerKind::Done => unreachable!(),
        }
    }

    pub(crate) fn slice_before(&self) -> &str {
        let start = self.span().start;
        &self.source[self.last..start]
    }
}

impl<'source> Iterator for PeekableLexer<'source> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(peeked) = self.peeked.take() {
            peeked
        } else {
            match std::mem::take(&mut self.lexer) {
                LexerKind::Content(mut lexer) => {
                    self.last = lexer.span().end;
                    let tok = lexer.next()?;
                    if tok.is_tag_start() {
                        self.lexer = LexerKind::InTag(lexer.morph());
                    } else {
                        self.lexer = LexerKind::Content(lexer);
                    }
                    Some(Token::from_content(tok))
                }
                LexerKind::InTag(mut lexer) => {
                    self.last = lexer.span().end;
                    let tok = lexer.next()?;
                    if tok.is_tag_end() {
                        self.lexer = LexerKind::Content(lexer.morph());
                    } else {
                        self.lexer = LexerKind::InTag(lexer);
                    }
                    Some(Token::from_in_tag(tok))
                }
                LexerKind::Done => None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::Range;

    fn assert_lex(source: &str, tokens: &[(Token, &str, Range<usize>)], in_tag: bool) {
        let mut lex = if in_tag {
            PeekableLexer::new_in_tag(source)
        } else {
            PeekableLexer::new(source)
        };

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
            let mut lex = PeekableLexer::new_in_tag(t);
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
            let mut lex = PeekableLexer::new_in_tag(t);
            assert_eq!(lex.next().unwrap(), Token::Float(val));
            assert!(lex.next().is_none());
        }
    }

    #[test]
    fn can_lex_ident() {
        let tests = vec!["hello", "hello_", "hello_1", "HELLO", "_1"];
        for t in tests {
            let mut lex = PeekableLexer::new_in_tag(t);
            assert_eq!(lex.next().unwrap(), Token::Ident);
            assert!(lex.next().is_none());
        }
    }

    #[test]
    fn can_lex_all_types_of_strings() {
        let tests = vec![
            r#""a string12345""#,
            r#""a 'string""#,
            r#""a `string""#,
            r#""a \"string""#,
            r#"`a string12345`"#,
            r#"`a 'string`"#,
            r#"`a \`string`"#,
            r#"`a "string`"#,
            r#"'a string12345'"#,
            r#"'a \'string'"#,
            r#"'a `string'"#,
            r#"'a "string'"#,
        ];
        for t in tests {
            let mut lex = PeekableLexer::new_in_tag(t);
            assert_eq!(lex.next().unwrap(), Token::String);
            assert!(lex.next().is_none());
        }
    }

    #[test]
    fn can_lex_comments() {
        let tests = vec![
            "{# basic #}",
            "{# line1 \n line2 #}",
            "{# #}",
            "{# 'hey' 1 true +=*/% hello() #}",
            "{##}",
            "{###}",
            "{# some ### comments #}",
            "{# {# #}",
        ];
        for t in tests {
            let mut lex = PeekableLexer::new(t);
            assert_eq!(lex.next().unwrap(), Token::Comment);
            assert!(lex.next().is_none());
        }
    }

    #[test]
    fn can_lex_an_expression() {
        assert_lex(
            "name*10+1.0 }}",
            &[
                (Token::Ident, "name", 0..4),
                (Token::Op(Operator::Mul), "*", 4..5),
                (Token::Integer(10), "10", 5..7),
                (Token::Op(Operator::Add), "+", 7..8),
                (Token::Float(1.0), "1.0", 8..11),
                (Token::VariableEnd(false), "}}", 12..14),
            ],
            true,
        );
    }

    #[test]
    fn can_lex_something_like_a_template() {
        let mut lex =
            PeekableLexer::new("hello {b} {{-1+1}} {# hey -#} {% if true %} hey{} {%- endif%}");
        assert_eq!(lex.next().unwrap(), Token::VariableStart(true));
        assert_eq!(lex.slice_before(), "hello {b} ");
        assert_eq!(lex.next().unwrap(), Token::Integer(1));
        assert_eq!(lex.next().unwrap(), Token::Op(Operator::Add));
        assert_eq!(lex.next().unwrap(), Token::Integer(1));
        assert_eq!(lex.next().unwrap(), Token::VariableEnd(false));
        assert_eq!(lex.next().unwrap(), Token::Comment);
        assert!(lex.slice().starts_with("{#"));
        assert!(lex.slice().ends_with("-#}"));
        assert_eq!(lex.next().unwrap(), Token::TagStart(false));
        assert_eq!(lex.next().unwrap(), Token::Keyword(Keyword::If));
        assert_eq!(lex.next().unwrap(), Token::Bool(true));
        assert_eq!(lex.next().unwrap(), Token::TagEnd(false));
        assert_eq!(lex.next().unwrap(), Token::TagStart(true));
        assert_eq!(lex.slice_before(), " hey{} ");
        assert_eq!(lex.next().unwrap(), Token::Keyword(Keyword::EndIf));
        assert_eq!(lex.next().unwrap(), Token::TagEnd(false));

        assert!(lex.next().is_none());
    }
}
