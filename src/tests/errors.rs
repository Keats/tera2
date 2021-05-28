use codespan_reporting::diagnostic::{Diagnostic, Severity};
use codespan_reporting::files;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use crate::errors::ParsingError;
use crate::lexer::{Operator, Token};
use crate::parser::Parser;

fn output_diagnostic(tpl: &str, diag: &Diagnostic<()>) {
    let file = files::SimpleFile::new("test.tera", tpl);
    let writer = StandardStream::stdout(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();
    term::emit(&mut writer.lock(), &config, &file, &diag).unwrap();
}

#[test]
fn can_provide_good_error_messages() {
    let tests = vec![
        // (test, (error type, range, note message))

        // unexpected eof
        ("{{ 1+", (ParsingError::UnexpectedEof, 5..5, "")),
        // unexpected token in function param
        (
            "{{ hello(] }}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                9..10,
                "expected one of: an ident, `)` but found `]`",
            ),
        ),
        // unexpected operator
        (
            "{{ and }}",
            (
                ParsingError::UnexpectedOperator(Operator::Not, vec![]),
                3..6,
                "found `and` but only `not`, `+`, `-` can be used here",
            ),
        ),
        // unexpected symbol
        (
            "{{ = }}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                3..4,
                "found `=`",
            ),
        ),
        // double infix operators
        (
            "{{ 1++1 }}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                5..6,
                "found `+`",
            ),
        ),
        // infix + not
        (
            "{{ 1+not 1 }}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                5..8,
                "found `not`",
            ),
        ),
        // infix + and
        (
            "{{ 1 and + 1 }}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                9..10,
                "found `+`",
            ),
        ),
        // unexpected tokens in array
        (
            "{{ [1,) }}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                6..7,
                "found `)`",
            ),
        ),
        (
            "{{ [1,2) }}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                7..8,
                "expected one of: `,`, `]` but found `)`",
            ),
        ),
        // test args unknown token
        (
            "{{ 1 is odd(1=) }}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                13..14,
                "expected one of: `,`, `)` but found `=`",
            ),
        ),
        // kwargs errors
        (
            "{{ 1 | odd(1) }}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                11..12,
                "expected one of: an ident, `)` but found an integer",
            ),
        ),
        (
            "{{ 1 | odd(a'ho') }}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                12..16,
                "expected `=` but found a string",
            ),
        ),
        (
            "{{ 1 | odd(a='ho',hey=&) }}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                22..23,
                "found unexpected characters",
            ),
        ),
        (
            "{{ hey(arg|'ho') }}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                10..11,
                "expected `=` but found `|`",
            ),
        ),
        // parentheses
        (
            "{{ (2 * ]) }}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                8..9,
                "found `]`",
            ),
        ),
        // set
        (
            "{% set a = %}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                11..13,
                "found `%}`",
            ),
        ),
        // extends
        (
            "{% extends a %}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                11..12,
                "expected a string but found an ident",
            ),
        ),
        (
            "{% extends 'a' %}{% extends 'b' %}",
            (
                ParsingError::DuplicateExtend(String::new()),
                20..27,
                "Template is already extending 'a'",
            ),
        ),
        // includes
        (
            "{% include a %}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                11..12,
                "expected one of: a string, `[` but found an ident",
            ),
        ),
        (
            "{% include ['a', 1] %}",
            (
                ParsingError::InvalidInclude,
                11..19,
                "values in an include array must be strings",
            ),
        ),
        (
            "{% include ['a', 'b'] ignoremissing %}",
            (
                ParsingError::UnexpectedToken(Token::Error, vec![]),
                22..35,
                "expected one of: `ignore missing`, `-%}`, `%}` but found an ident",
            ),
        ),
    ];

    for (t, (error_type, range, note_msg)) in tests {
        println!("Testing: {}", t);
        let err = Parser::new(t).parse().unwrap_err();
        assert_eq!(err.range, range);
        let diag = err.report();
        // output_diagnostic(t, &diag);
        assert_eq!(diag.message, error_type.message());
        assert_eq!(diag.severity, Severity::Error);
        assert_eq!(diag.labels[0].range, err.range);
        assert_eq!(diag.labels[0].message, note_msg);
    }
}
