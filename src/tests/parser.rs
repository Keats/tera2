use std::collections::HashMap;
use std::fmt;

use crate::parsing::ast::{Expression, MacroDefinition, Node};
use crate::parsing::parser::Parser;
use crate::utils::{Span, Spanned};

struct Expressions(pub Vec<Expression>);

impl fmt::Display for Expressions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.iter().fold(Ok(()), |result, expr| {
            result.and_then(|_| writeln!(f, "{}", expr))
        })
    }
}

#[test]
fn parser_expressions_idents_success() {
    insta::glob!("parser_inputs/success/{expressions,idents}.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let nodes = &Parser::new(&contents).parse().unwrap();
        let mut expr_nodes = Vec::with_capacity(nodes.len());
        for node in nodes {
            match node {
                Node::Expression(n) => {
                    expr_nodes.push(n.clone());
                }
                _ => (),
            }
        }
        if !expr_nodes.is_empty() {
            insta::assert_display_snapshot!(Expressions(expr_nodes));
        }
    });
}

#[test]
fn parser_tags_success() {
    insta::glob!(
        "parser_inputs/success/{tags,blocks,for,if,filter_section}.txt",
        |path| {
            let contents = std::fs::read_to_string(path).unwrap();
            let nodes = &Parser::new(&contents).parse().unwrap();
            let mut res_nodes = Vec::with_capacity(nodes.len());
            for node in nodes {
                if matches!(
                    node,
                    Node::Set(..)
                        | Node::Include(..)
                        | Node::Block(..)
                        | Node::ForLoop(..)
                        | Node::If(..)
                        | Node::FilterSection(..)
                ) {
                    res_nodes.push(node);
                }
            }
            insta::assert_debug_snapshot!(&res_nodes);
        }
    );
}

#[test]
fn parser_macro_def_success() {
    let tests = vec![
        (
            "{% macro popup() -%} hello {%- endmacro %}",
            MacroDefinition {
                name: "popup".to_string(),
                kwargs: HashMap::new(),
                body: vec![Node::Content("hello".to_owned())],
            },
        ),
        (
            "{% macro another(hey='ho', optional) -%} hello {%- endmacro another %}",
            MacroDefinition {
                name: "another".to_owned(),
                kwargs: {
                    let mut kwargs = HashMap::new();
                    let mut span = Span::default();
                    span.start_line = 1;
                    span.start_col = 21;
                    span.end_line = 1;
                    span.end_col = 25;
                    span.range = 21..25;
                    kwargs.insert(
                        "hey".to_owned(),
                        Some(Expression::Str(Spanned::new("ho".to_string(), span))),
                    );
                    kwargs.insert("optional".to_owned(), None);
                    kwargs
                },
                body: vec![Node::Content("hello".to_owned())],
            },
        ),
    ];

    for (t, expected) in tests {
        let mut parser = Parser::new(t);
        parser.parse().unwrap();
        assert_eq!(parser.macros[&expected.name], expected);
    }
}

#[test]
fn parser_extends_success() {
    let mut parser = Parser::new("{% extends 'a.html' %}");
    parser.parse().unwrap();
    assert_eq!(parser.parent, Some("a.html".to_string()));
}

#[test]
fn parser_macro_import_success() {
    let mut parser = Parser::new(r#"{% import 'macros.html' as macros %}"#);
    parser.parse().unwrap();
    assert_eq!(
        parser.macro_imports,
        vec![("macros.html".to_string(), "macros".to_string())]
    );
}

#[test]
fn parser_templates_success() {
    insta::glob!("parser_inputs/success/tpl_*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let nodes = &Parser::new(&contents).parse().unwrap();
        insta::assert_debug_snapshot!(&nodes);
    });
}

// #[test]
// fn test_lexer_errors() {
//     insta::glob!("parser_inputs/errors/*.txt", |path| {
//         let contents = std::fs::read_to_string(path).unwrap();
//         let res: Result<Vec<_>, _> = tokenize(&contents).collect();
//         assert!(res.is_err());
//         insta::assert_debug_snapshot!(res.unwrap_err());
//     });
// }
