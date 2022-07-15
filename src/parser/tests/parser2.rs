use crate::parser::ast2::{Expression, Node};
use crate::parser::parser::Parser;
use std::fmt;

struct Expressions(pub Vec<Expression>);

impl fmt::Display for Expressions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.iter().fold(Ok(()), |result, expr| {
            result.and_then(|_| writeln!(f, "{}", expr))
        })
    }
}

#[test]
fn test_parser_expressions_success() {
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
fn test_parser_tags_success() {
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
fn test_parser_extends_success() {
    let mut parser = Parser::new("{% extends 'a.html' %}");
    parser.parse().unwrap();
    assert_eq!(parser.parent, Some("a.html".to_string()));
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
