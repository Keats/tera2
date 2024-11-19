use std::fmt;

use crate::parsing::ast::{Expression, MacroDefinition, Node};
use crate::parsing::parser::Parser;
use crate::template::Template;
use crate::utils::Spanned;
use crate::value::Value;
use crate::HashMap;

struct Expressions(pub Vec<Expression>);

impl fmt::Display for Expressions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.iter().fold(Ok(()), |result, expr| {
            result.and_then(|_| writeln!(f, "{expr}"))
        })
    }
}

#[test]
fn parser_expressions_success() {
    insta::glob!("parser_inputs/success/expr/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let nodes = &Parser::new(&contents).parse().unwrap().nodes;
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
            insta::assert_snapshot!(Expressions(expr_nodes));
        }
    });
}

#[test]
fn parser_errors() {
    insta::glob!("parser_inputs/errors/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let res = Template::new(
            path.file_name().unwrap().to_string_lossy().as_ref(),
            &contents,
            None,
        );
        insta::assert_snapshot!(res.unwrap_err());
    });
}

#[test]
fn parser_macros_success() {
    insta::glob!("parser_inputs/success/macros/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let nodes = &Parser::new(&contents).parse().unwrap().nodes;
        println!("{nodes:?}");

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
            insta::assert_snapshot!(Expressions(expr_nodes));
        }
    });
}

#[test]
fn parser_components_definition_success() {
    insta::glob!("parser_inputs/success/components/def/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let components = &Parser::new(&contents)
            .parse()
            .unwrap()
            .component_definitions;
        insta::assert_debug_snapshot!(components[0]);
    });
}

#[test]
fn parser_components_render_success() {
    insta::glob!("parser_inputs/success/components/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let nodes = &Parser::new(&contents).parse().unwrap().nodes;
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
            insta::assert_snapshot!(Expressions(expr_nodes));
        }
    });
}

#[test]
fn parser_tags_success() {
    insta::glob!("parser_inputs/success/tags/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let nodes = &Parser::new(&contents).parse().unwrap().nodes;
        let mut res_nodes = Vec::with_capacity(nodes.len());
        // println!("{:?}", nodes);
        for node in nodes {
            if matches!(
                node,
                Node::Set(..)
                    | Node::BlockSet(..)
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
    });
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
                    kwargs.insert("hey".to_owned(), Some(Value::from("ho")));
                    kwargs.insert("optional".to_owned(), None);
                    kwargs
                },
                body: vec![Node::Content("hello".to_owned())],
            },
        ),
    ];

    for (t, expected) in tests {
        let parser = Parser::new(t);
        let macros = parser.parse().unwrap().macro_definitions;
        assert_eq!(
            macros.iter().find(|x| x.name == expected.name).unwrap(),
            &expected
        );
    }
}

#[test]
fn parser_extends_success() {
    let parser = Parser::new("{% extends 'a.html' %}");
    let parent = parser.parse().unwrap().parent;
    assert_eq!(parent, Some("a.html".to_string()));
}

#[test]
fn parser_macro_import_success() {
    let parser = Parser::new(r#"{% import 'macros.html' as macros %}"#);
    let macro_imports = parser.parse().unwrap().macro_imports;
    assert_eq!(
        macro_imports,
        vec![("macros.html".to_string(), "macros".to_string())]
    );
}

#[test]
fn parser_can_convert_array_to_const_when_possible() {
    let parser = Parser::new(r#"{{ [1, 2, 3] }}"#);
    let nodes = parser.parse().unwrap().nodes;
    let expected = Value::from(vec![1, 2, 3]);
    match &nodes[0] {
        Node::Expression(e) => {
            assert_eq!(
                e,
                &Expression::Const(Spanned::new(expected, e.span().clone()))
            );
        }
        _ => unreachable!(),
    }
}

#[test]
fn parser_templates_success() {
    insta::glob!("parser_inputs/success/tpl/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let nodes = &Parser::new(&contents).parse().unwrap().nodes;
        insta::assert_debug_snapshot!(&nodes);
    });
}

#[test]
fn fuzzing_findings() {
    let inputs = vec![
        //         r#"
        //         /ff}zpp.%{%if
        // bT%}h
        //         "#,
    ];

    for txt in inputs {
        assert!(Parser::new(txt).parse().is_ok());
    }
}
