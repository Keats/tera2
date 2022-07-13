use crate::parser::ast2::Node;
use crate::parser::parser::Parser;

// TODO: move back to a *.txt glob?
#[test]
fn test_parser_expressions_success() {
    insta::glob!("parser_inputs/success/idents.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let nodes = &Parser::new(&contents).parse().unwrap();
        let mut expr_nodes = Vec::with_capacity(nodes.len());
        for node in nodes {
            match node {
                Node::Expression(_) => {
                    expr_nodes.push(node);
                }
                _ => (),
            }
        }
        insta::assert_debug_snapshot!(&expr_nodes);
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
