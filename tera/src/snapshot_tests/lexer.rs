use super::utils::normalize_line_endings;
use crate::parsing::lexer::tokenize;

#[test]
fn lexer_ok() {
    insta::glob!("lexer_inputs/success/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let normalized_contents = normalize_line_endings(&contents);
        let tokens: Result<Vec<_>, _> = tokenize(&normalized_contents).collect();
        let tokens = tokens.unwrap().into_iter().map(|x| x.0).collect::<Vec<_>>();
        insta::assert_debug_snapshot!(&tokens);
    });
}

#[test]
fn lexer_errors() {
    insta::glob!("lexer_inputs/errors/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let normalized_contents = normalize_line_endings(&contents);
        let res: Result<Vec<_>, _> = tokenize(&normalized_contents).collect();
        assert!(res.is_err());
        insta::assert_debug_snapshot!(res.unwrap_err());
    });
}
