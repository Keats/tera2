//! Build errors are errors that Tera catches when loading templates
//! eg a template inheriting from a template that isn't there
use crate::tera::Tera;

use crate::snapshot_tests::utils::{normalize_line_endings, split_multi_templates};

#[test]
fn build_errors() {
    insta::glob!("build_errors/**/*.txt", |path| {
        println!("{path:?}");
        let contents = std::fs::read_to_string(path).unwrap();
        let normalized_contents = normalize_line_endings(&contents);
        let tpls = split_multi_templates(&normalized_contents);
        let mut tera = Tera::default();
        let err = tera.add_raw_templates(tpls).unwrap_err();
        insta::assert_snapshot!(&err);
    });
}
