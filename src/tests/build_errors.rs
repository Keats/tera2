//! Build errors are errors that Tera catches when loading templates
//! eg a template inheriting from a template that isn't there
use crate::tera::Tera;

use crate::tests::utils::split_multi_templates;

#[test]
fn build_errors() {
    insta::glob!("build_errors/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let tpls = split_multi_templates(&contents);
        let mut tera = Tera::default();
        let err = tera.add_raw_templates(tpls).unwrap_err();
        insta::assert_display_snapshot!(&err);
    });
}