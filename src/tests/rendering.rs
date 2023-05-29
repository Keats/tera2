use crate::tera::Tera;
use crate::Context;

#[test]
fn rendering_ok() {
    insta::glob!("rendering_inputs/success/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let p = format!("{:?}", path.display());
        let mut tera = Tera::default();
        tera.add_raw_templates(vec![(&p, contents)]).unwrap();
        let out = tera.render(&p, &Context::new()).unwrap();
        insta::assert_display_snapshot!(&out);
    });
}
