use serde::Serialize;
use std::collections::HashMap;

use crate::tera::Tera;
use crate::{Context, Value};

#[derive(Debug, Serialize)]
pub struct Product {
    name: String,
}
impl Product {
    pub fn new() -> Product {
        Product {
            name: "Moto G".to_owned(),
        }
    }
}

#[test]
fn rendering_ok() {
    insta::glob!("rendering_inputs/success/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let p = format!("{:?}", path.display());
        let mut tera = Tera::default();
        tera.add_raw_templates(vec![(&p, contents)]).unwrap();
        let mut context = Context::new();
        context.insert("name", &"Bob");
        context.insert("description", &"<p>I should be escaped by default</p>");
        context.insert("age", &18);
        context.insert("product", &Product::new());
        context.insert("vectors", &vec![vec![0, 3, 6], vec![1, 4, 7]]);
        context.insert("empty", &Vec::<usize>::new());
        let mut data: HashMap<String, Value> = HashMap::new();
        data.insert(
            "names".to_string(),
            vec![
                "Tchoupi".to_string(),
                "Pilou".to_string(),
                "Fanny".to_string(),
            ]
            .into(),
        );
        data.insert("weights".to_string(), vec![50.6, 70.1].into());
        context.insert("data", &data);

        let out = tera.render(&p, &context).unwrap();
        insta::assert_display_snapshot!(&out);
    });
}

#[cfg(feature = "unicode")]
#[test]
fn can_iterate_on_graphemes() {
    let tpl = r#"{% for c in string -%}
{{loop.index}}.{{c}}
{% endfor %}"#;
    let mut tera = Tera::default();
    tera.add_raw_template("tpl", tpl).unwrap();
    let mut context = Context::default();
    context.insert("string", "a\r\nb🇺🇳🇮🇨");
    let out = tera.render("tpl", &context).unwrap();

    insta::assert_display_snapshot!(&out);
}