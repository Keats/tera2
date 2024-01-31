use serde::Serialize;
use std::collections::HashMap;

use crate::tera::Tera;
use crate::{Context, Value};

/// This will take our custom insta format to create multiple templates
/// The format is:
/// $$ filename
/// body
/// $$ other filename
/// other body
/// And returns the tera instance as well as the last template name
fn create_multi_templates_tera(body: &str) -> (Tera, String) {
    let parts: Vec<_> = body.split("$$ ").skip(1).collect();
    let mut tera = Tera::default();
    let mut tpls = Vec::with_capacity(parts.len());
    let mut last_filename = String::new();
    for part in parts {
        let mut chars = part.chars();
        let filename: String = chars.by_ref().take_while(|&c| c != '\n').collect();
        let content = chars.collect::<String>().trim().to_string();
        last_filename = filename.clone();
        tpls.push((filename, content));
    }

    tera.add_raw_templates(tpls).unwrap();

    (tera, last_filename)
}

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

#[derive(Debug, Serialize)]
pub struct Review {
    title: String,
    paragraphs: Vec<String>,
}
impl Review {
    pub fn new() -> Review {
        Review {
            title: "My review".to_owned(),
            paragraphs: vec!["A".to_owned(), "B".to_owned(), "C".to_owned()],
        }
    }
}

#[derive(Debug, Serialize)]
pub struct NestedObject {
    pub label: String,
    pub parent: Option<Box<NestedObject>>,
    pub numbers: Vec<usize>,
}

fn get_context() -> Context {
    let mut context = Context::new();
    context.insert("name", &"Bob");
    context.insert("description", &"<p>I should be escaped by default</p>");
    context.insert("age", &18);
    context.insert("one", &1);
    context.insert("product", &Product::new());
    context.insert("vectors", &vec![vec![0, 3, 6], vec![1, 4, 7]]);
    context.insert("empty", &Vec::<usize>::new());
    let parent = NestedObject {
        label: "Parent".to_string(),
        parent: None,
        numbers: vec![1, 2, 3],
    };
    let child = NestedObject {
        label: "Child".to_string(),
        parent: Some(Box::new(parent)),
        numbers: vec![1, 2, 3],
    };
    context.insert("objects", &vec![child]);
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
    context.insert("reviews", &vec![Review::new(), Review::new()]);
    context
}

#[test]
fn rendering_ok() {
    insta::glob!("rendering_inputs/success/*.txt", |path| {
        println!("{path:?}");
        let contents = std::fs::read_to_string(path).unwrap();
        let p = format!("{:?}", path.file_name().unwrap());
        let mut tera = Tera::default();
        tera.add_raw_templates(vec![(&p, contents)]).unwrap();
        let out = tera.render(&p, &get_context()).unwrap();
        insta::assert_display_snapshot!(&out);
    });
}

#[test]
fn rendering_inheritance_ok() {
    insta::glob!("rendering_inputs/success/inheritance/*.txt", |path| {
        println!("{path:?}");
        let contents = std::fs::read_to_string(path).unwrap();
        let (tera, tpl_name) = create_multi_templates_tera(&contents);
        let out = tera.render(&tpl_name, &get_context()).unwrap();
        insta::assert_display_snapshot!(&out);
    });
}

#[test]
fn rendering_macros_ok() {
    insta::glob!("rendering_inputs/success/macros/*.txt", |path| {
        println!("{path:?}");
        let contents = std::fs::read_to_string(path).unwrap();
        let (tera, tpl_name) = create_multi_templates_tera(&contents);
        let out = tera.render(&tpl_name, &get_context()).unwrap();
        insta::assert_display_snapshot!(&out);
    });
}

#[test]
fn rendering_runtime_errors() {
    insta::glob!("rendering_inputs/runtime_errors/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let p = format!("{:?}", path.file_name().unwrap());
        println!("{p:?}");
        let mut tera = Tera::default();
        tera.add_raw_templates(vec![(&p, contents)]).unwrap();
        let err = tera.render(&p, &get_context()).unwrap_err();
        insta::assert_display_snapshot!(&err);
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
