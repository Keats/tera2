use serde::Serialize;
use std::collections::HashMap;

use crate::args::Kwargs;
use crate::snapshot_tests::utils::create_multi_templates_tera;
use crate::tera::Tera;
use crate::vm::state::State;
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

#[derive(Debug, Serialize)]
pub struct YearData {
    id: usize,
    year: Option<usize>,
}

fn get_context() -> Context {
    let mut context = Context::new();
    context.insert("name", &"Bob");
    context.insert("description", &"<p>I should be escaped by default</p>");
    context.insert("some_html", &"<p>Some HTML chars & more</p>");
    context.insert("age", &18);
    context.insert("some_bool", &true);
    context.insert("one", &1);
    context.insert("product", &Product::new());
    context.insert("vectors", &vec![vec![0, 3, 6], vec![1, 4, 7]]);
    context.insert("numbers", &vec![1, 2, 3]);
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
    context.insert("to", &"&");
    context.insert("malicious", &"<html>");
    context.insert(
        "year_data",
        &vec![
            YearData {
                id: 1,
                year: Some(2015),
            },
            YearData {
                id: 2,
                year: Some(2015),
            },
            YearData {
                id: 3,
                year: Some(2016),
            },
            YearData {
                id: 4,
                year: Some(2017),
            },
            YearData {
                id: 5,
                year: Some(2018),
            },
            YearData { id: 6, year: None },
            YearData {
                id: 7,
                year: Some(2018),
            },
        ],
    );
    context
}

// Disable those tests with preserve_order since the order of printed maps would change
// and fail
#[cfg(not(feature = "preserve_order"))]
#[test]
fn rendering_ok() {
    insta::glob!("rendering_inputs/success/*.txt*", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let p = format!("{}", path.file_name().unwrap().to_string_lossy());
        let mut tera = Tera::default();
        tera.autoescape_on(vec![".txt"]);
        tera.add_raw_templates(vec![(&p, contents)]).unwrap();
        tera.register_filter("read_ctx", |x: &str, _: Kwargs, state: &State| {
            state.get_from_path(x)
        });
        let out = tera.render(&p, &get_context()).unwrap();
        insta::assert_snapshot!(&out);
    });
}

#[test]
fn rendering_components_ok() {
    insta::glob!("rendering_inputs/success/components/*.txt", |path| {
        println!("{path:?}");
        let contents = std::fs::read_to_string(path).unwrap();
        let (tera, tpl_name) = create_multi_templates_tera(&contents);
        let out = tera.render(&tpl_name, &get_context()).unwrap();
        insta::assert_snapshot!(&out);
    });
}

#[test]
fn rendering_inheritance_ok() {
    insta::glob!("rendering_inputs/success/inheritance/*.txt", |path| {
        println!("{path:?}");
        let contents = std::fs::read_to_string(path).unwrap();
        let (tera, tpl_name) = create_multi_templates_tera(&contents);
        let out = tera.render(&tpl_name, &get_context()).unwrap();
        insta::assert_snapshot!(&out);
    });
}


#[test]
fn rendering_inheritance_errors() {
    insta::glob!("rendering_inputs/errors/inheritance/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let (tera, tpl_name) = create_multi_templates_tera(&contents);
        let err = tera.render(&tpl_name, &get_context()).unwrap_err();
        insta::assert_snapshot!(&err);
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
    // s.chars() would give ['न', 'म', 'स', '्', 'त', 'े']
    // graphemes are ["न", "म", "स्", "ते"]
    context.insert("string", "नमस्ते");
    let out = tera.render("tpl", &context).unwrap();

    insta::assert_snapshot!(&out);
}

#[cfg(feature = "unicode")]
#[test]
fn can_slice_on_graphemes() {
    let tpl = r#"
{{ string[::-1] }}
{{ string[1:] }}
{{ string[:-1] }}
"#;
    let mut tera = Tera::default();
    tera.add_raw_template("tpl", tpl).unwrap();
    let mut context = Context::default();
    context.insert("string", "नमस्ते");
    let out = tera.render("tpl", &context).unwrap();

    insta::assert_snapshot!(&out);
}

#[cfg(feature = "preserve_order")]
#[test]
fn inline_map_preserve_order() {
    let tpl = r#"
{% set m = {"name": "Alex", "age": 42, "vip": true, } -%}
{{ m }}
{% for k, v in m -%}
{{ k }} = {{ v }}
{% endfor -%}
"#;
    let mut tera = Tera::default();
    tera.add_raw_template("tpl", tpl).unwrap();
    let mut context = Context::default();
    let out = tera.render("tpl", &context).unwrap();

    insta::assert_snapshot!(&out);
}
