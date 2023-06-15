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

#[test]
fn rendering_include_ok() {
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![
        ("world", r#"{% set a = "world" %}[Include => (a={{a}}, name={{ name }}, b={{ b }})]"#),
        ("hello", "<h1>Hello {% set b = 1 %} {% include \"world\" %} {% if a %}shouldfail{% endif %}name={{name}}</h1>"),
    ]).unwrap();
    let mut context = Context::new();
    context.insert("name", &"Bob");
    let out = tera.render("hello", &context).unwrap();
    insta::assert_display_snapshot!(&out);
}

#[test]
fn render_macros() {
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![
        ("macros", "{% macro hello()%}Hello{% endmacro hello %}"),
        (
            "tpl",
            "{% import \"macros\" as macros %}{{macros::hello()}}",
        ),
    ])
    .unwrap();

    let result = tera.render("tpl", &Context::new());
    assert_eq!(result.unwrap(), "Hello".to_string());
}

#[test]
fn render_macros_defined_in_template() {
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![(
        "tpl",
        "{% macro hello()%}Hello{% endmacro hello %}{{self::hello()}}",
    )])
    .unwrap();

    let result = tera.render("tpl", &Context::new());
    assert_eq!(result.unwrap(), "Hello".to_string());
}

#[test]
fn render_macros_expression_arg() {
    let mut context = Context::new();
    context.insert("pages", &vec![1, 2, 3, 4, 5]);
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![
        ("macros", "{% macro hello(val)%}{{val}}{% endmacro hello %}"),
        (
            "tpl",
            "{% import \"macros\" as macros %}{{macros::hello(val=pages[0])}}",
        ),
    ])
    .unwrap();

    let result = tera.render("tpl", &context);
    assert_eq!(result.unwrap(), "1".to_string());
}

#[test]
fn render_set_tag_macro() {
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![
        ("macros", "{% macro hello()%}Hello{% endmacro hello %}"),
        (
            "hello.html",
            "{% import \"macros\" as macros %}{% set my_var = macros::hello() %}{{my_var}}",
        ),
    ])
    .unwrap();
    let result = tera.render("hello.html", &Context::new());

    assert_eq!(result.unwrap(), "Hello".to_string());
}

#[test]
fn render_macros_with_default_args() {
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![
        (
            "macros",
            "{% macro hello(val=1) %}{{val}}{% endmacro hello %}",
        ),
        (
            "hello.html",
            "{% import \"macros\" as macros %}{{macros::hello()}}",
        ),
    ])
    .unwrap();
    let result = tera.render("hello.html", &Context::new());

    assert_eq!(result.unwrap(), "1".to_string());
}

#[test]
fn render_macros_override_default_args() {
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![
        (
            "macros",
            "{% macro hello(val=1) %}{{val}}{% endmacro hello %}",
        ),
        (
            "hello.html",
            "{% import \"macros\" as macros %}{{macros::hello(val=2)}}",
        ),
    ])
    .unwrap();
    let result = tera.render("hello.html", &Context::new());

    assert_eq!(result.unwrap(), "2".to_string());
}

#[test]
fn render_recursive_macro() {
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![
        (
            "macros",
            "{% macro factorial(n) %}{% if n > 1 %}{{ n }} - {{ self::factorial(n=n-1) }}{% else %}1{% endif %}{{ n }}{% endmacro factorial %}",
        ),
        ("hello.html", "{% import \"macros\" as macros %}{{macros::factorial(n=7)}}"),
    ]).unwrap();
    let result = tera.render("hello.html", &Context::new());

    assert_eq!(
        result.unwrap(),
        "7 - 6 - 5 - 4 - 3 - 2 - 11234567".to_string()
    );
}

// https://github.com/Keats/tera/issues/250
#[test]
fn render_macros_in_included() {
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![
        ("macros", "{% macro my_macro() %}my macro{% endmacro %}"),
        (
            "includeme",
            r#"{% import "macros" as macros %}{{ macros::my_macro() }}"#,
        ),
        ("example", r#"{% include "includeme" %}"#),
    ])
    .unwrap();
    let result = tera.render("example", &Context::new());

    assert_eq!(result.unwrap(), "my macro".to_string());
}

// https://github.com/Keats/tera/issues/255
#[test]
fn import_macros_into_other_macro_files() {
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![
        ("submacros", "{% macro test() %}Success!{% endmacro %}"),
        (
            "macros",
            r#"{% import "submacros" as sub %}{% macro test() %}{{ sub::test() }}{% endmacro %}"#,
        ),
        (
            "index",
            r#"{% import "macros" as macros %}{{ macros::test() }}"#,
        ),
    ])
    .unwrap();
    let result = tera.render("index", &Context::new());

    assert_eq!(result.unwrap(), "Success!".to_string());
}

#[test]
fn render_simple_inheritance() {
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![
        (
            "top",
            "{% block pre %}{% endblock pre %}{% block main %}{% endblock main %}",
        ),
        (
            "bottom",
            "{% extends \"top\" %}{% block main %}MAIN{% endblock %}",
        ),
    ])
    .unwrap();
    let result = tera.render("bottom", &Context::new());

    assert_eq!(result.unwrap(), "MAIN".to_string());
}
