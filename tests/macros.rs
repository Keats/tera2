use serde_derive::Serialize;

use tera::{Context, Tera};


// #[test]
// fn macro_param_arent_escaped() {
//     let mut tera = Tera::default();
//     tera.add_raw_templates(vec![
//         (
//             "macros.html",
//             r#"{% macro print(val) %}{{val|safe}}{% endmacro print %}"#,
//         ),
//         (
//             "hello.html",
//             r#"{% import "macros.html" as macros %}{{ macros::print(val=my_var)}}"#,
//         ),
//     ])
//     .unwrap();
//     let mut context = Context::new();
//     context.insert("my_var", &"&");
//     let result = tera.render("hello.html", &context);
//
//     assert_eq!(result.unwrap(), "&".to_string());
// }



#[test]
fn render_macros_unknown_arg() {
    let mut tera = Tera::default();
    let res = tera.add_raw_templates(vec![
        (
            "macros",
            "{% macro hello(val=1, other=2) %}{{val}}{% endmacro hello %}",
        ),
        (
            "hello.html",
            "{% import \"macros\" as macros %}{{macros::hello(lang='en')}}",
        ),
    ]);
    assert!(res.is_err());
    assert_eq!(format!("{}", res.unwrap_err()), "Template hello.html is calling macro hello with an argument lang which isn't present in its definition. Only the following are allowed: other, val.");
}


#[test]
fn errors_when_loading_macro_usage_not_found_in_namespace() {
    let mut tera = Tera::default();
    let err = tera
        .add_raw_templates(vec![
            ("macros", ""),
            (
                "parent",
                "{% import \"macros\" as macros %}{{ macros::test_global() }}",
            ),
        ])
        .unwrap_err();

    assert_eq!(
        err.to_string(),
        "Template `parent` is using macros `test_global` from `macros` which wasn't found"
    );
}
