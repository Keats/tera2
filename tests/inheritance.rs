use tera::{Context, Tera};

// #[test]
// fn render_filter_section_inheritance_no_override() {
//     let mut tera = Tera::default();
//     tera.add_raw_templates(vec![
//         ("top", "{% filter upper %}hello {% block main %}top{% endblock main %}{% endfilter %}"),
//         ("bottom", "{% extends 'top' %}"),
//     ])
//         .unwrap();
//     let result = tera.render("bottom", &Context::new());
//
//     assert_eq!(result.unwrap(), "HELLO TOP".to_string());
// }
//
// #[test]
// fn render_filter_section_inheritance() {
//     let mut tera = Tera::default();
//     tera.add_raw_templates(vec![
//         ("top", "{% filter upper %}hello {% block main %}top{% endblock main %}{% endfilter %}"),
//         ("bottom", "{% extends 'top' %}{% block main %}bottom{% endblock %}"),
//     ])
//         .unwrap();
//     let result = tera.render("bottom", &Context::new());
//
//     assert_eq!(result.unwrap(), "HELLO BOTTOM".to_string());
// }

#[test]
fn render_super_in_top_block_errors() {
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![(
        "index",
        "{% block content%}{{super()}}{% endblock content %}",
    )])
    .unwrap();

    let result = tera.render("index", &Context::new());
    assert!(result.is_err());
}

#[test]
fn errors_when_extending_itself() {
    let mut tera = Tera::default();
    let res = tera.add_raw_templates(vec![("grandparent", "{% extends 'grandparent' %}")]);
    assert!(res.is_err());
    let err = res.unwrap_err();
    assert!(err.to_string().contains("Circular"));
}
