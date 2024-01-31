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
