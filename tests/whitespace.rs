use tera::{Context, Tera};

// TODO: convert to insta test
// #[test]
// fn can_remove_whitespace_basic() {
//     let mut context = Context::new();
//     context.insert("numbers", &vec![1, 2, 3]);
//
//     let inputs = vec![
//         ("  {%- for n in numbers %}{{n}}{% endfor -%} ", "123"),
//         ("{%- for n in numbers %} {{n}}{%- endfor -%} ", " 1 2 3"),
//         ("{%- for n in numbers -%}\n {{n}}\n {%- endfor -%} ", "123"),
//         ("{%- if true -%}\n {{numbers}}\n {%- endif -%} ", "[1, 2, 3]"),
//         ("{%- if false -%}\n {{numbers}}\n {% else %} Nope{%- endif -%} ", " Nope"),
//         ("  {%- if false -%}\n {{numbers}}\n {% else -%} Nope {%- endif -%} ", "Nope"),
//         ("  {%- if false -%}\n {{numbers}}\n {% elif true -%} Nope {%- endif -%} ", "Nope"),
//         ("  {%- if false -%}\n {{numbers}}\n {% elif false -%} Nope {% else %} else {%- endif -%} ", " else"),
//         ("  {%- set var = 2 -%} {{var}}", "2"),
//         ("  {% set var = 2 -%} {{var}}", "  2"),
//         (" {% raw -%} {{2}} {% endraw -%} ", " {{2}} "),
//         ("  {% filter upper -%} hey {%- endfilter -%} ", "  HEY"),
//         ("  {{ \"hello\" -}} ", "  hello"),
//         ("  {{- \"hello\" }} ", "hello "),
//         ("  {{- \"hello\" -}} ", "hello"),
//         // Comments are not rendered so it should be just whitespace if anything
//         ("  {#- \"hello\" -#} ", ""),
//         ("  {# \"hello\" -#} ", "  "),
//         ("  {#- \"hello\" #} ", " "),
//     ];
//
//     for (input, expected) in inputs {
//         let mut tera = Tera::default();
//         tera.add_raw_template("tpl", input).unwrap();
//         println!("{} -> {:?}", input, expected);
//         assert_eq!(tera.render("tpl", &context).unwrap(), expected);
//     }
// }


// https://github.com/Keats/tera/issues/475
// #[test]
// fn works_with_filter_section() {
//     let mut context = Context::new();
//     context.insert("d", "d");
//     let input = r#"{% filter upper %}  {{ "c" }}   d{% endfilter %}"#;
//     let res = Tera::one_off(input, &context, true).unwrap();
//     assert_eq!(res, "  C   D");
// }
