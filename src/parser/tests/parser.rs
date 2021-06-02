use crate::parser::ast::{Block, Expression, FilterSection, ForLoop, If, MacroDefinition, Node};
use crate::parser::lexer::Operator;
use crate::parser::Parser;
use std::collections::HashMap;

macro_rules! hashmap {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(hashmap!(@single $rest)),*]));

    ($($key:expr => $value:expr,)+) => { hashmap!($($key => $value),+) };
    ($($key:expr => $value:expr),*) => {
        {
            let _cap = hashmap!(@count $($key),*);
            let mut _map = ::std::collections::HashMap::with_capacity(_cap);
            $(
                let _ = _map.insert($key, $value);
            )*
            _map
        }
    };
}

#[test]
fn can_parse_ident() {
    let tests = vec![
        "{{ hello }}",
        "{{ hello_ }}",
        "{{ hello_1 }}",
        "{{ HELLO }}",
        "{{ _1 }}",
        "{{ hey.ho }}",
        "{{ h }}",
        "{{ ho }}",
        "{{ hey.ho.hu }}",
        "{{ hey.0 }}",
        "{{ h.u }}",
        "{{ hey.ho.hu }}",
        "{{ hey.0 }}",
        "{{ h.u.x.0 }}",
        "{{ hey[0] }}",
        "{{ hey[a[0]] }}",
        "{{ hey['ho'][\"hu\"] }}",
        "{{ h['u'].x[0] }}",
    ];

    for t in tests {
        println!("{:?}", t);
        let mut parser = Parser::new(t);
        parser.parse().expect("parsed failed");
        match &parser.nodes[0] {
            Node::VariableBlock(e) => {
                assert_eq!(e.to_string(), t.replace("{{ ", "").replace(" }}", ""))
            }
            _ => unreachable!("Got something that wasn't an expression"),
        }
    }
}

#[test]
fn can_parse_expression() {
    let tests = vec![
        // literals + basic types
        ("{{ -1 }}", "-1"),
        ("{{ 1 }}", "1"),
        ("{{ 'hello' }}", "'hello'"),
        ("{{ true }}", "true"),
        ("{{ -1.2 }}", "-1.2"),
        ("{{ 1.2 }}", "1.2"),
        ("{{ a }}", "a"),
        ("{{ -a }}", "(- a)"),
        ("{{ +a }}", "(+ a)"),
        ("{{ - a * 2 }}", "(- (* a 2))"),
        ("{{ [1, 1.2, a, 'b', true] }}", "[1, 1.2, a, 'b', true]"),
        ("{{ [1, 1.2, a, 'b', true,] }}", "[1, 1.2, a, 'b', true]"), // Allows trailing `,`
        // Actual expressions
        ("{{ 1 + 2 + 3 }}", "(+ (+ 1 2) 3)"),
        ("{{ 1 + count }}", "(+ 1 count)"),
        ("{{ 1 + 2 * 3 }}", "(+ 1 (* 2 3))"),
        ("{{ a + b * c * d + e }}", "(+ (+ a (* (* b c) d)) e)"),
        // https://github.com/pallets/jinja/issues/119
        ("{{ 2 * 4 % 8 }}", "(% (* 2 4) 8)"),
        ("{{ [1 + 1, 2, 3 * 2,] }}", "[(+ 1 1), 2, (* 3 2)]"),
        // string concat
        ("{{ hey ~ ho }}", "(~ hey ho)"),
        ("{{ 1 ~ ho }}", "(~ 1 ho)"),
        ("{{ -1.2 ~ ho }}", "(~ -1.2 ho)"),
        ("{{ [] ~ ho }}", "(~ [] ho)"),
        ("{{ 'hey' ~ ho }}", "(~ 'hey' ho)"),
        ("{{ `hello` ~ ident ~ 'ho' }}", "(~ (~ 'hello' ident) 'ho')"),
        // Comparisons
        ("{{ a == b }}", "(== a b)"),
        ("{{ a != b }}", "(!= a b)"),
        ("{{ a <= b }}", "(<= a b)"),
        ("{{ a >= b }}", "(>= a b)"),
        ("{{ a < b }}", "(< a b)"),
        ("{{ a > b }}", "(> a b)"),
        ("{{ 1 + a > b }}", "(> (+ 1 a) b)"),
        ("{{ 1 + a > b * 8 }}", "(> (+ 1 a) (* b 8))"),
        // and/or
        ("{{ a and b }}", "(and a b)"),
        ("{{ a or b }}", "(or a b)"),
        (
            "{{ a + 1 == 2 or b * 3 > 10 }}",
            "(or (== (+ a 1) 2) (> (* b 3) 10))",
        ),
        // in
        ("{{ a in b }}", "(in a b)"),
        ("{{ a in b and b in c }}", "(and (in a b) (in b c))"),
        // https://github.com/mozilla/nunjucks/pull/336
        (
            "{{ msg.status in ['pending', 'confirmed'] and msg.body }}",
            "(and (in msg.status ['pending', 'confirmed']) msg.body)",
        ),
        // test
        ("{{ a is defined }}", "(is a defined)"),
        ("{{ a is not defined }}", "(not (is a defined))"),
        ("{{ a + 1 is odd }}", "(is (+ a 1) odd)"),
        ("{{ a + 1 is not odd }}", "(not (is (+ a 1) odd))"),
        ("{{ a is ending_with('s') }}", "(is a ending_with{'s'})"),
        // function calls
        (
            "{{ get_url(path=page.path, in_content=true) }}",
            "get_url{in_content=true, path=page.path}",
        ),
        ("{{ get_url() }}", "get_url{}"),
        // filters
        ("{{ a | round }}", "(| a round{})"),
        ("{{ a | round() }}", "(| a round{})"),
        ("{{ 1 + 2.1 | round }}", "(| (+ 1 2.1) round{})"),
        ("{{ [1] + [3, 2] | sort }}", "(| (+ [1] [3, 2]) sort{})"),
        ("{{ (1 + 2.1) | round }}", "(| (+ 1 2.1) round{})"),
        (
            "{{ value | json_encode | safe }}",
            "(| (| value json_encode{}) safe{})",
        ),
        (
            "{{ value | truncate(length=10) }}",
            "(| value truncate{length=10})",
        ),
        (
            "{{ get_content() | upper | safe }}",
            "(| (| get_content{} upper{}) safe{})",
        ),
        (
            "{{ admin | default or user == current_user }}",
            "(or (| admin default{}) (== user current_user))",
        ),
        (
            "{{ user == current_user or admin | default }}",
            "(or (== user current_user) (| admin default{}))",
        ),
        (
            "{{ members in interfaces | groupby(attribute='vlan') }}",
            "(in members (| interfaces groupby{attribute='vlan'}))",
        ),
        ("{{ a ~ b | upper }}", "(| (~ a b) upper{})"),
        (
            "{{ status == 'needs_restart' | ternary(truthy='restart', falsy='continue') }}",
            "(| (== status 'needs_restart') ternary{falsy='continue', truthy='restart'})",
        ),
        (
            "{{ (status == 'needs_restart') | ternary(truthy='restart', falsy='continue') }}",
            "(| (== status 'needs_restart') ternary{falsy='continue', truthy='restart'})",
        ),
        // Macro calls
        (
            "{{ macros::input(label='Name', type='text') }}",
            "macros::input{label='Name', type='text'}",
        ),
        ("{{ macros::input() | safe }}", "(| macros::input{} safe{})"),
        // Parentheses
        ("{{ ((1)) }}", "1"),
        ("{{ (2 * 3) / 10 }}", "(/ (* 2 3) 10)"),
        ("{{ (2 * 3) / 10 }}", "(/ (* 2 3) 10)"),
        // not
        ("{{ not a }}", "(not a)"),
        ("{{ not b * 1 }}", "(not (* b 1))"),
        ("{{ not a and 1 + b > 3 }}", "(and (not a) (> (+ 1 b) 3))"),
        (
            "{{ not id and not true and not 1 + c }}",
            "(and (and (not id) (not true)) (not (+ 1 c)))",
        ),
        ("{{ a not in b }}", "(not (in a b))"),
        (
            "{{ a is defined and b is not defined(1, 2) }}",
            "(and (is a defined) (not (is b defined{1, 2})))",
        ),
        (
            "{{ a is defined and not b is defined(1, 2) }}",
            "(and (is a defined) (not (is b defined{1, 2})))",
        ),
        (
            "{{ not admin | default(val=true) }}",
            "(not (| admin default{val=true}))",
        ),
    ];

    for (t, expected) in tests {
        println!("{:?}", t);
        let mut parser = Parser::new(t);
        parser.parse().expect("parsed failed");
        match &parser.nodes[0] {
            Node::VariableBlock(e) => {
                assert_eq!(e.to_string(), expected)
            }
            _ => unreachable!("Got something that wasn't an expression"),
        }
    }
}

#[test]
fn can_parse_set() {
    let tests = vec![
        ("{% set a = 1 %}", "1"),
        ("{% set a = (b + 1) | round %}", "(| (+ b 1) round{})"),
        ("{% set a = 'hi' %}", "'hi'"),
        ("{% set a = macros::something() %}", "macros::something{}"),
        ("{% set a = utcnow() %}", "utcnow{}"),
        ("{% set a = [1, true, 'hello'] %}", "[1, true, 'hello']"),
        ("{% set_global a = 1 %}", "1"),
    ];

    for (t, expr) in tests {
        println!("{:?}", t);
        let mut parser = Parser::new(t);
        parser.parse().expect("parsed failed");
        match &parser.nodes[0] {
            Node::Set(s) => {
                assert_eq!(s.key, "a");
                assert_eq!(s.global, t.starts_with("{% set_global"));
                assert_eq!(s.value.to_string(), expr);
            }
            _ => unreachable!("Got something that wasn't a set"),
        }
    }
}

#[test]
fn can_parse_basic_include() {
    let tests = vec![
        (
            "{% include 'a.html' %}",
            Node::Include {
                files: vec!["a.html".to_string()],
                ignore_missing: false,
            },
        ),
        (
            "{% include `a.html` %}",
            Node::Include {
                files: vec!["a.html".to_string()],
                ignore_missing: false,
            },
        ),
        (
            "{% include \"a.html\" %}",
            Node::Include {
                files: vec!["a.html".to_string()],
                ignore_missing: false,
            },
        ),
        (
            "{% include \"a.html\" ignore missing %}",
            Node::Include {
                files: vec!["a.html".to_string()],
                ignore_missing: true,
            },
        ),
        (
            "{% include ['a.html', 'b.html'] %}",
            Node::Include {
                files: vec!["a.html".to_string(), "b.html".to_string()],
                ignore_missing: false,
            },
        ),
        (
            "{% include ['a.html', 'b.html'] ignore missing %}",
            Node::Include {
                files: vec!["a.html".to_string(), "b.html".to_string()],
                ignore_missing: true,
            },
        ),
    ];

    for (t, expected) in tests {
        println!("{:?}", t);
        let mut parser = Parser::new(t);
        parser.parse().expect("parsed failed");
        assert_eq!(parser.nodes[0], expected);
    }
}

#[test]
fn can_parse_extends() {
    let tests = vec![
        "{% extends 'a.html' %}",
        "{% extends `a.html` %}",
        "{% extends \"a.html\" %}",
    ];

    for t in tests {
        println!("{:?}", t);
        let mut parser = Parser::new(t);
        parser.parse().expect("parsed failed");
        assert_eq!(parser.parent, Some("a.html".to_owned()));
    }
}

#[test]
fn can_parse_comments() {
    let mut parser = Parser::new("{# a comment #}");
    parser.parse().expect("parsed failed");
    assert!(parser.nodes.is_empty());
}

#[test]
fn can_handle_whitespace_trim_left_side() {
    let tests = vec![
        ("Hello {{- world }}", Node::Text("Hello".to_string())),
        ("Hello {#- world #}", Node::Text("Hello".to_string())),
    ];

    for (t, expected) in tests {
        println!("{:?}", t);
        let mut parser = Parser::new(t);
        parser.parse().expect("parsed failed");
        assert_eq!(parser.nodes[0], expected);
    }
}

#[test]
fn can_parse_raw() {
    let tests = vec![
        (
            "{% raw %}Hello {{world}} {%- endraw %}",
            Node::Raw("Hello {{world}}".to_string()),
        ),
        (
            "{% raw -%}\r\nHello {% raw %} {% endraw %}",
            Node::Raw("Hello {% raw %} ".to_string()),
        ),
    ];

    for (t, expected) in tests {
        println!("{:?}", t);
        let mut parser = Parser::new(t);
        parser.parse().expect("parsed failed");
        assert_eq!(parser.nodes[0], expected);
    }
}

#[test]
fn can_parse_block() {
    let tests = vec![
        (
            "{% block hey -%} Hello {{-world}} {%- endblock %}",
            Block {
                name: "hey".to_string(),
                body: vec![
                    Node::Text("Hello".to_string()),
                    Node::VariableBlock(Expression::Ident("world".to_string())),
                ],
            },
        ),
        (
            "{% block hey -%} Hello {{-world}} {%- endblock hey %}",
            Block {
                name: "hey".to_string(),
                body: vec![
                    Node::Text("Hello".to_string()),
                    Node::VariableBlock(Expression::Ident("world".to_string())),
                ],
            },
        ),
        (
            "{% block hey -%} {% block ho %}hey{% endblock %} {%- endblock hey %}",
            Block {
                name: "hey".to_string(),
                body: vec![Node::Block(Block {
                    name: "ho".to_string(),
                    body: vec![Node::Text("hey".to_string())],
                })],
            },
        ),
        (
            "{% block hey -%} {{ super() }}{%- endblock hey %}",
            Block {
                name: "hey".to_string(),
                body: vec![Node::Super],
            },
        ),
    ];

    for (t, expected) in tests {
        println!("{:?}", t);
        let mut parser = Parser::new(t);
        parser.parse().expect("parsed failed");
        assert_eq!(parser.nodes[0], Node::Block(expected.clone()));
        println!("{:?}", parser.blocks);
        assert_eq!(parser.blocks["hey"], expected);
        if parser.blocks.contains_key("ho") {
            assert_eq!(
                parser.blocks["ho"],
                Block {
                    name: "ho".to_owned(),
                    body: vec![Node::Text("hey".to_owned())]
                }
            );
        }
    }
}

#[test]
fn can_parse_for_loop() {
    let tests = vec![
        (
            "{% for v in my_array -%} {{ v }}{%- endfor %}",
            Node::ForLoop(ForLoop {
                key: None,
                value: "v".to_owned(),
                container: Expression::Ident("my_array".to_owned()),
                body: vec![Node::VariableBlock(Expression::Ident("v".to_owned()))],
                otherwise: vec![],
            }),
        ),
        (
            "{% for v in [1, 2,] -%} {{ v }}{%- endfor %}",
            Node::ForLoop(ForLoop {
                key: None,
                value: "v".to_owned(),
                container: Expression::Array(vec![Expression::Integer(1), Expression::Integer(2)]),
                body: vec![Node::VariableBlock(Expression::Ident("v".to_owned()))],
                otherwise: vec![],
            }),
        ),
        (
            "{% for v in 'hello' -%} {{ v }}{%- endfor %}",
            Node::ForLoop(ForLoop {
                key: None,
                value: "v".to_owned(),
                container: Expression::Str("hello".to_owned()),
                body: vec![Node::VariableBlock(Expression::Ident("v".to_owned()))],
                otherwise: vec![],
            }),
        ),
        (
            "{% for v in my_array | sort -%} {{ v }}{% else %}Empty{%- endfor %}",
            Node::ForLoop(ForLoop {
                key: None,
                value: "v".to_owned(),
                container: Expression::Expr(
                    Operator::Pipe,
                    vec![
                        Expression::Ident("my_array".to_owned()),
                        Expression::Function("sort".to_owned(), HashMap::new()),
                    ],
                ),
                body: vec![Node::VariableBlock(Expression::Ident("v".to_owned()))],
                otherwise: vec![Node::Text("Empty".to_owned())],
            }),
        ),
        (
            "{% for k, v in obj -%} {{ v }}{% else %}Empty{%- endfor %}",
            Node::ForLoop(ForLoop {
                key: Some("k".to_owned()),
                value: "v".to_owned(),
                container: Expression::Ident("obj".to_owned()),
                body: vec![Node::VariableBlock(Expression::Ident("v".to_owned()))],
                otherwise: vec![Node::Text("Empty".to_owned())],
            }),
        ),
    ];

    for (t, expected) in tests {
        println!("{:?}", t);
        let mut parser = Parser::new(t);
        parser.parse().expect("parsed failed");
        assert_eq!(parser.nodes[0], expected);
    }
}

#[test]
fn can_parse_if_elif_else() {
    let tests = vec![(
        "{% if true -%} Hello {%- elif false -%} otherwise{% elif b %}ELIF{%- else -%}{{world}} {%- endif %}",
        Node::If(If {
            conditions: vec![
                (
                    Expression::Bool(true),
                    vec![Node::Text("Hello".to_string())],
                ),
                (
                    Expression::Bool(false),
                    vec![Node::Text("otherwise".to_string())],
                ),
                (
                    Expression::Ident("b".to_string()),
                    vec![Node::Text("ELIF".to_string())],
                ),
            ],
            otherwise: vec![Node::VariableBlock(Expression::Ident("world".to_string()))],
        }),
    )];

    for (t, expected) in tests {
        println!("{:?}", t);
        let mut parser = Parser::new(t);
        parser.parse().expect("parsed failed");
        assert_eq!(parser.nodes[0], expected);
    }
}

#[test]
fn can_parse_filter_sections() {
    let tests = vec![
        (
            "{% filter safe %} hello {% endfilter %}",
            Node::FilterSection(FilterSection {
                name: "safe".to_string(),
                kwargs: HashMap::new(),
                body: vec![Node::Text(" hello ".to_string())],
            }),
        ),
        (
            "{% filter upper(hey=1) -%} hello {%- endfilter %}",
            Node::FilterSection(FilterSection {
                name: "upper".to_string(),
                kwargs: hashmap! {
                    "hey".to_string() => Expression::Integer(1),
                },
                body: vec![Node::Text("hello".to_string())],
            }),
        ),
        (
            "{% filter upper(hey=1) -%}{% if true %}a{%endif %}{%- endfilter %}",
            Node::FilterSection(FilterSection {
                name: "upper".to_string(),
                kwargs: hashmap! {
                    "hey".to_string() => Expression::Integer(1),
                },
                body: vec![Node::If(If {
                    conditions: vec![(Expression::Bool(true), vec![Node::Text("a".to_string())])],
                    otherwise: vec![],
                })],
            }),
        ),
    ];

    for (t, expected) in tests {
        println!("{:?}", t);
        let mut parser = Parser::new(t);
        parser.parse().expect("parsed failed");
        assert_eq!(parser.nodes[0], expected);
    }
}

#[test]
fn can_parse_macro_definitions() {
    let tests = vec![
        (
            "{% macro popup() -%} hello {%- endmacro %}",
            hashmap!(
                "popup".to_owned() => MacroDefinition {
                    name: "popup".to_owned(),
                    kwargs: HashMap::new(),
                    body: vec![Node::Text("hello".to_owned())]
                },
            ),
        ),
        (
            "{% macro another(hey='ho', optional) -%} hello {%- endmacro %}",
            hashmap!(
                "another".to_owned() => MacroDefinition {
                    name: "another".to_owned(),
                    kwargs: hashmap!(
                        "hey".to_owned() => Some(Expression::Str("ho".to_owned())),
                        "optional".to_owned() => None,
                    ),
                    body: vec![Node::Text("hello".to_owned())]
                },
            ),
        ),
    ];

    for (t, expected) in tests {
        println!("{:?}", t);
        let mut parser = Parser::new(t);
        parser.parse().expect("parsed failed");
        assert_eq!(parser.macros, expected);
    }
}

#[test]
fn can_parse_macro_imports() {
    let tests = vec![
        (
            r#"{% import 'macros.html' as macros %}"#,
            vec![("macros.html".to_owned(), "macros".to_owned())],
        ),
        (
            r#"{% import 'macros.html' as macros %}"#,
            vec![("macros.html".to_owned(), "macros".to_owned())],
        ),
    ];

    for (t, expected) in tests {
        println!("{:?}", t);
        let mut parser = Parser::new(t);
        parser.parse().expect("parsed failed");
        assert_eq!(parser.macro_imports, expected);
    }
}

#[test]
fn can_parse_a_basic_template() {
    let tpl = "
    <html>
      <head>
        <title>{{ product.name }}</title>
      </head>
      <body>
        <h1>{{ product.name }} - {{ product.manufacturer | upper }}</h1>
        <p>{{ product.summary }}</p>
        <p>£{{ product.price * 1.20 }} (VAT inc.)</p>
        <p>Look at reviews from your friends {{ username }}</p>
        <button>Buy!</button>
      </body>
    </html>
    ";
    let mut parser = Parser::new(tpl);
    parser.parse().expect("parsed failed");
    println!("{:#?}", parser.nodes);
    assert_eq!(parser.nodes.len(), 13);
}

#[test]
fn can_parse_slightly_complex_template() {
    let tpl = "
    <html>
      <head>
        <title>{% block title %}{{ product.name }}{% endblock title %}</title>
      </head>
      <body>
        {%- block content -%}
            {%- for item in items -%}
                {%- if item.show -%}{{item.name}}{%- else -%}-{%- endif -%}
            {%- else -%}
                No items.
            {%- endfor -%}
        {%- endblock -%}
      </body>
    </html>";
    let mut parser = Parser::new(tpl);
    parser.parse().expect("parsed failed");
    println!("{:#?}", parser.nodes);
    let expected = vec![
        Node::Text("\n    <html>\n      <head>\n        <title>".to_string()),
        Node::Block(Block {
            name: "title".to_owned(),
            body: vec![Node::VariableBlock(Expression::Ident(
                "product.name".to_owned(),
            ))],
        }),
        Node::Text("</title>\n      </head>\n      <body>".to_owned()),
        Node::Block(Block {
            name: "content".to_owned(),
            body: vec![Node::ForLoop(ForLoop {
                key: None,
                value: "item".to_owned(),
                container: Expression::Ident("items".to_string()),
                body: vec![Node::If(If {
                    conditions: vec![(
                        Expression::Ident("item.show".to_owned()),
                        vec![Node::VariableBlock(Expression::Ident(
                            "item.name".to_owned(),
                        ))],
                    )],
                    otherwise: vec![Node::Text("-".to_owned())],
                })],
                otherwise: vec![Node::Text("No items.".to_owned())],
            })],
        }),
        Node::Text("</body>\n    </html>".to_owned()),
    ];
    assert_eq!(parser.nodes, expected);
}

// TODO: Do we care about that in practice
// #[test]
// fn can_parse_expression_constant_folding() {
//     let tests = vec![
//         // https://github.com/Keats/tera/blob/master/src/parser/tests/parser.rs#L1074
//         ("{{`hello` ~ 'hey'}}", "'hellohey'"),
//         ("{{1 ~ 'ho'}}", "'1ho'"),
//         ("{{1 ~ 'ho' ~ 2}}", "'1ho2'"),
//         // comparisons
//         ("{{1 == 1}}", "true"),
//         ("{{1 == '1'}}", "false"),
//         ("{{1 == 0}}", "false"),
//     ];
//
//     for (input, expected) in tests {
//         let mut parser = Parser::new(input);
//         parser.parse().unwrap();
//         match &parser.nodes[0] {
//             Node::Expression(e) => {
//                 assert_eq!(e.to_string(), expected)
//             }
//             _ => unreachable!("Got something that wasn't an expression"),
//         }
//     }
// }