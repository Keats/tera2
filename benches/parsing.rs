#![feature(test)]
extern crate test;
use parser_test::Parser;

static SIMPLE_TEMPLATE: &'static str = "
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

static SLIGHTLY_COMPLEX_TEMPLATE: &'static str = "
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
</html>
";

#[bench]
fn bench_parsing_simple_template(b: &mut test::Bencher) {
    b.iter(|| {
        let mut parser = Parser::new(SIMPLE_TEMPLATE);
        parser.parse();
    });
}

#[bench]
fn bench_parsing_slightly_complex_template(b: &mut test::Bencher) {
    b.iter(|| {
        let mut parser = Parser::new(SLIGHTLY_COMPLEX_TEMPLATE);
        parser.parse();
    });
}
