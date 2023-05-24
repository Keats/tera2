use criterion::{black_box, criterion_group, criterion_main, Criterion};
use tera::Parser;

static SIMPLE_TEMPLATE: &str = "
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

static SLIGHTLY_COMPLEX_TEMPLATE: &str = "
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

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("simple", |b| {
        b.iter(|| {
            let mut parser = Parser::new(SIMPLE_TEMPLATE);
            black_box(parser.parse().unwrap());
        })
    });
    c.bench_function("slightly complex", |b| {
        b.iter(|| {
            let mut parser = Parser::new(SLIGHTLY_COMPLEX_TEMPLATE);
            black_box(parser.parse().unwrap());
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
