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

#[bench]
fn bench_parsing_simple_template(b: &mut test::Bencher) {
    b.iter(|| Parser::new(SIMPLE_TEMPLATE));
}
