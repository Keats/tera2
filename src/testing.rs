use parser_test::parser::Parser;

use codespan_reporting::files;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

const TEMPLATE: &str = r#"{{ hello(] }}"#;

fn main() {
    let mut parser = Parser::new(TEMPLATE);
    let res = parser.parse_expression(0);
    let file = files::SimpleFile::new("test.tpl", TEMPLATE);

    let r = res.unwrap_err();
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();
    term::emit(&mut writer.lock(), &config, &file, &r.report()).unwrap();
}
