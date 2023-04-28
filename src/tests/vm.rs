use crate::parsing::parser::Parser;
use crate::vm::Compiler;

#[test]
fn lexer_ok() {
    insta::glob!("vm_inputs/success/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let nodes = Parser::new(&contents).parse().unwrap();
        let mut compiler = Compiler::new(&path.file_name().unwrap().to_string_lossy(), "");
        compiler.compile(nodes);

        insta::assert_debug_snapshot!(compiler.chunk);
    });
}
