use crate::parsing::parser::Parser;
use crate::vm::Compiler;

#[test]
fn vm_ok() {
    insta::glob!("vm_inputs/success/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let nodes = Parser::new(&contents).parse().unwrap();
        let mut compiler = Compiler::new(&path.file_name().unwrap().to_string_lossy(), "");
        compiler.compile(nodes);

        insta::assert_debug_snapshot!(compiler.chunk);
    });
}

#[test]
fn vm_blocks() {
    insta::glob!("vm_inputs/blocks/*.txt", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        let nodes = Parser::new(&contents).parse().unwrap();
        let mut compiler = Compiler::new(&path.file_name().unwrap().to_string_lossy(), "");
        compiler.compile(nodes);

        let mut s = String::with_capacity(1000);
        s.push_str(&format!("{:?}", compiler.chunk));
        s.push_str("\n\n");

        let mut blocks: Vec<_> = compiler.blocks.into_iter().collect();
        blocks.sort_by(|a, b| a.0.cmp(&b.0));
        for (name, chunk) in blocks {
            s.push_str(&format!(">> Block: {name}\n"));
            s.push_str(&format!("{chunk:?}"));
            s.push_str("---\n");
        }
        insta::assert_snapshot!(s);
    });
}
