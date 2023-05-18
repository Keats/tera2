use std::collections::HashMap;

use crate::errors::TeraResult;
use crate::parsing::compiler::CompiledMacroDefinition;
use crate::parsing::parser::ParserOutput;
use crate::parsing::{Chunk, Compiler};
use crate::Parser;

/// A struct used to hold internal states about a template while it's being parsed
#[derive(Debug, PartialEq, Clone)]
struct InProgressTemplate {
    name: String,
    path: Option<String>,
    source: String,
    parent: Option<String>,
    chunk: Chunk,
    // (file, namespace)
    // TODO: use type alias to make things clearer and avoid using comments
    macro_imports: Vec<(String, String)>,
    blocks: HashMap<String, Chunk>,
    macro_definitions: HashMap<String, CompiledMacroDefinition>,
}

impl InProgressTemplate {
    pub fn new<S>(name: S, source: S, path: Option<String>) -> TeraResult<Self>
    where
        S: Into<String>,
    {
        let name = name.into();
        let source = source.into();
        let parser = Parser::new(&source);
        let parser_output = parser.parse()?;
        let mut body_compiler = Compiler::new(&name, &source);
        body_compiler.compile(parser_output.nodes);
        let chunk = body_compiler.chunk;
        let blocks = body_compiler.blocks;

        let mut macro_definitions = HashMap::with_capacity(parser_output.macro_definitions.len());
        for macro_def in parser_output.macro_definitions {
            let name = macro_def.name;
            let kwargs = macro_def.kwargs;
            let mut compiler = Compiler::new(&name, &source);
            compiler.compile(macro_def.body);
            macro_definitions.insert(
                name,
                CompiledMacroDefinition {
                    kwargs,
                    body: compiler.chunk,
                },
            );
        }

        Ok(Self {
            name,
            source,
            path,
            chunk,
            blocks,
            macro_definitions,
            parent: parser_output.parent,
            macro_imports: parser_output.macro_imports,
        })
    }

    pub fn into_template(self) {}
}

#[derive(Debug, PartialEq, Clone)]
struct Template {
    name: String,
    path: Option<String>,
    source: String,

    chunk: Chunk,

    // TODO what do we need
    blocks: HashMap<String, Chunk>,
    macro_namespaces: Vec<String>,
    macro_names: Vec<Vec<String>>,
    /// How many bytes of raw content we've seen
    /// TODO: take into account the fact that a child needs to get their parent bytes size roughly
    size_hint: usize,
}

impl Template {
    pub fn new<S>(name: S, source: S, path: Option<String>) -> TeraResult<Self>
    where
        S: Into<String>,
    {
        todo!("");
        // Ok(Self {
        //     name,
        //     source,
        //     path,
        // })
    }
}
