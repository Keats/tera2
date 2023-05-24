use std::collections::HashMap;

use crate::errors::{Error, TeraResult};
use crate::parsing::compiler::CompiledMacroDefinition;
use crate::parsing::{Chunk, Compiler};
use crate::Parser;

/// A struct used to hold internal states about a template while it's being parsed
#[derive(Debug, PartialEq, Clone)]
pub(crate) struct InProgressTemplate {
    name: String,
    source: String,
    path: Option<String>,
    parent: Option<String>,
    chunk: Chunk,
    // (file, name)
    // used for its index only in instructions. it is a set originally
    macro_calls: Vec<(String, String)>,
    blocks: HashMap<String, Chunk>,
    macro_definitions: HashMap<String, CompiledMacroDefinition>,
    raw_content_num_bytes: usize,
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

        // We are going to convert macro calls namespace -> name to filename -> name
        let mut macro_calls = Vec::with_capacity(body_compiler.macro_calls.len());
        for (namespace, name) in body_compiler.macro_calls {
            if let Some((filename, _)) = parser_output
                .macro_imports
                .iter()
                .find(|(n, _)| n == &namespace)
            {
                macro_calls.push((filename.to_string(), name));
            } else {
                return Err(Error::namespace_not_loaded(&name, namespace));
            }
        }

        let chunk = body_compiler.chunk;
        let blocks = body_compiler.blocks;
        let raw_content_num_bytes = body_compiler.raw_content_num_bytes;

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
            macro_calls,
            raw_content_num_bytes,
            parent: parser_output.parent,
        })
    }

    fn into_template(self) -> Template {
        Template {
            name: self.name,
            path: self.path,
            source: self.source,
            chunk: self.chunk,
            size_hint: (self.raw_content_num_bytes * 2).next_power_of_two(),
            parents: Vec::new(),
            macro_indices: Vec::new(),
            blocks_definitions: HashMap::new(),
        }
    }
}

// Recursive fn that finds all the parents and put them in an ordered Vec from closest to first parent
// parent template
fn find_parents(
    templates: &HashMap<String, InProgressTemplate>,
    start: &InProgressTemplate,
    template: &InProgressTemplate,
    mut parents: Vec<String>,
) -> Result<Vec<String>, Error> {
    if !parents.is_empty() && start.name == template.name {
        return Err(Error::circular_extend(&start.name, parents));
    }

    match template.parent {
        Some(ref p) => match templates.get(p) {
            Some(parent) => {
                parents.push(parent.name.clone());
                find_parents(templates, start, parent, parents)
            }
            None => Err(Error::missing_parent(&template.name, p)),
        },
        None => Ok(parents),
    }
}

// TODO: How to add a single template? That's not going to work in practice
// because we need it to work with existing templates
// Should we keep templates strings on Tera rather than the template? How would that work for
// error reporting?
// Macro indices is not going to work either unless we pass the current macros so we only push
// Same for blocks. Can we have raw content be just range to keep instructions small?
fn validate_templates(
    templates: HashMap<String, InProgressTemplate>,
) -> Result<HashMap<String, Template>, Error> {
    let mut output = HashMap::with_capacity(templates.len());
    // {file -> {name -> compiled macro}}
    let mut compiled_macros_by_file = HashMap::with_capacity(20);
    let mut tpl_parents = HashMap::new();
    let mut tpl_block_definitions = HashMap::new();
    let mut tpl_size_hint = HashMap::new();

    // 1st loop: we find the parents of each templates with all the block definitions
    // as well as copying the macro definitions defined in each template
    for (name, tpl) in &templates {
        let parents = find_parents(&templates, tpl, tpl, vec![])?;
        compiled_macros_by_file.insert(tpl.name.clone(), tpl.macro_definitions.clone());

        let mut blocks_definitions = HashMap::new();
        for (block_name, def) in &tpl.blocks {
            // push our own block first
            let mut definitions = vec![def.clone()];

            // and then see if our parents have it
            for parent in &parents {
                let t = &templates[parent];

                if let Some(b) = t.blocks.get(block_name) {
                    definitions.push(b.clone());
                }
            }
            blocks_definitions.insert(block_name.clone(), definitions);
        }

        let mut size_hint = tpl.raw_content_num_bytes;
        for parent in &parents {
            size_hint += &templates[parent].raw_content_num_bytes;
        }

        tpl_parents.insert(name.clone(), parents);
        tpl_block_definitions.insert(name.clone(), blocks_definitions);
        tpl_size_hint.insert(name.clone(), size_hint);
    }

    // 2nd loop: we are creating a global array of the macro definitions and creating indices for each
    let mut macro_definitions = Vec::with_capacity(50);
    let mut macro_indices = HashMap::with_capacity(50);
    for (filename, macros) in compiled_macros_by_file {
        for (macro_name, macro_def) in macros {
            let idx = macro_definitions.len();
            macro_definitions.push(macro_def);
            macro_indices.insert((filename.clone(), macro_name), idx);
        }
    }

    // 3rd loop: we create final templates with only the values needed
    for (name, mut tpl) in templates {
        tpl.raw_content_num_bytes += tpl_size_hint.remove(&name).unwrap();
        let parents = tpl_parents.remove(&name).unwrap();
        let blocks = tpl_block_definitions.remove(&name).unwrap();
        let macro_indices: Vec<_> = tpl.macro_calls.iter().map(|x| macro_indices[x]).collect();

        let mut final_tpl = tpl.into_template();
        final_tpl.parents = parents;
        final_tpl.macro_indices = macro_indices;
        final_tpl.blocks_definitions = blocks;
        output.insert(name, final_tpl);
    }

    Ok(output)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Template {
    name: String,
    path: Option<String>,
    source: String,
    chunk: Chunk,
    /// A hint for the buffer size to use
    size_hint: usize,
    /// The full list of parent templates
    parents: Vec<String>,
    /// Index into the global macro definition array
    macro_indices: Vec<usize>,
    /// The definition of all the blocks for the current template and the definition of those blocks
    /// in parent templates if there are some.
    /// Needed for super() to work without having to find them each time.
    /// TODO: tests that macros work for in blocks and with super()
    blocks_definitions: HashMap<String, Vec<Chunk>>,
}
