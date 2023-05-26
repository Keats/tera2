use std::collections::HashMap;

use crate::errors::{Error, TeraResult};
use crate::parsing::compiler::CompiledMacroDefinition;
use crate::parsing::{Chunk, Compiler};
use crate::Parser;

#[derive(Debug, PartialEq, Clone)]
pub struct Template {
    pub(crate) name: String,
    source: String,
    path: Option<String>,
    chunk: Chunk,
    // (file, name)
    // Used for its index in instructions
    pub(crate) macro_calls: Vec<(String, String)>,
    // Same as above, but just the definition directly
    pub(crate) macro_calls_def: Vec<CompiledMacroDefinition>,
    pub(crate) blocks: HashMap<String, Chunk>,
    pub(crate) macro_definitions: HashMap<String, CompiledMacroDefinition>,
    pub(crate) raw_content_num_bytes: usize,
    /// The full list of parent templates names
    pub(crate) parents: Vec<String>,
}

impl Template {
    pub(crate) fn new(name: &str, source: &str, path: Option<String>) -> TeraResult<Self> {
        let parser = Parser::new(source);
        let parser_output = parser.parse()?;
        let parents = if let Some(p) = parser_output.parent {
            vec![p]
        } else {
            vec![]
        };

        let mut body_compiler = Compiler::new(name, source);
        body_compiler.compile(parser_output.nodes);

        // We are going to convert macro calls {namespace -> name} to {filename -> name}
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

        // TODO: What do we do with macro calls in macros?
        // TODO: add tests for it + recursive macros
        let mut macro_definitions = HashMap::with_capacity(parser_output.macro_definitions.len());
        for macro_def in parser_output.macro_definitions {
            let name = macro_def.name;
            let kwargs = macro_def.kwargs;
            let mut compiler = Compiler::new(&name, source);
            compiler.compile(macro_def.body);
            macro_definitions.insert(
                name.clone(),
                CompiledMacroDefinition {
                    name,
                    kwargs,
                    body: compiler.chunk,
                },
            );
        }

        Ok(Self {
            name: name.to_string(),
            source: source.to_string(),
            path,
            blocks,
            macro_definitions,
            macro_calls,
            macro_calls_def: Vec::new(),
            raw_content_num_bytes,
            chunk,
            parents,
        })
    }
}

/// Recursive fn that finds all the parents and put them in an ordered Vec from closest to first parent
/// parent template
pub(crate) fn find_parents(
    templates: &HashMap<String, Template>,
    start: &Template,
    template: &Template,
    mut parents: Vec<String>,
) -> Result<Vec<String>, Error> {
    if !parents.is_empty() && start.name == template.name {
        return Err(Error::circular_extend(&start.name, parents));
    }

    match template.parents.last() {
        Some(ref p) => match templates.get(*p) {
            Some(parent) => {
                parents.push(parent.name.clone());
                find_parents(templates, start, parent, parents)
            }
            None => Err(Error::missing_parent(&template.name, p)),
        },
        None => {
            parents.reverse();
            Ok(parents)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_find_parents() {
        let mut tpls = HashMap::new();
        tpls.insert("a".to_string(), Template::new("a", "", None).unwrap());
        tpls.insert(
            "b".to_string(),
            Template::new("b", "{% extends 'a' %}", None).unwrap(),
        );
        tpls.insert(
            "c".to_string(),
            Template::new("c", "{% extends 'b' %}", None).unwrap(),
        );

        let parents_a = find_parents(&tpls, &tpls["a"], &tpls["a"], vec![]).unwrap();
        assert!(parents_a.is_empty());

        let parents_b = find_parents(&tpls, &tpls["b"], &tpls["b"], vec![]).unwrap();
        assert_eq!(parents_b, vec!["a".to_string()]);

        let parents_c = find_parents(&tpls, &tpls["c"], &tpls["c"], vec![]).unwrap();
        assert_eq!(parents_c, vec!["a".to_string(), "b".to_string()]);
    }
}
