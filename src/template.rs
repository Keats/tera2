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
    pub(crate) chunk: Chunk,
    /// (file, name)
    /// Used for its index in instructions
    pub(crate) macro_calls: Vec<(String, String)>,
    /// Same as above, but just the definition directly
    pub(crate) macro_calls_def: Vec<CompiledMacroDefinition>,
    /// The blocks contained in this template only
    pub(crate) blocks: HashMap<String, Chunk>,
    /// The macros defined in this template only
    pub(crate) macro_definitions: HashMap<String, CompiledMacroDefinition>,
    /// The number of bytes of raw content in its parents and itself
    pub(crate) raw_content_num_bytes: usize,
    /// The full list of parent templates names
    pub(crate) parents: Vec<String>,
    pub(crate) block_lineage: HashMap<String, Vec<Chunk>>,
}

impl Template {
    pub(crate) fn new(tpl_name: &str, source: &str, path: Option<String>) -> TeraResult<Self> {
        let parser = Parser::new(source);
        let parser_output = parser.parse()?;
        let parents = if let Some(p) = parser_output.parent {
            vec![p]
        } else {
            vec![]
        };

        let mut body_compiler = Compiler::new(tpl_name, source);
        body_compiler.compile(parser_output.nodes);

        // We are going to convert macro calls {namespace -> name} to {filename -> name}
        let mut macro_calls = Vec::with_capacity(body_compiler.macro_calls.len());

        // We need the macro handling logic both for the template and for each macro
        let mut handle_macro_call = |namespace: String, macro_name: String| -> TeraResult<()> {
            if &namespace == "self" {
                macro_calls.push((tpl_name.to_string(), macro_name));
                return Ok(());
            }

            if let Some((filename, _)) = parser_output
                .macro_imports
                .iter()
                .find(|(_, n)| n == &namespace)
            {
                macro_calls.push((filename.to_string(), macro_name));
                Ok(())
            } else {
                Err(Error::namespace_not_loaded(tpl_name, namespace))
            }
        };

        for (namespace, macro_name) in body_compiler.macro_calls {
            handle_macro_call(namespace, macro_name)?;
        }

        let chunk = body_compiler.chunk;
        let blocks = body_compiler.blocks;
        let raw_content_num_bytes = body_compiler.raw_content_num_bytes;

        let mut macro_definitions = HashMap::with_capacity(parser_output.macro_definitions.len());
        for macro_def in parser_output.macro_definitions {
            let macro_def_name = macro_def.name;
            let kwargs = macro_def.kwargs;
            let mut compiler = Compiler::new(&macro_def_name, source);
            compiler.compile(macro_def.body);

            for (namespace, macro_name) in compiler.macro_calls {
                handle_macro_call(namespace, macro_name)?;
            }

            macro_definitions.insert(
                macro_def_name.clone(),
                CompiledMacroDefinition {
                    name: macro_def_name,
                    kwargs,
                    chunk: compiler.chunk,
                },
            );
        }

        Ok(Self {
            name: tpl_name.to_string(),
            source: source.to_string(),
            path,
            blocks,
            macro_definitions,
            macro_calls,
            macro_calls_def: Vec::new(),
            raw_content_num_bytes,
            chunk,
            parents,
            block_lineage: HashMap::new(),
        })
    }

    pub(crate) fn size_hint(&self) -> usize {
        (self.raw_content_num_bytes * 2).next_power_of_two()
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
