use crate::errors::{Error, ErrorKind, TeraResult};
use crate::parsing::ast::ComponentDefinition;
use crate::parsing::{Chunk, Compiler};
use crate::{HashMap, Parser};

#[derive(Debug, PartialEq, Clone)]
pub struct Template {
    pub(crate) name: String,
    pub(crate) source: String,
    pub(crate) path: Option<String>,
    pub(crate) chunk: Chunk,
    /// The blocks contained in this template only
    pub(crate) blocks: HashMap<String, Chunk>,
    pub(crate) components: HashMap<String, (ComponentDefinition, Chunk)>,
    pub(crate) component_calls: Vec<String>,
    /// The number of bytes of raw content in its parents and itself
    pub(crate) raw_content_num_bytes: usize,
    /// The full list of parent templates names
    pub(crate) parents: Vec<String>,
    pub(crate) block_lineage: HashMap<String, Vec<Chunk>>,
    /// Whether this template came from a call to `Tera::extend`, so we do
    /// not remove it when we are doing a template reload
    pub(crate) from_extend: bool,
    /// Whether to auto-escape this template. It's set to `true` as default and will be updated
    /// when calling `Tera::autoescape_on` and when finalizing the templates
    pub(crate) autoescape_enabled: bool,
}

impl Template {
    pub(crate) fn new(tpl_name: &str, source: &str, path: Option<String>) -> TeraResult<Self> {
        let parser = Parser::new(source);
        let parser_output = match parser.parse() {
            Ok(p) => p,
            Err(e) => match e.kind {
                ErrorKind::SyntaxError(mut s) => {
                    s.generate_report(tpl_name, source, "Syntax error");
                    return Err(Error {
                        kind: ErrorKind::SyntaxError(s),
                        source: None,
                    });
                }
                _ => unreachable!("Parser got something other than a SyntaxError: {e}"),
            },
        };
        let parents = if let Some(p) = parser_output.parent {
            vec![p]
        } else {
            vec![]
        };

        let mut body_compiler = Compiler::new(tpl_name, source);
        body_compiler.compile(parser_output.nodes);

        let chunk = body_compiler.chunk;
        let blocks = body_compiler.blocks;
        let raw_content_num_bytes = body_compiler.raw_content_num_bytes;
        let components = parser_output
            .component_definitions
            .into_iter()
            .map(|c| {
                let mut compiler = Compiler::new(&c.name, source);
                // We don't need the nodes again after it's compiled
                compiler.compile(c.body.clone());
                (c.name.clone(), (c, compiler.chunk))
            })
            .collect();
        let component_calls = body_compiler
            .component_calls
            .into_iter()
            .map(|c| c.name)
            .collect();

        Ok(Self {
            name: tpl_name.to_string(),
            source: source.to_string(),
            path,
            blocks,
            raw_content_num_bytes,
            chunk,
            parents,
            components,
            component_calls,
            block_lineage: HashMap::new(),
            from_extend: false,
            autoescape_enabled: true,
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
