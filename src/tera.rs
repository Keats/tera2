use crate::escape_html;
use crate::template::Template;
use std::collections::HashMap;

/// Default template name used for `Tera::render_str` and `Tera::one_off`.
const ONE_OFF_TEMPLATE_NAME: &str = "__tera_one_off";

/// The escape function type definition
pub type EscapeFn = fn(&str) -> String;

#[derive(Clone)]
pub struct Tera {
    #[doc(hidden)]
    pub templates: HashMap<String, Template>,
    // Which extensions does Tera automatically autoescape on.
    // Defaults to [".html", ".htm", ".xml"]
    #[doc(hidden)]
    pub autoescape_suffixes: Vec<&'static str>,
    #[doc(hidden)]
    escape_fn: EscapeFn,
}

impl Tera {}

impl Default for Tera {
    fn default() -> Self {
        Self {
            templates: HashMap::new(),
            autoescape_suffixes: vec![".html", ".htm", ".xml"],
            escape_fn: escape_html,
        }
    }
}
