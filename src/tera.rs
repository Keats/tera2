use crate::args::ArgFromValue;
use crate::errors::{Error, TeraResult};
use crate::filters::{Filter, StoredFilter};
use crate::template::{find_parents, Template};
use crate::value::FunctionResult;
use crate::vm::interpreter::VirtualMachine;
use crate::{escape_html, Context, HashMap};

/// Default template name used for `Tera::render_str` and `Tera::one_off`.
const ONE_OFF_TEMPLATE_NAME: &str = "__tera_one_off";

/// The escape function type definition
pub type EscapeFn = fn(&str) -> String;

#[derive(Clone)]
pub struct Tera {
    #[doc(hidden)]
    pub templates: HashMap<String, Template>,
    /// Which extensions does Tera automatically autoescape on.
    /// Defaults to [".html", ".htm", ".xml"]
    #[doc(hidden)]
    autoescape_suffixes: Vec<&'static str>,
    #[doc(hidden)]
    escape_fn: EscapeFn,
    global_context: Context,
    pub(crate) filters: HashMap<&'static str, StoredFilter>,
}

impl Tera {
    pub fn new() -> Self {
        Self::default()
    }

    /// Select which suffix(es) to automatically do HTML escaping on.
    ///
    /// By default, autoescaping is performed on `.html`, `.htm` and `.xml` template files. Only
    /// call this function if you wish to change the defaults.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// # use tera::Tera;
    /// let mut tera = Tera::default();
    /// // escape only files ending with `.php.html`
    /// tera.autoescape_on(vec![".php.html"]);
    /// // disable autoescaping completely
    /// tera.autoescape_on(vec![]);
    /// ```
    pub fn autoescape_on(&mut self, suffixes: Vec<&'static str>) {
        self.autoescape_suffixes = suffixes;
    }
    /// Set user-defined function that is used to escape content.
    ///
    /// Often times, arbitrary data needs to be injected into a template without allowing injection
    /// attacks. For this reason, typically escaping is performed on all input. By default, the
    /// escaping function will produce HTML escapes, but it can be overridden to produce escapes
    /// more appropriate to the language being used.
    ///
    /// Inside templates, escaping can be turned off for specific content using the `safe` filter.
    /// For example, the string `{{ data }}` inside a template will escape data, while `{{ data |
    /// safe }}` will not.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// # use tera::{Tera, Context};
    /// // Create new Tera instance
    /// let mut tera = Tera::default();
    ///
    /// // Override escape function
    /// tera.set_escape_fn(|input| {
    ///     input.escape_default().collect()
    /// });
    ///
    /// // Create template and enable autoescape
    /// tera.add_raw_template("hello.js", "const data = \"{{ content }}\";").unwrap();
    /// tera.autoescape_on(vec!["js"]);
    ///
    /// // Create context with some data
    /// let mut context = Context::new();
    /// context.insert("content", &"Hello\n\'world\"!");
    ///
    /// // Render template
    /// let result = tera.render("hello.js", &context).unwrap();
    /// assert_eq!(result, r#"const data = "Hello\n\'world\"!";"#);
    /// ```
    pub fn set_escape_fn(&mut self, function: EscapeFn) {
        self.escape_fn = function;
    }

    /// Reset escape function to default [`escape_html()`].
    pub fn reset_escape_fn(&mut self) {
        self.escape_fn = escape_html;
    }

    /// Register a filter with Tera.
    ///
    /// If a filter with that name already exists, it will be overwritten
    ///
    /// ```no_compile
    /// tera.register_filter("upper", string::upper);
    /// ```
    pub fn register_filter<Func, Arg, Res>(&mut self, name: &'static str, filter: Func)
    where
        Func: Filter<Arg, Res> + for<'a> Filter<<Arg as ArgFromValue<'a>>::Output, Res>,
        Arg: for<'a> ArgFromValue<'a>,
        Res: FunctionResult,
    {
        self.filters.insert(name, StoredFilter::new(filter));
    }

    fn register_builtin_filters(&mut self) {
        self.register_filter("safe", crate::filters::safe);
        self.register_filter("default", crate::filters::default);
        self.register_filter("upper", crate::filters::upper);
        self.register_filter("lower", crate::filters::lower);
        self.register_filter("trim", crate::filters::trim);
        self.register_filter("trim_start", crate::filters::trim_start);
        self.register_filter("trim_end", crate::filters::trim_end);
        self.register_filter("replace", crate::filters::replace);
        self.register_filter("capitalize", crate::filters::capitalize);
        self.register_filter("title", crate::filters::title);
        self.register_filter("indent", crate::filters::indent);
        self.register_filter("str", crate::filters::str);
        self.register_filter("int", crate::filters::int);
        self.register_filter("float", crate::filters::float);
        self.register_filter("length", crate::filters::length);
        self.register_filter("reverse", crate::filters::reverse);
        self.register_filter("split", crate::filters::split);
        self.register_filter("abs", crate::filters::abs);
        self.register_filter("round", crate::filters::round);
        self.register_filter("first", crate::filters::first);
        self.register_filter("last", crate::filters::last);
        self.register_filter("nth", crate::filters::nth);
        self.register_filter("join", crate::filters::join);
        self.register_filter("slice", crate::filters::slice);
        self.register_filter("unique", crate::filters::unique);
        self.register_filter("map", crate::filters::map);
        self.register_filter("filter", crate::filters::filter);
        self.register_filter("group_by", crate::filters::group_by);
    }

    /// Optimizes the templates when possible and doing some light
    /// checks like whether blocks/macros/templates all exist when they are used
    fn finalize_templates(&mut self) -> TeraResult<()> {
        // {file -> {name -> compiled macro}}
        let mut compiled_macros_by_file = HashMap::with_capacity(20);
        let mut tpl_parents = HashMap::new();
        let mut tpl_size_hint = HashMap::new();

        // 1st loop: we find the parents of each templates with all the block definitions
        // as well as copying the macro definitions defined in each template
        for (name, tpl) in &self.templates {
            let parents = find_parents(&self.templates, tpl, tpl, vec![])?;
            compiled_macros_by_file.insert(tpl.name.clone(), tpl.macro_definitions.clone());

            let mut size_hint = tpl.raw_content_num_bytes;
            for parent in &parents {
                size_hint += &self.templates[parent].raw_content_num_bytes;
            }

            tpl_parents.insert(name.clone(), parents);
            tpl_size_hint.insert(name.clone(), size_hint);
        }

        // 2nd loop: we find the macro definition for each macro calls in each template
        // {tpl_name: vec![compiled macro]|
        let mut tpl_macro_definitions = HashMap::with_capacity(self.templates.len());
        let mut tpl_blocks = HashMap::with_capacity(self.templates.len());
        for (name, tpl) in &self.templates {
            let mut definitions = Vec::new();
            for macro_call in &tpl.macro_calls {
                // Safe unwrap, we will have filled the filename before getting there
                let tpl_name = macro_call.filename.as_ref().unwrap();
                let tpl_w_definition = self.templates.get(tpl_name).ok_or_else(|| {
                    Error::message(format!(
                        "Template `{name}` loads macros from `{tpl_name}` which isn't present in Tera"
                    ))
                })?;
                let macro_name = &macro_call.name;

                let definition = tpl_w_definition
                    .macro_definitions
                    .get(macro_name)
                    .ok_or_else(|| {
                        Error::message(format!(
                            "Template `{name}` is using macros `{macro_name}` from `{tpl_name}` which wasn't found",
                        ))
                    })?;
                macro_call
                    .validate_args_names(name, &definition.kwargs.keys().collect::<Vec<_>>())?;
                definitions.push(definition.clone());
            }
            tpl_macro_definitions.insert(name.to_string(), definitions);

            // TODO: can we avoid cloning the chunk?
            // TODO: Can we just point to things while avoiding hashmap lookups
            let mut blocks = HashMap::with_capacity(tpl.blocks.len());
            for (block_name, chunk) in &tpl.blocks {
                let mut all_blocks = vec![chunk.clone()];
                if chunk.is_calling_function("super") {
                    for parent_tpl_name in tpl_parents[name].iter().rev() {
                        let parent_tpl = self.get_template(parent_tpl_name)?;
                        if let Some(parent_chunk) = parent_tpl.blocks.get(block_name) {
                            all_blocks.push(parent_chunk.clone());
                            if !parent_chunk.is_calling_function("super") {
                                break;
                            }
                        }
                    }
                }
                blocks.insert(block_name.clone(), all_blocks);
            }
            tpl_blocks.insert(name.clone(), blocks);
        }

        // 3rd loop: we actually set everything we've done on the templates object
        for (name, tpl) in self.templates.iter_mut() {
            tpl.raw_content_num_bytes += tpl_size_hint.remove(name.as_str()).unwrap();
            tpl.parents = tpl_parents.remove(name.as_str()).unwrap();
            tpl.macro_calls_def = tpl_macro_definitions.remove(name.as_str()).unwrap();
            tpl.block_lineage = tpl_blocks.remove(name.as_str()).unwrap();
        }

        Ok(())
    }

    /// Add a single template to the Tera instance.
    ///
    /// This will error if the inheritance chain can't be built, such as adding a child
    /// template without the parent one.
    ///
    /// # Bulk loading
    ///
    /// If you want to add several templates, use
    /// [`add_raw_templates()`](Tera::add_raw_templates).
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// # use tera::Tera;
    /// let mut tera = Tera::default();
    /// tera.add_raw_template("new.html", "Blabla").unwrap();
    /// ```
    pub fn add_raw_template(&mut self, name: &str, content: &str) -> TeraResult<()> {
        let template = Template::new(name, content, None)
            // TODO: need to format the error message with source context
            .map_err(|e| Error::chain(format!("Failed to parse '{}'", name), e))?;

        self.templates.insert(name.to_string(), template);
        self.finalize_templates()?;
        Ok(())
    }

    /// Add all the templates given to the Tera instance
    ///
    /// This will error if the inheritance chain can't be built, such as adding a child
    /// template without the parent one.
    ///
    /// ```no_compile
    /// tera.add_raw_templates(vec![
    ///     ("new.html", "blabla"),
    ///     ("new2.html", "hello"),
    /// ]);
    /// ```
    pub fn add_raw_templates<I, N, C>(&mut self, templates: I) -> TeraResult<()>
    where
        I: IntoIterator<Item = (N, C)>,
        N: AsRef<str>,
        C: AsRef<str>,
    {
        for (name, content) in templates {
            let template = Template::new(name.as_ref(), content.as_ref(), None)
                // TODO: need to format the error message with source context
                .map_err(|e| Error::chain(format!("Failed to parse '{}'", name.as_ref()), e))?;

            self.templates.insert(name.as_ref().to_string(), template);
        }

        self.finalize_templates()?;

        Ok(())
    }

    #[inline]
    pub(crate) fn get_template(&self, template_name: &str) -> TeraResult<&Template> {
        match self.templates.get(template_name) {
            Some(tpl) => Ok(tpl),
            None => Err(Error::template_not_found(template_name)),
        }
    }

    /// Renders a Tera template given a [`Context`].
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// # use tera::{Tera, Context};
    /// // Create new tera instance with sample template
    /// let mut tera = Tera::default();
    /// tera.add_raw_template("info", "My age is {{ age }}.");
    ///
    /// // Create new context
    /// let mut context = Context::new();
    /// context.insert("age", &18);
    ///
    /// // Render template using the context
    /// let output = tera.render("info", &context).unwrap();
    /// assert_eq!(output, "My age is 18.");
    /// ```
    ///
    /// To render a template with an empty context, simply pass an empty [`Context`] object.
    ///
    /// ```
    /// # use tera::{Tera, Context};
    /// // Create new tera instance with demo template
    /// let mut tera = Tera::default();
    /// tera.add_raw_template("hello.html", "<h1>Hello</h1>");
    ///
    /// // Render a template with an empty context
    /// let output = tera.render("hello.html", &Context::new()).unwrap();
    /// assert_eq!(output, "<h1>Hello</h1>");
    /// ```
    pub fn render(&self, template_name: &str, context: &Context) -> TeraResult<String> {
        let template = self.get_template(template_name)?;
        let mut vm = VirtualMachine::new(self, template);
        // POC implementation of a global context
        // TODO: Optimize this with removing .clones()
        let mut ctx = self.global_context.clone();
        ctx.extend(context.clone());
        vm.render(&ctx)
    }

    /// Returns the global context, allowing modifications to it
    ///
    /// The global context is automatically included into every template,
    /// which is useful for sharing common data
    ///
    /// ```
    /// let mut tera = Tera::new();
    /// tera.global_context().insert("name", "John Doe");
    ///
    /// let content = tera
    ///     .render_str("Hello, {{ name }}!", &Context::new())
    ///     .unwrap();
    /// assert_eq!(content, "Hello, John Doe!".to_string());
    ///
    /// let content2 = tera
    ///     .render_str(
    ///         "UserID: {{ id }}, Username: {{ name }}",
    ///         &context! { id => &7489 },
    ///     )
    ///     .unwrap();
    /// assert_eq!(content2, "UserID: 7489, Username: John Doe");
    ///
    pub fn global_context(&mut self) -> &mut Context {
        &mut self.global_context
    }

    /// Renders a one off template (for example a template coming from a user
    /// input) given a `Context` and an instance of Tera. This allows you to
    /// render templates using custom filters or functions.
    ///
    /// Any errors will mention the `__tera_one_off` template: this is the name
    /// given to the template by Tera.
    ///
    /// ```no_compile
    /// let mut context = Context::new();
    /// context.insert("greeting", &"Hello");
    /// let string = tera.render_str("{{ greeting }} World!", &context)?;
    /// assert_eq!(string, "Hello World!");
    /// ```
    pub fn render_str(&mut self, input: &str, context: &Context) -> TeraResult<String> {
        self.add_raw_template(ONE_OFF_TEMPLATE_NAME, input)?;
        let result = self.render(ONE_OFF_TEMPLATE_NAME, context);
        self.templates.remove(ONE_OFF_TEMPLATE_NAME);
        result
    }

    /// Renders a one off template (for example a template coming from a user input) given a `Context`
    ///
    /// This creates a separate instance of Tera with no possibilities of adding custom filters
    /// or testers, parses the template and render it immediately.
    /// Any errors will mention the `__tera_one_off` template: this is the name given to the template by
    /// Tera
    ///
    /// ```no_compile
    /// let mut context = Context::new();
    /// context.insert("greeting", &"hello");
    /// Tera::one_off("{{ greeting }} world", &context, true);
    /// ```
    pub fn one_off(input: &str, context: &Context, autoescape: bool) -> TeraResult<String> {
        let mut tera = Tera::default();

        if autoescape {
            tera.autoescape_on(vec![ONE_OFF_TEMPLATE_NAME]);
        }

        tera.render_str(input, context)
    }
}

impl Default for Tera {
    fn default() -> Self {
        let mut tera = Self {
            templates: HashMap::new(),
            autoescape_suffixes: vec![".html", ".htm", ".xml"],
            escape_fn: escape_html,
            global_context: Context::new(),
            filters: HashMap::new(),
        };
        tera.register_builtin_filters();
        tera
    }
}

#[cfg(test)]
mod tests {
    use crate::context;

    use super::*;

    #[test]
    fn global_context() {
        let mut tera = Tera::new();
        tera.global_context().insert("name", "John Doe");

        let content = tera
            .render_str("Hello, {{ name }}!", &Context::new())
            .unwrap();
        assert_eq!(content, "Hello, John Doe!".to_string());

        let content2 = tera
            .render_str(
                "UserID: {{ id }}, Username: {{ name }}",
                &context! { id => &7489 },
            )
            .unwrap();
        assert_eq!(content2, "UserID: 7489, Username: John Doe");
    }
}
