use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

use crate::args::ArgFromValue;
use crate::errors::{Error, TeraResult};
use crate::filters::{Filter, StoredFilter};
use crate::functions::{Function, StoredFunction};
use crate::template::{find_parents, Template};
use crate::tests::{StoredTest, Test, TestResult};
use crate::value::FunctionResult;
use crate::vm::interpreter::VirtualMachine;
use crate::{escape_html, Context, HashMap};

#[cfg(feature = "glob_fs")]
use crate::globbing::load_from_glob;
use crate::parsing::ast::ComponentDefinition;
use crate::parsing::Chunk;

/// Default template name used for `Tera::render_str` and `Tera::one_off`.
const ONE_OFF_TEMPLATE_NAME: &str = "__tera_one_off";

/// The escape function type definition
pub type EscapeFn = fn(&[u8], &mut dyn std::io::Write) -> std::io::Result<()>;

#[derive(Clone)]
pub struct Tera {
    /// The glob used to load templates if there was one.
    /// Only used if the `glob_fs` feature is turned on
    #[doc(hidden)]
    #[allow(dead_code)]
    glob: Option<String>,
    #[doc(hidden)]
    pub templates: HashMap<String, Template>,
    /// Which extensions does Tera automatically autoescape on.
    /// Defaults to [".html", ".htm", ".xml"]
    #[doc(hidden)]
    autoescape_suffixes: Vec<&'static str>,
    #[doc(hidden)]
    pub(crate) escape_fn: EscapeFn,
    global_context: Context,
    pub(crate) filters: HashMap<&'static str, StoredFilter>,
    pub(crate) tests: HashMap<&'static str, StoredTest>,
    pub(crate) functions: HashMap<&'static str, StoredFunction>,
    pub(crate) components: HashMap<String, (ComponentDefinition, Chunk)>,
}

impl Tera {
    pub fn new() -> Self {
        Self::default()
    }

    #[cfg(feature = "glob_fs")]
    pub fn load_from_glob(&mut self, glob: &str) -> TeraResult<()> {
        self.glob = Some(glob.to_string());

        // We want to preserve templates that have been added through
        // `Tera::extend` so we only keep those
        self.templates.retain(|_, t| t.from_extend);

        let mut errors = Vec::new();
        for (path, name) in load_from_glob(glob)? {
            match self.add_file(&path, Some(&name)) {
                Ok(_) => (),
                Err(e) => errors.push(format!(
                    "Failed to load {}: {e:?} ({:?})",
                    path.display(),
                    e.source.as_ref().expect("to have a source")
                )),
            }
        }

        if !errors.is_empty() {
            Err(Error::message(errors.join("\n")))
        } else {
            self.finalize_templates()
        }
    }

    /// Re-parse all templates found in the glob given to Tera.
    ///
    /// Use this when you are watching a directory and want to reload everything,
    /// for example when a file is added.
    ///
    /// If you are adding templates without using a glob, we can't know when a template
    /// is deleted, which would result in an error if we are trying to reload that file.
    #[cfg(feature = "glob_fs")]
    pub fn full_reload(&mut self) -> TeraResult<()> {
        if let Some(glob) = self.glob.clone().as_ref() {
            self.load_from_glob(&glob)?;
            self.finalize_templates()
        } else {
            return Err(Error::message(
                "Reloading is only available if you are using a glob",
            ));
        }
    }

    fn set_templates_auto_escape(&mut self) {
        for (tpl_name, tpl) in self.templates.iter_mut() {
            tpl.autoescape_enabled = self
                .autoescape_suffixes
                .iter()
                .any(|s| tpl_name.ends_with(s));
        }
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
        self.set_templates_auto_escape();
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
    /// TODO: fix me
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

    /// Register a test with Tera.
    ///
    /// If a test with that name already exists, it will be overwritten
    ///
    /// ```no_compile
    /// tera.register_test("odd", |x: usize| x % 2 == 0);
    /// ```
    pub fn register_test<Func, Arg, Res>(&mut self, name: &'static str, test: Func)
    where
        Func: Test<Arg, Res> + for<'a> Test<<Arg as ArgFromValue<'a>>::Output, Res>,
        Arg: for<'a> ArgFromValue<'a>,
        Res: TestResult,
    {
        self.tests.insert(name, StoredTest::new(test));
    }

    /// Register a function with Tera.
    ///
    /// If a function with that name already exists, it will be overwritten
    pub fn register_function<Func, Res>(&mut self, name: &'static str, func: Func)
    where
        Func: Function<Res>,
        Res: FunctionResult,
    {
        self.functions.insert(name, StoredFunction::new(func));
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
        self.register_filter("str", crate::filters::as_str);
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
        self.register_filter("get", crate::filters::get);
        self.register_filter("map", crate::filters::map);
        self.register_filter("filter", crate::filters::filter);
        self.register_filter("group_by", crate::filters::group_by);
    }

    fn register_builtin_tests(&mut self) {
        self.register_test("string", crate::tests::is_string);
        self.register_test("number", crate::tests::is_number);
        self.register_test("map", crate::tests::is_map);
        self.register_test("bool", crate::tests::is_bool);
        self.register_test("array", crate::tests::is_array);
        self.register_test("integer", crate::tests::is_integer);
        self.register_test("float", crate::tests::is_float);
        self.register_test("null", crate::tests::is_null);
        self.register_test("undefined", crate::tests::is_undefined);
        self.register_test("odd", crate::tests::is_odd);
        self.register_test("even", crate::tests::is_even);
        self.register_test("divisible_by", crate::tests::is_divisible_by);
        self.register_test("starting_with", crate::tests::is_starting_with);
        self.register_test("ending_with", crate::tests::is_ending_with);
        self.register_test("containing", crate::tests::is_containing);
    }

    fn register_builtin_functions(&mut self) {
        self.register_function("range", crate::functions::range);
        self.register_function("throw", crate::functions::throw);
    }

    /// Optimizes the templates when possible and doing some light
    /// checks like whether blocks/macros/templates all exist when they are used
    fn finalize_templates(&mut self) -> TeraResult<()> {
        let mut tpl_parents = HashMap::new();
        let mut tpl_size_hint = HashMap::new();
        let mut components = HashMap::new();

        // 1st loop: we find the parents of each templates with all the block definitions
        // as well as copying the component definitions defined in each template
        for (name, tpl) in &self.templates {
            let parents = find_parents(&self.templates, tpl, tpl, vec![])?;
            for (component_name, c) in &tpl.components {
                if components.contains_key(component_name) {
                    todo!("Write a proper error message for duplicate components, with both filenames")
                }
                components.insert(component_name.clone(), c.clone());
            }
            let mut size_hint = tpl.raw_content_num_bytes;
            for parent in &parents {
                size_hint += &self.templates[parent].raw_content_num_bytes;
            }

            tpl_parents.insert(name.clone(), parents);
            tpl_size_hint.insert(name.clone(), size_hint);
        }

        // 2nd loop: we check whether we know all the components that are called are defined
        // as well as finding each block lineage
        let mut tpl_blocks = HashMap::with_capacity(self.templates.len());
        for (name, tpl) in &self.templates {
            for call in &tpl.component_calls {
                if !components.contains_key(call) {
                    return Err(Error::message(format!(
                        "Template `{name}` uses component `{call}` which isn't present in Tera"
                    )));
                }
            }

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

        // 3rd loop: we actually set everything we've done on the templates objects
        for (name, tpl) in self.templates.iter_mut() {
            tpl.raw_content_num_bytes += tpl_size_hint.remove(name.as_str()).unwrap();
            tpl.parents = tpl_parents.remove(name.as_str()).unwrap();
            tpl.block_lineage = tpl_blocks.remove(name.as_str()).unwrap();
        }

        self.components = components;
        self.set_templates_auto_escape();
        Ok(())
    }

    /// Add a single template to the Tera instance.
    ///
    /// This will error if there are errors in the inheritance, such as adding a child
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
    /// This will error if there are errors in the inheritance, such as adding a child
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

    /// Add a template from a path: reads the file and parses it.
    /// This will return an error if the template is invalid and doesn't check the validity of
    /// the new set of templates.
    fn add_file<P: AsRef<Path>>(&mut self, path: P, name: Option<&str>) -> TeraResult<()> {
        let path = path.as_ref();
        let tpl_name = name.unwrap_or_else(|| path.to_str().unwrap());

        let mut f = File::open(path)
            .map_err(|e| Error::chain(format!("Couldn't open template '{:?}'", path), e))?;

        let mut content = String::new();
        f.read_to_string(&mut content)
            .map_err(|e| Error::chain(format!("Failed to read template '{:?}'", path), e))?;

        let template = Template::new(tpl_name, &content, Some(path.to_str().unwrap().to_string()))
            // TODO: need to format the error message with source context
            .map_err(|e| Error::chain(format!("Failed to parse '{}'", tpl_name), e))?;

        self.templates.insert(tpl_name.to_string(), template);
        Ok(())
    }

    /// Add a single template from a path to the Tera instance. The default name for the template is
    /// the path given, but this can be renamed with the `name` parameter
    ///
    /// This will error if the inheritance chain can't be built, such as adding a child
    /// template without the parent one.
    /// If you want to add several file, use [Tera::add_template_files](struct.Tera.html#method.add_template_files)
    ///
    /// ```
    /// # use tera::Tera;
    /// let mut tera = Tera::default();
    /// // Rename template with custom name
    /// tera.add_template_file("examples/basic/templates/macros.html", Some("macros.html")).unwrap();
    /// // Use path as name
    /// tera.add_template_file("examples/basic/templates/base.html", None).unwrap();
    /// ```
    pub fn add_template_file<P: AsRef<Path>>(
        &mut self,
        path: P,
        name: Option<&str>,
    ) -> TeraResult<()> {
        self.add_file(path, name)?;
        self.finalize_templates()
    }

    /// Add several templates from paths to the Tera instance.
    ///
    /// The default name for the template is the path given, but this can be renamed with the
    /// second parameter of the tuple
    ///
    /// This will error if the inheritance chain can't be built, such as adding a child
    /// template without the parent one.
    ///
    /// ```no_run
    /// # use tera::Tera;
    /// let mut tera = Tera::default();
    /// tera.add_template_files(vec![
    ///     ("./path/to/template.tera", None), // this template will have the value of path1 as name
    ///     ("./path/to/other.tera", Some("hey")), // this template will have `hey` as name
    /// ]);
    /// ```
    pub fn add_template_files<I, P, N>(&mut self, files: I) -> TeraResult<()>
    where
        I: IntoIterator<Item = (P, Option<N>)>,
        P: AsRef<Path>,
        N: AsRef<str>,
    {
        for (path, name) in files {
            self.add_file(path, name.as_ref().map(AsRef::as_ref))?;
        }
        self.finalize_templates()
    }

    /// Extend this [`Tera`] instance with the templates, filters, testers and functions defined in
    /// another instance.
    ///
    /// Use that method when you want to add a given Tera instance templates/filters/testers/functions
    /// to your own. If a template/filter/tester/function with the same name already exists in your instance,
    /// it will not be overwritten.
    ///
    ///```no_compile
    /// // add all the templates from FRAMEWORK_TERA
    /// // except the ones that have an identical name to the ones in `my_tera`
    /// my_tera.extend(&FRAMEWORK_TERA);
    ///```
    pub fn extend(&mut self, other: &Tera) -> TeraResult<()> {
        for (name, template) in &other.templates {
            if !self.templates.contains_key(name) {
                let mut tpl = template.clone();
                tpl.from_extend = true;
                self.templates.insert(name.to_string(), tpl);
            }
        }

        for (name, filter) in &other.filters {
            if !self.filters.contains_key(name) {
                self.filters.insert(name, filter.clone());
            }
        }

        for (name, test) in &other.tests {
            if !self.tests.contains_key(name) {
                self.tests.insert(name, test.clone());
            }
        }

        for (name, function) in &other.functions {
            if !self.functions.contains_key(name) {
                self.functions.insert(name, function.clone());
            }
        }

        self.finalize_templates()
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

    /// Renders a Tera template given a [`Context`] to something that implements [`Write`].
    ///
    /// The only difference from [`render()`](Self::render) is that this version doesn't convert
    /// buffer to a String, allowing to render directly to anything that implements [`Write`]. For
    /// example, this could be used to write directly to a [`File`](std::fs::File).
    ///
    /// Any I/O error will be reported in the result.
    ///
    /// # Examples
    ///
    /// Rendering into a `Vec<u8>`:
    ///
    /// ```
    /// # use tera::{Context, Tera};
    /// let mut tera = Tera::default();
    /// tera.add_raw_template("index.html", "<p>{{ name }}</p>");
    ///
    /// // Rendering a template to an internal buffer
    /// let mut buffer = Vec::new();
    /// let mut context = Context::new();
    /// context.insert("name", "John Wick");
    /// tera.render_to("index.html", &context, &mut buffer).unwrap();
    /// assert_eq!(buffer, b"<p>John Wick</p>");
    /// ```
    pub fn render_to(
        &self,
        template_name: &str,
        context: &Context,
        write: impl Write,
    ) -> TeraResult<()> {
        let template = self.get_template(template_name)?;
        let mut vm = VirtualMachine::new(self, template);
        // POC implementation of a global context
        // TODO: Optimize this with removing .clones()
        let mut ctx = self.global_context.clone();
        ctx.extend(context.clone());
        vm.render_to(&ctx, write)
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
            glob: None,
            templates: HashMap::new(),
            autoescape_suffixes: vec![".html", ".htm", ".xml"],
            escape_fn: escape_html,
            global_context: Context::new(),
            filters: HashMap::new(),
            tests: HashMap::new(),
            functions: HashMap::new(),
            components: HashMap::new(),
        };
        tera.register_builtin_filters();
        tera.register_builtin_tests();
        tera.register_builtin_functions();
        tera
    }
}

#[cfg(test)]
mod tests {
    use crate::{context, Kwargs, State};

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

    #[test]
    fn extend_no_overlap() {
        let mut my_tera = Tera::default();
        my_tera
            .add_raw_templates(vec![
                ("one", "{% block hey %}1{% endblock hey %}"),
                ("two", "{% block hey %}2{% endblock hey %}"),
                ("three", "{% block hey %}3{% endblock hey %}"),
            ])
            .unwrap();

        let mut framework_tera = Tera::default();
        framework_tera
            .add_raw_templates(vec![("four", "Framework X")])
            .unwrap();
        framework_tera.register_filter("hello", |_: &str, _: Kwargs, _: &State| "hello");
        framework_tera.register_test("testing", |_: &str, _: Kwargs, _: &State| true);
        my_tera.extend(&framework_tera).unwrap();
        assert_eq!(my_tera.templates.len(), 4);
        let result = my_tera.render("four", &Context::default()).unwrap();
        assert_eq!(result, "Framework X");
        assert!(my_tera.filters.contains_key("hello"));
        assert!(my_tera.tests.contains_key("testing"));
    }

    #[test]
    fn extend_with_overlap() {
        let mut my_tera = Tera::default();
        my_tera
            .add_raw_templates(vec![
                ("one", "MINE"),
                ("two", "{% block hey %}2{% endblock hey %}"),
                ("three", "{% block hey %}3{% endblock hey %}"),
            ])
            .unwrap();

        let mut framework_tera = Tera::default();
        framework_tera
            .add_raw_templates(vec![("one", "FRAMEWORK"), ("four", "Framework X")])
            .unwrap();
        my_tera.extend(&framework_tera).unwrap();
        assert_eq!(my_tera.templates.len(), 4);
        let result = my_tera.render("one", &Context::default()).unwrap();
        assert_eq!(result, "MINE");
    }

    #[cfg(feature = "glob_fs")]
    #[test]
    fn can_full_reload() {
        let mut tera = Tera::default();
        tera.load_from_glob("examples/basic/templates/**/*")
            .unwrap();
        tera.full_reload().unwrap();

        assert!(tera.get_template("base.html").is_ok());
    }

    #[cfg(feature = "glob_fs")]
    #[test]
    fn full_reload_with_glob_after_extending() {
        let mut tera = Tera::default();
        tera.load_from_glob("examples/basic/templates/**/*")
            .unwrap();
        let mut framework_tera = Tera::default();
        framework_tera
            .add_raw_templates(vec![("one", "FRAMEWORK"), ("four", "Framework X")])
            .unwrap();
        tera.extend(&framework_tera).unwrap();
        tera.full_reload().unwrap();

        assert!(tera.get_template("base.html").is_ok());
        assert!(tera.get_template("one").is_ok());
    }
}
