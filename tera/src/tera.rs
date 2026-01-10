use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

use crate::args::ArgFromValue;
use crate::errors::{Error, ReportError, TeraResult};
use crate::filters::{Filter, StoredFilter};
use crate::functions::{Function, StoredFunction};
use crate::template::{Template, find_parents};
use crate::tests::{StoredTest, Test, TestResult};
use crate::value::FunctionResult;
use crate::value::Value;
use crate::vm::interpreter::VirtualMachine;
use crate::vm::state::State;
use crate::{Context, HashMap, escape_html};

use crate::delimiters::Delimiters;
#[cfg(feature = "glob_fs")]
use crate::globbing::load_from_glob;
use crate::parsing::Chunk;
use crate::parsing::ast::ComponentDefinition;

/// Default template name used for `Tera::render_str` and `Tera::one_off`.
const ONE_OFF_TEMPLATE_NAME: &str = "__tera_one_off";

/// The escape function type definition
pub type EscapeFn = fn(&[u8], &mut dyn Write) -> std::io::Result<()>;

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
    /// Custom delimiters for template syntax
    delimiters: Delimiters,
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
            self.load_from_glob(glob)?;
            self.finalize_templates()
        } else {
            Err(Error::message(
                "Reloading is only available if you are using a glob",
            ))
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

    /// Set custom delimiters for template syntax.
    ///
    /// This must be called before adding any templates.
    /// Returns an error if any delimiter is empty, if start delimiters conflict or if there are
    /// already templates added to the Tera instance.
    ///
    /// # Example
    /// ```
    /// use tera::{Tera, Delimiters};
    ///
    /// let mut tera = Tera::new();
    /// tera.set_delimiters(Delimiters {
    ///     block_start: "<%",
    ///     block_end: "%>",
    ///     variable_start: "<<",
    ///     variable_end: ">>",
    ///     comment_start: "<#",
    ///     comment_end: "#>",
    /// }).unwrap();
    /// tera.add_raw_template("example", "<< name >>").unwrap();
    /// ```
    pub fn set_delimiters(&mut self, delimiters: Delimiters) -> TeraResult<()> {
        if !self.templates.is_empty() {
            return Err(Error::message(
                "Delimiters cannot be modified if templates have already been added",
            ));
        }
        delimiters.validate()?;
        self.delimiters = delimiters;
        Ok(())
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
        self.register_filter("wordcount", crate::filters::wordcount);
        self.register_filter("escape_html", crate::filters::escape);
        self.register_filter("newlines_to_br", crate::filters::newlines_to_br);
        self.register_filter("pluralize", crate::filters::pluralize);
        self.register_filter("trim", crate::filters::trim);
        self.register_filter("trim_start", crate::filters::trim_start);
        self.register_filter("trim_end", crate::filters::trim_end);
        self.register_filter("replace", crate::filters::replace);
        self.register_filter("capitalize", crate::filters::capitalize);
        self.register_filter("title", crate::filters::title);
        self.register_filter("truncate", crate::filters::truncate);
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
        self.register_filter("sort", crate::filters::sort);
        self.register_filter("unique", crate::filters::unique);
        self.register_filter("get", crate::filters::get);
        self.register_filter("map", crate::filters::map);
        self.register_filter("values", crate::filters::values);
        self.register_filter("keys", crate::filters::keys);
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
        self.register_test("iterable", crate::tests::is_iterable);
        self.register_test("defined", crate::tests::is_defined);
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
        let mut tpl_parents: HashMap<String, Vec<String>> =
            HashMap::with_capacity(self.templates.len());
        let mut tpl_size_hint: HashMap<String, usize> =
            HashMap::with_capacity(self.templates.len());
        // Track which template defined each component: component_name -> (tpl_name, from_extend)
        // We only store references here; actual data is collected at the end
        let mut component_sources: HashMap<&str, (&str, bool)> = HashMap::new();

        // 1st loop: find parents of each template and check for duplicate components
        for (name, tpl) in &self.templates {
            let parents = find_parents(&self.templates, tpl, tpl, vec![])?;
            for component_name in tpl.components.keys() {
                // Components in templates added directly by the user override any existing
                // components extended
                match component_sources.get(component_name.as_str()) {
                    // User overrides extended component
                    Some(&(_, true)) if !tpl.from_extend => {
                        component_sources.insert(component_name, (&tpl.name, false));
                    }
                    Some(&(_, false)) if tpl.from_extend => {
                        // Keep existing user component, ignore extended one
                    }
                    Some(&(existing_tpl, _)) => {
                        // Both from same origin - true duplicate
                        let mut names = [existing_tpl, tpl.name.as_str()];
                        names.sort_unstable();
                        return Err(Error::message(format!(
                            "Component `{component_name}` is defined in both `{}` and `{}`",
                            names[0], names[1]
                        )));
                    }
                    None => {
                        component_sources.insert(component_name, (&tpl.name, tpl.from_extend));
                    }
                }
            }
            let mut size_hint = tpl.raw_content_num_bytes;
            for parent in &parents {
                size_hint += self.templates[parent].raw_content_num_bytes;
            }

            tpl_parents.insert(name.clone(), parents);
            tpl_size_hint.insert(name.clone(), size_hint);
        }

        // 2nd loop: we check whether all called components/filters/tests/functions are defined
        // as well as finding each block lineage
        let mut tpl_blocks: HashMap<String, HashMap<String, Vec<Chunk>>> =
            HashMap::with_capacity(self.templates.len());
        // Collect errors with their location for stable sorting
        let mut errors: Vec<(&str, usize, String)> = Vec::new();

        for (name, tpl) in &self.templates {
            for (component, spans) in &tpl.component_calls {
                if !component_sources.contains_key(component.as_str()) {
                    for span in spans {
                        let err = ReportError::new(
                            format!("Unknown component `{component}`"),
                            &tpl.name,
                            &tpl.source,
                            span,
                        );
                        errors.push((&tpl.name, span.range.start, err.generate_report()));
                    }
                }
            }
            for (filter, spans) in &tpl.filter_calls {
                if !self.filters.contains_key(filter.as_str()) {
                    for span in spans {
                        let err = ReportError::new(
                            format!("Unknown filter `{filter}`"),
                            &tpl.name,
                            &tpl.source,
                            span,
                        );
                        errors.push((&tpl.name, span.range.start, err.generate_report()));
                    }
                }
            }
            for (test, spans) in &tpl.test_calls {
                if !self.tests.contains_key(test.as_str()) {
                    for span in spans {
                        let err = ReportError::new(
                            format!("Unknown test `{test}`"),
                            &tpl.name,
                            &tpl.source,
                            span,
                        );
                        errors.push((&tpl.name, span.range.start, err.generate_report()));
                    }
                }
            }
            for (func, spans) in &tpl.function_calls {
                if func != "super" && !self.functions.contains_key(func.as_str()) {
                    for span in spans {
                        let err = ReportError::new(
                            format!("Unknown function `{func}`"),
                            &tpl.name,
                            &tpl.source,
                            span,
                        );
                        errors.push((&tpl.name, span.range.start, err.generate_report()));
                    }
                }
            }
            for (include_name, spans) in &tpl.include_calls {
                if !self.templates.contains_key(include_name) {
                    for span in spans {
                        let err = ReportError::new(
                            format!("Unknown template `{include_name}`"),
                            &tpl.name,
                            &tpl.source,
                            span,
                        );
                        errors.push((&tpl.name, span.range.start, err.generate_report()));
                    }
                }
            }

            // Check that blocks in child templates exist in at least one parent
            let parents = &tpl_parents[name];
            if !parents.is_empty() {
                for (block_name, span) in &tpl.block_name_spans {
                    let exists_in_parent = parents.iter().any(|parent_name| {
                        self.templates
                            .get(parent_name)
                            .map(|p| p.blocks.contains_key(block_name))
                            .unwrap_or(false)
                    });
                    if !exists_in_parent {
                        let err = ReportError::new(
                            format!("Block `{block_name}` is not defined in any parent template"),
                            &tpl.name,
                            &tpl.source,
                            span,
                        );
                        errors.push((&tpl.name, span.range.start, err.generate_report()));
                    }
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

        // Add inherited blocks from parents that aren't overridden in child templates
        for (name, parents) in &tpl_parents {
            for parent_name in parents.iter().rev() {
                if let Some(parent_blocks) = tpl_blocks.get(parent_name).cloned() {
                    let child_blocks = tpl_blocks.get_mut(name).unwrap();
                    for (block_name, lineage) in parent_blocks {
                        child_blocks.entry(block_name).or_insert(lineage);
                    }
                }
            }
        }

        if !errors.is_empty() {
            // Sort by template name, then by position in source
            errors.sort_by(|a, b| a.0.cmp(b.0).then(a.1.cmp(&b.1)));
            let reports: Vec<String> = errors.into_iter().map(|(_, _, report)| report).collect();
            return Err(Error::message(reports.join("\n\n")));
        }

        // Collect components from the winning templates (based on component_sources)
        let mut components = HashMap::with_capacity(component_sources.len());
        for (component_name, (tpl_name, _)) in component_sources {
            let tpl = &self.templates[tpl_name];
            let data = tpl.components[component_name].clone();
            components.insert(component_name.to_string(), data);
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
        let template = Template::new(name, content, None, self.delimiters)?;
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
            let template = Template::new(name.as_ref(), content.as_ref(), None, self.delimiters)?;
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

        let template = Template::new(
            tpl_name,
            &content,
            Some(path.to_str().unwrap().to_string()),
            self.delimiters,
        )?;

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
        vm.render(context, &self.global_context)
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
        vm.render_to(context, &self.global_context, write)
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

    /// Renders a component by name with the given context and optional body content.
    ///
    /// The context should contain the component's arguments as key-value pairs.
    /// Autoescape is determined by the source template's filename (e.g., components defined
    /// in `.html` files will have autoescape enabled) with the default Tera settings.
    ///
    /// # Examples
    ///
    /// ```no_compile
    /// let tera = Tera::new("templates/**/*")?;
    ///
    /// // Render a component with arguments
    /// let html = tera.render_component(
    ///     "Button",
    ///     &context! { label => "Click me" },
    ///     None,
    /// )?;
    ///
    /// // Render a component with body content
    /// let html = tera.render_component(
    ///     "Card",
    ///     &context! { title => "My Card" },
    ///     Some("<p>Card content here</p>"),
    /// )?;
    /// ```
    pub fn render_component(
        &self,
        component_name: &str,
        context: &Context,
        body: Option<&str>,
    ) -> TeraResult<String> {
        let mut output = Vec::new();
        self.render_component_to(component_name, context, body, &mut output)?;
        Ok(String::from_utf8(output)?)
    }

    /// Renders a component by name to something that implements [`Write`].
    ///
    /// Same as [`render_component`](Self::render_component) but writes to a [`Write`] implementor
    /// instead of returning a String.
    ///
    /// # Examples
    ///
    /// ```no_compile
    /// let tera = Tera::new("templates/**/*")?;
    /// let mut buffer = Vec::new();
    /// tera.render_component_to(
    ///     "Button",
    ///     &context! { label => "Click me" },
    ///     None,
    ///     &mut buffer,
    /// )?;
    /// ```
    pub fn render_component_to(
        &self,
        component_name: &str,
        context: &Context,
        body: Option<&str>,
        mut write: impl Write,
    ) -> TeraResult<()> {
        let (component_def, chunk) = self
            .components
            .get(component_name)
            .ok_or_else(|| Error::component_not_found(component_name))?;

        // Get the source template, we'll need it for the VM
        let template = self
            .templates
            .get(&chunk.name)
            .expect("Component source template must exist");

        // Build the component context by validating and applying defaults
        let body_value = body.map(Value::safe_string);
        let component_context = component_def
            .build_context(
                context.data.keys().map(|k| k.as_ref()),
                |key| context.get(key).cloned(),
                body_value,
            )
            .map_err(Error::message)?;

        let vm = VirtualMachine::new(self, template);
        let mut state = State::new_with_chunk(&component_context, chunk);
        state.filters = Some(&self.filters);
        vm.interpret(&mut state, &mut write)?;

        Ok(())
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
            delimiters: Delimiters::default(),
        };
        tera.register_builtin_filters();
        tera.register_builtin_tests();
        tera.register_builtin_functions();
        tera
    }
}

#[cfg(test)]
mod tests {
    use crate::{Kwargs, State, context};

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

    #[test]
    fn can_override_component_template_from_content() {
        // Framework defines a component
        let mut framework_tera = Tera::default();
        framework_tera
            .add_raw_template(
                "components.html",
                "{% component hello() %}Framework{% endcomponent hello %}",
            )
            .unwrap();

        // User defines the same component with different content
        let mut my_tera = Tera::default();
        my_tera
            .add_raw_templates(vec![
                (
                    "my_components.html",
                    "{% component hello() %}User{% endcomponent hello %}",
                ),
                ("index.html", "{{<hello/>}}"),
            ])
            .unwrap();

        // Extend with framework - user's component should win
        my_tera.extend(&framework_tera).unwrap();

        let result = my_tera.render("index.html", &Context::default()).unwrap();
        assert_eq!(result, "User");
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

    #[test]
    fn test_render_component() {
        let mut tera = Tera::default();
        tera.add_raw_template(
            "components.html",
            r#"{% component Button(label, variant="primary") %}<button class="{{ variant }}">{{ label }}</button>{% endcomponent Button %}
{% component Card(title) %}<div><h1>{{ title }}</h1>{{ body }}</div>{% endcomponent Card %}
{% component Display(content) %}{{ content }}{% endcomponent Display %}"#,
        )
        .unwrap();
        tera.add_raw_template(
            "components.txt",
            "{% component Raw(content) %}{{ content }}{% endcomponent Raw %}",
        )
        .unwrap();

        // Basic + defaults
        insta::assert_snapshot!(
            tera.render_component("Button", &context! { label => "Click" }, None).unwrap(),
            @"<button class=\"primary\">Click</button>"
        );
        // Override default
        insta::assert_snapshot!(
            tera.render_component("Button", &context! { label => "X", variant => "secondary" }, None).unwrap(),
            @"<button class=\"secondary\">X</button>"
        );
        // With body
        insta::assert_snapshot!(
            tera.render_component("Card", &context! { title => "T" }, Some("<p>body</p>")).unwrap(),
            @"<div><h1>T</h1><p>body</p></div>"
        );
        // Autoescape (.html)
        insta::assert_snapshot!(
            tera.render_component("Display", &context! { content => "<script>" }, None).unwrap(),
            @"&lt;script&gt;"
        );
        // No autoescape (.txt)
        insta::assert_snapshot!(
            tera.render_component("Raw", &context! { content => "<script>" }, None).unwrap(),
            @"<script>"
        );
        // render_component_to variant
        let mut buffer = Vec::new();
        tera.render_component_to("Button", &context! { label => "Y" }, None, &mut buffer)
            .unwrap();
        insta::assert_snapshot!(String::from_utf8(buffer).unwrap(), @"<button class=\"primary\">Y</button>");

        // Errors
        assert!(
            tera.render_component("Nope", &Context::new(), None)
                .is_err()
        );
        assert!(
            tera.render_component("Button", &Context::new(), None)
                .is_err()
        );
        assert!(
            tera.render_component("Button", &context! { label => "x", bad => "y" }, None)
                .is_err()
        );
    }

    #[test]
    fn custom_delimiters() {
        let mut tera = Tera::new();
        tera.set_delimiters(Delimiters {
            block_start: "<%",
            block_end: "%>",
            variable_start: "<<",
            variable_end: ">>",
            comment_start: "<#",
            comment_end: "#>",
        })
        .unwrap();

        tera.add_raw_template(
            "test",
            "Hello, <# This is a comment #><% if show %><< name >>!<% endif %>",
        )
        .unwrap();
        let result = tera
            .render("test", &context! { name => "World", show => &true })
            .unwrap();
        insta::assert_snapshot!(result, @"Hello, World!");
    }
}
