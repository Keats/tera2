TODO:

- [x] Wire up new error struct in lexer2
- [x] Add insta tests for lexing errors
- [x] Start writing parser, copying most of parser.rs probably
  - [x] raw
  - [x] block
  - [x] for loop
  - [x] if/elif/else
  - [x] filter section
  - [x] macro definition
  - [x] macro imports
- [x] Delete logos lexer/tests/parser
- [x] Adds lots of tests, some with insta some without 
  - [x] Make sure the spans are always correct
  - [x] Handle all custom errors from previous parser
  - [x] Handle extends not being the first tag as error
  - [x] Handle nodes not allowed in certain places (eg macro def inside a macro def)
- [x] Fuzz _a lot_
- [x] Polish lexer + parser until it's perfect
- [x] More fuzzing
- [x] Generate bytecode
- [x] Decide where to calculate byte size hint, probably compiler
- [x] Fix blocks/macros AST -> bytecode generation
- [x] Fix macro call weird instruction (idx, idx).
- [x] Have a reasonable Template object (eg macro imports, extend)
- [x] Add support for block assignment in lexer/parser/compiler
- [x] Handle map literals (only allow string as keys for now?)
- [x] More fuzzing
- [x] Add a Context object that holds values like the one in Tera
- [x] Add an escape_html fn with benchmarks and a faster one featured gated
- [x] Add a Tera object that holds all the templates
- [x] Do the equivalent of `build_inheritance_chains` from v1. Maybe create a new VerifiedTemplate or whatever?
- [ ] Implement basic VM without filters/functions/tests
  - [ ] Port all the corresponding tests from Tera v1 + some more
  - [x] Ensure variables used in {{ }} are defined (vs if/tests where undefined are just falsy?)
  - [x] Fix bytecode generation when it's wrong
  - [x] Allow anything that can be converted to a key as a key for inline maps (eg add bool/integers)
  - [x] Make sure strings are escaped automatically (Value::String could be an enum with safe/unsafe string like markupsafe in python?) when printing
  - [ ] Fix/remove all the TODOs
  - [ ] Improve perf till it's better than Tera v1
  - [x] Iterating on strings should require an optional `unic_segment` feature for unicode
  - [ ] Ensure that errors point to the right thing (<--- that's the long one, but done after perf work)
  - [ ] Finish VM
- [ ] Design filters/functions/tests types
- [ ] Implement basic builtin filters/functions/tests
- [ ] Add filters/functions/tests to VM
- [ ] Uncomment all tests using filters/functions/tests
- [x] Parsing errors should report with the source context like Rust errors with the right spans
- [ ] Add more helpful errors when loading templates (eg a forloop with for key, key in value/missing includes etc)
- [ ] Allow escape chars (eg \n) in strings concat, there was an issue about that in Zola
- [ ] Feature to load templates from a glob with optional dep
- [ ] MAYBE: add a way to add global context to the Tera struct that are passed on every render automatically
- [ ] Shitload of tests
- [ ] More fuzzing


Ideas:

- Collect all functions/filters/macro call/includes/tests and make sure it's all available at compile time
- Pre-compute hashes for all keys used in the templates somehow? runtime phf? https://crates.io/crates/ph ?
- string interning for Key/Value?
- Pre-render macro calls without arguments?
- Check whether blocks have super() and if they don't just skip them when rendering and they are in the middle of the ancestry
- Have a way to merge chunks when handling inheritance, issue is macros since you need to refer to the right imports
- Collect include templates so we know whether we have all of them or not and error otherwise
- Make raw template keep spans rather String to avoid cloning it?
- Use Arc<str> in Value
- Mark literal strings in templates as safe?



Thoughts on making LoadName error, we can change the semantics from v1 a bit eg:
- `{{ hey }}` should error if hey is undefined
- `{{ existing.hey }}` should error if hey is undefined but existing is
- `{{ hey or 1 }}` should print 1
- `{% if hey or true %}` should be truthy
- `{% if hey.other or true %}` should error if `hey` is not defined (currently truthy)
- `{{ hey.other or 1 }}` should error if `hey` is not defined (currently prints "true")


## Breaking changes

- `{{ falsy or "hello" }}` prints "hello" instead of "true"
- `{% if not_existing.field %}` errors if `not_existing` is undefined, we only allow one level of undefinedness (hello undefined is not an object)
- include ignore missing has been removed