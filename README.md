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
- [x] Fuzz like there's no tomorrow
- [x] Polish lexer + parser until it's perfect
- [x] Fuzz like there's no tomorrow
- [x] Generate bytecode
- [x] Decide where to calculate byte size hint, probably compiler
- [x] Fix blocks/macros AST -> bytecode generation
- [x] Fix macro call weird instruction (idx, idx).
- [x] Have a reasonable Template object (eg macro imports, extend)
- [x] Do the equivalent of `build_inheritance_chains` from v1. Maybe create a new VerifiedTemplate or whatever?
  - [x] Investigate merging chunks for inheritance so we don't need to look at other templates during rendering
- [x] Add support for block assignment in lexer/parser/compiler
- [x] Handle map literals (only allow string as keys for now?)
- [x] More fuzzing
- [x] Add a Context object that holds values like the one in Tera
- [x] Add an escape_html fn with benchmarks and a faster one featured gated
- [ ] Add a Tera object that holds all the templates
- [ ] Implement basic VM without filters/functions/tests
  - [ ] Fix bytecode generation because it's likely wrong
  - [ ] Ensure that runtime errors point to the right thing
  - [ ] Finish VM without filters/functions
  - [ ] Fuzz rendering
- [ ] Design filters/functions/tests types
- [ ] Implement basic builtin filters/functions/tests
- [ ] Add filters/functions/tests to VM
- [ ] Allow escape chars (eg \n) in strings concat, there was an issue about that in Zola
- [ ] Nice reporting with Ariadne (make it optional)
- [ ] Feature to load templates from a glob with optional dep
- [ ] Add a way to add global variables to the Tera struct that are passed on every render
- [ ] Shitload of tests
- [ ] More fuzzing


Ideas:

- Collect all functions/filters/macro call/includes/tests and make sure it's all available at compile time
- Pre-compute hashes for all keys used in the templates somehow? runtime phf?
- Pre-render macro calls without arguments?