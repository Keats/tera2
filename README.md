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
- [ ] Fix macro call weird instruction (idx, idx). Pass values on stack?
- [ ] Have a reasonable Template object (eg macro imports, extend)
- [ ] Add a Context object that holds values like the one in Tera
- [ ] Implement basic VM without filters/functions/tests
  - [ ] Fix bytecode generation because it's likely wrong
  - [ ] Ensure that runtime errors point to the right thing
  - [ ] Finish VM without filters/functions
- [ ] Add filters/functions/tests to VM
- [ ] Do the equivalent of `build_inheritance_chains` from v1. Maybe create a new VerifiedTemplate or whatever?
  - [ ] Investigate merging chunks for inheritance so we don't need to look at other templates during rendering
- [ ] Allow escape chars (eg \n) in strings concat, there was an issue about that in Zola
- [ ] Handle map literals (only allow string as keys for now?)
- [ ] Nice reporting with Ariadne (make it optional)
- [ ] Feature to load templates from a glob with optional dep
- [ ] Shitload of tests
- [ ] More fuzzing


Ideas:

- Collect all functions/filters/macro call/includes/tests and make sure it's all available at compile time
- Pre-compute hashes for all keys used in the templates somehow? runtime phf?
- Pre-render macro calls without arguments?
- Have a global map of filename -> hashmap<name, compiled_macro_def> rather than being on a template? Issues for 
reporting though since you need to load the right source code