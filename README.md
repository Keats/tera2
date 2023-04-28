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
- [ ] Generate bytecode
- [ ] Implement VM
- [ ] Allow escape chars (eg \n) in strings concat, there was an issue about that in Zola
- [ ] Handle map literals (only allow string as keys for now?)
- [ ] Nice reporting with Ariadne (make it optional)


Ideas:

- Collect all functions/filters/macro call/includes/tests and make sure it's all available at compile time
- Pre-compute hashes for all keys used in the templates somehow? runtime phf?
