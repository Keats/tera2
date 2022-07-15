TODO:

- [x] Wire up new error struct in lexer2
- [x] Add insta tests for lexing errors
- [ ] Start writing parser, copying most of parser.rs probably
  - [x] raw
  - [x] block
  - [x] for loop
  - [x] if/elif/else
  - [x] filter section
  - [ ] macro definition
  - [ ] macro imports
- [ ] Delete logos lexer/tests/parser
- [ ] Adds lots of tests, some with insta some without + including error reporting
  - [ ] Handle extends not being the first tag as error
  - [ ] Handle nodes not allowed in certain places (eg macro def inside a macro def)
  - [ ] Make sure the spans are always correct
- [ ] Polish lexer + parser until it's perfect (eg, allow `\n` in text to avoid some workarounds when building strings)
- [ ] Investigate minijinja bytecode approach vs AST (more performant probably but error reporting?)
- [ ] Handle map literals (only allow string as keys for now?)