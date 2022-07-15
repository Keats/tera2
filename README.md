TODO:

- [x] Wire up new error struct in lexer2
- [x] Add insta tests for lexing errors
- [ ] Start writing parser, copying most of parser.rs probably
  - [ ] raw
  - [ ] block
  - [ ] for loop
  - [ ] if/elif/else
  - [ ] macro definition
- [ ] Adds lots of tests, some with insta some without -> including error reporting
  - Handle extends not being the first tag as error
  - Make sure the spans are correct
- [ ] Delete logos lexer/tests/parser
- [ ] Polish lexer + parser until it's perfect (eg, allow `\n` in text to avoid some workarounds when building strings)
- [ ] Investigate minijinja bytecode approach vs AST (more performant probably but error reporting?)
- [ ] Handle map literals