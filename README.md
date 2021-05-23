DONE:
1. count newlines (logos extra: https://github.com/maciejhirsz/logos/blob/master/tests/tests/simple.rs) in lexer, make sure we still get the newlines


DOING:
2. Start writing parser (expression only for now) using Tera AST if it makes sense: https://github.com/Keats/tera/blob/master/src/parser/ast.rs 
3. Add pratt parser: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

TODO:

4. Add error handling with nice error reporting with https://github.com/brendanzab/codespan and LOTS OF tests
5. Validation + Constant folding in parser for expression (https://github.com/Keats/tera/blob/master/src/parser/tests/parser.rs#L1074), 
   can also constant fold at expression level (1==1)
6. Finish parser
7. Fuzz
8. Profit


Old Tera parser https://github.com/Keats/tera/blob/2e7fb71fc4834c8dd557a70f1a512bb37001a74e/src/parser.rs (2016)


Question:

- do we care about filters on bool values?

Order of operators:

https://github.com/pallets/jinja/issues/379
