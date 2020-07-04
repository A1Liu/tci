# Code Organization
The source files in this project are as follows:

- `debug_allocator.h`/`debug_allocator.c` - Replacement global allocator.
- `util.h` - utility functions.
- `lexer.h` - C lexer
- `parser_1.h` - Parses tokens into a stage 1 AST.
- `main.c` - Program runs from here.

## 2 Stage Parsing
To simplify C parsing, we parse in 2 stages: global and local. In the first stage,
we parse declarations normally, but parse function definitions as a declaration
plus an array of tokens. Then, after resolving namespacing issues, we parse the
body of each function using namespace information, so that we can evaluate things
like:

```c
Typename *a;
Typename *b;
a * b;
```

without namespace information, we wouldn't know whether to parse any of the above
statements as a declaration or an expression.

