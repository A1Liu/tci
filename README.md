# Teaching C Interpreter
The goals of this compiler are:
1. Provide better error messages for new programmers
2. Generate code that checks for segmentation faults at runtime and provides debug
   information
3. Output warnings when the user is doing something they shouldn't be.

## Restrictions and Incompatibilities

- Implicit types on functions aren't supported
- Implicit function declarations aren't supported
- Higher order functions and function pointers aren't supported
- No support for multithreading
- No support for macros
- No support for multi-declarations in global context
- No support for user-defined varargs
- Type declarations are not allowed inside functions
- Type declarations are not allowed inside other type declarations
- Struct literals and array literals are allowed inside places where expressions
  are expected, and are type inferred.
- No support for Goto
- Typedefs must begin with an uppercase letter
- Variables must begin with a lowercase letter

## Credit


