# C Compiler
The goals of this compiler are:

1. Provide better error messages for new programmers
2. Make debugging C programs easier

## Features
- [ ] Understandable error messages, written in plain english
- [ ] Stack traces when the program crashes
- [ ] Errors are louder: the following all cause a segmentation fault
  - [ ] use-after-free
  - [ ] buffer overflow
  - [ ] double free
  - [ ] out of memory
  - [ ] stack overflow
  - [ ] calling string functions with a string that isn't null-terminated
- [ ] Leak Detection
- [ ] Replayable execution

## Restrictions and Incompatibilities
- Implicit types on functions will never be supported (e.g. K&R style)
- Implicit function declarations will never be supported

## Baseline Compliance
Lots still to do. In the compiler:

- [ ] Operations on registers
- [ ] Function calls
- [ ] Varargs
- [ ] Structs
- [ ] Unions
- [ ] Enums
- [ ] Macros

In the runtime:

- [ ] Program arguments && Standard input - good functionality to have
- [ ] `unistd.h`: because sometimes professors be like that
- [ ] `time.h`: because sometimes you gotta do that

In the UI:

- [ ] Working file system explorer, source and runtime files
- [ ] Terminal emulator (without shell)
- [ ] Service worker for caching stuff locally
- [ ] Github Issue/Bug-fix-request button
- [ ] User interface for runtime/compiler options
- [ ] Compiler errors/warnings pop up as messages in the editor

## Resources
- Preprocessor General Info - https://gcc.gnu.org/onlinedocs/cpp/index.html
- Macro Expansion Algo - https://gcc.gnu.org/onlinedocs/cppinternals/index.html
- Translation of C standard to AST types - https://github.com/vickenty/lang-c/blob/master/src/ast.rs
- Compiler Architecture - https://scholarworks.iu.edu/dspace/handle/2022/24749
