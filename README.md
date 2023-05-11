# TCI - C Compiler for Students
The goals of this compiler are:

1. Provide better error messages for new programmers
2. Make debugging C programs easier

## Features
- [ ] Understandable error messages, written in plain english
- [ ] Stack traces when the program crashes
- [ ] Doing wrong stuff will crash instead of doing weird stuff to memory:
  - [ ] use-after-free
  - [ ] buffer overflow
  - [ ] double free
  - [ ] calling string functions with a string that isn't null-terminated
- [ ] Leak Detection
- [ ] Replayable execution

## Restrictions and Incompatibilities
- Implicit types on functions will never be supported (e.g. K&R style)
- Implicit function declarations will never be supported

## Development
Here's some commands to use:

```shell
cargo wasm # build wasm project
cargo run-test # run a test file
yarn link-wasm # link compiler-web into the web project; only needs to be done once
yarn wasm # build wasm project and bundle into web project
yarn dev # run web project
```

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
- [ ] `stdio.h` - not just printf, real files too
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
- Monaco Editor Quick Fixes - https://stackoverflow.com/questions/57994101/show-quick-fix-for-an-error-in-monaco-editor
- Fuzzer to look into - https://github.com/rust-fuzz/afl.rs
- WASM interpreter to look into - https://github.com/paritytech/wasmi

## Credits
- [Aaron Hsu's PhD Thesis on Data-Parallel Compiler Architecture](https://scholarworks.iu.edu/dspace/handle/2022/24749) -
  TCI's architecture is almost fully based on work done in 2019 by Aaron Hsu.
- [`lang-c` by `vickenty`](https://github.com/vickenty/lang-c) -
  TCI used the source of `lang-c` as a reference and as inspiration when designing the AST.
  `lang-c` source code was also very useful as a point of reference for the C specification,
  because specifications are difficult to read.
- [Precendence Climbing Explanation](https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing) -
  An explanation for precedence climbing with pseudo-code in Python that I use
  because I always forget the details.
- GNU GCC's [Documentation](https://gcc.gnu.org/onlinedocs/cpp/index.html)
  and [Internals Documentation](https://gcc.gnu.org/onlinedocs/cppinternals/index.html) - 
  Documentation that acted as a reference when building the lexer/macro code
