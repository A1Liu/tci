# Teaching C Interpreter
The goals of this compiler are:

1. Provide better error messages for new programmers
2. Make debugging C programs easier

## Rationale
Learning C is hard. Students have to navigate pointers, segmentation faults,
and bit manipulation, all while working without garbage collection, usually
for the first time. This interpreter aims to address this problem by giving
better error messages and more runtime diagnostic tools.

## Features
- Better debug information
  - Stack traces on segmentation fault
- Harder errors: the following all cause a segmentation fault
  - use-after-free
  - buffer overflow
  - double free
  - out of memory
  - stack overflow
  - calling string functions with a string that isn't null-terminated

## Todo
Lots of stuff left to do still.

#### Compiler
- Const
- Enums
- Designated initializers
- full support for `#if`
- Better macro debug messages
- Better parsing error messages
- Hardening against invalid input
- Unified error system

#### Runtime Environment
- Leak detection: manual memory management is hard sometimes
- Processes and threads: students often learn multithreading paradigms in C
- Program arguments && Standard input - good functionality to have
- `unistd.h`: because sometimes professors be like that
- `time.h`: because sometimes you gotta do that
- Variable/type information at runtime: allows student to use TCI as a debugger
- Full kernel replayability: go backwards in time when using TCI as debugger

#### Frontend
- Working file system explorer
- Runtime file explorer
- Terminal emulator (without shell)
- Service worker for caching stuff locally
- Github Issue/Bug-fix-request button
- User interface for runtime/compiler options

## Restrictions and Incompatibilities
- Implicit types on functions will never be supported
- Implicit function declarations will never be supported

# Credit
- TCI uses an error renderer plagiarised from
  [`codespan-reporting`](https://github.com/brendanzab/codespan) to make error messages.
- TCI uses a parser plagiarized from [`lang-c`](https://github.com/vickenty/lang-c)
  to handle parsing the C language


