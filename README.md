# Teaching C Interpreter
The goals of this compiler are:

1. Provide better error messages for new programmers
2. Output warnings when the user is doing something they shouldn't be.
3. Make debugging C programs easier

## Rationale
Learning C is hard. Students have to navigate pointers, segmentation faults,
and bit manipulation, all while working without garbage collection, usually
for the first time. This interpreter aims to address this problem by giving
better error messages and more runtime diagnostic tools.

## Features
- Fully replayable memory (WIP)
- Includes
- Stack traces on segmentation fault
- Segmentation fault on use-after-free and buffer overflow

## Restrictions and Incompatibilities
- Implicit types on functions aren't supported
- Implicit function declarations aren't supported
- No support for multithreading

# Credit
- TCI uses [`codespan-reporting`](https://github.com/brendanzab/codespan) to make
  nice error messages.
- TCI uses a parser plagiarized from [`lang-c`](https://github.com/vickenty/lang-c)
  to handle parsing the C language


