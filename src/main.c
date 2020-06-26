#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define debug(args...) (printf("%s:%u: ", __FILE__, __LINE__), printf(args))

// clang-format off
#include "debug_allocator.c"
#include "util.c"
#include "lexer.c"
#include "parser.c"
// clang-format on

int main(int argc, char **argv) {
  if (argc <= 1)
    return 0;

  char *file_contents = read_file(argv[1]);
  printf("---\n%s\n---\n", file_contents);

  BucketList *list = bump_new();
  Parser parser = parser_new(list, file_contents);
  ASTNodeProgram prog = program_new();
  bool failed = parser_parse(&parser, &prog);
  if (failed) {
    printf("FAILED\n");
    return 1;
  }

  CharDynArray arr = char_array_new();
  String str = ast_node_program_str(&arr, &prog);

  printf("%.*s\n", (uint32_t)str.len, str.str);
  return 0;
}
