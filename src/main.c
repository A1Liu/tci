#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// clang-format off
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
  ASTNodeProgram prog = parser_parse(&parser);

  CharDynArray arr = char_array_new();
  String str = ast_node_program_str(&arr, &prog);

  printf("%.*s\n", (uint32_t)str.len, str.str);
}
