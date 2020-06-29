#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define debug(args...) (printf("%s:%u: ", __FILE__, __LINE__), printf(args))

// clang-format off
#include "dyn_array_ptr.h"
#include "debug_allocator.h"
#include "util.h"
#include "ast.h"
#include "lexer.h"
#include "parser.h"
// clang-format on

int main(int argc, char **argv) {
  if (argc <= 1)
    return 0;

  char *file_contents = read_file(argv[1]);
  printf("---\n%s\n---\n", file_contents);

  BumpList *list = bump_new();
  Parser parser = parser_new(list, file_contents);

  return 0;
}
