#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// clang-format off
#include "util.c"
#include "lexer.c"
// clang-format on

int main(int argc, char **argv) {
  if (argc <= 1)
    return 0;

  char *file_contents = read_file(argv[1]);
  printf("%s\n", file_contents);
}
