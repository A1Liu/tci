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
  Lexer lex = lexer_new(file_contents);
  printf("---\n%s\n---\n", file_contents);

  Token tok = lexer_next(&lex);
  BucketList *list = bump_new();

  printf("%s\n", lexer_token_str(list, &tok));
}
