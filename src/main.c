#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define debug(args...) (printf("%s:%u: ", __FILE__, __LINE__), printf(args))

// clang-format off
#include "debug_allocator.h"
#include "dyn_array_ptr.h"
#include "hashtable.h"
#include "util.h"
#include "errors.h"
#include "lexer.h"
#include "ast.h"
#include "parser_1.h"
#include "type_checking_1.h"
// clang-format on

int main(int argc, char **argv) {
  if (argc <= 1)
    return 0;

  char *file_contents = read_file(argv[1]);
  if (file_contents == NULL) {
    printf("FILE DOESN'T EXIST\n");
    exit(1);
  }

  printf("---\n%s\n---\n", file_contents);

  BumpList *list = bump_new();
  Parser parser = parser_new(list, file_contents);
  ASTNodeStmt *stmts = dyn_array_new(ASTNodeStmt);
  char *char_array = dyn_array_new(char);

  while (parser_peek(&parser).kind != TokEnd) {
    ASTNodeStmt stmt = parser_parse_global_decl(&parser);
    if (stmt.kind == ASTStmtError) {
      String err = error_str(&char_array, stmt.err);
      printf("%.*s\n", (uint32_t)err.len, err.str);
      exit(1);
    }
    dyn_array_add(&stmts, stmt);
  }

  uint64_t len = dyn_array_len(stmts);
  for (uint64_t i = 0; i < len; i++) {
    String out = ast_node_stmt_str(&char_array, &stmts[i]);
    printf("%.*s\n", (uint32_t)out.len, out.str);
  }

  return 0;
}
