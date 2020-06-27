typedef enum { ASTReturn, ASTFunction, ASTVarDecl } ASTNodeStmtKind;
typedef enum { ASTIntLiteral, ASTIdent } ASTNodeExprKind;
typedef enum { ASTInt } ASTNodeTypeKind;

struct astNodeExpr;
struct astNodeType;
struct astNodeStmt;

typedef struct {
  struct astNodeStmt *begin;
  uint32_t end;
  uint32_t capacity;
} ASTNodeStmtDynArray;

typedef struct astNodeExpr {
  ASTNodeExprKind kind;
  String str;
  union {
    uint32_t int_value;
    uint32_t ident_symbol;
  };
} ASTNodeExpr;

typedef struct astNodeType {
  ASTNodeTypeKind kind;
  String str;
} ASTNodeType;

typedef struct {
  ASTNodeType *return_type;
  uint32_t ident;
  ASTNodeStmtDynArray stmts;
} ASTNodeFunction;

typedef struct astNodeStmt {
  ASTNodeStmtKind kind;
  String str;
  union {
    ASTNodeExpr *return_expr;
    ASTNodeFunction func;
  };
} ASTNodeStmt;

ASTNodeStmtDynArray stmt_array_new(void) {
  ASTNodeStmtDynArray arr = {NULL, 0, 0};
  return arr;
}

uint32_t stmt_array_add(ASTNodeStmtDynArray *arr, ASTNodeStmt stmt) {
  if (arr->begin == NULL) {
    arr->begin = malloc(32 * sizeof(r));
    arr->capacity = 32;
  }

  if (arr->capacity == arr->end) {
    arr->capacity = arr->capacity / 2 + arr->capacity;
    arr->begin = realloc(arr->begin, arr->capacity * sizeof(stmt));
  }

  uint32_t begin = arr->end;
  arr->begin[arr->end++] = stmt;
  return begin;
}

// PRINTING THE PARSE TREE

String ast_node_program_str(CharDynArray *, ASTNodeProgram *);
String ast_node_decl_str(CharDynArray *, ASTNodeDecl *);
String ast_node_type_str(CharDynArray *, ASTNodeType *);
String ast_node_stmt_str(CharDynArray *, ASTNodeStmt *);
String ast_node_expr_str(CharDynArray *, ASTNodeExpr *);

String ast_node_program_str(CharDynArray *arr, ASTNodeProgram *node) {
  uint64_t begin = char_array_add_string(arr, string_new("["));
  for (uint32_t i = 0; i < node->end; i++) {
    ast_node_decl_str(arr, &node->begin[i]);
    char_array_add_string(arr, string_new(","));
  }

  char_array_add_string(arr, string_new("]"));
  return string_from_parts(&arr->begin[begin], arr->end - begin);
}

String ast_node_decl_str(CharDynArray *arr, ASTNodeDecl *node) {
  switch (node->kind) {
  case ASTFunction: {
    uint64_t begin = char_array_add_string(arr, string_new("Function(ret="));
    ast_node_type_str(arr, node->function.return_type);

    char_array_add_string(arr, string_new(",name="));
    uint32_t sym_length = snprintf(NULL, 0, "%d", node->function.name);
    snprintf(CHAR_ARRAY, sym_length + 1, "%d", node->function.name);
    char_array_add_string(arr, string_from_parts(CHAR_ARRAY, sym_length));

    char_array_add_string(arr, string_new(",stmts=["));
    for (uint32_t i = 0; i < node->function.stmts.end; i++) {
      ast_node_stmt_str(arr, &node->function.stmts.begin[i]);
      char_array_add_string(arr, string_new(","));
    }

    char_array_add_string(arr, string_new("])"));
    return string_from_parts(&arr->begin[begin], arr->end - begin);
  }
  case ASTVarDecl: {
    uint64_t begin = char_array_add_string(arr, string_new("Declare(type="));
    ast_node_type_str(arr, node->declaration.variable_type);

    char_array_add_string(arr, string_new(",name="));
    char_array_add_string(arr, t_itoa(node->function.name));
    char_array_add_string(arr, string_new(")"));

    return string_from_parts(&arr->begin[begin], arr->end - begin);
  } break;
  }
}

String ast_node_type_str(CharDynArray *arr, ASTNodeType *node) {
  switch (node->kind) {
  case ASTInt: {
    uint64_t begin = char_array_add_string(arr, string_new("int"));
    return string_from_parts(&arr->begin[begin], arr->end - begin);
  }
  }
}

String ast_node_stmt_str(CharDynArray *arr, ASTNodeStmt *node) {
  switch (node->kind) {
  case ASTReturn: {
    uint64_t begin = char_array_add_string(arr, string_new("Return("));
    ast_node_expr_str(arr, node->return_expr);
    char_array_add_string(arr, string_new(")"));
    return string_from_parts(&arr->begin[begin], arr->end - begin);
  }
  }
}

String ast_node_expr_str(CharDynArray *arr, ASTNodeExpr *node) {
  switch (node->kind) {
  case ASTIntLiteral: {
    uint64_t begin = char_array_add_string(arr, t_itoa(node->int_value));
    return string_from_parts(&arr->begin[begin], arr->end - begin);
  }
  case ASTIdent: {
    uint64_t begin = char_array_add_string(arr, string_new("<symbol "));
    char_array_add_string(arr, t_itoa(node->ident_symbol));
    char_array_add_string(arr, string_new(">"));
    return string_from_parts(&arr->begin[begin], arr->end - begin);
  }
  }
}

// Symbol Table

typedef enum { SymType, SymName } SymEntryKind;
