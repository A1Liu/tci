typedef enum { ASTReturn, ASTFunction, ASTDecl } ASTNodeStmtKind;
typedef enum { ASTIntLiteral, ASTIdent } ASTNodeExprKind;
typedef enum { ASTInt } ASTNodeTypeKind;

struct astNodeExpr;
struct astNodeType;
struct astNodeStmt;

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
  struct astNodeStmt *stmts;
} ASTNodeFunction;

typedef struct {
  ASTNodeType *type;
  uint32_t ident;
  ASTNodeExpr expr;
} ASTNodeDecl;

typedef struct astNodeStmt {
  ASTNodeStmtKind kind;
  String str;
  union {
    ASTNodeExpr *return_expr;
    ASTNodeFunction func;
    ASTNodeDecl decl;
  };
} ASTNodeStmt;

// PRINTING THE PARSE TREE

String ast_node_type_str(char **, ASTNodeType *);
String ast_node_stmt_str(char **, ASTNodeStmt *);
String ast_node_expr_str(char **, ASTNodeExpr *);

String ast_node_type_str(char **arr, ASTNodeType *node) {
  switch (node->kind) {
  case ASTInt: {
    uint64_t begin = char_array_add_string(arr, string_new("int"));
    return string_from_parts(&(*arr)[begin], dyn_array_len(*arr) - begin);
  }
  }
}

String ast_node_stmt_str(char **arr, ASTNodeStmt *node) {
  switch (node->kind) {
  case ASTReturn: {
    uint64_t begin = char_array_add_string(arr, string_new("Return("));
    ast_node_expr_str(arr, node->return_expr);
    char_array_add_string(arr, string_new(")"));
    return string_from_parts(&(*arr)[begin], dyn_array_len(*arr) - begin);
  }
  case ASTFunction: {
    uint64_t begin = char_array_add_string(arr, string_new("Function(ret="));
    ast_node_type_str(arr, node->func.return_type);

    char_array_add_string(arr, string_new(",name="));
    uint32_t sym_length = snprintf(NULL, 0, "%d", node->func.ident);
    snprintf(CHAR_ARRAY, sym_length + 1, "%d", node->func.ident);
    char_array_add_string(arr, string_from_parts(CHAR_ARRAY, sym_length));

    char_array_add_string(arr, string_new(",stmts=["));
    for (uint32_t i = 0; i < dyn_array_len(node->func.stmts); i++) {
      ast_node_stmt_str(arr, &node->func.stmts[i]);
      char_array_add_string(arr, string_new(","));
    }

    char_array_add_string(arr, string_new("])"));
    return string_from_parts(&(*arr)[begin], dyn_array_len(*arr) - begin);
  }
  case ASTDecl: {
    uint64_t begin = char_array_add_string(arr, string_new("Declare(type="));
    ast_node_type_str(arr, node->decl.type);

    char_array_add_string(arr, string_new(",name="));
    char_array_add_string(arr, t_itoa(node->func.ident));
    char_array_add_string(arr, string_new(")"));

    return string_from_parts(&(*arr)[begin], dyn_array_len(*arr) - begin);
  } break;
  }
}

String ast_node_expr_str(char **arr, ASTNodeExpr *node) {
  switch (node->kind) {
  case ASTIntLiteral: {
    uint64_t begin = char_array_add_string(arr, t_itoa(node->int_value));
    return string_from_parts(&(*arr)[begin], dyn_array_len(*arr) - begin);
  }
  case ASTIdent: {
    uint64_t begin = char_array_add_string(arr, string_new("<symbol "));
    char_array_add_string(arr, t_itoa(node->ident_symbol));
    char_array_add_string(arr, string_new(">"));
    return string_from_parts(&(*arr)[begin], dyn_array_len(*arr) - begin);
  }
  }
}

// Symbol Table

typedef enum { SymType, SymName } SymEntryKind;
