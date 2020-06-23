typedef enum {
  ASTFunction,
} ASTNodeOuterDeclKind;

typedef enum {
  ASTReturn,
} ASTNodeStmtKind;

typedef enum {
  ASTIntLiteral,
} ASTNodeExprKind;

typedef enum {
  ASTInt,
} ASTNodeTypeKind;

typedef struct {
  ASTNodeExprKind kind;
  uint32_t len;
  char *begin;
  union {
    uint32_t int_value;
  };
} ASTNodeExpr;

typedef struct {
  ASTNodeTypeKind kind;
  uint32_t len;
  char *begin;
} ASTNodeType;

typedef struct {
  ASTNodeStmtKind kind;
  uint32_t len;
  char *begin;
  union {
    ASTNodeExpr *return_expr;
  };
} ASTNodeStmt;

typedef struct {
  ASTNodeOuterDeclKind kind;
} ASTNodeOuterDecl;

typedef struct {
  BucketList *list;
  Lexer lex;
  Token current;
} Parser;

Parser parser_new(BucketList *list, char *data) {
  Parser parser;
  parser.list = list;
  parser.lex = lexer_new(data);
  parser.current = lexer_next(&parser.lex);
  return parser;
}

Token parser_pop(Parser *parser) {
  Token tok = parser->current;
  parser->current = lexer_next(&parser->lex);
  return tok;
}

Token parser_peek(Parser *parser) { return parser->current; }

// PRINTING THE PARSE TREE

String ast_node_outer_decl_str(StringDynArray *, ASTNodeOuterDecl *);
String ast_node_type_str(StringDynArray *, ASTNodeType *);
String ast_node_stmt_str(StringDynArray *, ASTNodeStmt *);
String ast_node_expr_str(StringDynArray *, ASTNodeExpr *);

String ast_node_outer_decl_str(StringDynArray *arr, ASTNodeOuterDecl *node) {
  String s;
  return s;
}
String ast_node_type_str(StringDynArray *arr, ASTNodeType *node) {
  switch (node->kind) {
  case ASTInt: {
    uint64_t begin = char_array_add_string(arr, string_new("int"));
    return string_from_parts(&arr->begin[begin], arr->end - begin);
  }
  }
}
String ast_node_stmt_str(StringDynArray *arr, ASTNodeStmt *node) {
  switch (node->kind) {
  case ASTReturn: {
    uint64_t begin = char_array_add_string(arr, string_new("Return("));
    ast_node_expr_str(arr, node->return_expr);
    char_array_add_string(arr, string_new(")"));
    return string_from_parts(&arr->begin[begin], arr->end - begin);
  }
  }
}
String ast_node_expr_str(StringDynArray *arr, ASTNodeExpr *node) {
  switch (node->kind) {
  case ASTIntLiteral: {
    uint32_t length = snprintf(NULL, 0, "%d", node->int_value);
    snprintf(CHAR_ARRAY, length, "%d", node->int_value);
    uint64_t begin =
        char_array_add_string(arr, string_from_parts(CHAR_ARRAY, length));
    return string_from_parts(&arr->begin[begin], arr->end - begin);
  }
  }
  String s;
  return s;
}

// PARSING TOKENS INTO A TREE

ASTNodeOuterDecl *parser_parse_outer_decl(Parser *);
ASTNodeType *parser_parse_type(Parser *);
ASTNodeStmt *parser_parse_stmt(Parser *);
ASTNodeExpr *parser_parse_atom(Parser *);

ASTNodeOuterDecl *parser_parse_outer_decl(Parser *parser) {
  ASTNodeType *type = parser_parse_type(parser);

  return NULL;
}

ASTNodeType *parser_parse_type(Parser *parser) {
  Token tok = parser_peek(parser);
  if (tok.kind != TokInt) {
    return NULL;
  }

  ASTNodeType *type = bump_alloc(parser->list, sizeof(ASTNodeType));
  type->kind = ASTInt;
  type->begin = tok.begin;
  type->len = tok.len;
  return type;
}

ASTNodeStmt *parser_parse_stmt(Parser *parser) {
  Token tok = parser_peek(parser);
  if (tok.kind != TokReturn) {
    return NULL;
  }

  parser_pop(parser);
  ASTNodeStmt *stmt = bump_alloc(parser->list, sizeof(ASTNodeStmt));
  stmt->kind = ASTReturn;
  stmt->begin = tok.begin;
  if ((stmt->return_expr = parser_parse_atom(parser)) == NULL) {
    return NULL;
  }

  stmt->len = stmt->return_expr->len + (stmt->return_expr->begin - stmt->begin);

  return stmt;
}

ASTNodeExpr *parser_parse_atom(Parser *parser) {
  Token tok = lexer_next(&parser->lex);
  if (tok.kind != TokInt) {
    return NULL;
  }

  ASTNodeExpr *expr = bump_alloc(parser->list, sizeof(ASTNodeExpr));
  expr->kind = ASTIntLiteral;
  expr->int_value = tok.int_value;
  expr->len = tok.len;
  expr->begin = tok.begin;
  return expr;
}
