typedef enum {
  ASTFunction,
} ASTNodeDeclKind;

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
  ASTNodeStmt *begin;
  uint32_t end;
  uint32_t capacity;
} ASTNodeStmtDynArray;

typedef struct {
  ASTNodeDeclKind kind;
  union {
    struct {
      ASTNodeType *return_type;
      uint32_t name;
      ASTNodeStmtDynArray stmts;
    } function;
  };
} ASTNodeDecl;

typedef struct {
  ASTNodeDecl *begin;
  uint32_t end;
  uint32_t capacity;
} ASTNodeProgram;

ASTNodeProgram program_new(void) {
  ASTNodeProgram p = {NULL, 0, 0};
  return p;
}

ASTNodeDecl *program_allocate_element(ASTNodeProgram *arr) {
  if (arr->begin == NULL) {
    arr->begin = malloc(16 * sizeof(*arr->begin));
    arr->capacity = 16;
  }

  if (arr->capacity == arr->end) {
    arr->capacity = arr->capacity / 2 + arr->capacity;
    arr->begin = realloc(arr->begin, arr->capacity * sizeof(*arr->begin));
  }

  return &arr->begin[arr->end++];
}

ASTNodeStmtDynArray stmt_array_new(void) {
  ASTNodeStmtDynArray arr = {NULL, 0, 0};
  return arr;
}

ASTNodeStmt *stmt_array_allocate_element(ASTNodeStmtDynArray *arr) {
  if (arr->begin == NULL) {
    arr->begin = malloc(16 * sizeof(*arr->begin));
    arr->capacity = 16;
  }

  if (arr->capacity == arr->end) {
    arr->capacity = arr->capacity / 2 + arr->capacity;
    arr->begin = realloc(arr->begin, arr->capacity * sizeof(*arr->begin));
  }

  return &arr->begin[arr->end++];
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
    uint32_t length = snprintf(NULL, 0, "%d", node->int_value);
    snprintf(CHAR_ARRAY, length + 1, "%d", node->int_value);
    uint64_t begin =
        char_array_add_string(arr, string_from_parts(CHAR_ARRAY, length));
    return string_from_parts(&arr->begin[begin], arr->end - begin);
  }
  }
  String s;
  return s;
}

// BASIC PARSER FUNCTIONALITY

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

// PARSING TOKENS INTO A TREE

ASTNodeProgram parser_parse(Parser *);
bool parser_parse_decl(Parser *, ASTNodeDecl *);
bool parser_parse_type(Parser *, ASTNodeType *);
bool parser_parse_stmt(Parser *, ASTNodeStmt *);
bool parser_parse_atom(Parser *, ASTNodeExpr *);

ASTNodeProgram parser_parse(Parser *parser) {
  ASTNodeProgram prog = program_new();

  while (parser_peek(parser).kind != TokEnd &&
         parser_peek(parser).kind != TokInvalid) {
    if (parser_parse_decl(parser, program_allocate_element(&prog))) {
      return prog;
    }
  }

  return prog;
}

bool parser_parse_decl(Parser *parser, ASTNodeDecl *decl) {
  ASTNodeType *type = bump_alloc(parser->list, sizeof(ASTNodeType));
  if (parser_parse_type(parser, type)) {
    return true;
  }
  decl->function.return_type = type;

  Token tok = parser_pop(parser);
  if (tok.kind != TokIdent) {
    return true;
  }

  decl->function.name = tok.ident_symbol;

  tok = parser_pop(parser);
  if (tok.kind != TokLeftParen) {
    return true;
  }

  tok = parser_pop(parser);
  if (tok.kind != TokRightParen) {
    return true;
  }

  tok = parser_pop(parser);
  if (tok.kind != TokLeftBrace) {
    return true;
  }

  for (tok = parser_peek(parser); tok.kind != TokRightBrace &&
                                  tok.kind != TokInvalid && tok.kind != TokEnd;
       tok = parser_peek(parser)) {
    if (parser_parse_stmt(parser,
                          stmt_array_allocate_element(&decl->function.stmts))) {
      return true;
    }
  }

  parser_pop(parser);

  return false;
}

bool parser_parse_type(Parser *parser, ASTNodeType *type) {
  Token tok = parser_pop(parser);
  if (tok.kind != TokInt) {
    return true;
  }

  type->kind = ASTInt;
  type->begin = tok.begin;
  type->len = tok.len;
  return false;
}

bool parser_parse_stmt(Parser *parser, ASTNodeStmt *stmt) {
  Token tok = parser_peek(parser);
  if (tok.kind != TokReturn) {
    return true;
  }

  parser_pop(parser);
  stmt->kind = ASTReturn;
  stmt->begin = tok.begin;
  stmt->return_expr = bump_alloc(parser->list, sizeof(ASTNodeExpr));
  if (parser_parse_atom(parser, stmt->return_expr)) {
    return true;
  }

  stmt->len = stmt->return_expr->len + (stmt->return_expr->begin - stmt->begin);

  if (parser_pop(parser).kind != TokSemicolon) {
    return true;
  }

  return false;
}

bool parser_parse_atom(Parser *parser, ASTNodeExpr *expr) {
  Token tok = lexer_next(&parser->lex);
  if (tok.kind != TokInt) {
    return true;
  }

  expr->kind = ASTIntLiteral;
  expr->int_value = tok.int_value;
  expr->len = tok.len;
  expr->begin = tok.begin;
  return false;
}
