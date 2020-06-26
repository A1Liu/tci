typedef enum {
  ASTFunction,
} ASTNodeDeclKind;

typedef enum {
  ASTReturn,
} ASTNodeStmtKind;

typedef enum {
  ASTIntLiteral,
  ASTIdent,
} ASTNodeExprKind;

typedef enum {
  ASTInt,
} ASTNodeTypeKind;

struct astNodeExpr;
struct astNodeType;
struct astNodeStmt;
struct astNodeDecl;

typedef struct astNodeExpr {
  ASTNodeExprKind kind;
  uint32_t len;
  char *begin;
  union {
    uint32_t int_value;
    uint32_t ident_symbol;
  };
} ASTNodeExpr;

typedef struct astNodeType {
  ASTNodeTypeKind kind;
  uint32_t len;
  char *begin;
} ASTNodeType;

typedef struct astNodeStmt {
  ASTNodeStmtKind kind;
  uint32_t len;
  char *begin;
  union {
    ASTNodeExpr *return_expr;
    struct astNodeDecl *decl;
  };
} ASTNodeStmt;

typedef struct {
  ASTNodeStmt *begin;
  uint32_t end;
  uint32_t capacity;
} ASTNodeStmtDynArray;

typedef struct astNodeDecl {
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
    uint32_t length = snprintf(NULL, 0, "%u", node->int_value);
    snprintf(CHAR_ARRAY, length + 1, "%u", node->int_value);
    uint64_t begin =
        char_array_add_string(arr, string_from_parts(CHAR_ARRAY, length));
    return string_from_parts(&arr->begin[begin], arr->end - begin);
  }
  case ASTIdent: {
    uint64_t begin = char_array_add_string(arr, string_new("<symbol "));
    uint32_t length = snprintf(NULL, 0, "%u", node->ident_symbol);
    snprintf(CHAR_ARRAY, length + 1, "%u", node->ident_symbol);
    char_array_add_string(arr, string_from_parts(CHAR_ARRAY, length));
    char_array_add_string(arr, string_new(">"));
    return string_from_parts(&arr->begin[begin], arr->end - begin);
  }
  }
}

// BASIC PARSER FUNCTIONALITY

typedef struct {
  BucketList *list;
  Lexer lex;
  Token *begin;
  uint32_t end;
  uint32_t capacity;
} Parser;

Parser parser_new(BucketList *list, char *data) {
  Parser parser;
  parser.list = list;
  parser.lex = lexer_new(data);
  parser.begin = NULL;
  parser.end = 0;
  parser.capacity = 0;
  return parser;
}

Token parser_pop(Parser *parser) {
  if (parser->end) {
    Token tok = parser->begin[--parser->end];
    return tok;
  }

  return lexer_next(&parser->lex);
}

void parser_push(Parser *parser, Token tok) {
  if (parser->begin == NULL || parser->end == parser->capacity) {
    parser->capacity = parser->capacity / 2 + parser->capacity + 16;
    parser->begin = realloc(parser->begin, parser->capacity * sizeof(Token));
  }

  parser->begin[parser->end++] = tok;
}

// PARSING TOKENS INTO A TREE

bool parser_parse(Parser *, ASTNodeProgram *);
bool parser_parse_decl(Parser *, ASTNodeDecl *);
bool parser_parse_type(Parser *, ASTNodeType *);
bool parser_parse_stmt(Parser *, ASTNodeStmt *);
bool parser_parse_atom(Parser *, ASTNodeExpr *);

bool parser_parse(Parser *parser, ASTNodeProgram *prog) {
  Token tok = parser_pop(parser);
  while (tok.kind != TokEnd && tok.kind != TokInvalid) {
    parser_push(parser, tok);
    if (parser_parse_decl(parser, program_allocate_element(prog))) {
      return true;
    }
    tok = parser_pop(parser);
  }

  return false;
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

  for (tok = parser_pop(parser); tok.kind != TokRightBrace &&
                                 tok.kind != TokInvalid && tok.kind != TokEnd;
       tok = parser_pop(parser)) {
    parser_push(parser, tok);
    if (parser_parse_stmt(parser,
                          stmt_array_allocate_element(&decl->function.stmts))) {
      return true;
    }
  }

  if (tok.kind != TokRightBrace) {
    parser_push(parser, tok);
    return true;
  }

  return false;
}

bool parser_parse_type(Parser *parser, ASTNodeType *type) {
  Token tok = parser_pop(parser);
  if (tok.kind != TokInt) {
    return true;
  }

  type->kind = ASTInt;
  type->begin = tok.str.str;
  type->len = tok.str.len;
  return false;
}

bool parser_parse_stmt(Parser *parser, ASTNodeStmt *stmt) {
  Token tok = parser_pop(parser);
  if (tok.kind != TokReturn) {
    parser_push(parser, tok);
    return true;
  }

  stmt->kind = ASTReturn;
  stmt->begin = tok.str.str;
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
  Token tok = parser_pop(parser);
  switch (tok.kind) {
  case TokInt: {
    expr->kind = ASTIntLiteral;
    expr->int_value = tok.int_value;
    expr->len = tok.str.len;
    expr->begin = tok.str.str;
    return false;
  } break;
  case TokIdent: {
    expr->kind = ASTIdent;
    expr->ident_symbol = tok.ident_symbol;
    expr->len = tok.str.len;
    expr->begin = tok.str.str;
    return false;
  } break;
  default:
    return true;
  }
}
