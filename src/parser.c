typedef enum {
  ASTFunction,
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
  Token token;
  char *begin;
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

typedef enum {
  Function,
} ASTNodeOuterDeclKind;

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

ASTNodeType *parser_parse_type(Parser *);
ASTNodeStmt *parser_parse_stmt(Parser *);
ASTNodeExpr *parser_parse_atom(Parser *);

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
  expr->token = tok;
  expr->len = tok.len;
  expr->begin = tok.begin;
  return expr;
}
