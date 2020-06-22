typedef enum {
  ASTFunction,
  ASTReturn,
} ASTNodeStmtType;

typedef enum {
  ASTIntLiteral,
} ASTNodeExprType;

typedef enum {
  ASTInt,
} ASTNodeTypeType;

typedef struct {
  ASTNodeExprType type;
  uint32_t len;
  Token token;
  char *begin;
} ASTNodeExpr;

typedef struct {
  ASTNodeTypeType type;
  uint32_t len;
  char *begin;
} ASTNodeType;

typedef struct {
  ASTNodeStmtType type;
  uint32_t len;
  char *begin;
  union {
    ASTNodeExpr *return_expr;
  };
} ASTNodeStmt;

typedef struct {
  BucketList *list;
  Lexer lex;
  Token current;
} Parser;

ASTNodeStmtExpr *parser_parse_stmt(Parser *);
ASTNodeExpr *parser_parse_atom(Parser *);

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

ASTNodeStmt *parser_parse_stmt(Parser *parser) {
  Token tok = parser_peek(parser);
  if (tok.type != Return) {
    return NULL;
  }

  parser_pop(parser);
  ASTNodeStmt *stmt = bump_alloc(parser->list, sizeof(ASTNodeStmt));
  stmt->type = ASTReturn;
  stmt->begin = tok.begin;
  if ((stmt->return_expr = parser_parse_atom(parser)) == NULL) {
    return NULL;
  }

  stmt->len = stmt->return_expr->len + (stmt->return_expr->begin - stmt->begin);

  return stmt;
}

ASTNodeExpr *parser_parse_atom(Parser *parser) {
  Token tok = lexer_next(&parser->lex);
  if (tok.type != Int) {
    return NULL;
  }

  ASTNodeExpr *expr = bump_alloc(parser->list, sizeof(ASTNodeExpr));
  expr->type = ASTIntLiteral;
  expr->token = tok;
  expr->len = tok.len;
  expr->begin = tok.begin;
  return expr;
}
