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
} Parser;
