typedef enum {
  Function,
  ParseReturn,
} ParseNodeType;

typedef struct {
  ParseNodeType type;
  Token token;
  char *begin;
  uint32_t len;
} ParseNodeExpression;

typedef struct {
  ParseNodeType type;
  Token token;
  char *begin;
  uint32_t len;
  union {
    ParseNodeExpression *return_expression;
  };
} ParseNode;

typedef struct {
  BucketList *list;
} Parser;
