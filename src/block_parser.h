typedef struct {
  Token *begin;
  size_t end;
  size_t capacity;
} TokenDynArray;

typedef struct {
  TokenDynArray tokens;
} Block;

typedef struct {
  TokenDynArray tokens;
} BlockParser;
