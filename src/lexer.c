typedef struct {
  uint32_t begin;
  uint32_t len;
} Range;

typedef struct {
  Range *begin;
  uint32_t end;
  uint32_t capacity;
} RangeDynArray;

typedef struct {
  char *str;
  CharDynArray symbol_values;
  RangeDynArray symbols;
} Lexer;

typedef enum {
  TokIdent,
  TokIntLiteral,
  TokLongLiteral,
  TokUIntLiteral,
  TokULongLiteral,
  TokFloatLiteral,
  TokDoubleLiteral,

  TokIf,
  TokElse,
  TokReturn,

  TokInt,

  TokLeftBrace,
  TokRightBrace,
  TokLeftParen,
  TokRightParen,
  TokSemicolon,

  TokInvalid,
  TokEnd,
} TokenKind;

typedef struct {
  TokenKind kind;
  char *begin;
  uint32_t len;
  union {
    uint32_t int_value;
    uint32_t ident_symbol;
  };
} Token;

Range range_new(uint32_t begin, uint32_t len) {
  Range r = {begin, len};
  return r;
}

RangeDynArray range_array_new() {
  RangeDynArray arr = {NULL, 0, 0};
  return arr;
}

uint64_t range_array_add(RangeDynArray *arr, Range r) {
  if (arr->begin == NULL) {
    arr->begin = malloc(32 * sizeof(r));
    arr->capacity = 32;
  }

  if (arr->capacity == arr->end) {
    arr->capacity = arr->capacity / 2 + arr->capacity;
    arr->begin = realloc(arr->begin, arr->capacity * sizeof(r));
  }

  uint64_t begin = arr->end;
  arr->begin[arr->end++] = r;
  return begin;
}

Lexer lexer_new(char *data) {
  Lexer lex;
  lex.str = data;
  lex.symbol_values = char_array_new();
  lex.symbols = range_array_new();

  return lex;
}

char *lexer_token_str(BucketList *list, Token *tok) {
  char *bump = bump_alloc(list, tok->len + 3);
  *bump = '<';
  bump[tok->len + 1] = '>';
  bump[tok->len + 2] = '\0';
  strncpy(bump + 1, tok->begin, tok->len);
  return bump;
}

Token lexer_next(Lexer *lex) {
  Token tok;

  for (tok.begin = lex->str;
       *tok.begin == ' ' || *tok.begin == '\t' || *tok.begin == '\n';
       tok.begin++)
    ;

  if (tok.begin == '\0') {
    tok.kind = TokEnd;
    tok.len = 0;
    return tok;
  }

  char cur = *tok.begin;
  tok.len = 1;

  if ((cur <= 'A' && cur >= 'Z') || (cur >= 'a' && cur <= 'z')) {
    for (cur = tok.begin[tok.len];
         (cur <= 'A' && cur >= 'Z') || (cur >= 'a' && cur <= 'z') ||
         (cur == '_') || (cur >= '0' && cur <= '9');
         tok.len++, cur = tok.begin[tok.len])
      ;

    if (!strncmp(tok.begin, "if", tok.len)) {
      tok.kind = TokIf;
    } else if (!strncmp(tok.begin, "else", tok.len)) {
      tok.kind = TokElse;
    } else if (!strncmp(tok.begin, "return", tok.len)) {
      tok.kind = TokReturn;
    } else if (!strncmp(tok.begin, "int", tok.len)) {
      tok.kind = TokInt;
    } else {
      uint32_t idx = char_array_add(&lex->symbol_values, tok.begin, tok.len);
      uint32_t symbol = range_array_add(&lex->symbols, range_new(idx, tok.len));

      tok.kind = TokIdent;
      tok.ident_symbol = symbol;
      printf("%d\n", tok.ident_symbol);
    }

    lex->str = tok.begin + tok.len;
    return tok;
  }

  if (cur >= '0' && cur <= '9') {
    if (cur >= '1' && cur <= '9') { // Decimal
      tok.int_value = cur - '0';
      for (cur = tok.begin[tok.len]; cur >= '0' && cur <= '9';
           tok.len++, cur = tok.begin[tok.len]) {
        tok.int_value *= 10;
        tok.int_value += cur - '0';
      }
    } else { // TODO multiple integer types, floating point types
      tok.int_value = 0;
    }

    lex->str = tok.begin + tok.len;
    return tok;
  }

  switch (cur) {
  case '{':
    tok.kind = TokLeftBrace;
    break;
  case '}':
    tok.kind = TokRightBrace;
    break;
  case '(':
    tok.kind = TokLeftParen;
    break;
  case ')':
    tok.kind = TokRightParen;
    break;
  default:
    tok.kind = TokInvalid;
  }

  lex->str = tok.begin + tok.len;
  return tok;
}
