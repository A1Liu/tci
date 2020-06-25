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
  TokDo,
  TokWhile,
  TokFor,
  TokBreak,
  TokContinue,
  TokReturn,

  TokVoid,
  TokChar,
  TokInt,
  TokUnsigned,
  TokLong,
  TokFloat,
  TokDouble,
  TokShort,

  TokDot,
  TokNot,
  TokTilde,
  TokStar,
  TokSlash,
  TokPlus,
  TokDash,
  TokPercent,
  TokPlusPlus,
  TokDashDash,
  TokEq,
  TokEqEq,
  TokNeq,
  TokLeq,
  TokLt,
  TokGeq,
  TokGt,
  TokAmp,
  TokAmpAmp,
  TokLine,
  TokLineLine,
  TokCaret,
  TokAmpEq,
  TokLineEq,
  TokCaretEq,
  TokPlusEq,
  TokDashEq,
  TokSlashEq,
  TokStarEq,
  TokPercentEq,

  TokLeftBrace,
  TokRightBrace,
  TokLeftParen,
  TokRightParen,
  TokLeftBracket,
  TokRightBracket,

  TokSemicolon,

  TokInvalid,
  TokEnd,
} TokenKind;

typedef struct {
  TokenKind kind;
  char *begin;
  uint32_t len;
  union {
    int32_t int_value;
    uint32_t uint_value;
    int64_t long_value;
    uint64_t ulong_value;
    float float_value;
    double double_value;

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
    } else if (!strncmp(tok.begin, "do", tok.len)) {
      tok.kind = TokDo;
    } else if (!strncmp(tok.begin, "while", tok.len)) {
      tok.kind = TokWhile;
    } else if (!strncmp(tok.begin, "for", tok.len)) {
      tok.kind = TokFor;
    } else if (!strncmp(tok.begin, "break", tok.len)) {
      tok.kind = TokBreak;
    } else if (!strncmp(tok.begin, "continue", tok.len)) {
      tok.kind = TokContinue;
    } else if (!strncmp(tok.begin, "return", tok.len)) {
      tok.kind = TokReturn;
    } else if (!strncmp(tok.begin, "void", tok.len)) {
      tok.kind = TokVoid;
    } else if (!strncmp(tok.begin, "char", tok.len)) {
      tok.kind = TokChar;
    } else if (!strncmp(tok.begin, "int", tok.len)) {
      tok.kind = TokInt;
    } else if (!strncmp(tok.begin, "short", tok.len)) {
      tok.kind = TokShort;
    } else if (!strncmp(tok.begin, "long", tok.len)) {
      tok.kind = TokLong;
    } else if (!strncmp(tok.begin, "unsigned", tok.len)) {
      tok.kind = TokUnsigned;
    } else if (!strncmp(tok.begin, "float", tok.len)) {
      tok.kind = TokFloat;
    } else if (!strncmp(tok.begin, "double", tok.len)) {
      tok.kind = TokDouble;
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
  case '[':
    tok.kind = TokLeftBracket;
    break;
  case ']':
    tok.kind = TokLeftBracket;
    break;
  case '~':
    tok.kind = TokTilde;
    break;
  case '.':
    tok.kind = TokDot;
    break;

  case '+': {
    cur = tok.begin[tok.len];
    if (cur == '+') {
      tok.len++;
      tok.kind = TokPlusPlus;
    } else if (cur == '=') {
      tok.len++;
      tok.kind = TokPlusEq;
    } else {
      tok.kind = TokPlus;
    }
  } break;
  case '-': {
    cur = tok.begin[tok.len];
    if (cur == '-') {
      tok.len++;
      tok.kind = TokDashDash;
    } else if (cur == '=') {
      tok.len++;
      tok.kind = TokDashEq;
    } else {
      tok.kind = TokDash;
    }
  } break;
  case '/': {
    cur = tok.begin[tok.len];
    if (cur == '=') {
      tok.len++;
      tok.kind = TokSlashEq;
    } else {
      tok.kind = TokSlash;
    }
  } break;
  case '*': {
    cur = tok.begin[tok.len];
    if (cur == '=') {
      tok.len++;
      tok.kind = TokStarEq;
    } else {
      tok.kind = TokStar;
    }
  } break;
  case '%': {
    cur = tok.begin[tok.len];
    if (cur == '=') {
      tok.len++;
      tok.kind = TokPercentEq;
    } else {
      tok.kind = TokPercent;
    }
  } break;

  case '>': {
    cur = tok.begin[tok.len];
    if (cur == '=') {
      tok.len++;
      tok.kind = TokGeq;
    } else {
      tok.kind = TokGt;
    }
  } break;
  case '<': {
    cur = tok.begin[tok.len];
    if (cur == '=') {
      tok.len++;
      tok.kind = TokLeq;
    } else {
      tok.kind = TokLt;
    }
  } break;
  case '!': {
    cur = tok.begin[tok.len];
    if (cur == '=') {
      tok.len++;
      tok.kind = TokNeq;
    } else {
      tok.kind = TokNot;
    }
  } break;
  case '=': {
    cur = tok.begin[tok.len];
    if (cur == '=') {
      tok.len++;
      tok.kind = TokEqEq;
    } else {
      tok.kind = TokEq;
    }
  } break;

  case '|': {
    cur = tok.begin[tok.len];
    if (cur == '|') {
      tok.len++;
      tok.kind = TokLineLine;
    } else if (cur == '=') {
      tok.len++;
      tok.kind = TokLineEq;
    } else {
      tok.kind = TokLine;
    }
  } break;
  case '&': {
    cur = tok.begin[tok.len];
    if (cur == '&') {
      tok.len++;
      tok.kind = TokAmpAmp;
    } else if (cur == '=') {
      tok.len++;
      tok.kind = TokAmpEq;
    } else {
      tok.kind = TokAmp;
    }
  } break;
  case '^': {
    cur = tok.begin[tok.len];
    if (cur == '=') {
      tok.len++;
      tok.kind = TokAmpEq;
    } else {
      tok.kind = TokAmp;
    }
  } break;
  default:
    tok.kind = TokInvalid;
  }

  lex->str = tok.begin + tok.len;
  return tok;
}
