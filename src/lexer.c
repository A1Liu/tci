typedef struct {
  char *str;
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
} TokenKind;

typedef struct {
  TokenKind kind;
  char *begin;
  uint32_t len;
  union {
    int32_t int_value;
  };
} Token;

Lexer lexer_new(char *data) {
  Lexer lex;
  lex.str = data;
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
      tok.kind = TokIdent;
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
