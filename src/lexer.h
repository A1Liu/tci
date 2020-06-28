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
  String *symbols;
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
  TokStruct,
  TokUnion,
  TokTypedef,

  TokSizeof,
  TokCast,

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
  String str;
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

Lexer lexer_new(char *data) {
  Lexer lex;
  lex.str = data;
  lex.symbols = dyn_array_new(String);

  return lex;
}

char *lexer_token_str(BumpList *list, Token *tok) {
  char *bump = bump_alloc(list, tok->str.len + 3);
  *bump = '<';
  bump[tok->str.len + 1] = '>';
  bump[tok->str.len + 2] = '\0';
  strncpy(bump + 1, tok->str.str, tok->str.len);
  return bump;
}

Token lexer_next(Lexer *lex) {
  Token tok;

  for (tok.str.str = lex->str;
       *tok.str.str == ' ' || *tok.str.str == '\t' || *tok.str.str == '\n';
       tok.str.str++)
    ;

  if (tok.str.str == '\0') {
    tok.kind = TokEnd;
    tok.str.len = 0;
    return tok;
  }

  char cur = *tok.str.str;
  tok.str.len = 1;

  if ((cur >= 'A' && cur <= 'Z') || (cur >= 'a' && cur <= 'z')) {
    for (cur = tok.str.str[tok.str.len];
         (cur <= 'A' && cur >= 'Z') || (cur >= 'a' && cur <= 'z') ||
         (cur == '_') || (cur >= '0' && cur <= '9');
         tok.str.len++, cur = tok.str.str[tok.str.len])
      ;

    if (streq(tok.str, "if")) {
      tok.kind = TokIf;
    } else if (streq(tok.str, "else")) {
      tok.kind = TokElse;
    } else if (streq(tok.str, "do")) {
      tok.kind = TokDo;
    } else if (streq(tok.str, "while")) {
      tok.kind = TokWhile;
    } else if (streq(tok.str, "for")) {
      tok.kind = TokFor;
    } else if (streq(tok.str, "break")) {
      tok.kind = TokBreak;
    } else if (streq(tok.str, "continue")) {
      tok.kind = TokContinue;
    } else if (streq(tok.str, "return")) {
      tok.kind = TokReturn;
    } else if (streq(tok.str, "struct")) {
      tok.kind = TokStruct;
    } else if (streq(tok.str, "union")) {
      tok.kind = TokUnion;
    } else if (streq(tok.str, "typedef")) {
      tok.kind = TokTypedef;
    } else if (streq(tok.str, "void")) {
      tok.kind = TokVoid;
    } else if (streq(tok.str, "char")) {
      tok.kind = TokChar;
    } else if (streq(tok.str, "int")) {
      tok.kind = TokInt;
    } else if (streq(tok.str, "short")) {
      tok.kind = TokShort;
    } else if (streq(tok.str, "long")) {
      tok.kind = TokLong;
    } else if (streq(tok.str, "unsigned")) {
      tok.kind = TokUnsigned;
    } else if (streq(tok.str, "cast")) {
      tok.kind = TokCast;
    } else if (streq(tok.str, "sizeof")) {
      tok.kind = TokSizeof;
    } else if (streq(tok.str, "float")) {
      tok.kind = TokFloat;
    } else if (streq(tok.str, "double")) {
      tok.kind = TokDouble;
    } else {
      uint64_t symbol = dyn_array_add(&lex->symbols, tok.str);
      tok.kind = TokIdent;
      tok.ident_symbol = symbol;
    }

    lex->str = tok.str.str + tok.str.len;
    return tok;
  }

  if (cur >= '0' && cur <= '9') {
    tok.kind = TokInt;
    if (cur >= '1' && cur <= '9') { // Decimal
      tok.int_value = cur - '0';
      for (cur = tok.str.str[tok.str.len]; cur >= '0' && cur <= '9';
           tok.str.len++, cur = tok.str.str[tok.str.len]) {
        tok.int_value *= 10;
        tok.int_value += cur - '0';
      }
    } else { // TODO multiple integer types, floating point types
      tok.int_value = 0;
    }

    lex->str = tok.str.str + tok.str.len;
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
  case ';':
    tok.kind = TokSemicolon;
    break;

  case '+': {
    cur = tok.str.str[tok.str.len];
    if (cur == '+') {
      tok.str.len++;
      tok.kind = TokPlusPlus;
    } else if (cur == '=') {
      tok.str.len++;
      tok.kind = TokPlusEq;
    } else {
      tok.kind = TokPlus;
    }
  } break;
  case '-': {
    cur = tok.str.str[tok.str.len];
    if (cur == '-') {
      tok.str.len++;
      tok.kind = TokDashDash;
    } else if (cur == '=') {
      tok.str.len++;
      tok.kind = TokDashEq;
    } else {
      tok.kind = TokDash;
    }
  } break;
  case '/': {
    cur = tok.str.str[tok.str.len];
    if (cur == '=') {
      tok.str.len++;
      tok.kind = TokSlashEq;
    } else {
      tok.kind = TokSlash;
    }
  } break;
  case '*': {
    cur = tok.str.str[tok.str.len];
    if (cur == '=') {
      tok.str.len++;
      tok.kind = TokStarEq;
    } else {
      tok.kind = TokStar;
    }
  } break;
  case '%': {
    cur = tok.str.str[tok.str.len];
    if (cur == '=') {
      tok.str.len++;
      tok.kind = TokPercentEq;
    } else {
      tok.kind = TokPercent;
    }
  } break;

  case '>': {
    cur = tok.str.str[tok.str.len];
    if (cur == '=') {
      tok.str.len++;
      tok.kind = TokGeq;
    } else {
      tok.kind = TokGt;
    }
  } break;
  case '<': {
    cur = tok.str.str[tok.str.len];
    if (cur == '=') {
      tok.str.len++;
      tok.kind = TokLeq;
    } else {
      tok.kind = TokLt;
    }
  } break;
  case '!': {
    cur = tok.str.str[tok.str.len];
    if (cur == '=') {
      tok.str.len++;
      tok.kind = TokNeq;
    } else {
      tok.kind = TokNot;
    }
  } break;
  case '=': {
    cur = tok.str.str[tok.str.len];
    if (cur == '=') {
      tok.str.len++;
      tok.kind = TokEqEq;
    } else {
      tok.kind = TokEq;
    }
  } break;

  case '|': {
    cur = tok.str.str[tok.str.len];
    if (cur == '|') {
      tok.str.len++;
      tok.kind = TokLineLine;
    } else if (cur == '=') {
      tok.str.len++;
      tok.kind = TokLineEq;
    } else {
      tok.kind = TokLine;
    }
  } break;
  case '&': {
    cur = tok.str.str[tok.str.len];
    if (cur == '&') {
      tok.str.len++;
      tok.kind = TokAmpAmp;
    } else if (cur == '=') {
      tok.str.len++;
      tok.kind = TokAmpEq;
    } else {
      tok.kind = TokAmp;
    }
  } break;
  case '^': {
    cur = tok.str.str[tok.str.len];
    if (cur == '=') {
      tok.str.len++;
      tok.kind = TokAmpEq;
    } else {
      tok.kind = TokAmp;
    }
  } break;
  default:
    tok.kind = TokInvalid;
  }

  lex->str = tok.str.str + tok.str.len;
  return tok;
}
