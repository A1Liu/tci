typedef struct {
  char *str;
  size_t current;
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

  TokVoid,
  TokChar,
  TokInt,
  TokUnsigned,
  TokLong,
  TokFloat,
  TokDouble,
  TokShort,
  TokStruct,
  TokUnion,

  TokIf,
  TokElse,
  TokDo,
  TokWhile,
  TokFor,
  TokBreak,
  TokContinue,
  TokReturn,
  TokTypedef,

  TokSizeof,
  TokCast,

  TokDot,
  TokArrow,
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
  TokComma,

  TokInvalid,
  TokEnd,
} TokenKind;

typedef struct {
  TokenKind kind;
  Range range;
  union {
    uint32_t int_value;
    uint32_t ident_symbol;
  };
} Token;

Lexer lexer_new(char *data) {
  Lexer lex;
  lex.str = data;
  lex.current = 0;
  lex.symbols = dyn_array_new(String);

  return lex;
}

char *lexer_token_str(Lexer *lex, BumpList *list, Token *tok) {
  uint32_t len = tok->range.end - tok->range.begin;
  char *bump = bump_alloc(list, len + 3);
  *bump = '<';
  strncpy(bump + 1, &lex->str[tok->range.begin], len);
  bump[len + 1] = '>';
  bump[len + 2] = '\0';
  return bump;
}

Token lexer_next(Lexer *lex) {
  Token tok;

  char cur;
  for (tok.range.begin = lex->current, cur = lex->str[tok.range.begin];
       cur == ' ' || cur == '\t' || cur == '\n';
       tok.range.begin++, cur = lex->str[tok.range.begin])
    ;

  if (cur == '\0') {
    tok.kind = TokEnd;
    lex->current = tok.range.begin;
    tok.range.end = tok.range.begin;
    return tok;
  }

  tok.range.end = tok.range.begin + 1;

  if ((cur >= 'A' && cur <= 'Z') || (cur >= 'a' && cur <= 'z')) {
    for (cur = lex->str[tok.range.end];
         (cur <= 'A' && cur >= 'Z') || (cur >= 'a' && cur <= 'z') ||
         (cur == '_') || (cur >= '0' && cur <= '9');
         tok.range.end++, cur = lex->str[tok.range.end])
      ;

    String str = string_from_range(lex->str, tok.range);
    if (streq(str, "if")) {
      tok.kind = TokIf;
    } else if (streq(str, "else")) {
      tok.kind = TokElse;
    } else if (streq(str, "do")) {
      tok.kind = TokDo;
    } else if (streq(str, "while")) {
      tok.kind = TokWhile;
    } else if (streq(str, "for")) {
      tok.kind = TokFor;
    } else if (streq(str, "break")) {
      tok.kind = TokBreak;
    } else if (streq(str, "continue")) {
      tok.kind = TokContinue;
    } else if (streq(str, "return")) {
      tok.kind = TokReturn;
    } else if (streq(str, "struct")) {
      tok.kind = TokStruct;
    } else if (streq(str, "union")) {
      tok.kind = TokUnion;
    } else if (streq(str, "typedef")) {
      tok.kind = TokTypedef;
    } else if (streq(str, "void")) {
      tok.kind = TokVoid;
    } else if (streq(str, "char")) {
      tok.kind = TokChar;
    } else if (streq(str, "int")) {
      tok.kind = TokInt;
    } else if (streq(str, "short")) {
      tok.kind = TokShort;
    } else if (streq(str, "long")) {
      tok.kind = TokLong;
    } else if (streq(str, "unsigned")) {
      tok.kind = TokUnsigned;
    } else if (streq(str, "cast")) {
      tok.kind = TokCast;
    } else if (streq(str, "sizeof")) {
      tok.kind = TokSizeof;
    } else if (streq(str, "float")) {
      tok.kind = TokFloat;
    } else if (streq(str, "double")) {
      tok.kind = TokDouble;
    } else {
      tok.kind = TokIdent;
      uint64_t sym_len = dyn_array_len(lex->symbols);

      // TODO replace with hashtable
      for (uint32_t i = 0; i < sym_len; i++) {
        if (string_equals(str, lex->symbols[i])) {
          tok.ident_symbol = i;
          lex->current = tok.range.end;
          return tok;
        }
      }

      tok.ident_symbol = dyn_array_add(&lex->symbols, str);
    }

    lex->current = tok.range.end;
    return tok;
  }

  if (cur >= '0' && cur <= '9') {
    tok.kind = TokIntLiteral;
    if (cur >= '1' && cur <= '9') { // Decimal
      tok.int_value = cur - '0';
      for (cur = lex->str[tok.range.end]; cur >= '0' && cur <= '9';
           tok.range.end++, cur = lex->str[tok.range.end]) {
        tok.int_value *= 10;
        tok.int_value += cur - '0';
      }
    } else { // TODO multiple integer types, floating point types
      tok.int_value = 0;
    }

    lex->current = tok.range.end;
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
  case ',':
    tok.kind = TokComma;
    break;

  case '+': {
    cur = lex->str[tok.range.end];
    if (cur == '+') {
      tok.range.end++;
      tok.kind = TokPlusPlus;
    } else if (cur == '=') {
      tok.range.end++;
      tok.kind = TokPlusEq;
    } else {
      tok.kind = TokPlus;
    }
  } break;
  case '-': {
    cur = lex->str[tok.range.end];
    if (cur == '-') {
      tok.range.end++;
      tok.kind = TokDashDash;
    } else if (cur == '=') {
      tok.range.end++;
      tok.kind = TokDashEq;
    } else if (cur == '>') {
      tok.range.end++;
      tok.kind = TokArrow;
    } else {
      tok.kind = TokDash;
    }
  } break;
  case '/': {
    cur = lex->str[tok.range.end];
    if (cur == '=') {
      tok.range.end++;
      tok.kind = TokSlashEq;
    } else {
      tok.kind = TokSlash;
    }
  } break;
  case '*': {
    cur = lex->str[tok.range.end];
    if (cur == '=') {
      tok.range.end++;
      tok.kind = TokStarEq;
    } else {
      tok.kind = TokStar;
    }
  } break;
  case '%': {
    cur = lex->str[tok.range.end];
    if (cur == '=') {
      tok.range.end++;
      tok.kind = TokPercentEq;
    } else {
      tok.kind = TokPercent;
    }
  } break;

  case '>': {
    cur = lex->str[tok.range.end];
    if (cur == '=') {
      tok.range.end++;
      tok.kind = TokGeq;
    } else {
      tok.kind = TokGt;
    }
  } break;
  case '<': {
    cur = lex->str[tok.range.end];
    if (cur == '=') {
      tok.range.end++;
      tok.kind = TokLeq;
    } else {
      tok.kind = TokLt;
    }
  } break;
  case '!': {
    cur = lex->str[tok.range.end];
    if (cur == '=') {
      tok.range.end++;
      tok.kind = TokNeq;
    } else {
      tok.kind = TokNot;
    }
  } break;
  case '=': {
    cur = lex->str[tok.range.end];
    if (cur == '=') {
      tok.range.end++;
      tok.kind = TokEqEq;
    } else {
      tok.kind = TokEq;
    }
  } break;

  case '|': {
    cur = lex->str[tok.range.end];
    if (cur == '|') {
      tok.range.end++;
      tok.kind = TokLineLine;
    } else if (cur == '=') {
      tok.range.end++;
      tok.kind = TokLineEq;
    } else {
      tok.kind = TokLine;
    }
  } break;
  case '&': {
    cur = lex->str[tok.range.end];
    if (cur == '&') {
      tok.range.end++;
      tok.kind = TokAmpAmp;
    } else if (cur == '=') {
      tok.range.end++;
      tok.kind = TokAmpEq;
    } else {
      tok.kind = TokAmp;
    }
  } break;
  case '^': {
    cur = lex->str[tok.range.end];
    if (cur == '=') {
      tok.range.end++;
      tok.kind = TokAmpEq;
    } else {
      tok.kind = TokAmp;
    }
  } break;
  default:
    tok.kind = TokInvalid;
  }

  lex->current = tok.range.end;
  return tok;
}
