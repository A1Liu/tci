// BASIC PARSER FUNCTIONALITY

typedef struct {
  BumpList *bump;
  Lexer lex;
  Token *begin;
  uint32_t end;
  uint32_t capacity;
} Parser;

typedef struct {
  Range range;
  String message;
} ParseError;

Parser parser_new(BumpList *bump, char *data) {
  Parser parser;
  parser.bump = bump;
  parser.lex = lexer_new(data);
  parser.begin = NULL;
  parser.end = 0;
  parser.capacity = 0;
  return parser;
}

Token parser_pop(Parser *parser) {
  if (parser->end) {
    Token tok = parser->begin[--parser->end];
    return tok;
  }

  return lexer_next(&parser->lex);
}

void parser_push(Parser *parser, Token tok) {
  if (parser->begin == NULL || parser->end == parser->capacity) {
    parser->capacity = parser->capacity / 2 + parser->capacity + 16;
    parser->begin = realloc(parser->begin, parser->capacity * sizeof(Token));
  }

  parser->begin[parser->end++] = tok;
}

Token parser_peek(Parser *parser) {
  Token tok = parser_pop(parser);
  parser_push(parser, tok);
  return tok;
}

ASTNodeStmt parser_parse_global_decl(Parser *parser) {
  ASTNodeStmt stmt;
  Token tok = parser_peek(parser);

  switch (tok.kind) {
  case TokTypedef:
    break;

  case TokIdent:
  case TokVoid:
  case TokChar:
  case TokInt:
  case TokUnsigned:
  case TokLong:
  case TokFloat:
  case TokDouble:
  case TokShort:
  case TokStruct:
  case TokUnion:
    break;

  default:
    stmt.kind = ASTStmtError;
    stmt.err = error_new(string_new("found unrecognized token"));

    error_array_add(
        &stmt.err, tok.range,
        string_new("this token is not allowed in the global context"));
    break;
  }

  return stmt;
}
