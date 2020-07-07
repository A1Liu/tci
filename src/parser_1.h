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

ASTNodeStmt parser_parse_global_decl(Parser *parser);
ASTNodeStmt parser_parse_simple_decl(Parser *parser);
ASTNodeType parser_parse_type_prefix(Parser *parser);

ASTNodeStmt parser_parse_global_decl(Parser *parser) {
  ASTNodeStmt stmt = parser_parse_simple_decl(parser);
  if (stmt.kind == ASTStmtError)
    return stmt;

  Token tok = parser_pop(parser);
  if (tok.kind == TokSemicolon)
    return stmt;

  if (tok.kind == TokLeftParen && stmt.kind == ASTDecl) {
    ASTNodeDecl decl = stmt.decl;
    stmt.kind = ASTFuncBlock;
    stmt.func.return_type = decl.type;
    stmt.func.ident = decl.ident;
    stmt.func.is_decl = false;
    stmt.func.stmts = dyn_array_new(ASTNodeStmt);

    return stmt;
  }

  stmt.kind = ASTStmtError;
  stmt.err =
      error_new(string_new("unexpected token when parsing end of statement"));
  error_array_add(&stmt.err, tok.range,
                  string_new("this token is invalid in this context"));
  return stmt;
}

ASTNodeStmt parser_parse_simple_decl(Parser *parser) {
  Token tok = parser_peek(parser);
  ASTNodeStmt stmt;
  stmt.range.begin = tok.range.begin;
  ASTNodeType type = parser_parse_type_prefix(parser);
  if (type.kind == ASTTypeError) {
    stmt.kind = ASTStmtError;
    stmt.err = type.err;
    return stmt;
  }

  tok = parser_peek(parser);
  if (tok.kind == TokIdent) {
    parser_pop(parser);
    stmt.kind = ASTDecl;
    uint32_t ident = tok.ident_symbol;
    tok = parser_peek(parser);

    if (tok.kind == TokEq) {
      parser_pop(parser);
      debug("assignment declarations not implemented yet\n");
      exit(1);
    }

    stmt.decl.type = type;
    stmt.decl.ident = tok.ident_symbol;
    stmt.decl.expr.kind = ASTUninit;
    stmt.range.end = tok.range.end;
    return stmt;
  }

  stmt.kind = ASTTypeDecl;
  stmt.decl_type = type;
  stmt.range.end = tok.range.end;
  return stmt;
}

ASTNodeType parser_parse_type_prefix(Parser *parser) {
  ASTNodeType type;

  Token tok = parser_pop(parser);
  type.range = tok.range;

  switch (tok.kind) {
  case TokStruct: {
    type.kind = ASTStruct;
    Token ident_tok = parser_peek(parser);
    if (ident_tok.kind == TokIdent) {
      parser_pop(parser);
      type.struct_ident = ident_tok.ident_symbol;
      type.struct_has_ident = true;
    } else
      type.struct_has_ident = false;

    if (parser_pop(parser).kind != TokLeftBrace) {
      type.kind = ASTTypeError;
      type.err = error_new(string_new("expected '{' character"));
      error_array_add(
          &type.err, tok.range,
          string_new("this token is invalid for the current context"));
      return type;
    }

    type.struct_types = dyn_array_new(ASTNodeDecl);
    while (parser_peek(parser).kind != TokRightBrace) {
      ASTNodeStmt decl = parser_parse_simple_decl(parser);
      if (decl.kind == ASTStmtError) {
        type.kind = ASTTypeError;
        type.err = decl.err;
        return type;
      }

      Token tok = parser_pop(parser);
      if (tok.kind != TokSemicolon) {
        type.kind = ASTTypeError;
        type.err = error_new(string_new("expected ';' character"));
        error_array_add(
            &type.err, tok.range,
            string_new("this token is invalid for the current context"));
        return type;
      }

      dyn_array_add(&type.struct_types, decl.decl);
    }

    type.range.end = parser_pop(parser).range.end;
    return type;
  } break;
  case TokIdent:
    type.kind = ASTTypeIdent;
    type.ident_symbol = tok.ident_symbol;
    return type;
  case TokInt:
    type.kind = ASTInt;
    return type;
  default:
    type.kind = ASTTypeError;
    type.err =
        error_new(string_new("found unexpected token when parsing type"));

    error_array_add(&type.err, tok.range,
                    string_new("this token is not allowed to begin a type "
                               "in the global context"));
    return type;
  }
}
