// BASIC PARSER FUNCTIONALITY

typedef struct {
  BumpList *bump;
  Lexer lex;
  Token *tokens;
} Parser;

typedef struct {
  Range range;
  String message;
} ParseError;

Parser parser_new(BumpList *bump, char *data) {
  Parser parser;
  parser.bump = bump;
  parser.lex = lexer_new(data);
  parser.tokens = dyn_array_new(Token);
  return parser;
}

Token parser_pop(Parser *parser) {
  if (dyn_array_len(parser->tokens) > 0) {
    *__dyn_array_len_ptr(parser->tokens) = dyn_array_len(parser->tokens) - 1;
    return parser->tokens[dyn_array_len(parser->tokens)];
  }

  return lexer_next(&parser->lex);
}

Token parser_peek(Parser *parser) {
  if (dyn_array_len(parser->tokens) == 0)
    dyn_array_add(&parser->tokens, lexer_next(&parser->lex));
  return parser->tokens[dyn_array_len(parser->tokens) - 1];
}

ASTNodeStmt parser_parse_global_decl(Parser *);
ASTNodeStmt parser_parse_simple_decl(Parser *);
ASTNodeType parser_parse_type_prefix(Parser *);
ASTNodeExpr parser_parse_expr(Parser *parser);
ASTNodeExpr parser_parse_atom(Parser *);

ASTNodeStmt parser_parse_global_decl(Parser *parser) {
  ASTNodeStmt stmt = parser_parse_simple_decl(parser);
  if (stmt.kind == ASTStmtError)
    return stmt;

  Token tok = parser_pop(parser);
  if (tok.kind == TokSemicolon)
    return stmt;

  if (tok.kind != TokLeftParen || stmt.kind != ASTDecl) {
    stmt.kind = ASTStmtError;
    stmt.err =
        error_new(string_new("unexpected token when parsing end of statement"));
    error_array_add(&stmt.err, tok.range,
                    string_new("this token is invalid in this context"));
    return stmt;
  }

  ASTNodeDecl decl = stmt.decl;
  stmt.kind = ASTFuncBlock;
  stmt.func.return_type = decl.type;
  stmt.func.ident = decl.ident;
  stmt.func.params = dyn_array_new(ASTNodeStmt);
  stmt.func.body = dyn_array_new(Token);

  tok = parser_peek(parser);
  if (tok.kind != TokRightParen) {
    ASTNodeStmt param = parser_parse_simple_decl(parser);
    if (param.kind == ASTStmtError)
      return param;
    dyn_array_add(&stmt.func.params, param);
    tok = parser_peek(parser);
    while (tok.kind == TokComma) {
      parser_pop(parser);
      ASTNodeStmt param = parser_parse_simple_decl(parser);
      if (param.kind == ASTStmtError)
        return param;
      dyn_array_add(&stmt.func.params, param);
      tok = parser_peek(parser);
    }

    if (tok.kind != TokRightParen) {
      stmt.kind = ASTStmtError;
      stmt.err = error_new(
          string_new("unexpected token when parsing end of parameter"));
      error_array_add(&stmt.err, tok.range,
                      string_new("this token is invalid in this context"));
      return stmt;
    }
  }

  parser_pop(parser);
  tok = parser_pop(parser);

  if (tok.kind == TokSemicolon) {
    stmt.func.is_defn = false;
    return stmt;
  }

  if (tok.kind != TokLeftBrace) {
    stmt.kind = ASTStmtError;
    stmt.err = error_new(
        string_new("unexpected token when parsing beginning of function body"));
    error_array_add(&stmt.err, tok.range,
                    string_new("this token is invalid in this context"));
    return stmt;
  }

  stmt.func.is_defn = true;
  tok = parser_pop(parser);
  for (uint32_t brace_count = 1;
       brace_count > 0 && tok.kind != TokInvalid && tok.kind != TokEnd;
       tok = parser_peek(parser)) {
    parser_pop(parser);
    switch (tok.kind) {
    case TokLeftBrace:
      brace_count++;
      dyn_array_add(&stmt.func.body, tok);
      break;
    case TokRightBrace:
      brace_count--;
      dyn_array_add(&stmt.func.body, tok);
      break;
    default:
      dyn_array_add(&stmt.func.body, tok);
    }
  }

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

  while (parser_peek(parser).kind == TokStar) {
    parser_pop(parser);
    type.pointer_count++;
  }

  tok = parser_peek(parser);
  if (tok.kind == TokIdent) {
    parser_pop(parser);
    stmt.kind = ASTDecl;
    uint32_t ident = tok.ident_symbol;

    stmt.decl.expr.kind = ASTUninit;

    tok = parser_peek(parser);
    if (tok.kind == TokEq) {
      parser_pop(parser);
      ASTNodeExpr expr = parser_parse_expr(parser);
      if (expr.kind == ASTExprError) {
        stmt.kind = ASTStmtError;
        stmt.err = expr.err;
        return stmt;
      }

      stmt.decl.expr = expr;
    }

    stmt.decl.type = type;
    stmt.decl.ident = ident;
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
  type.pointer_count = 0;

  Token tok = parser_pop(parser);
  type.range = tok.range;

  switch (tok.kind) {
  case TokStruct: {
    type.kind = ASTStruct;
    type.struct_types = dyn_array_new(ASTNodeStmt);
    Token ident_tok = parser_peek(parser);
    if (ident_tok.kind == TokIdent) {
      parser_pop(parser);
      type.struct_ident = ident_tok.ident_symbol;
      type.struct_has_ident = true;
      if (parser_peek(parser).kind != TokLeftBrace) {
        type.is_struct_decl = false;
        return type;
      }
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

    type.is_struct_decl = true;
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

      dyn_array_add(&type.struct_types, decl);
    }

    type.range.end = parser_pop(parser).range.end;
    return type;
  } break;
  case TokIdent:
    type.kind = ASTTypeIdent;
    type.ident_symbol = tok.ident_symbol;
    return type;
  case TokChar:
    type.kind = ASTChar;
    return type;
  case TokInt:
    type.kind = ASTInt;
    return type;
  case TokVoid:
    type.kind = ASTVoid;
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

ASTNodeExpr parser_parse_expr(Parser *parser) {
  return parser_parse_atom(parser);
}

ASTNodeExpr parser_parse_atom(Parser *parser) {
  Token tok = parser_pop(parser);
  ASTNodeExpr expr;
  switch (tok.kind) {
  case TokIntLiteral:
    expr.kind = ASTIntLiteral;
    expr.int_value = tok.int_value;
    break;
  case TokIdent:
    expr.kind = ASTIdent;
    expr.ident_symbol = tok.ident_symbol;
    break;
  default:
    expr.kind = ASTExprError;
    debug("%u\n", tok.kind);
    expr.err =
        error_new(string_new("found unexpected token when parsing expression"));
    error_array_add(
        &expr.err, tok.range,
        string_new("this token is not allowed to be in an expression"));
  }
  return expr;
}
