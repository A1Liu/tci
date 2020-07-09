typedef struct {
  Hash types;
  Hash struct_types;
  Hash functions;
  Hash symbols;
} TypeChecker;

ASTNodeTYpe type_check_member(TypeChecker *, ASTNodeType *, uint32_t, bool);
ASTNodeType type_check_global_decl(TypeChecker *, ASTNodeStmt *);
ASTNodeType type_check_expr(TypeChecker *, ASTNodeExpr *);

ASTNodeType type_check_member(TypeChecker *checker, ASTNodeType *type,
                              uint32_t member, bool is_ptr) {
  ASTNodeType out;

  if (is_ptr && type->pointer_count != 1) {
    out.kind = ASTTypeError;
    out.err = error_new(string_new("type cannot be dereferenced with arrow"));
    return out;
  }

  if (!is_ptr && type->pointer_count != 0) {
    out.kind = ASTTypeError;
    out.err = error_new(string_new("type cannot be dereferenced with dot"));
  }

  if (type->kind != ASTStruct) {
    out.kind = ASTTypeError;
    out.err = error_new(string_new("type cannot be dereferenced"));
    return out;
  }

  if (!type->struct_is_defn) {
    debug("got non-defintion of type");
    exit(1);
  }

  uint32_t len = dyn_array_len(type->struct_types);
  for (uint32_t i = 0; i < len; i++) {
    ASTNodeStmt *member = &type->struct_types[i];
    switch (member->kind) {
    case ASTDecl:
      // look for the name
    case ASTTYpeDecl:
      // search within the type decl
    }
  }

  return type;
}
