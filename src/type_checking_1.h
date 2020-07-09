typedef struct {
  ASTNodeType return_type;
  bool defined;
  ASTNodeType *params;
} TCFunc;

typedef struct {
  Hash types;        // ASTNodeType (defn)
  Hash struct_types; // ASTNodeType (defn)
  Hash functions;    // TCFunc
  Hash symbols;      // ASTNodeType (defn)
} TypeChecker;

TypeChecker type_checker_new(void);

// Returns type defintion of type, or ASTTypeError if doesn't exist
ASTNodeType type_checker_lookup_type(TypeChecker *, ASTNodeType *);

// Returns type definition of member, or ASTTypeError on error
ASTNodeType type_checker_check_member(TypeChecker *, ASTNodeType *, uint32_t,
                                      bool);

// Checks a type definition, and adds it to tables if possible, returning
// whatever it added, or ASTTypeError if the type isn't valid
ASTNodeType type_checker_add_type_defn(TypeChecker *, ASTNodeType *);

// Checks a function declaration/definition and adds it to tables if possible,
// returning whatever it added, or a TCFunc with an ASTTypeError in the return
// type if the type isn't valid.
TCFunc type_checker_add_function(TypeChecker *, ASTNodeType *);

ASTNodeType type_checker_check_global_decl(TypeChecker *, ASTNodeStmt *);
ASTNodeType type_checker_check_expr(TypeChecker *, ASTNodeExpr *);

ASTNodeType type_checker_lookup_type(TypeChecker *checker, ASTNodeType *type) {
  ASTNodeType *out;
  if (type->kind == ASTTypeIdent) {
    out = hash_find(&checker->types, type->ident_symbol, sizeof(ASTNodeType));
    if (out == NULL) {
      ASTNodeType out_val;
      out_val.kind = ASTTypeError;
      out_val.err = error_new(string_new("couldn't find type"));
      return out_val;
    }

    return *out;
  }

  if (type->kind != ASTStruct)
    return *type;
  if (type->struct_is_defn)
    return *type;

  if (type->struct_has_ident) {
    out = hash_find(&checker->struct_types, type->struct_ident,
                    sizeof(ASTNodeType));
    if (out == NULL) {
      ASTNodeType out_val;
      out_val.kind = ASTTypeError;
      out_val.err = error_new(string_new("couldn't find type"));
      return out_val;
    }

    return *out;
  }

  debug("this shouldn't be possible");
  exit(1);
}

ASTNodeType type_checker_check_member(TypeChecker *checker, ASTNodeType *type,
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
    ASTNodeStmt *type_member = &type->struct_types[i];
    switch (type_member->kind) {
    case ASTDecl: {
      if (type_member->decl.ident == member)
        return type_checker_lookup_type(checker, &type_member->decl.type);
    } break;
    case ASTTypeDecl: {
      ASTNodeType decl_type =
          type_checker_lookup_type(checker, &type_member->decl_type);
      if (decl_type.kind == ASTTypeError) {
        return decl_type;
      }

      ASTNodeType result =
          type_checker_check_member(checker, type, member, is_ptr);
      if (result.kind != ASTTypeError)
        return result;
    } break;
    default:
      debug("invalid stmt inside of a struct definition");
      exit(1);
    }
  }

  out.kind = ASTTypeError;
  out.err = error_new(string_new("couldn't find member"));
  return out;
}
