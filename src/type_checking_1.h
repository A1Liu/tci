typedef enum { TCError, TCStruct, TCIdent, TCInt, TCChar, TCVoid } TCTypeKind;

struct structMember;

typedef struct tcType {
  TCTypeKind kind;
  union {
    Error err;
    struct {
      union {
        struct structMember *members;
        uint32_t ident_symbol;
      };
      uint32_t pointer_count;
    };
  };

} TCType;

typedef struct structMember {
  TCType type;
  bool is_anonymous;
  uint32_t ident;
} StructMember;

typedef struct {
  ASTNodeType return_type;
  bool defined;
  ASTNodeType *params;
} TCFunc;

typedef struct {
  Hash types;        // TCType
  Hash struct_types; // TCType
  Hash functions;    // TCFunc
  Hash symbols;      // TCType
} TypeChecker;

TypeChecker type_checker_new(void);

TCType type_checker_convert_primitive(ASTNodeType *);

// Returns type defintion of type, or ASTTypeError if doesn't exist.
TCType type_checker_lookup_type(TypeChecker *, ASTNodeType *);

// Returns type definition of member, or ASTTypeError on error
TCType type_checker_check_member(TypeChecker *, TCType *, uint32_t, bool);

// Checks a type definition, and adds it to tables if possible, returning
// whatever it added, or ASTTypeError if the type isn't valid
TCType type_checker_add_type_defn(TypeChecker *, ASTNodeType *, uint32_t *);

// Checks a function declaration/definition and adds it to tables if possible,
// returning whatever it added, or a TCFunc with an ASTTypeError in the return
// type if the type isn't valid.
TCFunc type_checker_add_function(TypeChecker *, ASTNodeStmt *);

TCType type_checker_check_global_decl(TypeChecker *, ASTNodeStmt *);
TCType type_checker_check_expr(TypeChecker *, ASTNodeExpr *);

TCType type_checker_convert_primitive(ASTNodeType *type_node) {
  TCType type;
  type.pointer_count = type_node->pointer_count;
  switch (type_node->kind) {
  case ASTInt:
    type.kind = TCInt;
    break;
  case ASTChar:
    type.kind = TCChar;
    break;
  case ASTVoid:
    type.kind = TCVoid;
    break;
  default: {
    char *char_array = dyn_array_new(char);
    String type_node_str = ast_node_type_str(&char_array, type_node);
    debug("got non-primitive when converting primitive: %.*s",
          (uint32_t)type_node_str.len, type_node_str.str);
    exit(1);
  }
  }
  return type;
}

TCType type_checker_lookup_type(TypeChecker *checker, ASTNodeType *type_node) {
  TCType *type;
  if (type_node->kind == ASTTypeIdent) {
    type = hash_find(&checker->types, type->ident_symbol, sizeof(TCType));
    if (type == NULL) {
      TCType out_val;
      out_val.kind = TCError;
      char *char_array = dyn_array_new(char);
      String type_node_str = ast_node_type_str(&char_array, type_node);
      out_val.err = error_new(string_new("couldn't find type"));
      error_array_add(&out_val.err, type_node->range, type_node_str);
      return out_val;
    }

    return *type;
  }

  if (type_node->kind != ASTStruct)
    return type_checker_convert_primitive(type_node);
  if (type_node->struct_has_ident) {
    type = hash_find(&checker->struct_types, type_node->struct_ident,
                     sizeof(TCType));
    if (type == NULL) {
      TCType out_val;
      out_val.kind = TCError;
      char *char_array = dyn_array_new(char);
      String type_node_str = ast_node_type_str(&char_array, type_node);
      out_val.err = error_new(string_new("couldn't find type"));
      error_array_add(&out_val.err, type_node->range, type_node_str);
      return out_val;
    }

    return *type;
  }

  char *char_array = dyn_array_new(char);
  String type_node_str = ast_node_type_str(&char_array, type_node);
  debug("called type_checker_lookup_type on wrong kind of value %.*s",
        (uint32_t)type_node_str.len, type_node_str.str);
  exit(1);
}

TCType type_checker_check_member(TypeChecker *checker, TCType *expr_type,
                                 uint32_t member, bool is_ptr) {
  TCType type;

  if (is_ptr && expr_type->pointer_count != 1) {
    type.kind = TCError;
    type.err = error_new(string_new("type cannot be dereferenced with arrow"));
    return type;
  }

  if (!is_ptr && expr_type->pointer_count != 0) {
    type.kind = TCError;
    type.err = error_new(string_new("type cannot be dereferenced with dot"));
    return type;
  }

  if (expr_type->kind != ASTStruct) {
    type.kind = TCError;
    type.err = error_new(string_new("type cannot be dereferenced"));
    return type;
  }

  uint32_t len = dyn_array_len(expr_type->members);
  for (uint32_t i = 0; i < len; i++) {
    StructMember *type_member = &expr_type->members[i];
    if (type_member->is_anonymous) {
      TCType result = type_checker_check_member(checker, &type_member->type,
                                                member, is_ptr);
      if (result.kind != TCError)
        return result;
    }

    if (member == type_member->ident)
      return type_member->type;
  }

  type.kind = TCError;
  type.err = error_new(string_new("couldn't find member"));
  return type;
}

/*
ASTNodeType type_checker_add_type_defn(TypeChecker *checker, ASTNodeType *type,
                                       uint32_t *ident) {
  if (check(type)->kind == ASTStruct) {
    if (!type->struct_is_defn) {
      debug("trying to add type that isn't a defintion");
      exit(1);
    }

    if (type->struct_has_ident) {
      ASTNodeType *slot = hash_insert(&checker->struct_types,
                                      type->struct_ident, sizeof(ASTNodeType));
      *slot = *type;
    }
  }

  if (ident == NULL)
    return *type;

  ASTNodeType *slot = hash_insert(&checker->types, *ident, sizeof(ASTNodeType));
  *slot = *type;
  return *type;
}

TCFunc type_checker_add_function(TypeChecker *checker, ASTNodeStmt *stmt) {
  TCFunc type;
  if (stmt->kind != ASTFuncBlock) {
    debug("trying to add non-function to function symbol table");
    exit(1);
  }
}
*/
