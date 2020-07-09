typedef enum {
  TCError,
  TCStruct,
  TCInt,
  TCIdent,
  TCChar,
  TCVoid,
} TCTypeKind;

typedef struct {
  TCTypeKind kind;
  union {
    struct {
      Range range;
      union {
        uint32_t ident_symbol;
        uint32_t int_value;
      };
    };
    Error err;
  };

} TCType;

typedef struct {
  Hash types;
  Hash functions;
  Hash symbols;
} TypeChecker;

ASTNodeType type_check_global_decl(TypeChecker *, ASTNodeStmt *);
ASTNodeType type_check_expr(TypeChecker *, ASTNodeExpr *);
