typedef struct {
  Hash types;
  Hash functions;
  Hash symbols;
} TypeChecker;

ASTNodeType type_check_global_decl(TypeChecker *, ASTNodeStmt *);
ASTNodeType type_check_expr(TypeChecker *, ASTNodeExpr *);
