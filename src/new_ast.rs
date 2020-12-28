use crate::util::*;

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Index,
    Lt,
    Gt,
    Leq,
    Geq,
    Eq,
    Neq,
    LShift,
    RShift,
    BitAnd,
    BitXor,
    BitOr,
    BoolAnd,
    BoolOr,

    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignMod,
    AssignLShift,
    AssignRShift,
    AssignBitAnd,
    AssignBitXor,
    AssignBitOr,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
pub enum UnaryOp {
    Neg,
    BoolNot,
    BitNot,
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'a> {
    IntLiteral(i32),
    CharLiteral(i8),
    StringLiteral(&'a str),
    SizeofType(&'a TypeName<'a>),
    SizeofExpr(&'a Expr<'a>),
    Ident(u32),
    BinOp(BinOp, &'a Expr<'a>, &'a Expr<'a>),
    UnaryOp(UnaryOp, &'a Expr<'a>),
    Call {
        function: &'a Expr<'a>,
        params: &'a [Expr<'a>],
    },
    Cast {
        type_name: TypeName<'a>,
        expr: &'a Expr<'a>,
    },
    Member {
        base: &'a Expr<'a>,
        member: u32,
    },
    PtrMember {
        base: &'a Expr<'a>,
        member: u32,
    },
    PostIncr(&'a Expr<'a>),
    PostDecr(&'a Expr<'a>),
    Ref(&'a Expr<'a>),
    Deref(&'a Expr<'a>),
    Ternary {
        condition: &'a Expr<'a>,
        if_true: &'a Expr<'a>,
        if_false: &'a Expr<'a>,
    },
    Uninit,
}

#[derive(Debug, Clone, Copy)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct Declaration<'a> {
    pub specifiers: &'a [DeclarationSpecifier<'a>],
    pub declarators: &'a [InitDeclarator<'a>],
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum DeclarationSpecifier<'a> {
    StorageClass(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier<'a>),
    TypeQualifier(TypeQualifier),
    Inline,   // __inline__
    Noreturn, // _Noreturn
}

#[derive(Debug, Clone, Copy)]
pub struct InitDeclarator<'a> {
    pub declarator: Declarator<'a>,
    pub initializer: Option<Initializer<'a>>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum StorageClassSpecifier {
    Typedef,
    Static,
    // Extern,
    // ThreadLocal,
    // Auto,
    // Register,
}

#[derive(Debug, Clone, Copy)]
pub enum TypeSpecifierKind<'a> {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Struct(StructType<'a>),
    Union(StructType<'a>),
    Ident(u32),
}

#[derive(Debug, Clone, Copy)]
pub struct TypeSpecifier<'a> {
    pub kind: TypeSpecifierKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum TypeQualifierKind {
    Const,
    Volatile,
    Restrict,
    Atomic,
}

#[derive(Debug, Clone, Copy)]
pub struct TypeQualifier {
    pub kind: TypeQualifierKind,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct StructType<'a> {
    pub ident: n32,
    pub declarations: &'a [StructField<'a>],
}

#[derive(Debug, Clone, Copy)]
pub struct StructField<'a> {
    pub specifiers: &'a [SpecifierQualifier<'a>],
    pub declarators: &'a [StructDeclarator<'a>],
}

#[derive(Debug, Clone, Copy)]
pub enum SpecifierQualifier<'a> {
    TypeSpecifier(TypeSpecifier<'a>),
    TypeQualifier(TypeQualifier),
}

#[derive(Debug, Clone, Copy)]
pub struct TypeName<'a> {
    pub specifiers: &'a [SpecifierQualifier<'a>],
    pub declarator: Option<Declarator<'a>>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct StructDeclarator<'a> {
    pub declarator: Declarator<'a>,
    pub bit_width: Option<&'a Expr<'a>>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum DeclaratorKind<'a> {
    Abstract,
    Identifier(u32),
    Declarator(&'a Declarator<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct Declarator<'a> {
    pub kind: DeclaratorKind<'a>,
    pub derived: &'a [DerivedDeclarator<'a>],
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum DerivedDeclaratorKind<'a> {
    Pointer(&'a [TypeQualifier]),
    Array(ArrayDeclarator<'a>),
    Function(FunctionDeclarator<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct DerivedDeclarator<'a> {
    pub kind: DerivedDeclaratorKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct ArrayDeclarator<'a> {
    pub qualifiers: TypeQualifier,
    pub size: ArraySize<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDeclarator<'a> {
    pub parameters: &'a [ParameterDeclaration<'a>],
    pub varargs: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum ArraySizeKind<'a> {
    Unknown,
    VariableUnknown,
    VariableExpression(&'a Expr<'a>),
    StaticExpression(&'a Expr<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct ArraySize<'a> {
    pub kind: ArraySizeKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct ParameterDeclaration<'a> {
    pub specifiers: &'a [DeclarationSpecifier<'a>],
    pub declarator: Option<Declarator<'a>>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum Initializer<'a> {
    Expr(&'a Expr<'a>),
    List(&'a [Expr<'a>]), // TODO support initializer list syntax
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDefinition<'a> {
    pub specifiers: &'a [DeclarationSpecifier<'a>],
    pub declarator: &'a Declarator<'a>,
    pub declarations: &'a [Declaration<'a>],
    pub statements: &'a [Statement<'a>],
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct Block<'a> {
    pub stmts: &'a [Statement<'a>],
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum StatementKind<'a> {
    Declaration(Declaration<'a>),
    Expr(Expr<'a>),
    Ret,
    RetVal(Expr<'a>),
    Branch {
        if_cond: Expr<'a>,
        if_body: Block<'a>,
        else_body: Block<'a>,
    },
    Block(Block<'a>),
    For {
        at_start: Option<Expr<'a>>,
        condition: Option<Expr<'a>>,
        post_expr: Option<Expr<'a>>,
        body: Block<'a>,
    },
    ForDecl {
        decl: Option<Declaration<'a>>,
        condition: Option<Expr<'a>>,
        post_expr: Option<Expr<'a>>,
        body: Block<'a>,
    },
    While {
        condition: Expr<'a>,
        body: Block<'a>,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy)]
pub struct Statement<'a> {
    pub kind: StatementKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum GlobalStatementKind<'a> {
    Declaration(Declaration<'a>),
    FunctionDefinition(FunctionDefinition<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct GlobalStatement<'a> {
    pub kind: GlobalStatementKind<'a>,
    pub loc: CodeLoc,
}
