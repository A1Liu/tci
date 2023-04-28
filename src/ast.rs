#[derive(Clone, StructOfArray)]
pub struct AstNode {
    pub kind: AstNodeKind,
    pub id: u32,
    pub start: u32,
    pub depth: u16,
    pub parent: u32,
    pub data: u64,

    // Some order to be used to decide which children are first
    pub pre_order: u32,
}

macro_attr! {
#[derive(Debug, Clone, Copy, EnumFromInner!)]
pub enum AstNodeKind {
    Expr(AstExpr),
    Statement(AstStatement),
    DerivedDeclarator(AstDerivedDeclarator),
    Declarator(AstDeclarator),
    InitDeclarator(AstInitDeclarator),
    Specifier(AstSpecifier),
    StructField(AstStructField),
    StructDeclaration(AstStructDeclaration),
    ParameterDeclaration(AstParameterDeclaration),
    Declaration(AstDeclaration),
    FunctionDefinition(AstFunctionDefinition),
}
}

#[derive(Debug, Clone, Copy)]
pub enum AstExpr {
    IntLit,     // data: i32
    LongLit,    // data: i64
    ULit,       // data: u32
    ULongLit,   // data: u64
    FloatLit,   // data: f32
    DoubleLit,  // data: f64
    CharLit,    // data: i8
    StringLit,  // data: end of string text
    ParenList,  // data: number of children of paren ; children: expressions in the paren list
    Ident,      // data: Symbol
    Assign,     // children: expression being assigned to, expression being assigned
    SizeofExpr, // children: expression that's being queried
    SizeofTy,   // children: type that's being queried
    Cast,       // children: expression being cased and type being cast to
    Member,     // data: field name ; children: base of expression
    PtrMember,  // data: field name ; children: base of expression
    Call,       // data: id of function parameter, children: function and children
    Ternary,    // data: id of condition parameter, children: condition, if_true, and if_false

    UnaryOp(UnaryOp),   // children: expression that's operated on
    BinOp(BinOp),       // children: operands
    BinOpAssign(BinOp), // children: expression being assigned to, expression being assigned
}

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
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
pub enum UnaryOp {
    Neg,
    BoolNot,
    BitNot,
    PostIncr,
    PostDecr,
    PreIncr,
    PreDecr,
    Deref,
    Ref,
}

/// Handles struct and union declarations:
/// struct a { int b; }
/// In the above, it would have children for each field
/// declaration, and a child for the identifier as well.
#[derive(Debug, Clone, Copy)]
pub enum AstStructDeclaration {
    Struct,
    Union,
}

#[derive(Debug, Clone, Copy)]
pub enum AstStatement {
    Labeled,            // data: label ; children: statement that is being labelled
    CaseLabeled,        // children: case value expression, statement that is being labelled
    DefaultCaseLabeled, // children: statement that is being labelled
    Goto,               // data: label ; children: statement that is being labelled
    Expr,               // children: expression
    Branch,             // children: condition, if_true body, if_false body
    Block,              // children: statements; maybe this is unnecessary
    For,                // children: start expression, condition, post expression, body
    ForDecl,            // children: declaration, condition, post expression, body
    While,              // children: condition expression, body
    DoWhile,            // children: body, condition expression
    Switch,             // children: switch expression, body
    RetVal,             // children: expression to return
    Ret,
    Break,
    Continue,
}

/// A derived declarator. This is the `*const` part of
/// `int *const a`, or the `[3]` part of `int b[3]`
///
/// Children: AstSpecifer for each type qualifier
#[derive(Debug, Clone, Copy)]
pub enum AstDerivedDeclarator {
    Pointer = 0,

    /// []
    ArrayUnknown = 1,
    /// `[*]`
    ArrayVariableUnknown = 2,
    /// `[10]`
    /// Children: size expression
    ArrayVariableExpression = 3,
    /// `[static 10]`
    /// Children: size expression
    ArrayStaticExpression = 4,

    /// x(int param1, char, long param3)
    /// children: AstParameterDeclarators
    Function = 5,

    /// x(int param1, char, long param3, ...)
    /// children: AstParameterDeclarators
    FunctionElipsis = 6,
}

/// struct a { int b; }
/// StructField is the int b; part.
/// It has children that represent the declarators.
///
/// Children: AstSpecifiers for the type and qualifiers, then a AstDeclarator for each declared variable.
#[derive(Debug, Clone, Copy)]
pub struct AstStructField {}

/// children: a AstDerivedDeclarator for each derived declarator
#[derive(Debug, Clone, Copy)]
pub enum AstDeclarator {
    Abstract,
    /// data: Symbol
    Ident,
    /// The declarator forwards to another declarator using parentheses
    NestedWithChild,
}

/// A declarator which can be initialized with a value
/// children: a AstDeclarator and an optional AstExpr
#[derive(Debug, Clone, Copy)]
pub struct AstInitDeclarator {}

#[derive(Debug, Clone, Copy)]
pub enum AstSpecifier {
    Extern,
    Static,
    Typedef,
    Register,
    Inline,
    Noreturn,

    Const,
    Volatile,
    Restrict,
    Atomic,

    Void,
    Char,
    Short,
    Int,
    Long,

    Float,
    Double,

    UChar,
    UShort,
    UInt,
    ULong,

    Struct, // children: ident declaration of struct, field declarations of struct
    Union,  // children: ident declaration of union, field declarations of union
    Ident,  // data: Symbol
}

/// A declaration of a parameter.
///
/// Children: AstSpecifier for each specifier, AstStructDeclaration if necessary, optional declarator
#[derive(Debug, Clone, Copy)]
pub struct AstParameterDeclaration {}

/// A typical declaration; this is a stand-in for
/// int *i[1] = {NULL}; or something similar
///
/// Children: AstSpecifier for each specifier, AstStructDeclaration if necessary, an AstInitDeclarator for each declared variable
#[derive(Debug, Clone, Copy)]
pub struct AstDeclaration {}

/// A typical declaration; this is a stand-in for
/// int *i[1] = {NULL}; or something similar
///
/// Data: DeclarationSpecifiers
/// Children: AstSpecifier for each specifier, san AstDeclarator, and all the statements associated with the function
#[derive(Debug, Clone, Copy)]
pub struct AstFunctionDefinition {}
