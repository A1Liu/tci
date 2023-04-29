use crate::api::*;

#[derive(Debug, Clone, Copy, StructOfArray)]
pub struct AstNode {
    pub kind: AstNodeKind,
    pub start: u32,
    pub depth: u16,
    pub data: u64,

    // pub id: u32,
    // pub parent: u32,

    // Some order to be used to decide which children are first
    pub post_order: u32,
}

macro_attr! {
#[derive(Debug, Clone, Copy, EnumFromInner!)]
#[cfg_attr(all(debug_assertions), derive(Serialize, Deserialize))]
pub enum AstNodeKind {
    Expr(AstExpr),
    Statement(AstStatement),
    DerivedDeclarator(AstDerivedDeclarator),
    Declarator(AstDeclarator),
    Specifier(AstSpecifier),
    Declaration(AstDeclaration),
    FunctionDefinition(AstFunctionDefinition),
}
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(all(debug_assertions), derive(Serialize, Deserialize))]
pub enum AstExpr {
    IntLit,     // data: i32
    LongLit,    // data: i64
    ULit,       // data: u32
    ULongLit,   // data: u64
    FloatLit,   // data: f32
    DoubleLit,  // data: f64
    CharLit,    // data: i8
    StringLit,  // data: end of string text
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
#[cfg_attr(all(debug_assertions), derive(Serialize, Deserialize))]
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

    Comma,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
#[cfg_attr(all(debug_assertions), derive(Serialize, Deserialize))]
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
#[cfg_attr(all(debug_assertions), derive(Serialize, Deserialize))]
pub enum StructDeclaration {
    Struct,
    Union,
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(all(debug_assertions), derive(Serialize, Deserialize))]
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
    Ret,                // children: optional expression to return
    Break,
    Continue,
}

/// A derived declarator. This is the `*const` part of
/// `int *const a`, or the `[3]` part of `int b[3]`
///
/// Children: AstSpecifer for each type qualifier
#[derive(Debug, Clone, Copy)]
#[cfg_attr(all(debug_assertions), derive(Serialize, Deserialize))]
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

/// children: a AstDerivedDeclarator for each derived declarator
#[derive(Debug, Clone, Copy)]
#[cfg_attr(all(debug_assertions), derive(Serialize, Deserialize))]
pub enum AstDeclarator {
    Abstract,
    /// data: Symbol
    Ident,
    /// The declarator forwards to another declarator using parentheses
    NestedWithChild,
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(all(debug_assertions), derive(Serialize, Deserialize))]
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

    Struct(StructDeclaration), // children: ident declaration of struct, field declarations of struct
    Ident,                     // data: Symbol
}

/// A typical declaration; this is a stand-in for
/// int *i[1] = {NULL}; or something similar
///
/// Children: AstSpecifier for each specifier, AstStructDeclaration if necessary, an AstInitDeclarator for each declared variable
#[derive(Debug, Clone, Copy)]
#[cfg_attr(all(debug_assertions), derive(Serialize, Deserialize))]
pub struct AstDeclaration;

/// A typical declaration; this is a stand-in for
/// int *i[1] = {NULL}; or something similar
///
/// Data: DeclarationSpecifiers
/// Children: AstSpecifier for each specifier, san AstDeclarator, and all the statements associated with the function
#[derive(Debug, Clone, Copy)]
#[cfg_attr(all(debug_assertions), derive(Serialize, Deserialize))]
pub struct AstFunctionDefinition;
