use crate::api::*;

#[derive(Clone, StructOfArray)]
pub struct AstNode {
    pub kind: AstNodeKind,
    pub id: u32,
    pub start: u32,
    pub depth: u16,
    pub parent: u32,
    pub data: u64,

    // Some order to be used to decide which children are first;
    // Haven't decided the exact order yet.
    pub parse_order: u32,
}

#[derive(Clone, Copy)]
pub enum AstNodeKind {
    Expr(AstExpr),
    Type(AstType),
}

#[derive(Debug, Clone, Copy)]
pub enum AstExpr {
    IntLit,           // data: i32
    LongLit,          // data: i64
    ULit,             // data: u32
    ULongLit,         // data: u64
    FloatLit,         // data: f32
    DoubleLit,        // data: f64
    CharLit,          // data: i8
    StringLit,        // data: end of string text
    ParenList,        // data: number of children of paren ; children: expressions in the paren list
    Ident,            // data: Symbol
    BinOp(BinOp),     // children: operands
    UnaryOp(UnaryOp), // children: expression that's operated on
    Assign(AssignOp), // children: expression being assigned to, expression being assigned
    SizeofExpr,       // children: expression that's being queried
    SizeofTy,         // children: type that's being queried
    Cast,             // children: expression being cased and type being cast to
    Member,           // data: field name ; children: base of expression
    PtrMember,        // data: field name ; children: base of expression
    Call,             // data: id of function parameter, children: function and children
    Ternary,          // data: id of condition parameter, children: condition, if_true, and if_false
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
pub enum AssignOp {
    Assign,
    MutAssign(BinOp),
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

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
#[repr(packed)]
pub struct DeclarationSpecifiers {
    extern_: bool,
    static_: bool,
    typedef: bool,
    register: bool,
    inline: bool,
    noreturn: bool,
}

#[derive(Debug, Clone, Copy)]
#[repr(packed)]
pub struct TypeQualifiers {
    const_: bool,
    volatile: bool,
    restrict: bool,
    atomic: bool,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
pub enum AstType {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Pointer,
    Struct, // children: ident declaration of struct, field declarations of struct
    Union,  // children: ident declaration of union, field declarations of union
    Ident(Symbol),
}
