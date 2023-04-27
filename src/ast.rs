use num_derive::FromPrimitive;

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
    Statement(AstStatement),
    DerivedDeclarator(AstDerivedDeclarator),
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

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy, FromPrimitive)]
#[repr(u8)]
pub enum TypeSpecifier {
    Void = 0,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Struct, // children: ident declaration of struct, field declarations of struct
    Union,  // children: ident declaration of union, field declarations of union
    Ident,  // data: Symbol
}

impl From<u8> for TypeSpecifier {
    fn from(orig: u8) -> Self {
        return FromPrimitive::from_u8(orig).unwrap();
    }
}

impl Into<u8> for TypeSpecifier {
    fn into(self) -> u8 {
        return self as u8;
    }
}

#[bitfield(u8)]
#[derive(PartialEq, Eq)]
pub struct TypeQualifiers {
    // The order of this stuff matters!
    // These first four values are in the least-significant 4 digits
    // of the byte, meaning that when this object is stored in 4 bits,
    // no information is lost. If
    const_: bool,
    volatile: bool,
    restrict: bool,
    atomic: bool,

    #[bits(4)]
    __ignore: usize,
}

#[test]
fn bitfield_correctness() {
    let qualifiers = TypeQualifiers::new()
        .with_const_(true)
        .with_restrict(true)
        .with_volatile(true)
        .with_atomic(true);
    let ty = AstType::new().with_qualifiers(qualifiers);
    assert_eq!(ty.qualifiers(), qualifiers);
    // println!("{:x}", u8::from(qualifiers));
}

#[bitfield(u8)]
pub struct AstType {
    #[bits(4)]
    qualifiers: TypeQualifiers,
    // This is just BARELY enough bits to represent this enum.
    #[bits(4)]
    ty: TypeSpecifier,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub enum AstDerivedDeclarator {
    Pointer,
    ArrayUnsized,
    ArraySized, // children: size expr
    Function,   // children
    EmptyFunction,
}
