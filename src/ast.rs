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

#[derive(Debug, Clone, Copy)]
pub enum AstNodeKind {
    Expr(AstExpr),
    Statement(AstStatement),
    DerivedDeclarator(AstDerivedDeclarator),
    Declarator(AstDeclarator),
    InitDeclarator(AstInitDeclarator),
    StructField(AstStructField),
    StructDeclaration(AstStructDeclaration),
    ParameterDeclaration(AstParameterDeclaration),
    Declaration(AstDeclaration),
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

// NOTE: THIS ACTUALLY NEEDS TO BE 4 BITS, NOT 8
// BE CAREFUL
#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
#[repr(u8)]
pub enum TypeSpecifier {
    Void = 0,
    Char = 1,
    Short = 2,
    Int = 3,
    Long = 4,

    Float = 5,
    Double = 6,

    UChar = 7,
    UShort = 8,
    UInt = 9,
    ULong = 10,

    Struct = 11, // children: ident declaration of struct, field declarations of struct
    Union = 12,  // children: ident declaration of union, field declarations of union
    Ident = 13,  // data: Symbol
}

impl From<u8> for TypeSpecifier {
    fn from(orig: u8) -> Self {
        if orig < 14 {
            return unsafe { core::mem::transmute(orig) };
        }

        panic!("invalid byte for type specifier");
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
#[bitfield(u8)]
pub struct AstDerivedDeclarator {
    #[bits(4)]
    qualifiers: TypeQualifiers,
    #[bits(4)]
    kind: DerivedDeclaratorKind,
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum DerivedDeclaratorKind {
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

impl From<u8> for DerivedDeclaratorKind {
    fn from(orig: u8) -> Self {
        if orig < 7 {
            return unsafe { core::mem::transmute(orig) };
        }

        panic!("invalid byte for derived declarator kind");
    }
}

impl Into<u8> for DerivedDeclaratorKind {
    fn into(self) -> u8 {
        return self as u8;
    }
}

/// Describes the type in a declaration
#[derive(Debug, Clone, Copy)]
pub struct BaseType {
    qualifiers: TypeQualifiers,
    specifier: TypeSpecifier,
}

/// struct a { int b; }
/// StructField is the int b; part.
/// It has children that represent the declarators.
///
/// Data: BaseType
/// Children: AstStructDeclaration if necessary, then a AstDeclarator for each declared variable.
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

/// Specifiers for things like storage class and typedefs
/// Used for declarations and not for types.
#[bitfield(u8)]
pub struct NonTypeSpecifiers {
    extern_: bool,
    static_: bool,
    typedef: bool,
    register: bool,
    inline: bool,
    noreturn: bool,

    #[bits(2)]
    __ignore: usize,
}

/// Specifiers which apply to declarations
#[derive(Debug, Clone, Copy)]
pub struct DeclarationSpecifiers {
    non_type_specifiers: NonTypeSpecifiers,
    qualifiers: TypeQualifiers,
    specifier: TypeSpecifier,
}

/// A declaration of a parameter.
///
/// Data: DeclarationSpecifiers
/// Children: AstStructDeclaration if necessary, optional declarator
#[derive(Debug, Clone, Copy)]
pub struct AstParameterDeclaration {}

/// A typical declaration; this is a stand-in for
/// int *i[1] = {NULL}; or something similar
///
/// Data: DeclarationSpecifiers
/// Children: AstStructDeclaration if necessary, an AstInitDeclarator for each declared variable
#[derive(Debug, Clone, Copy)]
pub struct AstDeclaration {}

#[test]
fn bitfield_correctness() {
    #[bitfield(u8)]
    struct A {
        #[bits(4)]
        qualifiers: TypeQualifiers,
        #[bits(4)]
        ty: TypeSpecifier,
    }
    let qualifiers = TypeQualifiers::new()
        .with_const_(true)
        .with_restrict(true)
        .with_volatile(true)
        .with_atomic(true);
    let ty = A::new().with_qualifiers(qualifiers);
    assert_eq!(ty.qualifiers(), qualifiers);
    // println!("{:x}", u8::from(qualifiers));
}
