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
pub enum ExprKind {
    IntLiteral(i32),
    CharLiteral(i8),
    StringLiteral(&'static str),
    ParenList(&'static [Expr]),
    SizeofType(&'static TypeName),
    SizeofExpr(&'static Expr),
    Ident(u32),
    BinOp(BinOp, &'static Expr, &'static Expr),
    UnaryOp(UnaryOp, &'static Expr),
    Call {
        function: &'static Expr,
        params: &'static [Expr],
    },
    Cast {
        type_name: TypeName,
        expr: &'static Expr,
    },
    Member {
        base: &'static Expr,
        member: u32,
    },
    PtrMember {
        base: &'static Expr,
        member: u32,
    },
    PostIncr(&'static Expr),
    PostDecr(&'static Expr),
    Ref(&'static Expr),
    Deref(&'static Expr),
    Ternary {
        condition: &'static Expr,
        if_true: &'static Expr,
        if_false: &'static Expr,
    },
    Uninit,
}

#[derive(Debug, Clone, Copy)]
pub struct Expr {
    pub kind: ExprKind,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct Declaration {
    pub specifiers: &'static [DeclarationSpecifier],
    pub declarators: &'static [InitDeclarator],
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum DeclarationSpecifierKind {
    Extern,
    Static,
    Typedef,
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
    Inline,   // __inline__
    Noreturn, // _Noreturn
}

#[derive(Debug, Clone, Copy)]
pub struct DeclarationSpecifier {
    pub kind: DeclarationSpecifierKind,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct InitDeclarator {
    pub declarator: Declarator,
    pub initializer: Option<Initializer>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Struct(StructType),
    Union(StructType),
    Ident(u32),
}

#[derive(Debug, Clone, Copy)]
pub enum TypeQualifierKind {
    // Const,
    Volatile,
    // Restrict,
    Atomic,
}

#[derive(Debug, Clone, Copy)]
pub struct TypeQualifier {
    pub kind: TypeQualifierKind,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct StructType {
    pub ident: n32,
    pub declarations: &'static [StructField],
}

#[derive(Debug, Clone, Copy)]
pub struct StructField {
    pub specifiers: &'static [SpecifierQualifier],
    pub declarators: &'static [StructDeclarator],
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum SpecifierQualifierKind {
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
}

#[derive(Debug, Clone, Copy)]
pub struct SpecifierQualifier {
    pub kind: SpecifierQualifierKind,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct TypeName {
    pub specifiers: &'static [SpecifierQualifier],
    pub declarator: Option<Declarator>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct StructDeclarator {
    pub declarator: Declarator,
    pub bit_width: Option<&'static Expr>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum DeclaratorKind {
    Abstract,
    Identifier(u32),
    Declarator(&'static Declarator),
}

#[derive(Debug, Clone, Copy)]
pub struct Declarator {
    pub kind: DeclaratorKind,
    pub derived: &'static [DerivedDeclarator],
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct PointerQuals {
    pub quals: &'static [TypeQualifier],
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum DerivedDeclaratorKind {
    Pointer(&'static [TypeQualifier]),
    Array(ArrayDeclarator),
    Function(FunctionDeclarator),
    EmptyFunction,
}

#[derive(Debug, Clone, Copy)]
pub struct DerivedDeclarator {
    pub kind: DerivedDeclaratorKind,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct ArrayDeclarator {
    pub qualifiers: &'static [TypeQualifier],
    pub size: ArraySize,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDeclarator {
    pub parameters: &'static [ParameterDeclaration],
    pub varargs: bool,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum ArraySizeKind {
    Unknown,
    // VariableUnknown,
    VariableExpression(&'static Expr),
    // StaticExpression(&'static Expr),
}

#[derive(Debug, Clone, Copy)]
pub struct ArraySize {
    pub kind: ArraySizeKind,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct ParameterDeclaration {
    pub specifiers: &'static [DeclarationSpecifier],
    pub declarator: Option<Declarator>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum InitializerKind {
    Expr(&'static Expr),
    List(&'static [Expr]), // TODO support initializer list syntax
}

#[derive(Debug, Clone, Copy)]
pub struct Initializer {
    pub kind: InitializerKind,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDefinition {
    pub specifiers: &'static [DeclarationSpecifier],
    pub ident: u32,
    pub pointer: &'static [PointerQuals],
    pub params: Option<FunctionDeclarator>,
    pub statements: Block,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum BlockItemKind {
    Statement(Statement),
    Declaration(Declaration),
}

#[derive(Debug, Clone, Copy)]
pub struct BlockItem {
    pub kind: BlockItemKind,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct Block {
    pub stmts: &'static [BlockItem],
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum StatementKind {
    Labeled {
        label: u32,
        label_loc: CodeLoc,
        stmt: &'static Statement,
    },
    CaseLabeled {
        case_value: Expr,
        stmt: &'static Statement,
    },
    DefaultCaseLabeled(&'static Statement),
    Goto {
        label: u32,
        label_loc: CodeLoc,
    },
    Expr(Expr),
    Ret,
    RetVal(Expr),
    Branch {
        if_cond: Expr,
        if_body: &'static Statement,
        else_body: Option<&'static Statement>,
    },
    Block(Block),
    For {
        at_start: Option<Expr>,
        condition: Option<Expr>,
        post_expr: Option<Expr>,
        body: &'static Statement,
    },
    ForDecl {
        decl: Declaration,
        condition: Option<Expr>,
        post_expr: Option<Expr>,
        body: &'static Statement,
    },
    While {
        condition: Expr,
        body: &'static Statement,
    },
    DoWhile {
        condition: Expr,
        body: &'static Statement,
    },
    Switch {
        expr: Expr,
        body: &'static Statement,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy)]
pub struct Statement {
    pub kind: StatementKind,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum GlobalStatementKind {
    Declaration(Declaration),
    FunctionDefinition(FunctionDefinition),
    Pragma(&'static str),
}

#[derive(Debug, Clone, Copy)]
pub struct GlobalStatement {
    pub kind: GlobalStatementKind,
    pub loc: CodeLoc,
}
