use crate::util::*;
use core::sync::atomic::{AtomicUsize, Ordering};

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

#[derive(Debug, Clone, Copy, strum::AsRefStr)]
pub enum ExprKind {
    IntLit(i32),
    LongLit(i64),
    ULit(u32),
    ULongLit(u64),
    FloatLit(f32),
    DoubleLit(f64),
    CharLit(i8),
    StringLit(&'static str),
    ParenList(&'static [Expr]),
    Ident(u32),
    BinOp(BinOp, &'static Expr, &'static Expr),
    Assign {
        op: AssignOp,
        to: &'static Expr,
        val: &'static Expr,
    },
    SizeofExpr(&'static Expr),
    SizeofTy(TypeName),
    Cast {
        to: TypeName,
        from: &'static Expr,
    },
    Member {
        member: u32,
        base: &'static Expr,
    },
    PtrMember {
        member: u32,
        base: &'static Expr,
    },
    UnaryOp(UnaryOp, &'static Expr),
    Call {
        function: &'static Expr,
        params: &'static [Expr],
    },
    Ternary {
        condition: &'static Expr,
        if_true: &'static Expr,
        if_false: &'static Expr,
    },
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
    Register,
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
pub enum StructTypeKind {
    Named(u32),
    NamedDecl {
        ident: u32,
        declarations: &'static [StructField],
    },
    UnnamedDecl {
        declarations: &'static [StructField],
    },
}

#[derive(Debug, Clone, Copy)]
pub struct StructType {
    pub kind: StructTypeKind,
    pub loc: CodeLoc,
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

#[derive(Debug, Clone, Copy, strum::AsRefStr)]
pub enum StatementKind {
    Labeled {
        label: u32,
        label_loc: CodeLoc,
        labeled: &'static Statement,
    },
    CaseLabeled {
        case_value: Expr,
        labeled: &'static Statement,
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
