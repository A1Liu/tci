use crate::util::*;

#[derive(Debug, Clone, Copy)]
pub struct ASTProgram<'a> {
    pub stmts: &'a [GlobalStmt<'a>],
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
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'a> {
    IntLiteral(i32),
    CharLiteral(i8),
    StringLiteral(&'a str),
    SizeofType {
        sizeof_type: ASTType<'a>,
        pointer_count: u32,
    },
    SizeofExpr(&'a Expr<'a>),
    Ident(u32),
    BinOp(BinOp, &'a Expr<'a>, &'a Expr<'a>),
    UnaryOp(UnaryOp, &'a Expr<'a>),
    Not(&'a Expr<'a>),
    Assign(&'a Expr<'a>, &'a Expr<'a>),
    MutAssign {
        target: &'a Expr<'a>,
        value: &'a Expr<'a>,
        op: BinOp,
    },
    Call {
        function: &'a Expr<'a>,
        params: &'a [Expr<'a>],
    },
    Cast {
        cast_to: ASTType<'a>,
        pointer_count: u32,
        cast_to_loc: CodeLoc,
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
    BraceList(&'a [Expr<'a>]),
    ParenList(&'a [Expr<'a>]),
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
pub struct DeclReceiver<'a> {
    pub pointer_count: u32,
    pub ident: u32,
    pub array_dims: &'a [u32],
    pub loc: CodeLoc,
}

#[derive(Debug)]
pub struct InnerStructDecl<'a> {
    pub decl_type: ASTType<'a>,
    pub recv: DeclReceiver<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone)]
pub enum ParamKind<'a> {
    StructLike {
        decl_type: ASTType<'a>,
        recv: DeclReceiver<'a>,
    },
    TypeOnly {
        decl_type: ASTType<'a>,
        pointer_count: u32,
        array_dims: &'a [u32],
    },
    Vararg,
}

#[derive(Debug, Clone)]
pub struct ParamDecl<'a> {
    pub kind: ParamKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum StructDecl<'a> {
    Named(u32),
    NamedDef {
        ident: u32,
        members: &'a [InnerStructDecl<'a>],
    },
    Unnamed(&'a [InnerStructDecl<'a>]),
}

#[derive(Debug, Clone, Copy)]
pub struct Decl<'a> {
    pub recv: DeclReceiver<'a>,
    pub loc: CodeLoc,
    pub expr: Expr<'a>,
}

#[derive(Debug, Clone)]
pub enum GlobalStmtKind<'a> {
    Func {
        // TODO func declaration span separate from global_stmt.span
        return_type: ASTType<'a>,
        pointer_count: u32,
        ident: u32,
        params: &'a [ParamDecl<'a>],
        body: &'a [Stmt<'a>],
    },
    FuncDecl {
        return_type: ASTType<'a>,
        pointer_count: u32,
        ident: u32,
        params: &'a [ParamDecl<'a>],
    },
    StructDecl(StructDecl<'a>),
    Typedef {
        ast_type: ASTType<'a>,
        recv: DeclReceiver<'a>,
    },
    Decl {
        decl_type: ASTType<'a>,
        decls: &'a [Decl<'a>],
    },
    PragmaEnableBuiltins,
}

#[derive(Debug, Clone)]
pub struct GlobalStmt<'a> {
    pub kind: GlobalStmtKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum ASTTypeKind<'a> {
    Struct(StructDecl<'a>),
    Ident(u32),
    Int,
    Long,
    Char,
    Unsigned,
    Void,
    LongLong,
    UnsignedLong,
    UnsignedLongLong,
    UnsignedChar,
}

#[derive(Debug, Clone, Copy)]
pub struct ASTType<'a> {
    pub kind: ASTTypeKind<'a>,
    pub is_static: bool,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone)]
pub enum StmtKind<'a> {
    Decl {
        decl_type: ASTType<'a>,
        decls: &'a [Decl<'a>],
    },
    Expr(Expr<'a>),
    Nop,
    Ret,
    RetVal(Expr<'a>),
    Branch {
        if_cond: Expr<'a>,
        if_body: Block<'a>,
        else_body: Block<'a>,
    },
    Block(Block<'a>),
    For {
        at_start: Expr<'a>,
        condition: Expr<'a>,
        post_expr: Expr<'a>,
        body: Block<'a>,
    },
    ForDecl {
        at_start_decl_type: ASTType<'a>,
        at_start: &'a [Decl<'a>],
        condition: Expr<'a>,
        post_expr: Expr<'a>,
        body: Block<'a>,
    },
    While {
        condition: Expr<'a>,
        body: Block<'a>,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct Block<'a> {
    pub stmts: &'a [Stmt<'a>],
    pub loc: CodeLoc,
}
