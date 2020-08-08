use crate::*;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind<'a> {
    IntLiteral(u32),
    Ident(u32),
    Add(&'a Expr<'a>, &'a Expr<'a>),
    Subtract(&'a Expr<'a>, &'a Expr<'a>),
    Call {
        function: &'a Expr<'a>,
        params: &'a [Expr<'a>],
    },
    Member {
        expr: &'a Expr<'a>,
        member: u32,
    },
    PtrMember {
        expr: &'a Expr<'a>,
        member: u32,
    },
    Index {
        ptr: &'a Expr<'a>,
        index: &'a Expr<'a>,
    },
    List(&'a [Expr<'a>]),
    PostIncr(&'a Expr<'a>),
    PostDecr(&'a Expr<'a>),
    Uninit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub range: Range,
}

#[derive(Debug)]
pub struct InnerStructDecl {
    pub decl_type: ASTType,
    pub pointer_count: u32,
    pub ident: u32,
    pub range: Range,
}

#[derive(Debug)]
pub struct StructDecl<'a> {
    pub ident: u32,
    pub ident_range: Range,
    pub members: Option<&'a [InnerStructDecl]>,
    pub range: Range,
}

#[derive(Debug, Clone)]
pub struct Decl<'a> {
    pub pointer_count: u32,
    pub ident: u32,
    pub range: Range,
    pub expr: Expr<'a>,
}

#[derive(Debug)]
pub enum GlobalStmtKind<'a> {
    Func {
        return_type: ASTType,
        pointer_count: u32,
        ident: u32,
        params: &'a [InnerStructDecl],
        body: &'a [Stmt<'a>],
    },
    FuncDecl {
        return_type: ASTType,
        pointer_count: u32,
        ident: u32,
        params: &'a [InnerStructDecl],
    },
    StructDecl(StructDecl<'a>),
    Decl {
        decl_type: ASTType,
        decls: &'a [Decl<'a>],
    },
}

#[derive(Debug)]
pub struct GlobalStmt<'a> {
    pub kind: GlobalStmtKind<'a>,
    pub range: Range,
}

#[derive(Debug)]
pub enum ASTTypeKind {
    Int,
    Struct { ident: u32 },
    Char,
    Void,
}

#[derive(Debug)]
pub struct ASTType {
    pub kind: ASTTypeKind,
    pub range: Range,
}

#[derive(Debug)]
pub enum StmtKind<'a> {
    Decl {
        decl_type: ASTType,
        decls: &'a [Decl<'a>],
    },
    Expr(Expr<'a>),
    Nop,
    Ret,
    RetVal(Expr<'a>),
    Branch {
        if_cond: Expr<'a>,
        if_body: &'a [Stmt<'a>],
        else_body: Option<&'a [Stmt<'a>]>,
    },
    Block(&'a [Stmt<'a>]),
    For {
        at_start: Expr<'a>,
        condition: Expr<'a>,
        post_expr: Expr<'a>,
        body: &'a [Stmt<'a>],
    },
    ForDecl {
        at_start_decl_type: ASTType,
        at_start: &'a [Decl<'a>],
        condition: Expr<'a>,
        post_expr: Expr<'a>,
        body: &'a [Stmt<'a>],
    },
    While {
        condition: Expr<'a>,
        body: &'a [Stmt<'a>],
    },
}

#[derive(Debug)]
pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub range: Range,
}
