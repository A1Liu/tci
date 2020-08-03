pub use crate::ast::{ASTType, ASTTypeKind, Decl};
use core::ops::Range;

#[derive(Debug)]
pub enum ExprKind<'a> {
    IntLiteral(u32),
    Ident(u32),
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
    PostIncr(&'a Expr<'a>),
    PostDecr(&'a Expr<'a>),
}

#[derive(Debug)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub range: Range<u32>,
}

pub enum StmtKind<'a> {
    Decl(Decl<'a>),
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
        at_start: Decl<'a>,
        condition: Expr<'a>,
        post_expr: Expr<'a>,
        body: &'a [Stmt<'a>],
    },
    While {
        condition: Expr<'a>,
        body: &'a [Stmt<'a>],
    },
}

pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub range: Range<u32>,
}
