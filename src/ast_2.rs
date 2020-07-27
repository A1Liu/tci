pub use crate::ast::{ASTType, ASTTypeKind, Decl, Expr, ExprKind};
use core::ops::Range;

pub enum StmtKind<'a> {
    Decl(Decl<'a>),
    Expr(Expr),
    Nop,
    Ret,
    RetVal(Expr),
    Branch {
        if_cond: Expr,
        if_body: &'a [Stmt<'a>],
        else_body: Option<&'a [Stmt<'a>]>,
    },
    Block(&'a [Stmt<'a>]),
    For {
        at_start: Expr,
        condition: Expr,
        post_expr: Expr,
        body: &'a [Stmt<'a>],
    },
    ForDecl {
        at_start: Decl<'a>,
        condition: Expr,
        post_expr: Expr,
        body: &'a [Stmt<'a>],
    },
}

pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub range: Range<u32>,
}
