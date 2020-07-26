pub use crate::ast::{ASTType, ASTTypeKind, Decl, Expr, ExprKind};
use core::ops::Range;

pub enum StmtKind<'a> {
    Decl(Decl<'a>),
}

pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub range: Range<u32>,
}
