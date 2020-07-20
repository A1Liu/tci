pub use crate::ast::{Expr, ExprKind};
use core::ops::Range;

#[derive(Debug)]
pub struct Decl {
    pub decl_type: ASTType,
    pub ident: Option<u32>,
    pub value: Option<Expr>,
    pub range: Range<u32>,
}

pub enum StmtKind {
    Decl(Decl),
}

pub struct Stmt {
    kind: StmtKind,
    range: Range<u32>,
}

#[derive(Debug)]
pub enum ASTTypeKind {
    Int,
    Ident(u32),
    Struct { ident: u32 },
    Char,
    Void,
}

#[derive(Debug)]
pub struct ASTType {
    pub kind: ASTTypeKind,
    pub range: Range<u32>,
    pub pointer_count: u32,
}
