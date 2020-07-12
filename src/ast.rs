use crate::lexer::Token;
use core::ops::Range;

#[derive(Debug)]
pub enum ExprKind {
    IntLiteral,
    Ident,
    Uninit,
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub range: Range<u32>,
}

#[derive(Debug)]
pub struct Decl<'a> {
    pub decl_type: ASTType<'a>,
    pub ident: Option<u32>,
    pub value: Option<Expr>,
    pub range: Range<u32>,
}

#[derive(Debug)]
pub enum GlobalStmtKind<'a> {
    Func {
        return_type: ASTType<'a>,
        ident: u32,
        params: &'a [Decl<'a>],
        body: &'a [Token],
    },
    FuncDecl {
        return_type: ASTType<'a>,
        ident: u32,
        params: &'a [Decl<'a>],
    },
    Decl(Decl<'a>),
}

#[derive(Debug)]
pub struct GlobalStmt<'a> {
    pub kind: GlobalStmtKind<'a>,
    pub range: Range<u32>,
}

#[derive(Debug)]
pub enum ASTTypeKind<'a> {
    Int,
    Ident(u32),
    Struct {
        ident: u32,
    },
    StructDefn {
        ident: Option<u32>,
        members: &'a [Decl<'a>],
    },
    Char,
    Void,
}

#[derive(Debug)]
pub struct ASTType<'a> {
    pub kind: ASTTypeKind<'a>,
    pub range: Range<u32>,
    pub pointer_count: u32,
}
