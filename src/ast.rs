use crate::lexer::Token;
use core::ops::Range;

#[derive(Debug)]
pub enum ExprKind {
    IntLiteral(u32),
    Ident(u32),
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub range: Range<u32>,
}

#[derive(Debug)]
pub enum DeclKind<'a> {
    Type(ASTType<'a>),
    Uninit {
        decl_type: ASTType<'a>,
        ident: u32,
    },
    WithValue {
        decl_type: ASTType<'a>,
        ident: u32,
        value: Expr,
    },
}

#[derive(Debug)]
pub struct Decl<'a> {
    pub kind: DeclKind<'a>,
    pub range: Range<u32>,
}

impl<'a> Decl<'a> {
    pub fn decl_type(&self) -> &ASTType<'a> {
        match &self.kind {
            DeclKind::Type(decl_type) => decl_type,
            DeclKind::Uninit { decl_type, ident } => decl_type,
            DeclKind::WithValue {
                decl_type,
                ident,
                value,
            } => decl_type,
        }
    }
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
