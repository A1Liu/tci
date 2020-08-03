use crate::lexer::Token;
use core::ops::Range;

#[derive(Debug)]
pub struct InnerStructDecl {
    pub decl_type: ASTType,
    pub pointer_count: u32,
    pub ident: u32,
    pub range: Range<u32>,
}

#[derive(Debug)]
pub struct StructDecl<'a> {
    pub ident: u32,
    pub ident_range: Range<u32>,
    pub members: Option<&'a [InnerStructDecl]>,
    pub range: Range<u32>,
}

#[derive(Debug)]
pub struct DeclIdent {
    pub pointer_count: u32,
    pub ident: u32,
    pub range: Range<u32>,
}

#[derive(Debug)]
pub enum GlobalStmtKind<'a> {
    Func {
        return_type: ASTType,
        pointer_count: u32,
        ident: u32,
        params: &'a [InnerStructDecl],
        body: &'a [Token],
    },
    FuncDecl {
        return_type: ASTType,
        pointer_count: u32,
        ident: u32,
        params: &'a [InnerStructDecl],
    },
    StructDecl(StructDecl<'a>),
    SingletonDecl {
        decl_type: ASTType,
        pointer_count: u32,
        ident: u32,
    },
    Decl {
        decl_type: ASTType,
        tokens: &'a [Token],
    },
}

#[derive(Debug)]
pub struct GlobalStmt<'a> {
    pub kind: GlobalStmtKind<'a>,
    pub range: Range<u32>,
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
    pub range: Range<u32>,
}
