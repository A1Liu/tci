use crate::lexer::Token;
use core::ops::Range;

// #[derive(Debug)]
// pub enum ExprKind<'a> {
//     IntLiteral(u32),
//     Ident(u32),
//     Call {
//         function: &'a Expr<'a>,
//         params: &'a [Expr<'a>],
//     },
//     Member {
//         expr: &'a Expr<'a>,
//         member: u32,
//     },
//     PtrMember {
//         expr: &'a Expr<'a>,
//         member: u32,
//     },
//     Index {
//         ptr: &'a Expr<'a>,
//         index: &'a Expr<'a>,
//     },
//     PostIncr(&'a Expr<'a>),
//     PostDecr(&'a Expr<'a>),
// }
//
// #[derive(Debug)]
// pub struct Expr<'a> {
//     pub kind: ExprKind<'a>,
//     pub range: Range<u32>,
// }

#[derive(Debug)]
pub struct InnerStructDecl {
    pub decl_type: ASTType,
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
pub enum DeclKind<'a> {
    Uninit {
        decl_type: ASTType,
        ident: u32,
    },
    WithValue {
        decl_type: ASTType,
        ident: u32,
        value: &'a [Token],
    },
}

#[derive(Debug)]
pub struct Decl<'a> {
    pub kind: DeclKind<'a>,
    pub range: Range<u32>,
}

impl<'a> Decl<'a> {
    pub fn decl_type(&self) -> &ASTType {
        match &self.kind {
            DeclKind::Uninit { decl_type, .. } => decl_type,
            DeclKind::WithValue { decl_type, .. } => decl_type,
        }
    }
}

#[derive(Debug)]
pub enum GlobalStmtKind<'a> {
    Func {
        return_type: ASTType,
        ident: u32,
        params: &'a [InnerStructDecl],
        body: &'a [Token],
    },
    FuncDecl {
        return_type: ASTType,
        ident: u32,
        params: &'a [InnerStructDecl],
    },
    StructDecl(StructDecl<'a>),
    Decl(Decl<'a>),
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
    pub pointer_count: u32,
}
