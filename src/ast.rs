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

#[derive(Debug, Clone)]
pub struct TCStructMember {
    pub decl_type: TCType,
    pub ident: u32,
    pub range: Range,
}

#[derive(Debug, Clone)]
pub struct TCStructDefn<'a> {
    defn_idx: u32,
    members: &'a [TCStructMember],
}

#[derive(Debug)]
pub struct TCStruct<'a> {
    pub decl_idx: u32,
    pub defn: Option<TCStructDefn<'a>>,
    pub ident_range: Range,
    pub range: Range,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TCTypeKind {
    Int,
    Char,
    Void,
    Struct { ident: u32 },
}

#[derive(Debug, Clone, Copy)]
pub struct TCType {
    pub kind: TCTypeKind,
    pub pointer_count: u32,
}

#[derive(Debug, Clone)]
pub struct TCGlobalValue {
    pub decl_type: TCType,
    pub range: Range,
    pub decl_idx: u32,
}

#[derive(Debug, Clone)]
pub struct TCVar {
    pub decl_type: TCType,
    pub range: Range,
}

#[derive(Debug, Clone)]
pub struct TCFuncParam {
    pub decl_type: TCType,
    pub ident: u32,
    pub range: Range,
}

#[derive(Debug, Clone, Copy)]
pub struct TCFuncType<'a> {
    pub decl_idx: u32,
    pub return_type: TCType,
    pub range: Range,
    pub params: &'a [TCFuncParam],
}

#[derive(Debug, Clone)]
pub struct TCFunc<'a> {
    pub func_type: TCFuncType<'a>,
    pub defn_idx: u32,
    pub range: Range,
    pub stmts: &'a [TCStmt<'a>],
}

#[derive(Debug, Clone)]
pub enum TCStmtKind<'a> {
    RetVal(TCExpr<'a>),
}

#[derive(Debug, Clone)]
pub struct TCStmt<'a> {
    pub kind: TCStmtKind<'a>,
    pub range: Range,
}

#[derive(Debug, Clone)]
pub enum TCExprKind<'a> {
    AddI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    WidenTo32(&'a TCExpr<'a>),
    IntLiteral(u32),
}

#[derive(Debug, Clone)]
pub struct TCExpr<'a> {
    pub kind: TCExprKind<'a>,
    pub expr_type: TCType,
    pub range: Range,
}

impl PartialEq for TCType {
    fn eq(&self, other: &Self) -> bool {
        return self.kind == other.kind && self.pointer_count == other.pointer_count;
    }
}

impl<'a> PartialEq for TCFuncType<'a> {
    fn eq(&self, other: &Self) -> bool {
        if self.return_type != other.return_type {
            return false;
        }

        if self.params.len() != other.params.len() {
            return false;
        }

        for i in 0..self.params.len() {
            if self.params[i].decl_type != other.params[i].decl_type {
                return false;
            }
        }

        return true;
    }
}

pub fn convert_type(type_node: &ASTType, pointer_count: u32) -> TCType {
    let mut out = TCType {
        kind: TCTypeKind::Int,
        pointer_count: pointer_count,
    };

    match &type_node.kind {
        ASTTypeKind::Int => {
            out.kind = TCTypeKind::Int;
            return out;
        }
        ASTTypeKind::Char => {
            out.kind = TCTypeKind::Char;
            return out;
        }
        ASTTypeKind::Void => {
            out.kind = TCTypeKind::Void;
            return out;
        }
        ASTTypeKind::Struct { ident } => {
            out.kind = TCTypeKind::Struct { ident: *ident };
            return out;
        }
    }
}
