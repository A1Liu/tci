use crate::*;

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
pub enum BinOp {
    Add,
    Sub,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind<'a> {
    IntLiteral(i32),
    CharLiteral(u8),
    StringLiteral(&'a str),
    Ident(u32),
    BinOp(BinOp, &'a Expr<'a>, &'a Expr<'a>),
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

#[derive(Debug, Clone, PartialEq)]
pub struct TCStructMember {
    pub decl_type: TCType,
    pub ident: u32,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TCStructDefn<'a> {
    defn_idx: u32,
    members: &'a [TCStructMember],
}

#[derive(Debug, PartialEq)]
pub struct TCStruct<'a> {
    pub decl_idx: u32,
    pub defn: Option<TCStructDefn<'a>>,
    pub ident_range: Range,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TCTypeKind {
    I32, // int
    U64, // unsigned long
    Char,
    Void,
    Struct { ident: u32, size: u32 },
}

#[derive(Debug, Clone, Copy)]
pub struct TCType {
    pub kind: TCTypeKind,
    pub pointer_count: u32,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum TCShallowType {
    I32, // int
    U64, // unsigned long
    Char,
    Void,
    Struct,
    Pointer,
}

#[derive(Debug, Clone)]
pub struct TCVar {
    pub decl_type: TCType,
    pub loc: CodeLoc, // we allow extern in include files so the file is not known apriori
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
    pub loc: CodeLoc,
    pub params: &'a [TCFuncParam],
}

#[derive(Debug, Clone)]
pub struct TCFuncDefn<'a> {
    pub defn_idx: u32,
    pub loc: CodeLoc,
    pub stmts: &'a [TCStmt<'a>],
}

#[derive(Debug, Clone)]
pub struct TCFunc<'a> {
    pub func_type: TCFuncType<'a>,
    pub defn: Option<TCFuncDefn<'a>>,
}

#[derive(Debug, Clone)]
pub enum TCStmtKind<'a> {
    RetVal(TCExpr<'a>),
    Ret,
    Expr(TCExpr<'a>),
}

#[derive(Debug, Clone)]
pub struct TCStmt<'a> {
    pub kind: TCStmtKind<'a>,
    pub range: Range,
}

#[derive(Debug, Clone)]
pub enum TCExprKind<'a> {
    IntLiteral(i32),
    StringLiteral(&'a str),

    AddI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    AddU64(&'a TCExpr<'a>, &'a TCExpr<'a>),

    SubI32(&'a TCExpr<'a>, &'a TCExpr<'a>),

    SConv8To32(&'a TCExpr<'a>),
    SConv32To64(&'a TCExpr<'a>),

    ZConv8To32(&'a TCExpr<'a>),
    ZConv32To64(&'a TCExpr<'a>),

    Call { func: u32, params: &'a [TCExpr<'a>] },
}

#[derive(Debug, Clone)]
pub struct TCExpr<'a> {
    pub kind: TCExprKind<'a>,
    pub expr_type: TCType,
    pub range: Range,
}

impl TCType {
    pub fn to_shallow(&self) -> TCShallowType {
        if self.pointer_count > 0 {
            return TCShallowType::Pointer;
        }

        match self.kind {
            TCTypeKind::I32 => TCShallowType::I32,
            TCTypeKind::U64 => TCShallowType::U64,
            TCTypeKind::Char => TCShallowType::Char,
            TCTypeKind::Void => TCShallowType::Void,
            TCTypeKind::Struct { ident, size } => TCShallowType::Struct,
        }
    }

    pub fn size(&self) -> u32 {
        if self.pointer_count > 0 {
            return 8;
        }

        match self.kind {
            TCTypeKind::U64 => 8,
            TCTypeKind::I32 => 4,
            TCTypeKind::Char => 1,
            TCTypeKind::Void => 0,
            TCTypeKind::Struct { ident, size } => size,
        }
    }
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
