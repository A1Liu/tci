use crate::filedb::*;
use crate::util::*;
use serde::Serialize;

#[derive(Debug, Clone, Copy)]
pub struct ASTProgram<'a> {
    pub stmts: &'a [GlobalStmt<'a>],
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Index,
    Lt,
    Gt,
    Leq,
    Geq,
    Eq,
    Neq,
    BitAnd,
    BitXor,
    BitOr,
    BoolAnd,
    BoolOr,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'a> {
    IntLiteral(i32),
    CharLiteral(u8),
    StringLiteral(&'a str),
    SizeofType {
        sizeof_type: ASTType,
        pointer_count: u32,
    },
    SizeofExpr(&'a Expr<'a>),
    Ident(u32),
    BinOp(BinOp, &'a Expr<'a>, &'a Expr<'a>),
    UnaryOp(UnaryOp, &'a Expr<'a>),
    Not(&'a Expr<'a>),
    Assign(&'a Expr<'a>, &'a Expr<'a>),
    Call {
        function: &'a Expr<'a>,
        params: &'a [Expr<'a>],
    },
    Cast {
        cast_to: ASTType,
        pointer_count: u32,
        cast_to_loc: CodeLoc,
        expr: &'a Expr<'a>,
    },
    Member {
        base: &'a Expr<'a>,
        member: u32,
    },
    PtrMember {
        base: &'a Expr<'a>,
        member: u32,
    },
    BraceList(&'a [Expr<'a>]),
    ParenList(&'a [Expr<'a>]),
    PostIncr(&'a Expr<'a>),
    PostDecr(&'a Expr<'a>),
    Ref(&'a Expr<'a>),
    Deref(&'a Expr<'a>),
    Ternary {
        condition: &'a Expr<'a>,
        if_true: &'a Expr<'a>,
        if_false: &'a Expr<'a>,
    },
    Uninit,
}

#[derive(Debug, Clone, Copy)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct DeclReceiver<'a> {
    pub pointer_count: u32,
    pub ident: u32,
    pub array_dims: &'a [u32],
    pub loc: CodeLoc,
}

#[derive(Debug)]
pub struct InnerStructDecl<'a> {
    pub decl_type: ASTType,
    pub recv: DeclReceiver<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone)]
pub enum ParamKind<'a> {
    StructLike {
        decl_type: ASTType,
        recv: DeclReceiver<'a>,
    },
    Vararg,
}

#[derive(Debug, Clone)]
pub struct ParamDecl<'a> {
    pub kind: ParamKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone)]
pub struct StructDecl<'a> {
    pub ident: u32,
    pub ident_loc: CodeLoc,
    pub members: Option<&'a [InnerStructDecl<'a>]>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct Decl<'a> {
    pub recv: DeclReceiver<'a>,
    pub loc: CodeLoc,
    pub expr: Expr<'a>,
}

#[derive(Debug, Clone)]
pub enum GlobalStmtKind<'a> {
    Func {
        return_type: ASTType,
        pointer_count: u32,
        ident: u32,
        params: &'a [ParamDecl<'a>],
        body: &'a [Stmt<'a>],
    },
    FuncDecl {
        return_type: ASTType,
        pointer_count: u32,
        ident: u32,
        params: &'a [ParamDecl<'a>],
    },
    StructDecl(StructDecl<'a>),
    Decl {
        decl_type: ASTType,
        decls: &'a [Decl<'a>],
    },
}

#[derive(Debug, Clone)]
pub struct GlobalStmt<'a> {
    pub kind: GlobalStmtKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum ASTTypeKind {
    Int,
    Struct { ident: u32 },
    Char,
    Void,
}

#[derive(Debug, Clone, Copy)]
pub struct ASTType {
    pub kind: ASTTypeKind,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone)]
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
        if_body: Block<'a>,
        else_body: Block<'a>,
    },
    Block(Block<'a>),
    For {
        at_start: Expr<'a>,
        condition: Expr<'a>,
        post_expr: Expr<'a>,
        body: Block<'a>,
    },
    ForDecl {
        at_start_decl_type: ASTType,
        at_start: &'a [Decl<'a>],
        condition: Expr<'a>,
        post_expr: Expr<'a>,
        body: Block<'a>,
    },
    While {
        condition: Expr<'a>,
        body: Block<'a>,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct Block<'a> {
    pub stmts: &'a [Stmt<'a>],
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash, Serialize)]
pub struct SizeAlign {
    pub size: u32,
    pub align: u32,
}

pub fn sa(size: u32, align: u32) -> SizeAlign {
    SizeAlign { size, align }
}

pub const TC_UNKNOWN_SIZE: u32 = !0;
pub const TC_UNKNOWN_ALIGN: u32 = !0;
pub const TC_UNKNOWN_ARRAY_SIZE: u32 = 0;
pub const TC_UNKNOWN_SA: SizeAlign = SizeAlign {
    size: TC_UNKNOWN_SIZE,
    align: 0,
};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct TCStructMember {
    pub decl_type: TCType,
    pub ident: u32,
    pub loc: CodeLoc,
    pub offset: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TCStructDefn<'a> {
    pub defn_idx: u32,
    pub members: &'a [TCStructMember],
    pub loc: CodeLoc,
    pub sa: SizeAlign,
}

#[derive(Debug, PartialEq)]
pub struct TCStruct<'a> {
    pub decl_idx: u32,
    pub defn: Option<TCStructDefn<'a>>,
    pub decl_loc: CodeLoc,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, Serialize)]
#[serde(tag = "kind", content = "data")]
pub enum TCTypeKind {
    I32, // int
    U64, // unsigned long
    I64, // long
    Char,
    Void,
    Struct { ident: u32, sa: SizeAlign },
    Uninit { size: u32 },
    BraceList,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, Serialize)]
#[serde(tag = "kind", content = "data")]
pub enum TCArrayKind {
    None,
    Fixed(u32),
    // Fixed2d { rows: u32, cols: u32 },
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize)]
pub struct TCType {
    pub kind: TCTypeKind,
    pub pointer_count: u32,
    pub array_kind: TCArrayKind,
}

impl TCType {
    pub fn new(kind: TCTypeKind, pointer_count: u32) -> Self {
        Self {
            kind,
            pointer_count,
            array_kind: TCArrayKind::None,
        }
    }

    pub fn new_array(kind: TCTypeKind, pointer_count: u32, array_kind: TCArrayKind) -> Self {
        Self {
            kind,
            pointer_count,
            array_kind,
        }
    }

    pub fn to_shallow(&self) -> TCShallowType {
        match self.array_kind {
            TCArrayKind::None => {}
            TCArrayKind::Fixed(_) => return TCShallowType::Pointer,
        }

        if self.pointer_count > 0 {
            if let TCTypeKind::Void = self.kind {
                return TCShallowType::VoidPointer;
            }
            return TCShallowType::Pointer;
        }

        match self.kind {
            TCTypeKind::I32 => TCShallowType::I32,
            TCTypeKind::U64 => TCShallowType::U64,
            TCTypeKind::I64 => TCShallowType::I64,
            TCTypeKind::Char => TCShallowType::Char,
            TCTypeKind::Void => TCShallowType::Void,
            TCTypeKind::Struct { .. } => TCShallowType::Struct,
            TCTypeKind::Uninit { .. } => panic!("cannot make shallow of uninit"),
            TCTypeKind::BraceList => panic!("cannot make shallow of brace list"),
        }
    }

    #[inline]
    pub fn size(&self) -> u32 {
        let multiplier = match self.array_kind {
            TCArrayKind::None => 1,
            TCArrayKind::Fixed(len) => len,
        };

        if self.pointer_count > 0 {
            return 8;
        }

        let element_size = match self.kind {
            TCTypeKind::U64 | TCTypeKind::I64 => 8,
            TCTypeKind::I32 => 4,
            TCTypeKind::Char => 1,
            TCTypeKind::Void => 0,
            TCTypeKind::Struct { sa, .. } => {
                debug_assert!(sa.size != TC_UNKNOWN_SIZE);
                sa.size
            }
            TCTypeKind::Uninit { size } => size,
            TCTypeKind::BraceList => TC_UNKNOWN_ALIGN,
        };

        return element_size * multiplier;
    }

    #[inline]
    pub fn align(&self) -> u32 {
        if self.pointer_count > 0 {
            return 8;
        }

        match self.kind {
            TCTypeKind::U64 | TCTypeKind::I64 => 8,
            TCTypeKind::I32 => 4,
            TCTypeKind::Char => 1,
            TCTypeKind::Void => 0,
            TCTypeKind::Struct { sa, .. } => {
                debug_assert!(sa.size != TC_UNKNOWN_SIZE);
                sa.align
            }
            TCTypeKind::Uninit { size } => size,
            TCTypeKind::BraceList => TC_UNKNOWN_ALIGN,
        }
    }

    pub fn display(&self, files: &FileDb) -> String {
        let mut writer = StringWriter::new();
        #[rustfmt::skip]
        let result = match self.kind {
            TCTypeKind::I32 => write!(writer, "int"),
            TCTypeKind::U64 => write!(writer, "unsigned long"),
            TCTypeKind::I64 => write!(writer, "long"),
            TCTypeKind::Char => write!(writer, "char"),
            TCTypeKind::Void => write!(writer, "void"),
            TCTypeKind::Struct { ident, .. } => write!(writer, "struct {}", files.symbol_to_str(ident)),
            TCTypeKind::Uninit { .. } => return "void".to_string(),
            TCTypeKind::BraceList => return "brace_list".to_string()
        };
        result.unwrap();

        for i in 0..self.pointer_count {
            write!(writer, "*").unwrap();
        }

        match self.array_kind {
            TCArrayKind::Fixed(len) => write!(writer, "[{}]", len).unwrap(),
            TCArrayKind::None => {}
        }

        return writer.into_string();
    }
}

pub const VOID: TCType = TCType {
    kind: TCTypeKind::Void,
    pointer_count: 0,
    array_kind: TCArrayKind::None,
};

pub const BRACE_LIST: TCType = TCType {
    kind: TCTypeKind::BraceList,
    pointer_count: 0,
    array_kind: TCArrayKind::None,
};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum TCShallowType {
    I32, // int
    U64, // unsigned long
    I64, // long
    Char,
    Void,
    Struct,
    Pointer,
    VoidPointer,
}

// TODO TCVarKind with global and local
#[derive(Debug, Clone)]
pub struct TCVar {
    pub decl_type: TCType,
    pub var_offset: i16, // The offset from the frame pointer for this variable
    pub loc: CodeLoc,    // we allow extern in include files so the file is not known apriori
}

#[derive(Debug, Clone)]
pub struct TCFuncParam {
    pub decl_type: TCType,
    pub ident: u32,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct TCFuncType<'a> {
    pub decl_idx: u32,
    pub return_type: TCType,
    pub loc: CodeLoc,
    pub params: &'a [TCFuncParam],
    pub varargs: bool,
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

#[derive(Debug, Clone, Copy)]
pub enum TCAssignKind<'a> {
    LocalIdent { var_offset: i16 },
    Ptr(&'a TCExpr<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct TCAssignTarget<'a> {
    pub kind: TCAssignKind<'a>,
    pub defn_loc: Option<CodeLoc>,
    pub target_loc: CodeLoc,
    pub target_type: TCType,
    pub offset: u32,
}

#[derive(Debug, Clone, Copy)]
pub enum TCStmtKind<'a> {
    RetVal(TCExpr<'a>),
    Ret,
    Expr(TCExpr<'a>),
    Decl {
        symbol: u32,
        init: TCExpr<'a>,
    },
    Branch {
        cond: TCExpr<'a>,
        if_body: TCBlock<'a>,
        else_body: TCBlock<'a>,
    },
    Block(TCBlock<'a>),
    Loop(TCBlock<'a>),
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy)]
pub struct TCStmt<'a> {
    pub kind: TCStmtKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct TCBlock<'a> {
    pub stmts: &'a [TCStmt<'a>],
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum TCExprKind<'a> {
    Uninit,
    IntLiteral(i32),
    LongLiteral(i64),
    StringLiteral(&'a str),
    LocalIdent {
        var_offset: i16,
    },
    LocalArrayIdent {
        var_offset: i16,
    },

    FixedArrayToPtr(&'a TCExpr<'a>),
    Array(&'a [TCExpr<'a>]),

    BraceList(&'a [TCExpr<'a>]),
    ParenList(&'a [TCExpr<'a>]),

    AddU32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    AddU64(&'a TCExpr<'a>, &'a TCExpr<'a>),

    SubI32(&'a TCExpr<'a>, &'a TCExpr<'a>),

    MulI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    MulI64(&'a TCExpr<'a>, &'a TCExpr<'a>),

    DivI32(&'a TCExpr<'a>, &'a TCExpr<'a>),

    LtI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    GtI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    EqI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    NeqI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    LeqI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    GeqI32(&'a TCExpr<'a>, &'a TCExpr<'a>),

    SConv8To32(&'a TCExpr<'a>),
    SConv32To64(&'a TCExpr<'a>),

    ZConv8To32(&'a TCExpr<'a>),
    ZConv32To64(&'a TCExpr<'a>),

    Assign {
        target: TCAssignTarget<'a>,
        value: &'a TCExpr<'a>,
    },

    Ternary {
        condition: &'a TCExpr<'a>,
        if_true: &'a TCExpr<'a>,
        if_false: &'a TCExpr<'a>,
    },

    Member {
        base: &'a TCExpr<'a>,
        offset: u32,
    },
    PtrMember {
        base: &'a TCExpr<'a>,
        offset: u32,
    },

    Deref(&'a TCExpr<'a>),
    Ref(TCAssignTarget<'a>),

    Call {
        func: u32,
        params: &'a [TCExpr<'a>],
        varargs: bool,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct TCExpr<'a> {
    pub kind: TCExprKind<'a>,
    pub expr_type: TCType,
    pub loc: CodeLoc,
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
