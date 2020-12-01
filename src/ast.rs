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
    Mod,
    Index,
    Lt,
    Gt,
    Leq,
    Geq,
    Eq,
    Neq,
    LShift,
    RShift,
    BitAnd,
    BitXor,
    BitOr,
    BoolAnd,
    BoolOr,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
pub enum UnaryOp {
    Neg,
    BoolNot,
    BitNot,
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'a> {
    IntLiteral(i32),
    CharLiteral(i8),
    StringLiteral(&'a str),
    SizeofType {
        sizeof_type: ASTType<'a>,
        pointer_count: u32,
    },
    SizeofExpr(&'a Expr<'a>),
    Ident(u32),
    BinOp(BinOp, &'a Expr<'a>, &'a Expr<'a>),
    UnaryOp(UnaryOp, &'a Expr<'a>),
    Not(&'a Expr<'a>),
    Assign(&'a Expr<'a>, &'a Expr<'a>),
    MutAssign {
        target: &'a Expr<'a>,
        value: &'a Expr<'a>,
        op: BinOp,
    },
    Call {
        function: &'a Expr<'a>,
        params: &'a [Expr<'a>],
    },
    Cast {
        cast_to: ASTType<'a>,
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
    pub decl_type: ASTType<'a>,
    pub recv: DeclReceiver<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone)]
pub enum ParamKind<'a> {
    StructLike {
        decl_type: ASTType<'a>,
        recv: DeclReceiver<'a>,
    },
    TypeOnly {
        decl_type: ASTType<'a>,
        pointer_count: u32,
        array_dims: &'a [u32],
    },
    Vararg,
}

#[derive(Debug, Clone)]
pub struct ParamDecl<'a> {
    pub kind: ParamKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum StructDecl<'a> {
    Named(u32),
    NamedDef {
        ident: u32,
        members: &'a [InnerStructDecl<'a>],
    },
    Unnamed(&'a [InnerStructDecl<'a>]),
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
        // TODO func declaration span separate from global_stmt.span
        return_type: ASTType<'a>,
        pointer_count: u32,
        ident: u32,
        params: &'a [ParamDecl<'a>],
        body: &'a [Stmt<'a>],
    },
    FuncDecl {
        return_type: ASTType<'a>,
        pointer_count: u32,
        ident: u32,
        params: &'a [ParamDecl<'a>],
    },
    StructDecl(StructDecl<'a>),
    Typedef {
        ast_type: ASTType<'a>,
        recv: DeclReceiver<'a>,
    },
    Decl {
        decl_type: ASTType<'a>,
        decls: &'a [Decl<'a>],
    },
}

#[derive(Debug, Clone)]
pub struct GlobalStmt<'a> {
    pub kind: GlobalStmtKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum ASTTypeKind<'a> {
    Struct(StructDecl<'a>),
    Ident(u32),
    Int,
    Long,
    Char,
    Unsigned,
    Void,

    LongInt,
    LongLongInt,
    LongLong,

    UnsignedInt,
    UnsignedLong,
    UnsignedLongInt,
    UnsignedLongLong,
    UnsignedLongLongInt,
    UnsignedChar,
}

#[derive(Debug, Clone, Copy)]
pub struct ASTType<'a> {
    pub kind: ASTTypeKind<'a>,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone)]
pub enum StmtKind<'a> {
    Decl {
        decl_type: ASTType<'a>,
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
        at_start_decl_type: ASTType<'a>,
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
    align: TC_UNKNOWN_ALIGN,
};

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub struct TCStructMember {
    pub decl_type: TCType,
    pub ident: u32,
    pub loc: CodeLoc,
    pub offset: u32,
    pub decl_idx: u32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TCStructDefnMeta {
    pub defn_idx: u32,
    pub loc: CodeLoc,
    pub sa: SizeAlign,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TCStructDefn {
    pub members: Vec<TCStructMember>,
    pub meta: TCStructDefnMeta,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TCStruct {
    pub decl_idx: u32,
    pub defn: Option<TCStructDefn>,
    pub decl_loc: CodeLoc,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TCTypedef {
    pub typedef: TCType,
    pub defn_idx: u32,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, Serialize)]
pub enum TCPrimType {
    I32, // int
    U32, // unsigned int
    U64, // unsigned long
    I64, // long
    I8,  // char
    U8,  // unsigned char
    Pointer { size: u32 },
}

impl TCPrimType {
    pub fn idx(&self) -> u8 {
        match self {
            TCPrimType::I32 => 0,
            TCPrimType::U32 => 1,
            TCPrimType::U64 => 2,
            TCPrimType::I64 => 3,
            TCPrimType::I8 => 4,
            TCPrimType::U8 => 5,
            TCPrimType::Pointer { .. } => 6,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, Serialize)]
#[serde(tag = "kind", content = "data")]
pub enum TCTypeKind {
    I32, // int
    U32, // unsigned int
    U64, // unsigned long
    I64, // long
    I8,  // char
    U8,  // unsigned char
    Void,
    Struct { ident: u32, sa: SizeAlign },
    AnonStruct { loc: CodeLoc, sa: SizeAlign },
    Ident { ident: u32, sa: SizeAlign },
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

    pub fn is_array(&self) -> bool {
        return self.array_kind != TCArrayKind::None;
    }

    pub fn to_shallow(&self) -> TCShallowType {
        match self.array_kind {
            TCArrayKind::None => {}
            TCArrayKind::Fixed(_) => return TCShallowType::Pointer,
        }

        if let TCTypeKind::Void = self.kind {
            if self.pointer_count == 1 {
                return TCShallowType::VoidPointer;
            }
        }

        if self.pointer_count > 0 {
            return TCShallowType::Pointer;
        }

        match self.kind {
            TCTypeKind::I32 => TCShallowType::I32,
            TCTypeKind::U32 => TCShallowType::U32,
            TCTypeKind::I64 => TCShallowType::I64,
            TCTypeKind::U64 => TCShallowType::U64,
            TCTypeKind::I8 => TCShallowType::I8,
            TCTypeKind::U8 => TCShallowType::U8,
            TCTypeKind::Void => TCShallowType::Void,
            TCTypeKind::Struct { .. } => TCShallowType::Struct,
            TCTypeKind::AnonStruct { .. } => TCShallowType::Struct,
            TCTypeKind::Ident { .. } => panic!("cannot make shallow of ident"),
            TCTypeKind::Uninit { .. } => panic!("cannot make shallow of uninit"),
            TCTypeKind::BraceList => panic!("cannot make shallow of brace list"),
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self.array_kind {
            TCArrayKind::None => {}
            TCArrayKind::Fixed(_) => return true,
        }

        return self.pointer_count > 0;
    }

    pub fn rank(&self) -> u32 {
        match self.array_kind {
            TCArrayKind::None => {}
            TCArrayKind::Fixed(_) => return u32::MAX,
        }

        if self.pointer_count > 0 {
            return u32::MAX;
        }

        match self.kind {
            TCTypeKind::U8 => return 1,
            TCTypeKind::I8 => return 2,
            TCTypeKind::I32 => return 9,
            TCTypeKind::U32 => return 10,
            TCTypeKind::I64 => return 11,
            TCTypeKind::U64 => return 12,
            _ => {}
        }

        return 0;
        // https://overiq.com/c-programming-101/implicit-type-conversion-in-c/
        // If one operand is of type long double, then the other operand will be
        // converted to long double and then the result of the operation will be a long double.
        // Otherwise, If one operand is of type double then the other operand will be
        // converted to double and the result of the operation will be a double.
        // Otherwise, If one operand is of type float then the other operand will be
        // converted to float and the result of the operation will be a float.
        // Otherwise, If one operand is of type unsigned long int then the other operand
        // will be converted to unsigned long int and the result of the operation will be an unsigned long int.
        // Otherwise, If one operand is of type long intand the other is of type unsigned
        // int then there are two possibilities:
        //     If long int can represent all the values of an unsigned int, the operand of
        //     type unsigned int will be converted to long int and the result will be a long int.
        //     Otherwise, If long int can't represent all the values of an unsigned int,
        //     the operand of both of the operands will be converted to unsigned long int and
        //     the result will be an unsigned long int.
        // Otherwise, If one operand is of type long int then the other operand will be
        // converted to long int and the result of the operation will be a long int.
        // Otherwise, If one operand is of type unsigned int then the other operand will be
        // converted to unsigned int and the result of the operation will be an unsigned int.
        // Otherwise, If one operand is of type int then the other operand will be converted
        // to int and the result of the operation will be an int.
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

        use TCTypeKind as TCTK;
        let element_size = match self.kind {
            TCTK::U64 | TCTK::I64 => 8,
            TCTK::I32 | TCTK::U32 => 4,
            TCTK::I8 | TCTK::U8 => 1,
            TCTK::Void => 0,
            TCTK::Struct { sa, .. } => {
                debug_assert!(sa.size != TC_UNKNOWN_SIZE);
                sa.size
            }
            TCTK::AnonStruct { sa, .. } => {
                debug_assert!(sa.size != TC_UNKNOWN_SIZE);
                sa.size
            }
            TCTK::Ident { sa, .. } => {
                debug_assert!(sa.size != TC_UNKNOWN_SIZE);
                sa.size
            }
            TCTK::Uninit { size } => size,
            TCTK::BraceList => TC_UNKNOWN_SIZE,
        };

        return element_size * multiplier;
    }

    pub fn repr_size(&self) -> u32 {
        match self.array_kind {
            TCArrayKind::Fixed(len) => return 8,
            TCArrayKind::None => {}
        }

        return self.size();
    }

    #[inline]
    pub fn align(&self) -> u32 {
        if self.pointer_count > 0 {
            return 8;
        }

        use TCTypeKind as TCTK;
        match self.kind {
            TCTK::U64 | TCTK::I64 => 8,
            TCTK::I32 | TCTK::U32 => 4,
            TCTK::I8 | TCTK::U8 => 1,
            TCTK::Void => 0,
            TCTK::Struct { sa, .. } => {
                debug_assert!(sa != TC_UNKNOWN_SA);
                sa.align
            }
            TCTK::AnonStruct { sa, .. } => {
                debug_assert!(sa != TC_UNKNOWN_SA);
                sa.align
            }
            TCTK::Ident { sa, .. } => {
                debug_assert!(sa != TC_UNKNOWN_SA);
                sa.align
            }
            TCTK::Uninit { size } => size,
            TCTK::BraceList => TC_UNKNOWN_ALIGN,
        }
    }

    pub fn display(&self, files: &FileDb) -> String {
        let mut writer = StringWriter::new();
        #[rustfmt::skip]
        let result = match self.kind {
            TCTypeKind::I32 => write!(writer, "int"),
            TCTypeKind::U32 => write!(writer, "unsigned int"),
            TCTypeKind::U64 => write!(writer, "unsigned long"),
            TCTypeKind::I64 => write!(writer, "long"),
            TCTypeKind::I8 => write!(writer, "char"),
            TCTypeKind::U8 => write!(writer, "unsigned char"),
            TCTypeKind::Void => write!(writer, "void"),
            TCTypeKind::Struct { ident, .. } => write!(writer, "struct {}", files.symbol_to_str(ident)),
            TCTypeKind::AnonStruct { .. } => write!(writer, "struct ?"),
            TCTypeKind::Ident{ident, ..} => write!(writer, "{}", files.symbol_to_str(ident)),
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
    U32, // unsigned int
    U64, // unsigned long
    I64, // long
    I8,  // char
    U8,  // unsigned char
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

#[derive(Debug, Clone, Copy)]
pub struct TCFuncParam {
    pub param_type: TCType,
    pub ident: u32,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone)]
pub struct TCFuncType {
    pub decl_idx: u32,
    pub return_type: TCType,
    pub loc: CodeLoc,
    pub params: Vec<(TCType, CodeLoc)>,
    pub varargs: bool,
}

#[derive(Debug, Clone)]
pub struct TCFuncDefn<'a> {
    pub defn_idx: u32,
    pub loc: CodeLoc,
    pub params: Vec<TCFuncParam>,
    pub stmts: &'a [TCStmt<'a>],
}

#[derive(Debug, Clone)]
pub struct TCFunc<'a> {
    pub func_type: TCFuncType,
    pub defn: Option<TCFuncDefn<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub enum TCAssignTargetKind<'a> {
    LocalIdent { var_offset: i16 },
    Ptr(&'a TCExpr<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct TCAssignTarget<'a> {
    pub kind: TCAssignTargetKind<'a>,
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
    I8Literal(i8),
    I32Literal(i32),
    I64Literal(i64),
    U64Literal(u64),
    StringLiteral(&'a str),
    LocalIdent {
        var_offset: i16,
    },

    Array(&'a [TCExpr<'a>]),
    TypePun(&'a TCExpr<'a>),

    BraceList(&'a [TCExpr<'a>]),
    ParenList(&'a [TCExpr<'a>]),

    SubI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    MulI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    DivI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    LtI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    GtI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    LeqI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    GeqI32(&'a TCExpr<'a>, &'a TCExpr<'a>),

    Eq32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    Neq32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    Eq64(&'a TCExpr<'a>, &'a TCExpr<'a>),

    AddU32(&'a TCExpr<'a>, &'a TCExpr<'a>),

    AddU64(&'a TCExpr<'a>, &'a TCExpr<'a>),
    SubU64(&'a TCExpr<'a>, &'a TCExpr<'a>),
    DivU64(&'a TCExpr<'a>, &'a TCExpr<'a>),
    GeqU64(&'a TCExpr<'a>, &'a TCExpr<'a>),
    LtU64(&'a TCExpr<'a>, &'a TCExpr<'a>),

    MulI64(&'a TCExpr<'a>, &'a TCExpr<'a>),
    MulU64(&'a TCExpr<'a>, &'a TCExpr<'a>),

    RShiftI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    LShiftI32(&'a TCExpr<'a>, &'a TCExpr<'a>),

    BitAndI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    BitOrI32(&'a TCExpr<'a>, &'a TCExpr<'a>),
    BitXorI32(&'a TCExpr<'a>, &'a TCExpr<'a>),

    BitAndI8(&'a TCExpr<'a>, &'a TCExpr<'a>),
    BitOrI8(&'a TCExpr<'a>, &'a TCExpr<'a>),

    SConv8To32(&'a TCExpr<'a>),
    SConv32To64(&'a TCExpr<'a>),

    ZConv8To32(&'a TCExpr<'a>),
    ZConv32To64(&'a TCExpr<'a>),

    Conv32To8(&'a TCExpr<'a>),
    Conv64To32(&'a TCExpr<'a>),

    PostIncrU32(TCAssignTarget<'a>),
    PostIncrU64(TCAssignTarget<'a>),

    Assign {
        target: TCAssignTarget<'a>,
        value: &'a TCExpr<'a>,
    },

    MutAssign {
        target: TCAssignTarget<'a>,
        value: &'a TCExpr<'a>,
        op: BinOp,
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

impl PartialEq for TCFuncType {
    fn eq(&self, other: &Self) -> bool {
        if self.return_type != other.return_type {
            return false;
        }

        if self.params.len() != other.params.len() {
            return false;
        }

        for i in 0..self.params.len() {
            if self.params[i].0 != other.params[i].0 {
                return false;
            }
        }

        return true;
    }
}
