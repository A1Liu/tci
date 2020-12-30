// use crate::filedb::*;
pub use crate::new_ast::BinOp;
use crate::util::*;
use serde::Serialize;
// use std::io::Write;

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

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, Serialize)]
pub enum TCPrimType {
    I32, // int
    U32, // unsigned int
    U64, // unsigned long
    I64, // long
    I8,  // char
    U8,  // unsigned char
    Pointer { stride_length: u32 },
}

pub type TCPrimTypeDiscr = std::mem::Discriminant<TCPrimType>;

impl TCPrimType {
    pub fn discriminant(&self) -> std::mem::Discriminant<TCPrimType> {
        return std::mem::discriminant(self);
    }

    pub fn signed(self) -> bool {
        match self {
            TCPrimType::I8 | TCPrimType::I32 | TCPrimType::I64 => return true,
            _ => return false,
        }
    }

    pub fn size(self) -> u8 {
        match self {
            TCPrimType::I8 | TCPrimType::U8 => return 1,
            TCPrimType::I32 | TCPrimType::U32 => return 4,
            TCPrimType::I64 | TCPrimType::U64 | TCPrimType::Pointer { .. } => return 8,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TCOpcodeKind {
    Allocate(u32),
    Drop {
        // go_back indicates how many instructions to go back to find the associated allocate
        go_back: u32,
    },
    Goto(i32), // a user-generated goto
    BranchGoto {
        // A conditional goto, always going forwards; not checked by assembler
        condition: TCExpr,
        goto: u32, // always goes forwards
    },
    BackGoto(u32), // A goto meant for looping, always going backwards; not checked by assembler
    ForwardGoto(u32), // A goto, always going forwards; not checked by assembler
    Expr(TCExpr),
    Ret,
    RetVal(TCExpr),
}

#[derive(Debug, Clone, Copy)]
pub struct TCOpcode {
    pub kind: TCOpcodeKind,
    pub loc: CodeLoc,
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
    Ident {
        ident: u32,
        sa: SizeAlign,
    },
    Function {
        ret: &'static TCType,
        params: &'static [TCType],
    },
}

#[derive(Debug, PartialEq, Hash, Eq, Clone, Copy, Serialize)]
pub struct TCType {
    pub kind: TCTypeKind,
    pub pointer_count: u32,
    pub array_kind: TCArrayKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, Serialize)]
#[serde(tag = "kind", content = "data")]
pub enum TCArrayKind {
    None,
    Fixed(u32),
    // Fixed2d { rows: u32, cols: u32 },
}

#[derive(Debug, Clone, Copy)]
pub enum TCBuiltin {
    PushTempStack {
        ptr: &'static TCExpr,  // always of type void*
        size: &'static TCExpr, // always of type size_t
    },
    Ecall(&'static TCExpr), // always of type `int`
}

#[derive(Debug, Clone, Copy)]
pub enum TCExprKind {
    Uninit,
    I8Literal(i8),
    I32Literal(i32),
    I64Literal(i64),
    U64Literal(u64),
    StringLiteral(&'static str),
    LocalIdent {
        var_offset: i16,
    },

    Array(&'static [TCExpr]),

    BraceList(&'static [TCExpr]),
    ParenList(&'static [TCExpr]),

    BinOp {
        op: BinOp,
        op_type: TCPrimType,
        left: &'static TCExpr,
        right: &'static TCExpr,
    },

    Conv {
        from: TCPrimType,
        to: TCPrimType,
        expr: &'static TCExpr,
    },

    PostIncrU32(TCAssignTarget),
    PostIncrU64(TCAssignTarget),

    Assign {
        target: TCAssignTarget,
        value: &'static TCExpr,
    },

    MutAssign {
        target: TCAssignTarget,
        value: &'static TCExpr,
        op: BinOp,
        op_type: TCPrimType,
    },

    Ternary {
        condition: &'static TCExpr,
        if_true: &'static TCExpr,
        if_false: &'static TCExpr,
    },

    Member {
        base: &'static TCExpr,
        offset: u32,
    },
    PtrMember {
        base: &'static TCExpr,
        offset: u32,
    },

    Deref(&'static TCExpr),
    Ref(TCAssignTarget),

    Call {
        func: &'static TCExpr,
        params: &'static [TCExpr],
        named_count: u32,
    },
    Builtin(TCBuiltin),
}

#[derive(Debug, Clone, Copy)]
pub struct TCExpr {
    pub kind: TCExprKind,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum TCAssignTargetKind {
    LocalIdent { var_offset: i16 },
    Ptr(&'static TCExpr),
}

#[derive(Debug, Clone, Copy)]
pub struct TCAssignTarget {
    pub kind: TCAssignTargetKind,
    pub defn_loc: Option<CodeLoc>,
    pub target_loc: CodeLoc,
    pub target_size: u32,
    pub offset: u32,
}
