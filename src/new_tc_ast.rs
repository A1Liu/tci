// use crate::filedb::*;
use crate::buckets::*;
pub use crate::new_ast::BinOp;
use crate::util::*;
use serde::Serialize;
// use std::io::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TCIdent {
    Ident(u32),
    // this isn't enough for statics; assembler needs to tag idents with translation unit they came from as well
    ScopedIdent { scope: CodeLoc, ident: u32 },
    Anonymous(CodeLoc),
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
    AllocateInit {
        var_type: TCType,
        init: TCExpr,
    },
    Allocate(TCType),
    Label(u32),
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

#[derive(Debug, Clone, Copy, PartialEq, Hash, Serialize)]
#[serde(tag = "kind", content = "data")]
pub enum TCTypeBase {
    I32, // int
    U32, // unsigned int
    U64, // unsigned long
    I64, // long
    I8,  // char
    U8,  // unsigned char
    Void,
    Typedef {
        refers_to: &'static TCType,
        typedef: (n32, CodeLoc),
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Serialize)]
#[serde(tag = "modifier", content = "data")]
pub enum TCTypeModifier {
    Pointer, // TODO add qualifiers
    Array(u32),
    VariableArray,
    BeginParam(TCType),
    Param(TCType),
    VarargsParam,
    NoParams,
    UnknownParams,
}

pub trait TCTy {
    fn base(&self) -> TCTypeBase;
    fn mods(&self) -> &[TCTypeModifier];

    fn is_void(&self) -> bool {
        if let TCTypeBase::Void = self.base() {
            if self.mods().len() == 0 {
                return true;
            }
        }

        return false;
    }

    fn is_function(&self) -> bool {
        if self.mods().len() == 0 {
            return false;
        }

        if let TCTypeModifier::NoParams = self.mods()[0] {
            return true;
        }

        if let TCTypeModifier::UnknownParams = self.mods()[0] {
            return true;
        }

        if let TCTypeModifier::BeginParam(_) = self.mods()[0] {
            return true;
        }

        return false;
    }

    fn ty_eq(l: &impl TCTy, r: &impl TCTy) -> bool {
        return l.base() == r.base() && l.mods() == r.mods();
    }
}

#[derive(Debug, Clone, Copy, Hash, Serialize)]
pub struct TCType {
    pub base: TCTypeBase,
    pub mods: &'static [TCTypeModifier],
}

impl TCType {
    pub fn to_tc_func_type(&self, alloc: impl Allocator<'static>) -> Option<TCFuncType> {
        if self.mods.len() == 0 {
            return None;
        }

        let tc_func_type = match self.mods[0] {
            TCTypeModifier::UnknownParams => TCFuncType {
                return_type: TCType {
                    base: self.base,
                    mods: &self.mods[1..],
                },
                params: None,
            },
            TCTypeModifier::NoParams => TCFuncType {
                return_type: TCType {
                    base: self.base,
                    mods: &self.mods[1..],
                },
                params: Some(TCParamType {
                    types: &[],
                    varargs: false,
                }),
            },
            TCTypeModifier::BeginParam(param) => {
                let mut params = vec![param];
                let mut cursor = 1;
                let mut varargs = false;

                while cursor < self.mods.len() {
                    match self.mods[cursor] {
                        TCTypeModifier::Param(p) => params.push(p),
                        TCTypeModifier::VarargsParam => varargs = true,
                        _ => break,
                    }

                    cursor += 1;
                }

                let return_type = TCType {
                    base: self.base,
                    mods: &self.mods[cursor..],
                };

                let params = Some(TCParamType {
                    types: alloc.add_array(params),
                    varargs,
                });

                TCFuncType {
                    return_type,
                    params,
                }
            }
            _ => return None,
        };

        return Some(tc_func_type);
    }
}

impl PartialEq<TCType> for TCType {
    fn eq(&self, other: &TCType) -> bool {
        return TCType::ty_eq(self, other);
    }
}

impl TCTy for TCType {
    fn base(&self) -> TCTypeBase {
        return self.base;
    }

    fn mods(&self) -> &[TCTypeModifier] {
        return self.mods;
    }
}

impl TCType {
    pub fn new(base: TCTypeBase) -> Self {
        TCType { base, mods: &[] }
    }

    pub fn new_ptr(base: TCTypeBase) -> Self {
        TCType {
            base,
            mods: &[TCTypeModifier::Pointer],
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct TCTypeOwned {
    pub base: TCTypeBase,
    pub mods: Vec<TCTypeModifier>,
}

impl TCTypeOwned {
    pub fn new(base: TCTypeBase) -> Self {
        Self {
            base,
            mods: Vec::new(),
        }
    }

    pub fn to_ref(self, alloc: impl Allocator<'static>) -> TCType {
        TCType {
            base: self.base,
            mods: alloc.add_array(self.mods),
        }
    }

    pub fn canonicalize(&mut self) {
        let mut found_func = false;

        for modifier in &mut self.mods {
            match modifier {
                TCTypeModifier::NoParams
                | TCTypeModifier::UnknownParams
                | TCTypeModifier::BeginParam(_) => {
                    found_func = true;
                }
                TCTypeModifier::Array(_) | TCTypeModifier::VariableArray => {
                    if found_func {
                        *modifier = TCTypeModifier::Pointer;
                    }
                }
                _ => {}
            }
        }
    }

    pub fn canonicalize_param(&mut self) {
        if self.is_function() {
            self.mods.insert(0, TCTypeModifier::Pointer);
        }

        for modifier in &mut self.mods {
            match modifier {
                TCTypeModifier::Array(_) | TCTypeModifier::VariableArray => {
                    *modifier = TCTypeModifier::Pointer;
                }
                _ => {}
            }
        }
    }
}

impl TCTy for TCTypeOwned {
    fn base(&self) -> TCTypeBase {
        return self.base;
    }

    fn mods(&self) -> &[TCTypeModifier] {
        return &self.mods;
    }
}

#[derive(Debug, Clone, Copy, Hash)]
pub struct TCTypeMut<'a> {
    pub base: TCTypeBase,
    pub mods: &'a [TCTypeModifier],
}

impl<'a> TCTy for TCTypeMut<'a> {
    fn base(&self) -> TCTypeBase {
        return self.base;
    }

    fn mods(&self) -> &[TCTypeModifier] {
        return self.mods;
    }
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
    GlobalIdent {
        binary_offset: u32,
    },
    /// `ident` isn't ever anonymous
    FunctionIdent {
        ident: u32,
    },

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
    pub ty: TCType,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum TCAssignTargetKind {
    LocalIdent { var_offset: i16 },
    GlobalIdent { binary_offset: u32 },
    Ptr(&'static TCExpr),
}

#[derive(Debug, Clone, Copy)]
pub struct TCAssignTarget {
    pub kind: TCAssignTargetKind,
    pub defn_loc: CodeLoc,
    pub loc: CodeLoc,
    pub ty: TCType,
    pub offset: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct TCParamType {
    pub types: &'static [TCType],
    pub varargs: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct TCFuncType {
    pub return_type: TCType,
    pub params: Option<TCParamType>,
}

#[derive(Debug, Clone, Copy)]
pub enum OffsetOrLoc {
    LocalOffset(i16),
    StaticLoc(CodeLoc),
}

#[derive(Debug, Clone, Copy)]
pub struct TCVar {
    pub var_offset: OffsetOrLoc, // if none, it's a static; otherwise its offset from the frame pointer
    pub ty: TCType,
    pub loc: CodeLoc, // we allow extern in include files so the file is not known apriori
}

#[derive(Debug, Clone, Copy)]
pub enum StorageClass {
    Extern,
    Static,
    Typedef,
    Default,
}

#[derive(Debug, Clone, Copy)]
pub enum TCGlobalInit {
    Extern,
    Static(TCExprKind),
    Default(TCExprKind),
}

#[derive(Debug, Clone, Copy)]
pub struct TCGlobalVar {
    pub init: TCGlobalInit,
    pub var_idx: u32,
    pub ty: TCType,
    pub loc: CodeLoc, // we allow extern in include files so the file is not known apriori
}

#[derive(Debug, Clone, Copy)]
pub struct TCFuncDefn {
    pub ops: &'static [TCOpcode],
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct TCFunction {
    pub is_static: bool,
    pub func_type: TCFuncType,
    pub defn: Option<TCFuncDefn>,
}

#[derive(Debug, Clone, Copy)]
pub struct TCParamDeclaration {
    pub ty: TCType,
    pub ident: u32,
    pub loc: CodeLoc,
}

pub struct TCParamsDeclarator {
    pub params: &'static [TCParamDeclaration],
    pub varargs: bool,
}

pub struct TCFunctionDeclarator {
    pub is_static: bool,
    pub return_type: TCType,
    pub ident: u32,
    pub params: Option<TCParamsDeclarator>,
}

pub struct TranslationUnit {
    pub buckets: BucketListRef<'static>,
    pub typedefs: HashMap<(u32, CodeLoc), TCType>,
    pub variables: HashMap<TCIdent, TCGlobalVar>,
    pub functions: HashMap<u32, TCFunction>,
}

pub struct TCDecl {
    pub ty: TCType,
    pub ident: u32,
    pub expr: TCExpr,
}

pub enum DeclarationResult {
    Typedef { ty: TCType, ident: u32 },
    Static(Vec<TCDecl>),
    Default(Vec<TCDecl>),
    Extern(Vec<(TCType, u32)>),
}

impl TranslationUnit {
    pub fn new() -> Self {
        Self {
            buckets: BucketList::new(),
            typedefs: HashMap::new(),
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}
