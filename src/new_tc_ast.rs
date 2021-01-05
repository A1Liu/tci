use crate::buckets::*;
use crate::filedb::*;
pub use crate::new_ast::{BinOp, UnaryOp};
use crate::util::*;
use serde::Serialize;
use std::io::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TCIdent {
    Ident(u32),
    // this isn't enough for statics; assembler needs to tag idents with translation unit they came from as well
    ScopedIdent { scope: CodeLoc, ident: u32 },
    Anonymous(CodeLoc),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TCUnaryOp {
    Neg,
    BoolNot,
    BitNot,
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
    Pointer { stride: n32 },
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
    Label(u32),
    Goto(u32),
    InternalGoto(u32),
    BranchGoto {
        // A conditional goto, not checked by assembler
        condition: TCExpr,
        goto: u32,
    },

    ScopeBegin(&'static [TCType]),
    // points to scope beginning
    ScopeEnd(u32),

    // (expr, goto)
    Switch(&'static [(TCExpr, u32)]),

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
    InternalTypedef(&'static TCType),
    Typedef {
        refers_to: &'static TCType,
        typedef: (u32, CodeLoc),
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

pub struct TCTypeRef<'a> {
    pub base: TCTypeBase,
    pub mods: &'a [TCTypeModifier],
}

impl<'a> TCTy for TCTypeRef<'a> {
    fn base(&self) -> TCTypeBase {
        return self.base;
    }
    fn mods(&self) -> &[TCTypeModifier] {
        return self.mods;
    }
}

pub trait TCTy {
    fn base(&self) -> TCTypeBase;
    fn mods(&self) -> &[TCTypeModifier];

    fn get_typedef(&self) -> Option<&'static TCType> {
        match self.base() {
            TCTypeBase::InternalTypedef(td) => Some(td),
            TCTypeBase::Typedef { refers_to, .. } => Some(refers_to),
            _ => None,
        }
    }

    fn display(&self, files: &FileDb) -> String {
        let mut writer = StringWriter::new();

        match self.base() {
            TCTypeBase::I8 => write!(writer, "char"),
            TCTypeBase::U8 => write!(writer, "unsigned char"),
            TCTypeBase::I32 => write!(writer, "int"),
            TCTypeBase::U32 => write!(writer, "unsigned int"),
            TCTypeBase::I64 => write!(writer, "long"),
            TCTypeBase::U64 => write!(writer, "unsigned long"),
            TCTypeBase::Void => write!(writer, "void"),
            TCTypeBase::InternalTypedef(def) => write!(writer, "{}", def.display(files)), // TODO fix this
            TCTypeBase::Typedef { typedef, .. } => {
                write!(writer, "{}", files.symbol_to_str(typedef.0))
            }
        }
        .unwrap();

        return writer.to_string();
    }

    fn expand_typedef(&self, files: &FileDb) -> TCTypeOwned {
        let mut owned = if let Some(refers_to) = self.get_typedef() {
            let (base, mods) = (refers_to.base, Vec::from(refers_to.mods));
            TCTypeOwned { base, mods }
        } else {
            let (base, mods) = (self.base(), Vec::new());
            TCTypeOwned { base, mods }
        };

        owned.mods.extend(self.mods());
        return owned;
    }

    fn is_void(&self) -> bool {
        if let TCTypeBase::Void = self.base() {
            if self.mods().len() == 0 {
                return true;
            }
        }

        return false;
    }

    fn ignore_mods(&self) -> TCType {
        let base = self.base();
        TCType { base, mods: &[] }
    }

    fn is_function(&self) -> bool {
        if self.mods().len() == 0 {
            if let Some(def) = self.get_typedef() {
                return def.is_function();
            }

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

    fn is_callable(&self) -> bool {
        if self.mods().len() == 0 {
            if let Some(def) = self.get_typedef() {
                return def.is_callable();
            }

            return false;
        }

        if self.is_function() {
            return true;
        }

        if let TCTypeModifier::Pointer = self.mods()[0] {
            let (base, mods) = (self.base(), &self.mods()[1..]);

            if (TCTypeRef { base, mods }).is_function() {
                return true;
            }
        }

        return false;
    }

    fn pointer_stride(&self) -> n32 {
        if let Some(deref) = self.deref() {
            return deref.size();
        }

        return n32::NULL;
    }

    fn is_pointer(&self) -> bool {
        if self.mods().len() == 0 {
            if let Some(def) = self.get_typedef() {
                return def.is_pointer();
            }

            return false;
        }

        return let_expr!(TCTypeModifier::Pointer = self.mods()[0])
            || let_expr!(TCTypeModifier::Array(_) = self.mods()[0])
            || let_expr!(TCTypeModifier::VariableArray = self.mods()[0]);
    }

    fn is_integer(&self) -> bool {
        if self.mods().len() != 0 {
            return false;
        }

        match self.base() {
            TCTypeBase::I8 | TCTypeBase::U8 => return true,
            TCTypeBase::I32 | TCTypeBase::U32 | TCTypeBase::I64 | TCTypeBase::U64 => return true,
            TCTypeBase::Void => return false,
            TCTypeBase::Typedef { refers_to, .. } => return refers_to.is_integer(),
            TCTypeBase::InternalTypedef(def) => return def.is_integer(),
        }
    }

    fn is_array(&self) -> bool {
        if self.mods().len() == 0 {
            if let Some(def) = self.get_typedef() {
                return def.is_array();
            }

            return false;
        }

        if let TCTypeModifier::Array(_) = self.mods()[0] {
            return true;
        }

        if let TCTypeModifier::VariableArray = self.mods()[0] {
            return true;
        }

        return false;
    }

    fn is_complete(&self) -> bool {
        if let Some(first) = self.mods().first() {
            match first {
                TCTypeModifier::Pointer => return true,
                TCTypeModifier::BeginParam(_)
                | TCTypeModifier::NoParams
                | TCTypeModifier::UnknownParams => return false,
                TCTypeModifier::Param(_) | TCTypeModifier::VarargsParam => return true,
                TCTypeModifier::Array(i) => return true,
                TCTypeModifier::VariableArray => return false,
            }
        }

        match self.base() {
            TCTypeBase::I8 | TCTypeBase::U8 => return true,
            TCTypeBase::I32 | TCTypeBase::U32 | TCTypeBase::I64 | TCTypeBase::U64 => return true,
            TCTypeBase::Void => return false,
            TCTypeBase::Typedef { refers_to, .. } => return refers_to.is_complete(),
            TCTypeBase::InternalTypedef(def) => return def.is_complete(),
        };
    }

    fn size(&self) -> n32 {
        let mut multiplier = 1;
        let mut is_array = false;
        for modifier in self.mods() {
            match modifier {
                TCTypeModifier::Pointer => {
                    if is_array {
                        return (multiplier * 8).into();
                    } else {
                        return 8.into();
                    }
                }
                TCTypeModifier::BeginParam(_)
                | TCTypeModifier::NoParams
                | TCTypeModifier::UnknownParams => return n32::NULL,
                TCTypeModifier::Param(_) | TCTypeModifier::VarargsParam => unreachable!(),
                TCTypeModifier::Array(i) => {
                    multiplier *= i;
                    is_array = true;
                }
                TCTypeModifier::VariableArray => return n32::NULL,
            }
        }

        let base = match self.base() {
            TCTypeBase::I8 | TCTypeBase::U8 => 1,
            TCTypeBase::U32 | TCTypeBase::I32 => 4,
            TCTypeBase::U64 | TCTypeBase::I64 => 8,
            TCTypeBase::Void => return n32::NULL,
            TCTypeBase::InternalTypedef(def) => {
                let size = def.size();
                if size == n32::NULL {
                    return size;
                }

                size.into()
            }
            TCTypeBase::Typedef { refers_to, .. } => {
                let size = refers_to.size();
                if size == n32::NULL {
                    return size;
                }

                size.into()
            }
        };

        return (multiplier * base).into();
    }

    /// Panics if called on an incomplete type
    fn to_prim_type(&self) -> Option<TCPrimType> {
        for modifier in self.mods() {
            match modifier {
                TCTypeModifier::Pointer => {
                    let deref = TCTypeRef {
                        base: self.base(),
                        mods: &self.mods()[1..],
                    };

                    let stride = deref.size();
                    return Some(TCPrimType::Pointer { stride });
                }
                TCTypeModifier::Array(_) => {
                    let deref = TCTypeRef {
                        base: self.base(),
                        mods: &self.mods()[1..],
                    };

                    let stride = deref.size();
                    return Some(TCPrimType::Pointer { stride });
                }
                TCTypeModifier::BeginParam(_)
                | TCTypeModifier::NoParams
                | TCTypeModifier::UnknownParams => {
                    return Some(TCPrimType::Pointer { stride: n32::NULL });
                }
                TCTypeModifier::Param(_) | TCTypeModifier::VarargsParam => unreachable!(),
                TCTypeModifier::VariableArray => return None,
            }
        }

        return match self.base() {
            TCTypeBase::I8 => Some(TCPrimType::I8),
            TCTypeBase::U8 => Some(TCPrimType::U8),
            TCTypeBase::I32 => Some(TCPrimType::I32),
            TCTypeBase::U32 => Some(TCPrimType::U32),
            TCTypeBase::I64 => Some(TCPrimType::I64),
            TCTypeBase::U64 => Some(TCPrimType::U64),
            TCTypeBase::Void => None,
            TCTypeBase::Typedef { refers_to, .. } => return refers_to.to_prim_type(),
            TCTypeBase::InternalTypedef(def) => return def.to_prim_type(),
        };
    }

    /// Panics if the type is incomplete
    fn deref(&self) -> Option<TCTypeRef> {
        assert!(self.is_complete());

        if let Some(first) = self.mods().first() {
            let base = self.base();
            let mods = &self.mods()[1..];
            let to_ret = TCTypeRef { base, mods };

            match first {
                TCTypeModifier::Pointer => {
                    if to_ret.is_function() {
                        let mods = self.mods();
                        return Some(TCTypeRef { base, mods });
                    }

                    return Some(to_ret);
                }
                TCTypeModifier::Array(_) => return Some(to_ret),
                TCTypeModifier::BeginParam(_)
                | TCTypeModifier::NoParams
                | TCTypeModifier::UnknownParams => return None,
                _ => unreachable!(),
            }
        }

        if let Some(def) = self.get_typedef() {
            return TCTy::deref(def);
        }

        return None;
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
    pub fn to_func_type_strict(&self, alloc: impl Allocator<'static>) -> Option<TCFuncType> {
        if self.mods.len() == 0 {
            if let Some(def) = self.get_typedef() {
                return def.to_func_type(alloc);
            }

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

    pub fn to_func_type(&self, alloc: impl Allocator<'static>) -> Option<TCFuncType> {
        if let Some(deref) = self.deref() {
            return deref.to_func_type_strict(alloc);
        }

        return self.to_func_type_strict(alloc);
    }

    /// Panics if the type is incomplete
    pub fn deref(&self) -> Option<TCType> {
        assert!(self.is_complete());

        if let Some(first) = self.mods.first() {
            let base = self.base;
            let mods = &self.mods[1..];
            let to_ret = TCType { base, mods };

            match first {
                TCTypeModifier::Pointer => {
                    if to_ret.is_function() {
                        return Some(*self);
                    }

                    return Some(to_ret);
                }
                TCTypeModifier::Array(_) => return Some(to_ret),
                TCTypeModifier::BeginParam(_)
                | TCTypeModifier::NoParams
                | TCTypeModifier::UnknownParams => return None,
                _ => unreachable!(),
            }
        }

        if let Some(def) = self.get_typedef() {
            return def.deref();
        }

        return None;
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
        label: u32,
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

    UnaryOp {
        op: TCUnaryOp,
        op_type: TCPrimType,
        operand: &'static TCExpr,
    },

    Conv {
        from: TCPrimType,
        to: TCPrimType,
        expr: &'static TCExpr,
    },

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

    PostIncr {
        incr_ty: TCPrimType,
        value: TCAssignTarget,
    },
    PostDecr {
        decr_ty: TCPrimType,
        value: TCAssignTarget,
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

    Ref(TCAssignTarget),
    Deref(&'static TCExpr),

    Call {
        func: &'static TCExpr,
        params: &'static [TCExpr],
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
    LocalIdent { label: u32 },
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
pub enum LabelOrLoc {
    Param(u32),      // Parameter label local to the function
    LocalIdent(u32), // symbol label local to the function
    StaticLoc(CodeLoc),
}

#[derive(Debug, Clone, Copy)]
pub struct TCVar {
    pub symbol_label: LabelOrLoc,
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
pub enum TCDeclInit {
    Extern,
    ExternInit(TCExprKind),
    Static(TCExprKind),
    Default(TCExprKind),
    DefaultUninit,
}

#[derive(Debug, Clone, Copy)]
pub struct TCGlobalVar {
    pub init: TCDeclInit,
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
pub struct TCParamDecl {
    pub ty: TCType,
    pub ident: u32,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct TCParamsDeclarator {
    pub params: &'static [TCParamDecl],
    pub varargs: bool,
}

pub struct TCFunctionDeclarator {
    pub is_static: bool,
    pub return_type: TCType,
    pub ident: u32,
    pub params: Option<TCParamsDeclarator>,
    pub loc: CodeLoc,
}

pub struct TranslationUnit {
    pub buckets: BucketListRef<'static>,
    pub typedefs: HashMap<(u32, CodeLoc), TCType>,
    pub variables: HashMap<TCIdent, TCGlobalVar>,
    pub functions: HashMap<u32, TCFunction>,
}

pub struct TCDecl {
    pub ident: u32,
    pub init: TCDeclInit,
    pub ty: TCType,
    pub loc: CodeLoc,
}

pub enum DeclarationResult {
    Typedef {
        ty: TCType,
        ident: u32,
        loc: CodeLoc,
    },
    VarDecl(Vec<TCDecl>),
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
