use crate::filedb::*;
use crate::runtime::Opcode;
use crate::util::*;
use core::fmt::Write;
use serde::Serialize;

pub use crate::ast::{BinOp, UnaryOp};

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
    BoolNorm,
    BoolNot,
    BitNot,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash, Serialize)]
pub struct SizeAlign {
    pub size: n32,
    pub align: n32,
}

#[inline]
pub fn sa_new(size: u32, align: u32) -> SizeAlign {
    let (size, align) = (size.into(), align.into());
    SizeAlign { size, align }
}

pub const TC_UNKNOWN_SA: SizeAlign = SizeAlign {
    size: n32::NULL,
    align: n32::NULL,
};

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, Serialize)]
pub enum TCPrimType {
    I8,  // char
    U8,  // unsigned char
    I16, // short
    U16, // unsigned short
    I32, // int
    U32, // unsigned int
    U64, // unsigned long
    I64, // long
    F32, // float
    F64, // double
    Pointer { stride: n32 },
}

pub type TCPrimTypeDiscr = core::mem::Discriminant<TCPrimType>;

impl TCPrimType {
    pub fn discriminant(&self) -> core::mem::Discriminant<TCPrimType> {
        return core::mem::discriminant(self);
    }

    pub fn signed(self) -> bool {
        match self {
            TCPrimType::I8 | TCPrimType::I16 | TCPrimType::I32 | TCPrimType::I64 => return true,
            TCPrimType::F32 | TCPrimType::F64 => return true,
            _ => return false,
        }
    }

    pub fn is_floating_pt(self) -> bool {
        match self {
            TCPrimType::F32 | TCPrimType::F64 => return true,
            _ => return false,
        }
    }

    pub fn size(self) -> u8 {
        match self {
            TCPrimType::I8 | TCPrimType::U8 => return 1,
            TCPrimType::I16 | TCPrimType::U16 => return 2,
            TCPrimType::I32 | TCPrimType::U32 | TCPrimType::F32 => return 4,
            TCPrimType::I64 | TCPrimType::U64 | TCPrimType::F64 => return 8,
            TCPrimType::Pointer { .. } => return 8,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TCOpcodeKind {
    Label {
        label: u32,
        scope_idx: u32,
    },
    Goto {
        goto: u32,
        scope_idx: u32,
    },
    GotoIfZero {
        // unchecked by assembler; cannot cross scopes
        cond: TCExpr,
        cond_ty: TCPrimType,
        goto: u32,
        scope_idx: u32,
    },
    GotoIfNotZero {
        // unchecked by assembler; cannot cross scopes
        cond: TCExpr,
        cond_ty: TCPrimType,
        goto: u32,
        scope_idx: u32,
    },

    ScopeBegin(aliu::HashRef<'static, u32, TCType>, u32), // points to parent scope
    ScopeEnd {
        count: u32,
        begin: u32, // points to scope beginning
    },

    Switch {
        expr: TCExpr,
        cases: &'static [(TCExpr, u32)], // (expr, goto)
        default: u32,
    },

    Expr(TCExpr),
    Ret,
    RetVal(TCExpr),
}

#[derive(Debug, Clone, Copy)]
pub struct TCOpcode {
    pub kind: TCOpcodeKind,
    pub loc: CodeLoc,
}

impl TCOpcode {
    pub fn init_local(
        alloc: impl Allocator,
        label: u32,
        kind: TCExprKind,
        ty: TCType,
        loc: CodeLoc,
    ) -> TCOpcode {
        let init_expr = TCExpr { kind, ty, loc };

        let expr = TCExpr {
            kind: TCExprKind::Assign {
                target: TCAssignTarget {
                    kind: TCAssignTargetKind::LocalIdent { label },
                    defn_loc: loc,
                    ty,
                    loc,
                    offset: 0,
                },
                value: alloc.new(init_expr),
            },
            ty,
            loc,
        };

        let kind = TCOpcodeKind::Expr(expr);
        return TCOpcode { kind, loc };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Serialize)]
#[serde(tag = "kind", content = "data")]
pub enum TCTypeBase {
    I8,  // char
    U8,  // unsigned char
    I16, // short
    U16, // unsigned short
    I32, // int
    U32, // unsigned int
    U64, // unsigned long
    I64, // long
    F32, // float
    F64, // double
    Void,
    NamedUnion {
        ident: u32,
        sa: SizeAlign,
    },
    UnnamedUnion {
        loc: CodeLoc,
        sa: SizeAlign,
    },
    NamedStruct {
        ident: u32,
        sa: SizeAlign,
    },
    UnnamedStruct {
        loc: CodeLoc,
        sa: SizeAlign,
    },
    InternalTypedef(&'static TCType),
    Typedef {
        refers_to: &'static TCType,
        typedef: (u32, CodeLoc),
    },
}

impl TCTypeBase {
    pub fn size(&self) -> n32 {
        match self {
            TCTypeBase::I8 | TCTypeBase::U8 => 1u32.into(),
            TCTypeBase::I16 | TCTypeBase::U16 => 2u32.into(),
            TCTypeBase::U32 | TCTypeBase::I32 | TCTypeBase::F32 => 4u32.into(),
            TCTypeBase::U64 | TCTypeBase::I64 | TCTypeBase::F64 => 8u32.into(),
            TCTypeBase::Void => return n32::NULL,
            TCTypeBase::NamedStruct { sa, .. } => sa.size,
            TCTypeBase::UnnamedStruct { sa, .. } => sa.size,
            TCTypeBase::NamedUnion { sa, .. } => sa.size,
            TCTypeBase::UnnamedUnion { sa, .. } => sa.size,
            TCTypeBase::InternalTypedef(def) => def.size(),
            TCTypeBase::Typedef { refers_to, .. } => refers_to.size(),
        }
    }

    pub fn align(&self) -> n32 {
        match self {
            TCTypeBase::I8 | TCTypeBase::U8 => 1u32.into(),
            TCTypeBase::I16 | TCTypeBase::U16 => 2u32.into(),
            TCTypeBase::U32 | TCTypeBase::I32 | TCTypeBase::F32 => 4u32.into(),
            TCTypeBase::U64 | TCTypeBase::I64 | TCTypeBase::F64 => 8u32.into(),
            TCTypeBase::Void => return n32::NULL,
            TCTypeBase::NamedStruct { sa, .. } => sa.align,
            TCTypeBase::UnnamedStruct { sa, .. } => sa.align,
            TCTypeBase::NamedUnion { sa, .. } => sa.align,
            TCTypeBase::UnnamedUnion { sa, .. } => sa.align,
            TCTypeBase::InternalTypedef(def) => def.align(),
            TCTypeBase::Typedef { refers_to, .. } => refers_to.align(),
        }
    }

    pub fn is_floating_pt(self) -> bool {
        match self {
            TCTypeBase::F32 | TCTypeBase::F64 => return true,
            _ => return false,
        }
    }
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

impl TCTypeBase {
    fn get_typedef(&self) -> Option<&'static TCType> {
        match self {
            TCTypeBase::InternalTypedef(td) => Some(td),
            TCTypeBase::Typedef { refers_to, .. } => Some(refers_to),
            _ => None,
        }
    }
}

pub trait TCTy {
    fn base(&self) -> TCTypeBase;
    fn mods(&self) -> &[TCTypeModifier];

    #[inline]
    fn get_typedef(&self) -> Option<&'static TCType> {
        self.base().get_typedef()
    }

    fn get_id_strict(&self) -> Option<(bool, LabelOrLoc)> {
        if self.mods().len() != 0 {
            return None;
        }

        match self.base() {
            TCTypeBase::I8 | TCTypeBase::U8 => return None,
            TCTypeBase::I16 | TCTypeBase::U16 => return None,
            TCTypeBase::I32 | TCTypeBase::U32 => return None,
            TCTypeBase::I64 | TCTypeBase::U64 => return None,
            TCTypeBase::F32 | TCTypeBase::F64 => return None,
            TCTypeBase::Void => return None,
            TCTypeBase::UnnamedStruct { loc, .. } => return Some((true, LabelOrLoc::Loc(loc))),
            TCTypeBase::NamedStruct { ident, .. } => return Some((true, LabelOrLoc::Ident(ident))),
            TCTypeBase::UnnamedUnion { loc, .. } => return Some((false, LabelOrLoc::Loc(loc))),
            TCTypeBase::NamedUnion { ident, .. } => return Some((false, LabelOrLoc::Ident(ident))),
            TCTypeBase::InternalTypedef(def) => return def.get_id_strict(),
            TCTypeBase::Typedef { refers_to, .. } => return refers_to.get_id_strict(),
        }
    }

    // TODO make this nicer
    fn display(&self, symbols: &Symbols) -> String {
        let mut writer = String::new();

        match self.base() {
            TCTypeBase::I8 => write!(writer, "char"),
            TCTypeBase::U8 => write!(writer, "unsigned char"),
            TCTypeBase::I16 => write!(writer, "short"),
            TCTypeBase::U16 => write!(writer, "unsigned short"),
            TCTypeBase::I32 => write!(writer, "int"),
            TCTypeBase::U32 => write!(writer, "unsigned int"),
            TCTypeBase::I64 => write!(writer, "long"),
            TCTypeBase::U64 => write!(writer, "unsigned long"),
            TCTypeBase::F32 => write!(writer, "float"),
            TCTypeBase::F64 => write!(writer, "double"),
            TCTypeBase::Void => write!(writer, "void"),
            TCTypeBase::NamedStruct { ident, .. } => {
                write!(writer, "struct {}", symbols.to_str(ident).unwrap())
            }
            TCTypeBase::UnnamedStruct { .. } => write!(writer, "anonymous struct"),
            TCTypeBase::NamedUnion { ident, .. } => {
                write!(writer, "union {}", symbols.to_str(ident).unwrap())
            }
            TCTypeBase::UnnamedUnion { .. } => write!(writer, "anonymous union"),
            TCTypeBase::InternalTypedef(def) => write!(writer, "{}", def.display(symbols)), // TODO fix this
            TCTypeBase::Typedef { typedef, .. } => {
                write!(writer, "{}", symbols.to_str(typedef.0).unwrap())
            }
        }
        .unwrap();

        let mut is_func: Option<()> = None;
        for modifier in self.mods() {
            match modifier {
                TCTypeModifier::Pointer => {
                    is_func.take().map(|_| write!(writer, ")"));
                    write!(writer, "*")
                }
                TCTypeModifier::Array(dim) => {
                    is_func.take().map(|_| write!(writer, ")"));
                    write!(writer, "[{}]", dim)
                }
                TCTypeModifier::VariableArray => {
                    is_func.take().map(|_| write!(writer, ")"));
                    write!(writer, "[]")
                }
                TCTypeModifier::BeginParam(ty) => {
                    if is_func.replace(()).is_some() {
                        write!(writer, ")({}", ty.display(symbols))
                    } else {
                        write!(writer, "({}", ty.display(symbols))
                    }
                }
                TCTypeModifier::Param(ty) => {
                    write!(writer, ", {}", ty.display(symbols))
                }
                TCTypeModifier::NoParams => {
                    is_func.take().map(|_| write!(writer, ")"));
                    write!(writer, "(void)")
                }
                TCTypeModifier::VarargsParam => {
                    is_func.take().map(|_| write!(writer, ")"));
                    write!(writer, ", ...")
                }
                TCTypeModifier::UnknownParams => {
                    is_func.take().map(|_| write!(writer, ")"));
                    write!(writer, "()")
                }
            }
            .unwrap();
        }

        is_func.take().map(|_| write!(writer, ")"));

        return writer;
    }

    fn expand_typedef(&self) -> TCTypeOwned {
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

    fn to_ty_owned(&self) -> TCTypeOwned {
        let mods = self.mods().iter().map(|a| *a).collect();
        TCTypeOwned {
            base: self.base(),
            mods,
        }
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
            TCTypeBase::I16 | TCTypeBase::U16 => return true,
            TCTypeBase::I32 | TCTypeBase::U32 | TCTypeBase::I64 | TCTypeBase::U64 => return true,
            TCTypeBase::F32 | TCTypeBase::F64 => return false,
            TCTypeBase::Void => return false,
            TCTypeBase::NamedStruct { .. } | TCTypeBase::UnnamedStruct { .. } => return false,
            TCTypeBase::NamedUnion { .. } | TCTypeBase::UnnamedUnion { .. } => return false,
            TCTypeBase::Typedef { refers_to, .. } => return refers_to.is_integer(),
            TCTypeBase::InternalTypedef(def) => return def.is_integer(),
        }
    }

    fn is_floating_pt(&self) -> bool {
        if self.mods().len() != 0 {
            return false;
        }

        match self.base() {
            TCTypeBase::F32 | TCTypeBase::F64 => return true,
            _ => return false,
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
                | TCTypeModifier::UnknownParams => return true,
                TCTypeModifier::Param(_) | TCTypeModifier::VarargsParam => unreachable!(),
                TCTypeModifier::Array(i) => return true,
                TCTypeModifier::VariableArray => return false,
            }
        }

        match self.base() {
            TCTypeBase::I8 | TCTypeBase::U8 => return true,
            TCTypeBase::I16 | TCTypeBase::U16 => return true,
            TCTypeBase::I32 | TCTypeBase::U32 | TCTypeBase::I64 | TCTypeBase::U64 => return true,
            TCTypeBase::F32 | TCTypeBase::F64 => return true,
            TCTypeBase::Void => return false,
            TCTypeBase::NamedStruct { sa, .. } => return sa.size != n32::NULL,
            TCTypeBase::UnnamedStruct { sa, .. } => return sa.size != n32::NULL,
            TCTypeBase::NamedUnion { sa, .. } => return sa.size != n32::NULL,
            TCTypeBase::UnnamedUnion { sa, .. } => return sa.size != n32::NULL,
            TCTypeBase::Typedef { refers_to, .. } => return refers_to.is_complete(),
            TCTypeBase::InternalTypedef(def) => return def.is_complete(),
        };
    }

    fn repr_size(&self) -> u32 {
        for modifier in self.mods() {
            match modifier {
                TCTypeModifier::Pointer => return 8,
                TCTypeModifier::BeginParam(_)
                | TCTypeModifier::NoParams
                | TCTypeModifier::UnknownParams => return 8,
                TCTypeModifier::Param(_) | TCTypeModifier::VarargsParam => unreachable!(),
                TCTypeModifier::Array(i) => return 8,
                TCTypeModifier::VariableArray => return 8,
            }
        }

        return match self.base() {
            TCTypeBase::I8 | TCTypeBase::U8 => 1,
            TCTypeBase::I16 | TCTypeBase::U16 => return 2,
            TCTypeBase::U32 | TCTypeBase::I32 | TCTypeBase::F32 => 4,
            TCTypeBase::U64 | TCTypeBase::I64 | TCTypeBase::F64 => 8,
            TCTypeBase::Void => return 0,
            TCTypeBase::NamedStruct { sa, .. } => sa.size.into(),
            TCTypeBase::UnnamedStruct { sa, .. } => sa.size.into(),
            TCTypeBase::NamedUnion { sa, .. } => sa.size.into(),
            TCTypeBase::UnnamedUnion { sa, .. } => sa.size.into(),
            TCTypeBase::InternalTypedef(def) => def.repr_size(),
            TCTypeBase::Typedef { refers_to, .. } => refers_to.repr_size(),
        };
    }

    fn align(&self) -> n32 {
        for modifier in self.mods() {
            match modifier {
                TCTypeModifier::Pointer => return 8u32.into(),
                TCTypeModifier::BeginParam(_)
                | TCTypeModifier::NoParams
                | TCTypeModifier::UnknownParams => return n32::NULL,
                TCTypeModifier::Param(_) | TCTypeModifier::VarargsParam => unreachable!(),
                TCTypeModifier::Array(i) => {}
                TCTypeModifier::VariableArray => {}
            }
        }

        return self.base().align();
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
                        return 8u32.into();
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

        let base = self.base().size();
        if base == n32::NULL {
            return base;
        }
        let base: u32 = base.into();

        return (multiplier * base).into();
    }

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
                TCTypeModifier::Array(_) | TCTypeModifier::VariableArray => {
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
            }
        }

        return match self.base() {
            TCTypeBase::I8 => Some(TCPrimType::I8),
            TCTypeBase::U8 => Some(TCPrimType::U8),
            TCTypeBase::I16 => Some(TCPrimType::I16),
            TCTypeBase::U16 => Some(TCPrimType::U16),
            TCTypeBase::I32 => Some(TCPrimType::I32),
            TCTypeBase::U32 => Some(TCPrimType::U32),
            TCTypeBase::I64 => Some(TCPrimType::I64),
            TCTypeBase::U64 => Some(TCPrimType::U64),
            TCTypeBase::F32 => Some(TCPrimType::F32),
            TCTypeBase::F64 => Some(TCPrimType::F64),
            TCTypeBase::Void => None,
            TCTypeBase::NamedStruct { .. } => None,
            TCTypeBase::UnnamedStruct { .. } => None,
            TCTypeBase::NamedUnion { .. } => None,
            TCTypeBase::UnnamedUnion { .. } => None,
            TCTypeBase::Typedef { refers_to, .. } => return refers_to.to_prim_type(),
            TCTypeBase::InternalTypedef(def) => return def.to_prim_type(),
        };
    }

    /// Panics if the type is incomplete
    fn deref(&self) -> Option<TCTypeRef> {
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
                TCTypeModifier::Array(_) | TCTypeModifier::VariableArray => return Some(to_ret),
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
        let (l_base, l_mods) = (l.base(), StackLL::new(l.mods()));
        let (r_base, r_mods) = (r.base(), StackLL::new(r.mods()));
        return Self::ty_eq_partial(l_base, &l_mods, r_base, &r_mods);
    }

    fn ty_eq_partial<'a, 'b>(
        l_base: TCTypeBase,
        l_mods: &StackLL<&[TCTypeModifier]>,
        r_base: TCTypeBase,
        r_mods: &StackLL<&[TCTypeModifier]>,
    ) -> bool {
        if let Some(l_typedef) = l_base.get_typedef() {
            if let Some(r_typedef) = r_base.get_typedef() {
                let (l_base, l_mods) = (l_typedef.base(), l_mods.child(l_typedef.mods()));
                let (r_base, r_mods) = (r_typedef.base(), r_mods.child(r_typedef.mods()));
                return Self::ty_eq_partial(l_base, &l_mods, r_base, &r_mods);
            } else {
                let (l_base, l_mods) = (l_typedef.base(), l_mods.child(l_typedef.mods()));
                return Self::ty_eq_partial(l_base, &l_mods, r_base, &r_mods);
            }
        }

        if let Some(r_typedef) = r_base.get_typedef() {
            let (r_base, r_mods) = (r_typedef.base(), r_mods.child(r_typedef.mods()));
            return Self::ty_eq_partial(l_base, &l_mods, r_base, &r_mods);
        }

        if l_base != r_base {
            return false;
        }

        let mut l_mods = l_mods.into_iter().map(|a| *a).flatten();
        let mut r_mods = r_mods.into_iter().map(|a| *a).flatten();
        loop {
            let (l, r) = if let Some(l_mod) = l_mods.next() {
                if let Some(r_mod) = r_mods.next() {
                    (l_mod, r_mod)
                } else {
                    return false;
                }
            } else {
                if let Some(r_mod) = r_mods.next() {
                    return false;
                } else {
                    return true;
                }
            };

            if l != r {
                return false;
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, Serialize)]
pub struct TCType {
    pub base: TCTypeBase,
    pub mods: &'static [TCTypeModifier],
}

impl CloneInto<'static> for TCTypeBase {
    type CloneOutput = Self;

    fn clone_into_alloc(&self, alloc: &impl Allocator) -> Self {
        match self {
            Self::InternalTypedef(ty) => {
                Self::InternalTypedef(alloc.new(ty.clone_into_alloc(alloc)))
            }
            &Self::Typedef { refers_to, typedef } => {
                let refers_to = alloc.new(refers_to.clone_into_alloc(alloc));

                Self::Typedef { refers_to, typedef }
            }
            x => *self,
        }
    }
}

impl CloneInto<'static> for TCTypeModifier {
    type CloneOutput = Self;

    fn clone_into_alloc(&self, alloc: &impl Allocator) -> Self {
        match self {
            Self::BeginParam(ty) => Self::BeginParam(ty.clone_into_alloc(alloc)),
            Self::Param(ty) => Self::Param(ty.clone_into_alloc(alloc)),
            _ => *self,
        }
    }
}

impl CloneInto<'static> for TCType {
    type CloneOutput = Self;

    fn clone_into_alloc(&self, alloc: &impl Allocator) -> Self {
        let mut mods = Vec::new();
        for modifier in self.mods {
            mods.push(modifier.clone_into_alloc(alloc));
        }
        let mods = alloc.add_slice(&*mods);

        return TCType {
            base: self.base.clone_into_alloc(alloc),
            mods,
        };
    }
}

impl TCType {
    pub fn array_mod(len: u64, loc: CodeLoc) -> Result<TCTypeModifier, Error> {
        if len >= u32::MAX as u64 {
            return Err(error!(
                "array length must be a number less than UINT_MAX",
                loc,
                format!("found here to be {}", len)
            ));
        }

        return Ok(TCTypeModifier::Array(len as u32));
    }

    pub fn to_func_type_strict(&self, alloc: &impl Allocator) -> Option<TCFuncType> {
        if self.mods.len() == 0 {
            if let Some(def) = self.get_typedef() {
                return def.to_func_type_strict(alloc);
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
                    types: alloc.add_slice(&*params),
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

    pub fn to_func_type(&self, alloc: &impl Allocator) -> Option<TCFuncType> {
        if self.mods.len() == 0 {
            return self.get_typedef()?.to_func_type(alloc);
        }

        if self.is_pointer() {
            let mut clone = self.clone();
            clone.mods = &self.mods[1..];
            return clone.to_func_type_strict(&alloc);
        }

        return self.to_func_type_strict(alloc);
    }

    /// Panics if the type is incomplete
    pub fn deref(&self) -> Option<TCType> {
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
                TCTypeModifier::Array(_) | TCTypeModifier::VariableArray => return Some(to_ret),
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

    pub fn func_parts_strict(&self) -> Option<(TCType, &'static [TCTypeModifier])> {
        if self.mods.len() == 0 {
            if let Some(def) = self.get_typedef() {
                return def.func_parts_strict();
            }

            return None;
        }

        let tc_func_type = match self.mods[0] {
            TCTypeModifier::UnknownParams => {
                let return_type = TCType {
                    base: self.base,
                    mods: &self.mods[1..],
                };

                return Some((return_type, &self.mods[..1]));
            }
            TCTypeModifier::NoParams => {
                let return_type = TCType {
                    base: self.base,
                    mods: &self.mods[1..],
                };

                return Some((return_type, &self.mods[..1]));
            }
            TCTypeModifier::BeginParam(param) => {
                let mut cursor = 1;

                while cursor < self.mods.len() {
                    match self.mods[cursor] {
                        TCTypeModifier::Param(_) | TCTypeModifier::VarargsParam => {}
                        _ => break,
                    }

                    cursor += 1;
                }

                let return_type = TCType {
                    base: self.base,
                    mods: &self.mods[cursor..],
                };

                return Some((return_type, &self.mods[..cursor]));
            }
            _ => return None,
        };
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

    pub fn to_ref(self, alloc: impl Allocator) -> TCType {
        TCType {
            base: self.base,
            mods: alloc.add_slice(&*self.mods),
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

    pub fn array_mod(&mut self) -> Option<&mut TCTypeModifier> {
        while self.mods.len() == 0 {
            if let Some(ty) = self.base.get_typedef() {
                let mut mods: Vec<_> = ty.mods.iter().map(|a| *a).collect();
                mods.append(&mut self.mods);
                self.mods = mods;
                self.base = ty.base;
                continue;
            }

            return None;
        }

        match &mut self.mods[0] {
            x @ TCTypeModifier::VariableArray => return Some(x),
            x @ TCTypeModifier::Array(_) => return Some(x),
            _ => return None,
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
    Push(&'static TCExpr), // Any type
    Opcode(Opcode),
}

#[derive(Debug, Clone, Copy)]
pub enum TCExprKind {
    Uninit,
    I8Lit(i8),
    U8Lit(u8),
    I16Lit(i16),
    U16Lit(u16),
    I32Lit(i32),
    U32Lit(u32),
    I64Lit(i64),
    U64Lit(u64),
    F32Lit(f32),
    F64Lit(f64),
    StringLit(&'static str),
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

    TypePun(&'static TCExpr),

    ArrayInit {
        elems: &'static [(TCExprKind, CodeLoc)],
        elem_ty: TCType,
    },
    StructLit {
        fields: &'static [TCExpr],
        size: u32,
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
        cond_ty: TCPrimType,
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

impl CloneInto<'static> for TCParamType {
    type CloneOutput = Self;

    fn clone_into_alloc(&self, alloc: &impl Allocator) -> Self::CloneOutput {
        let mut types = Vec::new();
        for ty in self.types {
            types.push(ty.clone_into_alloc(alloc));
        }
        let (types, varargs) = (alloc.add_slice(&*types), self.varargs);

        return Self { types, varargs };
    }
}

impl PartialEq<TCParamType> for TCParamType {
    fn eq(&self, o: &Self) -> bool {
        if self.varargs != o.varargs {
            return false;
        }

        if self.types.len() != o.types.len() {
            return false;
        }

        for (t1, t2) in self.types.iter().zip(o.types) {
            if !TCType::ty_eq(t1, t2) {
                return false;
            }
        }

        return true;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TCFuncType {
    pub return_type: TCType,
    pub params: Option<TCParamType>,
}

impl CloneInto<'static> for TCFuncType {
    type CloneOutput = Self;

    fn clone_into_alloc(&self, alloc: &impl Allocator) -> Self::CloneOutput {
        let return_type = self.return_type.clone_into_alloc(alloc);
        let params = self.params.clone_into_alloc(alloc);

        return Self {
            return_type,
            params,
        };
    }
}

impl PartialEq<TCFuncType> for TCFuncType {
    fn eq(&self, other: &Self) -> bool {
        if !TCType::ty_eq(&self.return_type, &other.return_type) {
            return false;
        }

        return self.params == other.params;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LabelOrLoc {
    Ident(u32), // symbol label local to the function
    Loc(CodeLoc),
}

impl LabelOrLoc {
    pub fn is_loc(&self) -> bool {
        let_expr!(LabelOrLoc::Loc(_) = self)
    }
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

impl TCDeclInit {
    pub fn is_static(&self) -> bool {
        let_expr!(Self::Static(_) = self)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TCGlobalVar {
    pub init: TCDeclInit,
    pub var_idx: u32,
    pub ty: TCType,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct TCStaticInternalVar {
    pub init: TCExprKind,
    pub ty: TCType,
    pub var_idx: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct TCFuncDefn {
    pub param_count: u32,
    pub sym_count: u32,
    pub label_count: u32,
    pub ops: &'static [TCOpcode],
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct TCFunction {
    pub is_static: bool,
    pub func_type: TCFuncType,
    pub decl_loc: CodeLoc,
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
    pub buckets: aliu::BucketList,
    pub file: u32,

    pub typedefs: HashMap<(u32, CodeLoc), TCType>,

    pub functions: HashMap<u32, TCFunction>,

    pub var_count: u32,
    pub vars: HashMap<u32, TCGlobalVar>,
    pub static_internal_vars: HashMap<CodeLoc, TCStaticInternalVar>,
}

pub struct TCDecl {
    pub ident: u32,
    pub init: TCDeclInit,
    pub ty: TCType,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct TCStructField {
    pub name: u32,
    pub ty: TCType,
    pub offset: u32,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct TCStructDefn {
    pub fields: &'static [TCStructField],
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct TCStruct {
    pub defn: Option<TCStructDefn>,
    pub sa: SizeAlign,
    pub decl_loc: CodeLoc,
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
    pub fn new(file: u32) -> Self {
        Self {
            buckets: aliu::BucketList::new(),
            file,

            typedefs: HashMap::new(),

            functions: HashMap::new(),

            var_count: 0,
            static_internal_vars: HashMap::new(),
            vars: HashMap::new(),
        }
    }
}
