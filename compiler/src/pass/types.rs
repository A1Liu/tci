#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct TyId(u32);

macro_rules! ty_id_defns {
    ($idx:expr ; ) => {};

    ($idx:expr ; $id:ident $(, $rest:ident )* ) => {
        pub const $id : TyId = TyId($idx);
        ty_id_defns!($idx + 1 ; $( $rest ),* );
    };

    ($($id:ident),* ) => {
        struct TyIdInfo {
            id: TyId,
            name: &'static str,
            ptr: Option<TyId>,
            deref: Option<TyId>,
        }

        #[allow(non_upper_case_globals)]
        impl TyId {
            ty_id_defns!(0 ; $( $id ),* );
        }

        const TY_ID_INFO: &'static [TyIdInfo] = &[
        $(
            TyIdInfo {
                id: TyId::$id,
                name: stringify!($id),
                ptr: if TyId::$id.0 % 2 == 0 && TyId::$id.0 > 1 { Some(TyId(TyId::$id.0 + 1)) } else { None },
                deref: if TyId::$id.0 % 2 == 1 && TyId::$id.0 > 1 { Some(TyId(TyId::$id.0 - 1)) } else { None },
            },
        )*

        ];
    };
}

ty_id_defns!(
    Untyped,
    CheckFailure,
    Void,
    PtrVoid,
    U8,
    PtrU8,
    U16,
    PtrU16,
    U32,
    PtrU32,
    U64,
    PtrU64,
    S8,
    PtrS8,
    S16,
    PtrS16,
    S32,
    PtrS32,
    S64,
    PtrS64,
    F32,
    PtrF32,
    F64,
    PtrF64
);

impl core::fmt::Debug for TyId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(info) = TY_ID_INFO.get(self.0 as usize) {
            debug_assert!(info.id == *self);

            return f.write_str(info.name);
        }

        return write!(f, "TyId({})", self.0);
    }
}

impl Default for TyId {
    fn default() -> Self {
        Self::Untyped
    }
}

impl From<u64> for TyId {
    fn from(value: u64) -> Self {
        Self(value as u32)
    }
}

impl Into<u64> for TyId {
    fn into(self) -> u64 {
        self.0 as u64
    }
}

pub struct TyDb {
    types: TypeInfoVec,
}

impl TyDb {
    pub fn new() -> TyDb {
        let mut types = TypeInfoVec::new();

        for info in TY_ID_INFO {
            types.push(TypeInfo {
                kind: TypeKind::Qualified(info.id),
                qualifiers: TyQuals::new(),
            })
        }

        return Self { types };
    }

    fn add(&mut self, kind: TypeKind, qualifiers: TyQuals) -> TyId {
        let next_id = self.types.len() as u32;
        self.types.push(TypeInfo { kind, qualifiers });

        return TyId(next_id);
    }

    pub fn add_type(&mut self, id: TyId, qualifiers: TyQuals) -> TyId {
        if u8::from(qualifiers) == 0 {
            return id;
        }

        return self.add(TypeKind::Qualified(id), qualifiers);
    }

    pub fn add_ptr(&mut self, id: TyId, qualifiers: TyQuals) -> TyId {
        if let Some(TyIdInfo { ptr: Some(i), .. }) = TY_ID_INFO.get(id.0 as usize) {
            return *i;
        }

        return self.add(TypeKind::Pointer(id), qualifiers);
    }
}

#[derive(StructOfArray, Debug, Clone, Copy)]
pub struct TypeInfo {
    kind: TypeKind,
    qualifiers: TyQuals,
}

#[derive(Debug, Clone, Copy)]
pub enum TypeKind {
    Qualified(TyId),
    Pointer(TyId),
}

#[bitfield(u8)]
pub struct TyQuals {
    pub const_: bool,
    pub atomic_: bool,
    pub volatile_: bool,
    pub restrict_: bool,

    #[bits(4)]
    a: usize,
}

impl From<u64> for TyQuals {
    fn from(value: u64) -> Self {
        Self::from(value as u8)
    }
}

impl Into<u64> for TyQuals {
    fn into(self) -> u64 {
        u8::from(self) as u64
    }
}

/*
// I'm thinking of making the type database based on IDs
// The bottom bit tells you whether its a pointer
// Then, if it's not a pointer, you can check other things
// about it, e.g. read from the type database to get more info about it
#[non_exhaustive]
#[repr(u32)]
#[derive(Clone, Copy)]
pub enum TyIdIdk {
    Untyped = 0,
    CheckFailure = 2,

    Void = 4,
    VoidPtr = 5,

    U8 = 6,
    U16 = 8,
    U32 = 10,
    U64 = 12,

    S8 = 14,
    S16 = 16,
    S32 = 18,
    S64 = 20,

    F32 = 22,
    F64 = 24,
}

pub enum TyData {
    Qualified { id: TyId },
    Struct { id: u32 },
    Union { id: u32 },
    Pointer { base: u32 },
    ArrayWithSize { id: TyId, size: u16 },
    // TODO:

    // Something something here, idk
    // ArrayVarLen { base: u32 },

    // Could do some kind of like, Function {}, Param {}, EndFunction {} thing
    // or just place all of it into a separate table
    // Not quite sure yet.
    // Function { func_signature_id: u32 },
}

 */
