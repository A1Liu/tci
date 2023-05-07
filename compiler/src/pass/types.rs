#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct TyId(u32);

#[allow(non_upper_case_globals)]
impl TyId {
    pub const Untyped: Self = TyId(0);
    pub const CheckFailure: Self = TyId(1);
    pub const Void: Self = TyId(2);
    pub const VoidPtr: Self = TyId(3);
    pub const U8: Self = TyId(4);
    pub const U16: Self = TyId(5);
    pub const U32: Self = TyId(6);
    pub const U64: Self = TyId(7);
    pub const S8: Self = TyId(8);
    pub const S16: Self = TyId(9);
    pub const S32: Self = TyId(10);
    pub const S64: Self = TyId(11);
    pub const F32: Self = TyId(12);
    pub const F64: Self = TyId(13);
}

impl core::fmt::Debug for TyId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            TyId::Untyped => f.write_str("Untyped"),
            TyId::CheckFailure => f.write_str("Untyped"),
            TyId::Void => f.write_str("Void"),
            TyId::VoidPtr => f.write_str("VoidPtr"),
            TyId::U8 => f.write_str("u8"),
            TyId::U16 => f.write_str("u16"),
            TyId::U32 => f.write_str("u32"),
            TyId::U64 => f.write_str("u64"),
            TyId::S8 => f.write_str("s8"),
            TyId::S16 => f.write_str("s16"),
            TyId::S32 => f.write_str("s32"),
            TyId::S64 => f.write_str("s64"),
            TyId::F32 => f.write_str("F32"),
            TyId::F64 => f.write_str("F64"),

            _ => write!(f, "TyId({})", self.0),
        }
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

        let base_types = &[
            TyId::Untyped,
            TyId::CheckFailure,
            TyId::Void,
            TyId::VoidPtr,
            TyId::U8,
            TyId::U16,
            TyId::U32,
            TyId::U64,
            TyId::S8,
            TyId::S16,
            TyId::S32,
            TyId::S64,
            TyId::F32,
            TyId::F64,
        ];

        for &ty in base_types {
            types.push(TypeInfo {
                kind: TypeKind::Qualified(ty),
                qualifiers: TyQuals::new(),
            })
        }

        return Self { types };
    }

    pub fn add_type(&mut self, id: TyId, qualifiers: TyQuals) -> TyId {
        if u8::from(qualifiers) == 0 {
            return id;
        }

        let next_id = self.types.len() as u32;
        self.types.push(TypeInfo {
            kind: TypeKind::Qualified(id),
            qualifiers,
        });

        return unsafe { core::mem::transmute(next_id) };
    }

    pub fn add_ptr(&mut self, id: TyId, qualifiers: TyQuals) -> TyId {
        let next_id = self.types.len() as u32;
        self.types.push(TypeInfo {
            kind: TypeKind::Pointer(id),
            qualifiers,
        });

        return unsafe { core::mem::transmute(next_id) };
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
