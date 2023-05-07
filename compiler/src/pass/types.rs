#[non_exhaustive]
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TyId {
    Untyped,
    CheckFailure,

    Void,
    VoidPtr,

    U8,
    U16,
    U32,
    U64,

    S8,
    S16,
    S32,
    S64,

    F32,
    F64,
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
