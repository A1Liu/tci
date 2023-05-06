#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TyId(u32);

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
