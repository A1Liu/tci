use std::sync::Mutex;

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct TyId(u32);

macro_rules! ty_id_defns {
    ($idx:expr ; ) => {};

    ($idx:expr ; $id:ident $(, $rest:tt )* ) => {
        pub const $id : TyId = TyId($idx);
        ty_id_defns!($idx + 1 ; $( $rest ),* );
    };

    ( $( ($id:ident, $fmt:literal ) ),* ) => {
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
                name: $fmt,
                ptr: if TyId::$id.0 % 2 == 0 && TyId::$id.0 > 1 { Some(TyId(TyId::$id.0 + 1)) } else { None },
                deref: if TyId::$id.0 % 2 == 1 && TyId::$id.0 > 1 { Some(TyId(TyId::$id.0 - 1)) } else { None },
            },
        )*

        ];
    };
}

ty_id_defns!(
    (Untyped, "untyped"),
    (CheckFailure, "fail"),
    (Void, "void"),
    (PtrVoid, "*void"),
    (U8, "u8"),
    (PtrU8, "*u8"),
    (U16, "u16"),
    (PtrU16, "*u16"),
    (U32, "u32"),
    (PtrU32, "*u32"),
    (U64, "u64"),
    (PtrU64, "*u64"),
    (S8, "s8"),
    (PtrS8, "*s8"),
    (S16, "s16"),
    (PtrS16, "*s16"),
    (S32, "s32"),
    (PtrS32, "*s32"),
    (S64, "s64"),
    (PtrS64, "*s64"),
    (F32, "f32"),
    (PtrF32, "*f32"),
    (F64, "f64"),
    (PtrF64, "*f64")
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
    types: Mutex<TypeInfoVec>,
    param_values: Mutex<Vec<TyId>>,
}

impl core::fmt::Debug for TyDb {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("TyDb()")
    }
}

impl Default for TyDb {
    fn default() -> Self {
        return Self::new();
    }
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

        return Self {
            types: Mutex::new(types),
            param_values: Mutex::new(Vec::new()),
        };
    }

    fn add(&self, kind: TypeKind, qualifiers: TyQuals) -> TyId {
        let mut types = self.types.lock().unwrap();

        let next_id = types.len() as u32;
        types.push(TypeInfo { kind, qualifiers });

        return TyId(next_id);
    }

    pub fn add_type(&self, id: TyId, qualifiers: TyQuals) -> TyId {
        if u8::from(qualifiers) == 0 {
            return id;
        }

        return self.add(TypeKind::Qualified(id), qualifiers);
    }

    pub fn add_ptr(&self, id: TyId, qualifiers: TyQuals) -> TyId {
        if let Some(TyIdInfo { ptr: Some(i), .. }) = TY_ID_INFO.get(id.0 as usize) {
            return *i;
        }

        return self.add(TypeKind::Pointer(id), qualifiers);
    }

    pub fn add_func(&self, ret_type: TyId, params: &[TyId]) -> TyId {
        let mut param_values = self.param_values.lock().unwrap();
        let begin = param_values.len() as u32;
        param_values.push(ret_type);
        param_values.extend(params.into_iter());

        // TODO: length check

        return self.add(
            TypeKind::Function {
                params_begin_index: begin,
                param_count: params.len().try_into().unwrap(),
            },
            TyQuals::new(),
        );
    }

    pub fn format(&self, id: TyId) -> String {
        let mut out = String::new();

        self.write(&mut out, id);

        return out;
    }

    pub fn write(&self, out: &mut String, id: TyId) {
        if let Some(info) = TY_ID_INFO.get(id.0 as usize) {
            *out += info.name;
            return;
        }

        let ty = {
            // take mutex for as little time as possible
            let types = self.types.lock().unwrap();
            types.index(id.0 as usize).to_owned()
        };

        match ty.kind {
            TypeKind::Qualified(t) => self.write(out, t),
            TypeKind::Pointer(p) => {
                out.push('*');
                self.write(out, p);
            }
            TypeKind::Function {
                params_begin_index,
                param_count,
            } => {
                let param_count = param_count as usize;
                let begin = params_begin_index as usize;

                // Extra slot for the return type
                let end = begin + param_count + 1;

                let (ret, params) = {
                    let params = self.param_values.lock().unwrap();
                    let params = &params.as_slice()[begin..end];

                    (params[0], params[1..].to_owned())
                };

                out.push('(');

                let mut comma = false;
                for t in params {
                    if comma {
                        out.push(',');
                    }

                    self.write(out, t);
                    comma = true;
                }

                out.push_str(") => ");

                self.write(out, ret);
            }
        }
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
    Function {
        params_begin_index: u32,
        param_count: u16,
    },
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
