use crate::util::*;
use crate::IError;
use core::{fmt, mem};

pub enum Mutability {
    Mut,
    Const,
}

pub use Mutability::*;

#[derive(Clone, Copy, Debug)]
pub enum AllocTracker {
    StackLive { loc: CodeLoc, start: u32, len: u32 },
    StackDead { loc: CodeLoc },
    HeapLive { loc: CodeLoc, start: u32, len: u32 },
    HeapDead { loc: CodeLoc, free_loc: CodeLoc },
    Static { start: u32, len: u32 },
    Exe { start: u32, len: u32 },
}

impl AllocTracker {
    pub fn dereference(&self, ptr: VarPointer, mutable: Mutability) -> Result<(u32, u32), IError> {
        match *self {
            Self::StackLive { loc, start, len } => {
                return Ok((start, start + len));
            }

            Self::StackDead { loc } => {
                return ierr!(
                "InvalidPointer",
                "the pointer {} points to memory from a local variable, but that variable no longer exists because the function has already returned",
                ptr
            );
            }

            Self::HeapLive { loc, start, len } => {
                return Ok((start, start + len));
            }
            Self::HeapDead { loc, free_loc } => {
                return ierr!(
                    "InvalidPointer",
                    "the pointer {} points to freed memory",
                    ptr
                );
            }

            Self::Static { start, len } => {
                return Ok((start, start + len));
            }

            Self::Exe { start, len } => {
                if let Mut = mutable {
                    return ierr!("ModifiedExe", "tried to modify memory where code is stored");
                }

                return Ok((start, start + len));
            }
        }
    }

    pub fn is_live(&self) -> bool {
        return match *self {
            Self::StackLive { loc, start, len } => true,
            Self::StackDead { loc } => false,

            Self::HeapLive { loc, start, len } => true,
            Self::HeapDead { loc, free_loc } => false,

            Self::Static { start, len } => true,
            Self::Exe { start, len } => false,
        };
    }

    pub fn range(&self) -> Option<(u32, u32)> {
        let range = match *self {
            Self::StackLive { loc, start, len } => (start, start + len),
            Self::StackDead { loc } => {
                return None;
            }

            Self::HeapLive { loc, start, len } => (start, start + len),
            Self::HeapDead { loc, free_loc } => {
                return None;
            }

            Self::Static { start, len } => (start, start + len),

            Self::Exe { start, len } => (start, start + len),
        };

        return Some(range);
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Var<T> {
    pub idx: usize,
    pub meta: T,
}

impl<T> Var<T> {
    pub fn new(idx: usize, meta: T) -> Self {
        Self { idx, meta }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryData {
    pub data: Pod<u8>,
    pub vars: Pod<AllocTracker>,
}

impl BinaryData {
    pub fn new() -> Self {
        Self {
            data: Pod::new(),
            vars: Pod::new(),
        }
    }

    pub fn reserve_static(&mut self, len: u32) -> (VarPointer, &mut [u8]) {
        let data_len = self.data.len() as u32;

        self.data.reserve(len as usize);
        for _ in 0..len {
            self.data.push(0);
        }

        let id = self.vars.len() as u32;
        self.vars.push(AllocTracker::Static {
            start: data_len,
            len,
        });

        return (
            VarPointer::new(id, 0),
            &mut self.data[r(data_len, data_len + len)],
        );
    }

    pub fn add_exe_slice(&mut self, data: &[u8]) -> VarPointer {
        let data_len = self.data.len();

        self.data.extend_from_slice(data);

        let id = self.vars.len() as u32;
        self.vars.push(AllocTracker::Exe {
            start: data_len as u32,
            len: data.len() as u32,
        });

        return VarPointer::new(id, 0);
    }

    pub fn read<T: Copy>(&mut self, ptr: VarPointer) -> Option<T> {
        let var_idx = ptr.var_idx();
        let (lower, upper) = self.vars.get(var_idx)?.range()?;

        let data = &mut self.data[r(lower, upper)];
        let (idx, len) = (ptr.offset() as usize, mem::size_of::<T>());
        let from_bytes = data.get(idx..(idx + len))?;

        let mut out = mem::MaybeUninit::uninit();
        unsafe { any_as_u8_slice_mut(&mut out).copy_from_slice(from_bytes) };
        return Some(unsafe { out.assume_init() });
    }

    pub fn write<T: Copy>(&mut self, ptr: VarPointer, t: T) {
        let var_idx = ptr.var_idx();
        let (lower, upper) = self.vars[var_idx].range().unwrap();

        let data = &mut self.data[lower..upper];
        let (idx, len) = (ptr.offset() as usize, mem::size_of::<T>());
        let to_bytes = data.get_mut(idx..(idx + len)).unwrap();

        let from_bytes = any_as_u8_slice(&t);
        to_bytes.copy_from_slice(from_bytes);
    }
}

#[derive(Clone, Copy)]
pub struct VarPointer(u64);

impl fmt::Display for VarPointer {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        return write!(formatter, "0x{:0>16x}", self.0);
    }
}

impl fmt::Debug for VarPointer {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        return write!(formatter, "0x{:0>16x}", self.0);
    }
}
impl VarPointer {
    pub const TOP_BITS: u64 = (u32::MAX as u64) << 32;
    pub const BOTTOM_BITS: u64 = u32::MAX as u64;

    pub fn new(idx: u32, offset: u32) -> VarPointer {
        let idx = from_id(idx);
        let (idx, offset) = ((idx as u64) << 32, offset as u64);

        return Self(idx | offset);
    }

    pub fn var_idx(self) -> usize {
        let top = (self.0 >> 32) as u32;
        let top = to_id(top) as usize;

        return top;
    }

    pub fn with_offset(self, offset: u32) -> Self {
        return Self((self.0 & Self::TOP_BITS) | (offset as u64));
    }

    pub fn offset(self) -> u32 {
        return (self.0 & Self::BOTTOM_BITS) as u32;
    }

    pub fn add(self, add: u64) -> Self {
        return Self(self.0.wrapping_add(add));
    }

    pub fn sub(self, sub: u64) -> Self {
        return Self(self.0.wrapping_sub(sub));
    }

    pub fn align(self, align: u64) -> Self {
        return Self(align_u64(self.0, align));
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LinkName {
    pub name: u32,
    pub file: n32,
}

impl LinkName {
    pub fn new(name: u32) -> Self {
        Self {
            name,
            file: n32::NULL,
        }
    }

    pub fn new_static(name: u32, file: u32) -> Self {
        Self {
            name,
            file: file.into(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CallFrame {
    pub name: LinkName,
    pub loc: CodeLoc,
    pub fp: u16,
    pub pc: VarPointer,
}

impl CallFrame {
    pub fn new(name: LinkName, loc: CodeLoc, fp: u16, pc: VarPointer) -> Self {
        Self { name, loc, fp, pc }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, strum::EnumString)]
#[repr(u8)]
pub enum Opcode {
    Func,
    Loc,

    StackAlloc,
    StackDealloc,

    Make8,
    Make16,
    Make32,
    Make64,
    MakeFp,
    MakeSp,

    PushUndef,
    Pop,
    Swap,
    Dup,
    PushDyn,

    SExtend8To16,
    SExtend8To32,
    SExtend8To64,
    SExtend16To32,
    SExtend16To64,
    SExtend32To64,

    ZExtend8To16,
    ZExtend8To32,
    ZExtend8To64,
    ZExtend16To32,
    ZExtend16To64,
    ZExtend32To64,

    I64ToF32,
    U64ToF32,
    I64ToF64,
    U64ToF64,

    F32ToI64,
    F32ToU64,
    F64ToI64,
    F64ToU64,

    F32ToF64,
    F64ToF32,

    Get,
    Set,

    BoolNorm8,
    BoolNorm16,
    BoolNorm32,
    BoolNorm64,

    BoolNot8,
    BoolNot16,
    BoolNot32,
    BoolNot64,

    Add8,
    Add16,
    Add32,
    Add64,
    AddF32,
    AddF64,

    SubI8,
    SubU8,
    SubI16,
    SubU16,
    SubI32,
    SubU32,
    SubI64,
    SubU64,
    SubF32,
    SubF64,

    MulI8,
    MulU8,
    MulI16,
    MulU16,
    MulI32,
    MulU32,
    MulI64,
    MulU64,
    MulF32,
    MulF64,

    DivI8,
    DivU8,
    DivI16,
    DivU16,
    DivI32,
    DivU32,
    DivI64,
    DivU64,
    DivF32,
    DivF64,

    ModI8,
    ModU8,
    ModI16,
    ModU16,
    ModI32,
    ModU32,
    ModI64,
    ModU64,
    ModF32,
    ModF64,

    CompLtI8,
    CompLtU8,
    CompLtI16,
    CompLtU16,
    CompLtI32,
    CompLtU32,
    CompLtI64,
    CompLtU64,
    CompLtF32,
    CompLtF64,

    CompLeqI8,
    CompLeqU8,
    CompLeqI16,
    CompLeqU16,
    CompLeqI32,
    CompLeqU32,
    CompLeqI64,
    CompLeqU64,
    CompLeqF32,
    CompLeqF64,

    CompEq8,
    CompEq16,
    CompEq32,
    CompEq64,
    CompEqF32,
    CompEqF64,

    CompNeq8,
    CompNeq16,
    CompNeq32,
    CompNeq64,
    CompNeqF32,
    CompNeqF64,

    RShiftI8,
    RShiftU8,
    RShiftI16,
    RShiftU16,
    RShiftI32,
    RShiftU32,
    RShiftI64,
    RShiftU64,

    LShiftI8,
    LShiftU8,
    LShiftI16,
    LShiftU16,
    LShiftI32,
    LShiftU32,
    LShiftI64,
    LShiftU64,

    BitAnd8,
    BitAnd16,
    BitAnd32,
    BitAnd64,

    BitOr8,
    BitOr16,
    BitOr32,
    BitOr64,

    BitXor8,
    BitXor16,
    BitXor32,
    BitXor64,

    BitNot8,
    BitNot16,
    BitNot32,
    BitNot64,

    Jump,

    JumpIfZero8,
    JumpIfZero16,
    JumpIfZero32,
    JumpIfZero64,

    JumpIfNotZero8,
    JumpIfNotZero16,
    JumpIfNotZero32,
    JumpIfNotZero64,

    Ret,
    Call,

    AllocBegin,
    AllocEnd,
    HeapAlloc,
    HeapDealloc,

    // For use with varargs, so we can get the next variable argument from a
    // decrementing stack id
    MakeStackId,
    TranslateStackId,

    CopySrcToDest,
    Memset,

    Throw,

    Ecall,

    AssertStr,
}

// ABI matters here. This enum is linked to /lib/header/tci.h
#[repr(u32)]
#[derive(Debug, Clone, Copy)]
pub enum Ecall {
    /// exit the program with an error code
    Exit = 0,
    /// get the number of arguments in the program.
    Argc,
    /// get zero-indexed command line argument. Takes in a single int as a parameter,
    /// and pushes a pointer to the string on the heap as the result.
    Argv,

    /// Open a file descriptor (with options)
    OpenFd,
    /// read from a file descriptor
    ReadFd,
    /// write to a file descriptor
    WriteFd,
    /// append to a file descriptor
    AppendFd,
}

#[derive(Debug, Clone)]
pub enum EcallExt {
    Exit(i32),

    OpenFd {
        name: VarPointer,
        open_mode: OpenMode,
    },
    ReadFd {
        len: u32,
        buf: VarPointer,
        begin: u32,
        fd: u32,
    },
    WriteFd {
        buf: VarPointer,
        len: u32,
        begin: u32,
        fd: u32,
    },
    AppendFd {
        buf: VarPointer,
        len: u32,
        fd: u32,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum WriteEvt {
    StdinWrite,
    StdoutWrite,
    StderrWrite,
    StdlogWrite,
    CreateFile { fd: u32 },
    ClearFd { fd: u32 },
    WriteFd { begin: u32, fd: u32 },
    AppendFd { fd: u32 },
}

// ABI matters here. This enum is linked to /lib/header/tci.h
#[repr(u32)]
#[derive(Debug, Clone, Copy, serde_repr::Serialize_repr, serde_repr::Deserialize_repr)]
pub enum EcallError {
    // Files
    DoesntExist = 1,
    NameNotUTF8 = 2,
    TooManyFiles = 3,
    FilesTooLarge = 4,
    OutOfRange = 5,

    // stdin/stdout/stderr misuse
    ReadTermOut = 6,
    ReadTermErr = 7,
    ReadTermLog = 8,
    WriteTermIn = 9,
    StreamLen = 10,

    InvalidOpenMode = 11,
}

impl EcallError {
    pub fn to_u64(self) -> u64 {
        (self as u32 as u64) << 32
    }
}

// ABI matters here. This enum is linked to /lib/impl/files.c
#[repr(u32)]
#[derive(Debug, Clone, Copy, serde_repr::Serialize_repr)]
pub enum OpenMode {
    Read = 0,
    Create = 1,
    CreateClear = 2,
}

#[derive(Debug, Clone, Copy)]
pub enum FdKind {
    TermIn,
    TermOut,
    TermErr,
    TermLog,

    FileSys(u32),
    ProcessStdin(u32),
    ProcessStdout(u32),
    ProcessStderr(u32),
}

const ID_MASK: u32 = 0b10100110_01101010_01001010_10101010;
const ID_ADD: u32 = 2740160927;

// These two numbers are multiplicative inverses mod 2^32
const ID_MUL_TO: u32 = 0x01000193;
const ID_MUL_FROM: u32 = 0x359c449b;

// const ID_ROTATE_BITS: u32 = 16;
// let s2 = s1.swap_bytes();
// let s5 = s4.rotate_left(ID_ROTATE_BITS);

pub fn to_id(raw: u32) -> u32 {
    let s1 = raw ^ ID_MASK;
    let s2 = s1.wrapping_mul(ID_MUL_TO);
    let s3 = s2.wrapping_sub(ID_ADD);

    return s3;
}

pub fn from_id(id: u32) -> u32 {
    let s3 = id.wrapping_add(ID_ADD);
    let s2 = s3.wrapping_mul(ID_MUL_FROM);
    let s1 = s2 ^ ID_MASK;

    return s1;
}

#[test]
fn id_test() {
    assert_eq!(ID_MUL_TO.wrapping_mul(ID_MUL_FROM), 1);

    let tests = &[ID_MASK, ID_ADD, ID_MUL_TO, ID_MUL_FROM];

    for id in 0..100 {
        let value = from_id(id);
        let out_id = to_id(value);

        // println!("{} -> {}", id, value);

        // println!("{:>10}", value);

        assert_eq!(id, out_id);
    }

    for &id in tests {
        let value = from_id(id);
        let out_id = to_id(value);

        // println!("{} -> {}", id, value);

        assert_eq!(id, out_id);
    }

    for value in 0..100 {
        let id = to_id(value);
        let out_value = from_id(id);

        // println!("{} -> {}", id, value);

        assert_eq!(value, out_value);
    }

    // the null address needs to not be a sequential ID that we'll use often
    assert_eq!(to_id(0), u32::MAX);
}
