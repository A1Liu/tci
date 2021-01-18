use crate::util::*;
use core::mem;
use std::fmt;

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

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryData {
    pub data: Vec<u8>,
    pub vars: Vec<Var<()>>,
}

impl BinaryData {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            vars: Vec::new(),
        }
    }

    pub fn add_data(&mut self, data: &mut Vec<u8>) -> VarPointer {
        let data_len = self.data.len();
        self.data.append(data);
        self.vars.push(Var::new(data_len, ()));
        return VarPointer::new_binary(self.vars.len() as u32, 0);
    }

    pub fn add_slice(&mut self, data: &[u8]) -> VarPointer {
        let data_len = self.data.len();
        self.data.extend_from_slice(data);
        self.vars.push(Var::new(data_len, ()));
        return VarPointer::new_binary(self.vars.len() as u32, 0);
    }

    pub fn read<T: Copy>(&mut self, ptr: VarPointer) -> Option<T> {
        if ptr.var_idx() == 0 {
            return None;
        }

        let var_idx = ptr.var_idx() - 1;
        let lower = self.vars.get(var_idx)?.idx;
        let upper = self.vars.get(var_idx + 1).map(|a| a.idx);
        let upper = upper.unwrap_or(self.data.len());

        let data = &mut self.data[lower..upper];
        let (idx, len) = (ptr.offset() as usize, mem::size_of::<T>());
        let from_bytes = data.get(idx..(idx + len))?;

        let mut out = mem::MaybeUninit::uninit();
        unsafe { any_as_u8_slice_mut(&mut out).copy_from_slice(from_bytes) };
        return Some(unsafe { out.assume_init() });
    }

    pub fn write<T: Copy>(&mut self, ptr: VarPointer, t: T) {
        if ptr.var_idx() == 0 {
            panic!("passed in nullish pointer");
        }

        let var_idx = ptr.var_idx() - 1;
        let lower = self.vars.get(var_idx).unwrap().idx;
        let upper = self.vars.get(var_idx + 1).map(|a| a.idx);
        let upper = upper.unwrap_or(self.data.len());

        let data = &mut self.data[lower..upper];
        let (idx, len) = (ptr.offset() as usize, mem::size_of::<T>());
        let to_bytes = data.get_mut(idx..(idx + len)).unwrap();

        let from_bytes = any_as_u8_slice(&t);
        to_bytes.copy_from_slice(from_bytes);
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct VarPointerFields {
    _offset: u32,
    _idx: u16,
    _tid: u16,
}

#[derive(Clone, Copy)]
pub union VarPointer {
    fields: VarPointerFields,
    value: u64,
}

impl fmt::Display for VarPointer {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let fields = self.fields();

        return write!(
            formatter,
            "0x{:x}{:0>4x}{:0>8x}",
            u16::from_le(fields._tid),
            u16::from_le(fields._idx),
            u32::from_le(fields._offset)
        );
    }
}

impl fmt::Debug for VarPointer {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let fields = self.fields();

        return write!(
            formatter,
            "0x{:x}{:0>4x}{:0>8x}",
            u16::from_le(fields._tid),
            u16::from_le(fields._idx),
            u32::from_le(fields._offset)
        );
    }
}
impl VarPointer {
    pub const BINARY_BIT: u16 = 1u16 << 15;
    pub const STACK_BIT: u16 = 1u16 << 14;
    pub const RESERVED_BITS: u16 = Self::BINARY_BIT | Self::STACK_BIT;

    #[inline]
    pub fn fields(&self) -> VarPointerFields {
        unsafe { self.fields }
    }

    #[inline]
    pub fn fields_mut(&mut self) -> &mut VarPointerFields {
        unsafe { &mut self.fields }
    }

    pub fn new_stack(idx: u16, offset: u32) -> VarPointer {
        Self {
            fields: VarPointerFields {
                _tid: Self::STACK_BIT.to_le(),
                _idx: idx.to_le(),
                _offset: offset.to_le(),
            },
        }
    }

    pub fn new_heap(idx: u32, offset: u32) -> VarPointer {
        let tid = (idx >> 16) as u16;
        if tid & Self::RESERVED_BITS != 0 {
            panic!("idx is too large");
        }

        Self {
            fields: VarPointerFields {
                _tid: tid.to_le(),
                _idx: (idx as u16).to_le(),
                _offset: offset.to_le(),
            },
        }
    }

    pub fn new_binary(idx: u32, offset: u32) -> VarPointer {
        let tid = (idx >> 16) as u16;
        if tid & Self::RESERVED_BITS != 0 {
            panic!("idx is too large");
        }

        let tid = tid | Self::BINARY_BIT;

        Self {
            fields: VarPointerFields {
                _tid: tid.to_le(),
                _idx: (idx as u16).to_le(),
                _offset: offset.to_le(),
            },
        }
    }

    pub fn is_stack(&self) -> bool {
        return (u16::from_le(self.fields()._tid) & Self::STACK_BIT) != 0;
    }

    pub fn is_binary(&self) -> bool {
        return (u16::from_le(self.fields()._tid) & Self::BINARY_BIT) != 0;
    }

    pub fn is_heap(&self) -> bool {
        return (u16::from_le(self.fields()._tid) & Self::RESERVED_BITS) == 0;
    }

    // returns u16::MAX if not attached to a thread
    pub fn tid(&self) -> u16 {
        if self.is_stack() {
            return u16::from_le(self.fields()._tid) & !Self::RESERVED_BITS;
        }

        return u16::MAX;
    }

    pub fn var_idx(self) -> usize {
        if self.is_stack() {
            return u16::from_le(self.fields()._idx) as usize;
        }

        let top = ((u16::from_le(self.fields()._tid) & !Self::RESERVED_BITS) as u32) << 16;
        return (top | u16::from_le(self.fields()._idx) as u32) as usize;
    }

    pub fn with_offset(self, offset: u32) -> Self {
        let mut ptr = self;
        ptr.fields_mut()._offset = offset.to_le();
        return ptr;
    }

    pub fn offset(self) -> u32 {
        u32::from_le(self.fields()._offset)
    }

    pub fn set_offset(&mut self, offset: u32) {
        self.fields_mut()._offset = offset.to_le();
    }

    pub fn add(self, add: u64) -> Self {
        Self {
            value: (u64::from_le(unsafe { self.value }).wrapping_add(add)).to_le(),
        }
    }

    pub fn sub(self, add: u64) -> Self {
        Self {
            value: (u64::from_le(unsafe { self.value }).wrapping_sub(1)).to_le(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, serde::Serialize)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Opcode {
    Func,
    Loc,

    StackAlloc,
    StackDealloc,

    MakeI8,
    MakeU8,
    MakeI16,
    MakeU16,
    MakeI32,
    MakeU32,
    MakeI64,
    MakeU64,
    MakeF32,
    MakeF64,
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

    I8ToF32,
    U8ToF32,
    I8ToF64,
    U8ToF64,
    I16ToF32,
    U16ToF32,
    I16ToF64,
    U16ToF64,
    I32ToF32,
    U32ToF32,
    I32ToF64,
    U32ToF64,
    I64ToF32,
    U64ToF32,
    I64ToF64,
    U64ToF64,

    F32ToI8,
    F32ToU8,
    F64ToI8,
    F64ToU8,
    F32ToI16,
    F32ToU16,
    F64ToI16,
    F64ToU16,
    F32ToI32,
    F32ToU32,
    F64ToI32,
    F64ToU32,
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
    Ecall,
}

#[derive(Debug)]
pub enum WriteEvent {
    StdoutWrite,
    StderrWrite,
}

#[derive(Debug, Clone, Copy)]
pub enum RuntimeStatus {
    Running,
    Exited(i32),
}
