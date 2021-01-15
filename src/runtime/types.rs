use crate::util::*;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Var<T> {
    pub idx: usize,
    pub meta: T,
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
            value: (u64::from_le(unsafe { self.value }).wrapping_add(1)).to_le(),
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

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Opcode {
    Func,

    StackAlloc,
    StackDealloc,

    MakeI8,
    MakeI32,
    MakeU32,
    MakeI64,
    MakeU64,
    MakeF32,
    MakeF64,
    MakeBinaryPtr,
    MakeFp,
    MakeSp,

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

    Get,
    Set,

    AddU32,
    AddU64,

    SubI32,
    SubI64,
    SubU64,

    MulI32,
    MulI64,
    MulU64,

    DivI32,
    DivI64,
    DivU64,

    CompLtI32,
    CompLtU64,
    CompLeqI32,
    CompLeqU64,

    CompEq32,
    CompEq64,
    CompNeq32,
    CompNeq64,

    ModI32,
    ModI64,

    RShiftI32,
    LShiftI32,

    BitAndI8,
    BitAndI32,
    BitOrI8,
    BitOrI32,
    BitXorI32,

    Jump,

    JumpIfZero8,
    JumpIfZero16,
    JumpIfZero32,
    JumpIfZero64,

    JumpIfNotZero8,
    JumpIfNotZero16,
    JumpIfNotZero32,
    JumpIfNotZero64,

    Call,
    Ecall,
}

#[derive(Debug)]
pub enum WriteEvent {
    StdoutWrite,
    StderrWrite,
}
