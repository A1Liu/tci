use crate::util::*;
use core::str;

pub const ECALL_PRINT_INT: u32 = 0;
pub const ECALL_PRINT_STR: u32 = 1;

/// - GetLocal gets a value from the stack at a given stack and variable offset
/// - SetLocal sets a value on the stack at a given stack and variable offset to the value at the top
///   of the stack
/// - Set and Get are equivalent of GetLocal and SetLocal, but the location they access is
///   determined by popping the top of the stack first
#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    Func(FuncDesc), // Function header used for callstack manipulation

    StackAlloc(u32),    // Allocates space on the stack
    StackAllocPtr(u32), // Allocates space on the stack, then pushes a pointer to that space onto the stack
    Alloc(u32), // Allocates space on the heap, then pushes a pointer to that space onto the stack

    MakeTempInt64(i64),
    MakeTempFloat64(f64),
    LoadStr(u32),

    Pop64,

    GetLocal64 { var: i32, offset: u32 },
    SetLocal64 { var: i32, offset: u32 },

    Get64 { offset: i32 },
    Set64 { offset: i32 },

    AddU64,
    SubI64,
    MulI64,
    DivI64,
    ModI64,

    JumpIfZero64(u32),
    JumpIfNotZero64(u32),

    Ret, // Returns to caller

    AddCallstackDesc(CallFrame),
    RemoveCallstackDesc,

    Call(u32),
    Ecall(u32),
}

#[derive(Debug, Clone, Copy)]
pub struct TaggedOpcode {
    pub op: Opcode,
    pub line: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct Program<'a> {
    pub files: &'a [&'a str],
    pub strings: &'a [&'a str],
    pub functions: &'a [&'a str],
    pub ops: &'a [TaggedOpcode],
}
