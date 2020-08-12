use crate::util::*;
use core::str;
use std::collections::HashMap;

pub const ECALL_PRINT_INT: u32 = 0;
pub const ECALL_PRINT_STR: u32 = 1;

#[derive(Debug, Clone)]
pub enum PseudoOp {
    StackAlloc(u32),
    StackAllocPtr(u32),
    Alloc(u32),

    MakeTempInt64(i64),
    LoadString(String),

    GetLocal64 {
        var: i32,
        offset: u32,
        line: u32,
    },
    SetLocal64 {
        var: i32,
        offset: u32,
        line: u32,
    },
    Get64 {
        offset: i32,
        line: u32,
    },
    Set64 {
        offset: i32,
        line: u32,
    },

    Ret,

    AddCallstackDesc(CallFrame),
    RemoveCallstackDesc,

    Call {
        file: String,
        func: String,
        line: u32,
    },
    Ecall {
        call: u32,
        line: u32,
    },
}

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

    Pop8,
    Pop16,
    Pop32,
    Pop64,

    GetLocal64 { var: i32, offset: u32, line: u32 },
    SetLocal64 { var: i32, offset: u32, line: u32 },
    GetLocal32 { var: i32, offset: u32, line: u32 },
    SetLocal32 { var: i32, offset: u32, line: u32 },
    GetLocal16 { var: i32, offset: u32, line: u32 },
    SetLocal16 { var: i32, offset: u32, line: u32 },
    GetLocal8 { var: i32, offset: u32, line: u32 },
    SetLocal8 { var: i32, offset: u32, line: u32 },

    Get64 { offset: i32, line: u32 },
    Set64 { offset: i32, line: u32 },
    Get32 { offset: i32, line: u32 },
    Set32 { offset: i32, line: u32 },
    Get16 { offset: i32, line: u32 },
    Set16 { offset: i32, line: u32 },
    Get8 { offset: i32, line: u32 },
    Set8 { offset: i32, line: u32 },

    AddU64,
    SubI64,
    MulI64,
    DivI64,
    ModI64,

    JumpIfZero64(u32),
    JumpIfNotZero64(u32),
    JumpIfZero32(u32),
    JumpIfNotZero32(u32),
    JumpIfZero16(u32),
    JumpIfNotZero16(u32),
    JumpIfZero8(u32),
    JumpIfNotZero8(u32),

    Ret, // Returns to caller

    AddCallstackDesc(CallFrame),
    RemoveCallstackDesc,

    Call { func: u32, line: u32 },
    Ecall { call: u32, line: u32 },
}

impl Opcode {
    pub fn line(self) -> u32 {
        match self {
            Opcode::GetLocal64 { line, .. } => line,
            Opcode::SetLocal64 { line, .. } => line,
            Opcode::Get64 { line, .. } => line,
            Opcode::Set64 { line, .. } => line,
            Opcode::Ecall { line, .. } => line,
            Opcode::Call { line, .. } => line,
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Program<'a> {
    pub files: &'a [&'a str],
    pub strings: &'a [&'a str],
    pub functions: &'a [&'a str],
    pub ops: &'a [Opcode],
    unused: (), // prevents construction without Program::new
}

impl<'a> Program<'a> {
    pub fn new(program: HashMap<String, HashMap<String, Vec<PseudoOp>>>) -> Self {
        let mut bytes = Vec::new();
        let mut file_ranges = Vec::new();
        let mut string_ranges = Vec::new();
        let mut function_ranges = Vec::new();

        let mut function_ids = HashMap::new();
        let mut pseudo_functions = Vec::new();

        for (file_number, (file_name, functions_)) in program.into_iter().enumerate() {
            let start = bytes.len();
            bytes.extend_from_slice(file_name.as_bytes());
            file_ranges.push(start..bytes.len());

            for (name, ops) in functions_.into_iter() {
                let start = bytes.len();
                bytes.extend_from_slice(name.as_bytes());
                function_ranges.push(start..bytes.len());
                let file = file_number as u32;
                function_ids.insert((file_name.clone(), name), pseudo_functions.len() as u32);
                pseudo_functions.push((file, ops));
            }
        }

        let mut function_locations = HashMap::new();
        let mut ops = Vec::new();
        for (name, (file, body)) in pseudo_functions.into_iter().enumerate() {
            let name = name as u32;
            function_locations.insert(name, ops.len() as u32);

            ops.push(Opcode::Func(FuncDesc { file, name }));

            for op in body.into_iter() {
                let op = match op {
                    PseudoOp::StackAlloc(space) => Opcode::StackAlloc(space),
                    PseudoOp::StackAllocPtr(space) => Opcode::StackAllocPtr(space),
                    PseudoOp::Alloc(space) => Opcode::StackAllocPtr(space),

                    PseudoOp::MakeTempInt64(int) => Opcode::MakeTempInt64(int),
                    PseudoOp::LoadString(string) => {
                        let string_index = string_ranges.len();
                        let start = bytes.len();
                        bytes.extend_from_slice(string.as_bytes());
                        string_ranges.push(start..bytes.len());
                        Opcode::LoadStr(string_index as u32)
                    }

                    PseudoOp::GetLocal64 { var, offset, line } => {
                        Opcode::GetLocal64 { var, offset, line }
                    }
                    PseudoOp::SetLocal64 { var, offset, line } => {
                        Opcode::SetLocal64 { var, offset, line }
                    }
                    PseudoOp::Get64 { offset, line } => Opcode::Get64 { offset, line },
                    PseudoOp::Set64 { offset, line } => Opcode::Set64 { offset, line },

                    PseudoOp::Ret => Opcode::Ret,

                    PseudoOp::AddCallstackDesc(desc) => Opcode::AddCallstackDesc(desc),
                    PseudoOp::RemoveCallstackDesc => Opcode::RemoveCallstackDesc,

                    PseudoOp::Call { file, func, line } => Opcode::Call {
                        func: *function_ids.get(&(file, func)).unwrap(),
                        line,
                    },
                    PseudoOp::Ecall { call, line } => Opcode::Ecall { call, line },
                };

                ops.push(op);
            }
        }

        for op in &mut ops {
            match op {
                Opcode::Call { func, .. } => {
                    *func = *function_locations.get(func).unwrap();
                }
                _ => {}
            }
        }

        let data: &[u8] = Box::leak(bytes.into());
        let mut refs = Vec::new();

        let mut files: Vec<&str> = file_ranges
            .into_iter()
            .map(|range| unsafe { str::from_utf8_unchecked(&data[range]) })
            .collect();
        let start = refs.len();
        refs.append(&mut files);
        let files_range = start..refs.len();

        let mut strings: Vec<&str> = string_ranges
            .into_iter()
            .map(|range| unsafe { str::from_utf8_unchecked(&data[range]) })
            .collect();
        let start = refs.len();
        refs.append(&mut strings);
        let strings_range = start..refs.len();

        let mut functions: Vec<&str> = function_ranges
            .into_iter()
            .map(|range| unsafe { str::from_utf8_unchecked(&data[range]) })
            .collect();
        let start = refs.len();
        refs.append(&mut functions);
        let functions_range = start..refs.len();

        let refs: &[&str] = Box::leak(refs.into());
        let files = &refs[files_range];
        let strings = &refs[strings_range];
        let functions = &refs[functions_range];
        let ops: &[Opcode] = Box::leak(ops.into());

        Self {
            files,
            strings,
            functions,
            ops,
            unused: (),
        }
    }
}
