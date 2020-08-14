use crate::util::*;
use core::ops::{Deref, DerefMut};
use core::{fmt, mem, str};
use std::io::Write;

#[derive(Debug, Clone, Copy)]
pub struct FuncDesc {
    pub file: u32,
    pub name: u32,
}

impl FuncDesc {
    pub fn into_callframe(self, line: u32) -> CallFrame {
        CallFrame {
            file: self.file,
            name: self.name,
            line,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CallFrame {
    pub file: u32,
    pub name: u32,
    pub line: u32,
}

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

    StackAlloc(u32), // Allocates space on the stack
    StackDealloc,    // Pops a variable off of the stack
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
    pub file_names: &'a [&'a str],
    pub strings: &'a [&'a str],
    pub functions: &'a [&'a str],
    pub ops: &'a [TaggedOpcode],
}

#[derive(Debug)]
pub struct IError {
    pub short_name: String,
    pub message: String,
}

impl IError {
    pub fn new(short_name: &str, message: String) -> Self {
        Self {
            short_name: short_name.to_string(),
            message,
        }
    }

    pub fn render(
        &self,
        stack_trace: &Vec<CallFrame>,
        program: &Program,
    ) -> Result<String, std::io::Error> {
        let mut out = StringWriter::new();
        write!(out, "{}: {}\n", self.short_name, self.message)?;
        for frame in stack_trace {
            write!(
                out,
                "    file {} -> function {} -> line {}\n",
                program.file_names[frame.file as usize],
                program.functions[frame.name as usize],
                frame.line
            )?;
        }

        return Ok(out.to_string());
    }
}

macro_rules! error {
    ($arg1:tt,$($arg:tt)*) => {
        IError::new($arg1, format!($($arg)*))
    };
}

macro_rules! err {
    ($arg1:tt,$($arg:tt)*) => {
        Err(IError::new($arg1, format!($($arg)*)))
    };
}

#[derive(Debug, Clone, Copy)]
pub struct Var {
    pub idx: usize,
    pub len: u32, // len in bytes
    pub meta: u32,
}

impl Var {
    pub fn new() -> Self {
        Self {
            idx: 0,
            len: 0,
            meta: 0,
        }
    }

    pub fn upper(self) -> usize {
        self.idx + self.len as usize
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct VarPointer {
    _idx: u32,
    _offset: u32,
}

impl fmt::Display for VarPointer {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        return write!(
            formatter,
            "0x{:x}{:0>8x}",
            u32::from_be(self._idx),
            u32::from_be(self._offset)
        );
    }
}

impl VarPointer {
    pub fn new_stack(idx: u32, offset: u32) -> VarPointer {
        if idx & !(1u32 << 31) != idx {
            panic!("idx is too large");
        }

        let idx = idx | (1u32 << 31);
        Self {
            _idx: idx.to_be(),
            _offset: offset.to_be(),
        }
    }

    pub fn new_heap(idx: u32, offset: u32) -> VarPointer {
        if idx & !(1u32 << 31) != idx {
            panic!("idx is too large");
        }

        Self {
            _idx: idx.to_be(),
            _offset: offset.to_be(),
        }
    }

    pub fn var_idx(self) -> usize {
        (u32::from_be(self._idx) & !(1u32 << 31)) as usize
    }

    pub fn with_offset(self, offset: u32) -> Self {
        let mut ptr = self;
        ptr.set_offset(offset);
        return ptr;
    }

    pub fn offset(self) -> u32 {
        u32::from_be(self._offset)
    }

    pub fn set_offset(&mut self, offset: u32) {
        self._offset = offset.to_be();
    }

    pub fn is_stack(self) -> bool {
        u32::from_be(self._idx) & (1u32 << 31) != 0
    }
}

pub fn invalid_ptr(ptr: VarPointer) -> IError {
    return error!("InvalidPointer", "the pointer {} is invalid", ptr);
}

pub fn invalid_offset(var: Var, ptr: VarPointer) -> IError {
    let (start, end) = (ptr.with_offset(0), ptr.with_offset(var.len));
    return error!(
        "InvalidPointer",
        "the pointer {} is invalid; the nearest object is in the range {}..{}", ptr, start, end
    );
}

#[derive(Debug, Clone)]
pub struct VarBuffer {
    pub data: Vec<u8>,  // Allocator for variables
    pub vars: Vec<Var>, // Tracker for variables
}

impl VarBuffer {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            vars: Vec::new(),
        }
    }

    pub fn get_var_range(&self, ptr: VarPointer) -> Result<&[u8], IError> {
        if ptr.var_idx() == 0 {
            return Err(invalid_ptr(ptr));
        }

        let var = match self.vars.get(ptr.var_idx() - 1) {
            Some(x) => *x,
            None => return Err(invalid_ptr(ptr)),
        };

        if ptr.offset() >= var.len {
            return Err(invalid_offset(var, ptr));
        }

        return Ok(&self.data[(var.idx + ptr.offset() as usize)..(var.idx + var.len as usize)]);
    }

    pub fn write_bytes(&mut self, ptr: VarPointer, bytes: &[u8]) -> Result<Vec<u8>, IError> {
        if ptr.var_idx() == 0 {
            return Err(invalid_ptr(ptr));
        }

        let var = match self.vars.get(ptr.var_idx() - 1) {
            Some(x) => *x,
            None => return Err(invalid_ptr(ptr)),
        };

        if ptr.offset() >= var.len {
            return Err(invalid_offset(var, ptr));
        }

        let len = bytes.len() as u32; // TODO check for overflow here
        if ptr.offset() + len > var.len {
            println!("hello from canada");
            return Err(invalid_offset(var, ptr.with_offset(ptr.offset() + len)));
        }

        let start = var.idx + ptr.offset() as usize;
        let to_bytes = &mut self.data[start..(start + len as usize)];
        let mut previous_value = Vec::new();
        previous_value.extend_from_slice(to_bytes);
        to_bytes.copy_from_slice(bytes);
        return Ok(previous_value);
    }

    pub fn get_var<T: Copy>(&self, ptr: VarPointer) -> Result<T, IError> {
        let len = mem::size_of::<T>() as u32; // TODO check for overflow
        if ptr.var_idx() == 0 {
            return Err(invalid_ptr(ptr));
        }

        let var = match self.vars.get(ptr.var_idx() - 1) {
            Some(x) => *x,
            None => return Err(invalid_ptr(ptr)),
        };

        if ptr.offset() + len > var.len {
            return Err(invalid_offset(var, ptr));
        }

        let begin = var.idx + ptr.offset() as usize;
        let var_range = &self.data[begin..(begin + len as usize)];
        return Ok(unsafe { *(var_range.as_ptr() as *const T) });
    }

    pub fn add_var(&mut self, len: u32) -> u32 {
        let idx = self.data.len();
        self.vars.push(Var { idx, len, meta: 0 });
        self.data.resize(idx + len as usize, 0);
        let var_idx = self.vars.len() as u32; // TODO Check for overflow
        return var_idx;
    }

    pub fn set<T: Copy>(&mut self, ptr: VarPointer, t: T) -> Result<T, IError> {
        let len = mem::size_of::<T>() as u32; // TODO check for overflow
        if ptr.var_idx() == 0 {
            return Err(invalid_ptr(ptr));
        }

        let var = match self.vars.get(ptr.var_idx() - 1) {
            Some(x) => *x,
            None => return Err(invalid_ptr(ptr)),
        };

        if ptr.offset() + len > var.len {
            return Err(invalid_offset(var, ptr));
        }

        let begin = var.idx + ptr.offset() as usize;
        let to_bytes = &mut self.data[begin..(begin + len as usize)];
        let previous_value = unsafe { *(to_bytes.as_ptr() as *const T) };
        to_bytes.copy_from_slice(any_as_u8_slice(&t));
        return Ok(previous_value);
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MemoryAction {
    SetValue {
        ptr: VarPointer,
        value_start: usize,
        value_end_overwrite_start: usize,
        overwrite_end: usize,
    },
    PopStack {
        value_start: usize,
        value_end: usize,
    },
    PushStack {
        value_start: usize,
        value_end: usize,
    },
    PopStackVar {
        var_start: usize,
        var_end_stack_start: usize,
        stack_end: usize,
    },
    AllocStackVar {
        len: u32,
    },
    AllocHeapVar {
        len: u32,
    },
}

pub struct Memory {
    pub stack: VarBuffer,
    pub heap: VarBuffer,
    pub historical_data: Vec<u8>,
    pub history: Vec<MemoryAction>,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            stack: VarBuffer::new(),
            heap: VarBuffer::new(),
            historical_data: Vec::new(),
            history: Vec::new(),
        }
    }

    #[inline]
    pub fn get_var_range(&self, ptr: VarPointer) -> Result<&[u8], IError> {
        if ptr.is_stack() {
            return self.stack.get_var_range(ptr);
        } else {
            return self.heap.get_var_range(ptr);
        }
    }

    #[inline]
    pub fn get_var<T: Default + Copy>(&self, ptr: VarPointer) -> Result<T, IError> {
        if ptr.is_stack() {
            return self.stack.get_var(ptr);
        } else {
            return self.heap.get_var(ptr);
        }
    }

    #[inline]
    pub fn set<T: Copy>(&mut self, ptr: VarPointer, value: T) -> Result<(), IError> {
        let value_start = self.historical_data.len();
        self.historical_data
            .extend_from_slice(any_as_u8_slice(&value));

        let previous_value;
        if ptr.is_stack() {
            previous_value = self.stack.set(ptr, value)?;
        } else {
            previous_value = self.heap.set(ptr, value)?;
        }

        let value_end_overwrite_start = self.historical_data.len();
        self.historical_data
            .extend_from_slice(any_as_u8_slice(&previous_value));
        let overwrite_end = self.historical_data.len();
        self.history.push(MemoryAction::SetValue {
            ptr,
            value_start,
            value_end_overwrite_start,
            overwrite_end,
        });

        return Ok(());
    }

    #[inline]
    pub fn add_stack_var(&mut self, len: u32) -> VarPointer {
        let ptr = VarPointer::new_stack(self.stack.add_var(len), 0);
        self.history.push(MemoryAction::AllocStackVar { len });
        return ptr;
    }

    #[inline]
    pub fn add_heap_var(&mut self, len: u32) -> VarPointer {
        let ptr = VarPointer::new_heap(self.heap.add_var(len), 0);
        self.history.push(MemoryAction::AllocHeapVar { len });
        return ptr;
    }

    #[inline]
    pub fn write_bytes(&mut self, ptr: VarPointer, bytes: &[u8]) -> Result<(), IError> {
        let value_start = self.historical_data.len();
        self.historical_data.extend_from_slice(bytes);

        let previous_value;
        if ptr.is_stack() {
            previous_value = self.stack.write_bytes(ptr, bytes)?;
        } else {
            previous_value = self.heap.write_bytes(ptr, bytes)?;
        }

        let value_end_overwrite_start = self.historical_data.len();
        self.historical_data.extend_from_slice(&previous_value);
        let overwrite_end = self.historical_data.len();
        self.history.push(MemoryAction::SetValue {
            ptr,
            value_start,
            value_end_overwrite_start,
            overwrite_end,
        });

        return Ok(());
    }

    #[inline]
    pub fn stack_length(&self) -> u32 {
        return (self.stack.vars.len() + 1) as u32; // TODO check for overflow
    }

    pub fn pop_stack_var(&mut self) -> Result<Var, IError> {
        if let Some(var) = self.stack.vars.pop() {
            let var_start = self.historical_data.len();
            let var_end_stack_start = var_start + var.len as usize;
            self.historical_data
                .extend_from_slice(&self.stack.data[var.idx..]);
            let stack_end = self.historical_data.len();

            self.stack.data.resize(var.idx, 0);
            self.history.push(MemoryAction::PopStackVar {
                var_start,
                var_end_stack_start,
                stack_end,
            });

            return Ok(var);
        } else {
            return err!("StackIsEmpty", "tried to pop from stack when it is empty");
        }
    }

    #[inline]
    pub fn push_stack<T: Copy>(&mut self, value: T) {
        let from_bytes = any_as_u8_slice(&value);
        let value_start = self.historical_data.len();
        self.historical_data.extend_from_slice(from_bytes);
        let value_end = self.historical_data.len();

        self.stack.data.extend_from_slice(from_bytes);
        self.history.push(MemoryAction::PushStack {
            value_start,
            value_end,
        });
    }

    pub fn pop_stack<T: Copy>(&mut self) -> Result<T, IError> {
        if self.stack.data.len() < 8 {
            return err!("StackIsEmpty", "tried to pop from stack when it is empty");
        }

        if let Some(var) = self.stack.vars.last() {
            if self.stack.data.len() - var.upper() < 8 {
                return err!(
                    "StackPopInvalidatesVariable",
                    "popping from the stack would invalidate a variable"
                );
            }
        }

        let upper = self.stack.data.len();
        let lower = upper - 8;
        let from_bytes = &self.stack.data[lower..upper];

        let value_start = self.historical_data.len();
        self.historical_data.extend_from_slice(from_bytes);
        let value_end = self.historical_data.len();

        let out = unsafe { *(from_bytes.as_ptr() as *const T) };
        self.stack.data.resize(lower, 0);
        self.history.push(MemoryAction::PopStack {
            value_start,
            value_end,
        });

        return Ok(out);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MemorySnapshot<'a> {
    pub stack_data: &'a [u8],
    pub stack_vars: &'a [Var],
    pub heap_data: &'a [u8],
    pub heap_vars: &'a [Var],
}

#[derive(Debug, Clone)]
struct _Memory {
    stack: VarBuffer,
    heap: VarBuffer,
}

impl _Memory {
    pub fn new() -> Self {
        Self {
            stack: VarBuffer::new(),
            heap: VarBuffer::new(),
        }
    }

    pub fn new_from(stack: &VarBuffer, heap: &VarBuffer) -> Self {
        Self {
            stack: stack.clone(),
            heap: heap.clone(),
        }
    }

    pub fn var_buffer(&mut self, ptr: VarPointer) -> &mut VarBuffer {
        if ptr.is_stack() {
            &mut self.stack
        } else {
            &mut self.heap
        }
    }
}

#[derive(Debug, Clone)]
pub struct MemorySnapshotWalker<'a> {
    memory: _Memory,
    historical_data: &'a [u8],
    history: &'a [MemoryAction],
    index: usize,
}

impl<'a> MemorySnapshotWalker<'a> {
    pub fn next(&mut self) -> Option<MemorySnapshot> {
        if self.index > self.history.len() {
            return None;
        }

        if self.index > 0 {
            match self.history[self.index - 1] {
                MemoryAction::SetValue {
                    ptr,
                    value_start,
                    value_end_overwrite_start,
                    overwrite_end,
                } => {
                    let value_bytes = &self.historical_data[value_start..value_end_overwrite_start];
                    self.memory
                        .var_buffer(ptr)
                        .write_bytes(ptr, value_bytes)
                        .expect("this should never error");
                }
                MemoryAction::PopStack {
                    value_start,
                    value_end,
                } => {
                    let popped_len = value_end - value_start;
                    let data = &mut self.memory.stack.data;
                    data.resize(data.len() - popped_len, 0);
                }
                MemoryAction::PushStack {
                    value_start,
                    value_end,
                } => {
                    let popped_len = value_end - value_start;
                    let data = &mut self.memory.stack.data;
                    data.extend_from_slice(&self.historical_data[value_start..value_end]);
                }
                MemoryAction::PopStackVar {
                    var_start,
                    var_end_stack_start,
                    stack_end,
                } => {
                    let var = self.memory.stack.vars.pop().unwrap();
                    self.memory.stack.data.resize(var.idx, 0);
                }
                MemoryAction::AllocHeapVar { len } => {
                    self.memory.heap.add_var(len);
                }
                MemoryAction::AllocStackVar { len } => {
                    self.memory.stack.add_var(len);
                }
            }
        }
        self.index += 1;

        Some(MemorySnapshot {
            stack_data: &self.memory.stack.data,
            stack_vars: &self.memory.stack.vars,
            heap_data: &self.memory.heap.data,
            heap_vars: &self.memory.heap.vars,
        })
    }

    pub fn prev(&mut self) -> Option<MemorySnapshot> {
        if self.index == 0 {
            return None;
        }

        if self.index <= self.historical_data.len() {
            match self.history[self.index - 1] {
                MemoryAction::SetValue {
                    ptr,
                    value_start,
                    value_end_overwrite_start,
                    overwrite_end,
                } => {
                    let value_bytes =
                        &self.historical_data[value_end_overwrite_start..overwrite_end];
                    self.memory
                        .var_buffer(ptr)
                        .write_bytes(ptr, value_bytes)
                        .expect("this should never error");
                }
                MemoryAction::PopStack {
                    value_start,
                    value_end,
                } => {
                    let popped_len = value_end - value_start;
                    let data = &mut self.memory.stack.data;
                    data.extend_from_slice(&self.historical_data[value_start..value_end]);
                }
                MemoryAction::PushStack {
                    value_start,
                    value_end,
                } => {
                    let popped_len = value_end - value_start;
                    let data = &mut self.memory.stack.data;
                    data.resize(data.len() - popped_len, 0);
                }
                MemoryAction::PopStackVar {
                    var_start,
                    var_end_stack_start,
                    stack_end,
                } => {
                    let data = &mut self.memory.stack.data;
                    let idx = data.len(); // TODO check for overflow
                    let len = (var_end_stack_start - var_start) as u32;
                    data.extend_from_slice(&self.historical_data[var_start..stack_end]);
                    let vars = &mut self.memory.stack.vars;
                    vars.push(Var { idx, len, meta: 0 });
                }
                MemoryAction::AllocHeapVar { len } => {
                    let var = self.memory.heap.vars.pop().unwrap();
                    self.memory.heap.data.resize(var.idx, 0);
                }
                MemoryAction::AllocStackVar { len } => {
                    let var = self.memory.stack.vars.pop().unwrap();
                    self.memory.stack.data.resize(var.idx, 0);
                }
            }
        }

        self.index -= 1;

        Some(MemorySnapshot {
            stack_data: &self.memory.stack.data,
            stack_vars: &self.memory.stack.vars,
            heap_data: &self.memory.heap.data,
            heap_vars: &self.memory.heap.vars,
        })
    }
}

impl Memory {
    pub fn forwards_walker(&self) -> MemorySnapshotWalker {
        MemorySnapshotWalker {
            memory: _Memory::new(),
            historical_data: &self.historical_data,
            history: &self.history,
            index: 0,
        }
    }

    pub fn backwards_walker(&self) -> MemorySnapshotWalker {
        MemorySnapshotWalker {
            memory: _Memory::new_from(&self.stack, &self.heap),
            historical_data: &self.historical_data,
            history: &self.history,
            index: self.history.len() + 1,
        }
    }
}

pub trait RuntimeIO {
    type Out: Write;
    type Log: Write;
    type Err: Write;

    fn out(&mut self) -> &mut Self::Out;
    fn log(&mut self) -> &mut Self::Log;
    fn err(&mut self) -> &mut Self::Err;
}

pub struct InMemoryIO {
    pub out: StringWriter,
    pub log: StringWriter,
    pub err: StringWriter,
}

impl InMemoryIO {
    pub fn new() -> Self {
        Self {
            out: StringWriter::new(),
            log: StringWriter::new(),
            err: StringWriter::new(),
        }
    }
}

impl RuntimeIO for InMemoryIO {
    type Out = StringWriter;
    type Log = StringWriter;
    type Err = StringWriter;

    fn out(&mut self) -> &mut StringWriter {
        return &mut self.out;
    }
    fn err(&mut self) -> &mut StringWriter {
        return &mut self.err;
    }
    fn log(&mut self) -> &mut StringWriter {
        return &mut self.log;
    }
}

pub struct Runtime<IO: RuntimeIO> {
    pub memory: Memory,
    pub callstack: Vec<CallFrame>,
    pub io: IO,
}

impl<IO: RuntimeIO> DerefMut for Runtime<IO> {
    fn deref_mut(&mut self) -> &mut Memory {
        return &mut self.memory;
    }
}

impl<IO: RuntimeIO> Deref for Runtime<IO> {
    type Target = Memory;
    fn deref(&self) -> &Memory {
        return &self.memory;
    }
}

impl<IO: RuntimeIO> Runtime<IO> {
    pub fn new(io: IO) -> Self {
        Self {
            memory: Memory::new(),
            callstack: Vec::new(),
            io,
        }
    }

    pub fn fp_offset(fp: u32, var: i32) -> u32 {
        if var < 0 {
            // TODO make sure there's no overflow happening here
            let var = (var * -1) as u32;
            fp - var
        } else {
            fp + var as u32
        }
    }

    pub fn run_program(&mut self, program: Program) -> Result<(), IError> {
        self.memory = Memory::new();
        return self.run_func(&program, 0);
    }

    pub fn pop_callstack(&mut self) -> Result<(), IError> {
        if self.callstack.pop().is_none() {
            return err!(
                "CallstackEmpty",
                "tried to pop callstack when callstack was empty"
            );
        }
        return Ok(());
    }

    pub fn run_func(&mut self, program: &Program, pcounter: usize) -> Result<(), IError> {
        let func_desc = match program.ops[pcounter].op {
            Opcode::Func(desc) => desc,
            op => {
                return err!(
                    "InvalidFunctionHeader",
                    "found function header {:?} (this is an error in your compiler)",
                    op
                )
            }
        };

        let callstack_len = self.callstack.len();

        let fp = self.memory.stack_length();
        let mut pc: i32 = pcounter as i32;

        loop {
            let op = program.ops[pc as usize];
            write!(self.io.log(), "op: {:?}\n", op)
                .map_err(|err| error!("LoggingFailed", "failed to log ({})", err))?;

            self.callstack.push(func_desc.into_callframe(op.line));
            pc = self.run_op(fp, pc, program, op.op)?;
            if self.callstack.pop().is_none() {
                return err!(
                    "CallstackEmpty",
                    "tried to pop callstack when callstack was empty"
                );
            }

            write!(self.io.log(), "stack: {:0>3?}\n", self.memory.stack.data)
                .map_err(|err| error!("WriteFailed", "failed to write to logs ({})", err))?;
            write!(self.io.log(), "heap:  {:0>3?}\n\n", self.memory.heap.data)
                .map_err(|err| error!("WriteFailed", "failed to write to logs ({})", err))?;

            if pc < 0 {
                while self.memory.stack_length() > fp {
                    self.memory.pop_stack_var()?;
                }
                if self.callstack.len() < callstack_len {
                    return err!(
                        "InvalidCallstackState",
                        "callstack shrunk over course of function call"
                    );
                }

                self.callstack.resize(
                    callstack_len,
                    CallFrame {
                        file: 0,
                        name: 0,
                        line: 0,
                    },
                );

                return Ok(());
            }
        }
    }

    #[inline]
    pub fn run_op(
        &mut self,
        fp: u32,
        pc: i32,
        program: &Program,
        opcode: Opcode,
    ) -> Result<i32, IError> {
        match opcode {
            Opcode::Func(_) => {}

            Opcode::StackAlloc(space) => {
                self.add_stack_var(space);
            }
            Opcode::Alloc(space) => {
                let ptr = self.add_heap_var(space);
                self.push_stack(ptr);
            }
            Opcode::StackDealloc => {
                self.pop_stack_var()?;
            }

            Opcode::MakeTempInt64(value) => self.push_stack(value.to_be()),
            Opcode::MakeTempFloat64(value) => self.push_stack(value),
            Opcode::LoadStr(idx) => {
                let str_value = program.strings[idx as usize].as_bytes();
                let str_len = str_value.len() as u32; // TODO check for overflow

                let ptr = self.add_heap_var(str_len + 1);
                self.write_bytes(ptr, str_value)?;
                let mut end_ptr = ptr;
                end_ptr.set_offset(str_len);
                self.write_bytes(end_ptr, &vec![0])?;
                self.push_stack(ptr);
            }

            Opcode::Pop64 => {
                self.pop_stack::<u64>()?;
            }

            Opcode::GetLocal64 { var, offset } => {
                let ptr = VarPointer::new_stack(Self::fp_offset(fp, var), offset);
                let var = self.get_var::<u64>(ptr)?;
                self.push_stack(var);
            }
            Opcode::SetLocal64 { var, offset } => {
                let ptr = VarPointer::new_stack(Self::fp_offset(fp, var), offset);
                let word: u64 = self.pop_stack()?;
                println!("is_stack?: {}", ptr.is_stack());
                self.set(ptr, word)?;
            }

            Opcode::Get64 { offset } => {
                let mut ptr: VarPointer = self.pop_stack()?;
                ptr.set_offset(ptr.offset().wrapping_add(offset as u32)); // TODO check for overflow
                let var = self.get_var::<u64>(ptr)?;
                self.push_stack(var);
            }
            Opcode::Set64 { offset } => {
                let mut ptr: VarPointer = self.pop_stack()?;
                ptr.set_offset(ptr.offset().wrapping_add(offset as u32)); // TODO check for overflow
                let word = self.pop_stack::<u64>()?;
                self.set(ptr, word)?;
            }

            Opcode::AddU64 => {
                let word1: u64 = u64::from_be(self.pop_stack()?);
                let word2: u64 = u64::from_be(self.pop_stack()?);
                self.push_stack(word1.wrapping_add(word2).to_be());
            }
            Opcode::SubI64 => {
                let word1: u64 = u64::from_be(self.pop_stack()?);
                let word2: u64 = u64::from_be(self.pop_stack()?);
                self.push_stack(word1.wrapping_sub(word2).to_be());
            }
            Opcode::MulI64 => {
                let word1: u64 = u64::from_be(self.pop_stack()?);
                let word2: u64 = u64::from_be(self.pop_stack()?);
                self.push_stack(word1.wrapping_mul(word2).to_be());
            }
            Opcode::DivI64 => {
                let word1: u64 = u64::from_be(self.pop_stack()?);
                let word2: u64 = u64::from_be(self.pop_stack()?);
                self.push_stack(word1.wrapping_div(word2).to_be());
            }
            Opcode::ModI64 => {
                let word1: u64 = u64::from_be(self.pop_stack()?);
                let word2: u64 = u64::from_be(self.pop_stack()?);
                self.push_stack((word1 % word2).to_be());
            }

            Opcode::JumpIfZero64(target) => {
                let value: u64 = self.pop_stack()?;
                if value == 0 {
                    return Ok(target as i32);
                }
            }
            Opcode::JumpIfNotZero64(target) => {
                let value: u64 = self.pop_stack()?;
                if value != 0 {
                    return Ok(target as i32);
                }
            }

            Opcode::Ret => return Ok(-1),

            Opcode::Call(func) => {
                self.run_func(program, func as usize)?;
            }

            Opcode::Ecall(ECALL_PRINT_INT) => {
                let word: i64 = i64::from_be(self.pop_stack()?);
                write!(self.io.out(), "{}", word)
                    .map_err(|err| error!("WriteFailed", "failed to write to stdout ({})", err))?;
            }
            Opcode::Ecall(ECALL_PRINT_STR) => {
                let ptr: VarPointer = self.pop_stack()?;
                let str_bytes = self.memory.get_var_range(ptr)?;

                let mut idx = str_bytes.len();
                for (idx_, byte) in str_bytes.iter().enumerate() {
                    if *byte == 0 {
                        idx = idx_;
                        break;
                    }
                }

                if idx == str_bytes.len() {
                    return err!("MissingNullTerminator", "string missing null terminator");
                }

                let str_value = unsafe { str::from_utf8_unchecked(&str_bytes[0..idx]) };

                write!(self.io.out(), "{}", str_value)
                    .map_err(|err| error!("WriteFailed", "failed to write to stdout ({})", err))?;
            }
            Opcode::Ecall(call) => {
                return err!("InvalidEnviromentCall", "invalid ecall value of {}", call);
            }
        }

        return Ok(pc + 1);
    }
}

#[test]
fn test_simple_read_write() {
    let files = vec!["main.c"];
    let strings = vec!["hello, world!\n"];
    let functions = vec!["main", "helper"];
    let ops = vec![
        Opcode::Func(FuncDesc { file: 0, name: 0 }),
        Opcode::StackAlloc(8),
        Opcode::LoadStr(0),
        Opcode::SetLocal64 { var: 0, offset: 0 },
        Opcode::Call(6),
        Opcode::Ret,
        Opcode::Func(FuncDesc { file: 0, name: 1 }),
        Opcode::GetLocal64 { var: -1, offset: 0 },
        Opcode::Ecall(ECALL_PRINT_STR),
        Opcode::Ret,
    ];
    let tags = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let ops: Vec<TaggedOpcode> = ops
        .iter()
        .zip(tags)
        .map(|(&op, line)| TaggedOpcode { op, line })
        .collect();

    let program = Program {
        file_names: &files,
        strings: &strings,
        functions: &functions,
        ops: &ops,
    };

    let mut runtime = Runtime::new(InMemoryIO::new());
    let result = runtime.run_program(program);
    print!("{}", runtime.io.log().to_string());
    match result {
        Ok(()) => {}
        Err(err) => {
            println!(
                "{}",
                err.render(&runtime.callstack, &program)
                    .expect("why did this fail?")
            );
            panic!();
        }
    }
    assert_eq!(runtime.io.out().to_string(), "hello, world!\n");
}

#[test]
fn test_errors() {
    let files = vec!["main.c"];
    let strings = vec!["hello, world!\n"];
    let functions = vec!["main", "helper"];
    let ops = vec![
        Opcode::Func(FuncDesc { file: 0, name: 0 }),
        Opcode::StackAlloc(8),
        Opcode::LoadStr(0),
        Opcode::SetLocal64 { var: 0, offset: 0 },
        Opcode::Call(6),
        Opcode::Ret,
        Opcode::Func(FuncDesc { file: 0, name: 1 }),
        Opcode::GetLocal64 { var: -1, offset: 0 },
        Opcode::MakeTempInt64(15),
        Opcode::AddU64,
        Opcode::Ecall(ECALL_PRINT_STR),
        Opcode::Ret,
    ];
    let tags = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];
    let ops: Vec<TaggedOpcode> = ops
        .iter()
        .zip(tags)
        .map(|(&op, line)| TaggedOpcode { op, line })
        .collect();

    let program = Program {
        file_names: &files,
        strings: &strings,
        functions: &functions,
        ops: &ops,
    };

    let mut runtime = Runtime::new(InMemoryIO::new());
    match runtime.run_program(program) {
        Ok(()) => {
            println!("{}", runtime.io.log().to_string());
            println!("{}", runtime.io.out().to_string());
            panic!();
        }
        Err(err) => {
            println!(
                "{}",
                err.render(&runtime.callstack, &program)
                    .expect("why did this fail?")
            );
            println!("{}", runtime.io.log().to_string());
            assert_eq!(err.short_name, "InvalidPointer");
        }
    }
}

#[test]
fn test_walker() {
    let files = vec!["main.c"];
    let strings = vec!["hello, world!\n"];
    let functions = vec!["main", "helper"];
    let ops = vec![
        Opcode::Func(FuncDesc { file: 0, name: 0 }),
        Opcode::StackAlloc(8),
        Opcode::LoadStr(0),
        Opcode::SetLocal64 { var: 0, offset: 0 },
        Opcode::Call(6),
        Opcode::Ret,
        Opcode::Func(FuncDesc { file: 0, name: 1 }),
        Opcode::GetLocal64 { var: -1, offset: 0 },
        Opcode::MakeTempInt64(15),
        Opcode::AddU64,
        Opcode::Ecall(ECALL_PRINT_STR),
        Opcode::Ret,
    ];
    let tags = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];
    let ops: Vec<TaggedOpcode> = ops
        .iter()
        .zip(tags)
        .map(|(&op, line)| TaggedOpcode { op, line })
        .collect();

    let program = Program {
        file_names: &files,
        strings: &strings,
        functions: &functions,
        ops: &ops,
    };

    let mut runtime = Runtime::new(InMemoryIO::new());
    match runtime.run_program(program) {
        Ok(()) => {
            println!("{}", runtime.io.log().to_string());
            println!("{}", runtime.io.out().to_string());
            panic!();
        }
        Err(err) => {
            println!(
                "{}",
                err.render(&runtime.callstack, &program)
                    .expect("why did this fail?")
            );
            println!("{}", runtime.io.log().to_string());
            assert_eq!(err.short_name, "InvalidPointer");
        }
    }
}
