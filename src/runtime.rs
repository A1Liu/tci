use crate::util::*;
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
    pub file_names: &'a [&'a str],
    pub strings: &'a [&'a str],
    pub functions: &'a [&'a str],
    pub ops: &'a [TaggedOpcode],
}

#[derive(Debug)]
pub struct IError {
    pub short_name: String,
    pub message: String,
    pub stack_trace: Vec<CallFrame>,
}

impl IError {
    pub fn new(short_name: &str, message: String) -> Self {
        Self {
            short_name: short_name.to_string(),
            message,
            stack_trace: Vec::new(),
        }
    }

    pub fn render(&self, program: &Program) -> Result<String, std::io::Error> {
        let mut out = StringWriter::new();
        write!(out, "{}: {}\n", self.short_name, self.message)?;
        for frame in self.stack_trace.iter() {
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
    pub idx: u32,
    pub len: u32, // len in bytes
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
            "0x{:0>8x}{:0>8x}",
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

    pub fn offset(self) -> u32 {
        u32::from_be(self._offset)
    }

    pub fn set_offset(&mut self, offset: u32) {
        self._offset = offset.to_be();
    }

    pub fn bytes(self) -> u64 {
        let idx = (u32::from_be(self._idx) as u64) << 32;
        let offset = u32::from_be(self._offset) as u64;
        return (idx + offset).to_be();
    }

    pub fn is_stack(self) -> bool {
        self._idx & (1u32 << 31) != 0
    }
}

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

    pub fn invalid_ptr(ptr: VarPointer) -> IError {
        return error!("InvalidPointer", "the pointer {} is invalid", ptr);
    }

    pub fn invalid_offset(var: Var, ptr: VarPointer) -> IError {
        let (mut start, mut end) = (ptr, ptr);
        start._offset = 0;
        end._offset = var.len;
        return error!(
            "InvalidPointer",
            "the pointer {} is invalid; the nearest object is in the range {}..{}", ptr, start, end
        );
    }

    pub fn get_var_range_mut(&mut self, ptr: VarPointer, len: u32) -> Result<&mut [u8], IError> {
        if ptr.var_idx() == 0 {
            return Err(Self::invalid_ptr(ptr));
        }

        let var = match self.vars.get(ptr.var_idx() - 1) {
            Some(x) => *x,
            None => return Err(Self::invalid_ptr(ptr)),
        };

        if ptr.offset() + len > var.len {
            return Err(Self::invalid_offset(var, ptr));
        }

        let begin = var.idx + ptr.offset();
        return Ok(&mut self.data[begin as usize..((begin + len) as usize)]);
    }

    pub fn get_var_range(&self, ptr: VarPointer, len: u32) -> Result<&[u8], IError> {
        if ptr.var_idx() == 0 {
            return Err(Self::invalid_ptr(ptr));
        }

        let var = match self.vars.get(ptr.var_idx() - 1) {
            Some(x) => *x,
            None => return Err(Self::invalid_ptr(ptr)),
        };

        if ptr.offset() + len > var.len {
            return Err(Self::invalid_offset(var, ptr));
        }

        let begin = var.idx + ptr.offset();
        return Ok(&self.data[begin as usize..((begin + len) as usize)]);
    }

    pub fn get_full_var_range(&self, ptr: VarPointer) -> Result<&[u8], IError> {
        if ptr.var_idx() == 0 {
            return Err(Self::invalid_ptr(ptr));
        }

        let var = match self.vars.get(ptr.var_idx() - 1) {
            Some(x) => *x,
            None => return Err(Self::invalid_ptr(ptr)),
        };

        if ptr.offset() >= var.len {
            return Err(Self::invalid_offset(var, ptr));
        }

        return Ok(&self.data[(var.idx + ptr.offset()) as usize..((var.idx + var.len) as usize)]);
    }

    pub fn get_var<T: Copy>(&self, ptr: VarPointer) -> Result<T, IError> {
        let len = mem::size_of::<T>();
        return Ok(unsafe { *(self.get_var_range(ptr, len as u32)?.as_ptr() as *const T) });
    }

    pub fn upper_bound(&self) -> u32 {
        return (self.vars.len() + 1) as u32;
    }

    pub fn add_var(&mut self, len: u32) -> (u32, &mut [u8]) {
        let idx = self.data.len() as u32;
        self.vars.push(Var { idx, len });
        self.data.resize((idx + len) as usize, 0);
        let var_idx = self.vars.len() as u32;
        let var_range = idx as usize..((idx + len) as usize);
        return (var_idx, &mut self.data[var_range]);
    }

    pub fn pop_var(&mut self) -> Result<Var, IError> {
        if let Some(var) = self.vars.pop() {
            self.data.resize(var.idx as usize, 0);
            return Ok(var);
        } else {
            return err!("StackIsEmpty", "tried to pop from stack when it is empty");
        }
    }

    pub fn shrink_vars_to(&mut self, len: u32) {
        let len = (len - 1) as usize;
        if len > self.vars.len() {
            panic!("shrinking to a length larger than the vars array");
        }

        self.vars.resize(len, Var { idx: 0, len: 0 });
        if let Some(var) = self.vars.last() {
            self.data.resize((var.idx + var.len) as usize, 0);
        } else {
            self.data.resize(0, 0);
        }
    }

    pub fn push<T: Copy>(&mut self, value: T) {
        self.data.extend_from_slice(any_as_u8_slice(&value));
    }

    pub fn pop<T: Copy>(&mut self) -> Result<T, IError> {
        if self.data.len() < 8 {
            return err!("StackIsEmpty", "tried to pop from stack when it is empty");
        }

        if let Some(var) = self.vars.last() {
            let upper = (var.idx + var.len) as usize;
            if self.data.len() - upper < 8 {
                return err!(
                    "StackPopInvalidatesVariable",
                    "popping from the stack would invalidate a variable"
                );
            }
        }

        let upper = self.data.len();
        let lower = upper - 8;
        let out = unsafe { *(self.data[lower..upper].as_ptr() as *const T) };
        self.data.resize(lower, 0);
        return Ok(out);
    }

    pub fn set<T: Copy>(&mut self, ptr: VarPointer, t: T) -> Result<(), IError> {
        let to_bytes = self.get_var_range_mut(ptr, mem::size_of::<T>() as u32)?;
        to_bytes.copy_from_slice(any_as_u8_slice(&t));
        return Ok(());
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
    out: StringWriter,
    log: StringWriter,
    err: StringWriter,
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
    pub stack: VarBuffer,
    pub heap: VarBuffer,
    pub callstack: Vec<CallFrame>,
    pub io: IO,
}

impl<IO: RuntimeIO> Runtime<IO> {
    pub fn new(io: IO) -> Self {
        Self {
            stack: VarBuffer::new(),
            heap: VarBuffer::new(),
            callstack: Vec::new(),
            io,
        }
    }

    pub fn fp_offset(fp: u32, var: i32) -> u32 {
        if var < 0 {
            let var = (var * -1) as u32;
            fp - var
        } else {
            fp + var as u32
        }
    }

    pub fn get_var<T: Default + Copy>(&self, ptr: VarPointer) -> Result<T, IError> {
        if ptr.is_stack() {
            return self.stack.get_var(ptr);
        } else {
            return self.heap.get_var(ptr);
        }
    }

    pub fn get_var_range(&self, ptr: VarPointer, len: u32) -> Result<&[u8], IError> {
        if ptr.is_stack() {
            return self.stack.get_var_range(ptr, len);
        } else {
            return self.heap.get_var_range(ptr, len);
        }
    }

    pub fn get_var_range_mut(&mut self, ptr: VarPointer, len: u32) -> Result<&mut [u8], IError> {
        if ptr.is_stack() {
            return self.stack.get_var_range_mut(ptr, len);
        } else {
            return self.heap.get_var_range_mut(ptr, len);
        }
    }

    pub fn set<T: Copy>(&mut self, ptr: VarPointer, value: T) -> Result<(), IError> {
        if ptr.is_stack() {
            return self.stack.set(ptr, value);
        } else {
            return self.heap.set(ptr, value);
        }
    }

    pub fn run_program(&mut self, program: Program) -> Result<(), IError> {
        match self.run_func(&program, 0) {
            Ok(()) => Ok(()),
            Err(mut err) => {
                err.stack_trace.reserve(self.callstack.len());
                for callframe in self.callstack.iter() {
                    err.stack_trace.push(*callframe);
                }

                return Err(err);
            }
        }
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

        let fp = self.stack.upper_bound();
        let mut pc: i32 = (pcounter + 1) as i32;

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

            write!(self.io.log(), "stack: {:0>3?}\n", self.stack.data)
                .map_err(|err| error!("LoggingFailed", "failed to log ({})", err))?;
            write!(self.io.log(), "heap:  {:0>3?}\n\n", self.heap.data)
                .map_err(|err| error!("LoggingFailed", "failed to log ({})", err))?;

            if pc < 0 {
                self.stack.shrink_vars_to(fp);
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
                self.stack.add_var(space);
            }
            Opcode::Alloc(space) => {
                let (var, _) = self.heap.add_var(space);
                self.stack.push(VarPointer::new_heap(var, 0));
            }
            Opcode::StackDealloc => {
                self.stack.pop_var()?;
            }

            Opcode::MakeTempInt64(value) => self.stack.push(value.to_be()),
            Opcode::MakeTempFloat64(value) => self.stack.push(value),
            Opcode::LoadStr(idx) => {
                let str_value = program.strings[idx as usize].as_bytes();
                let str_len = str_value.len() as u32;

                let (idx, bytes) = self.heap.add_var(str_len + 1);
                let ptr = VarPointer::new_heap(idx, 0);

                let last_idx = bytes.len() - 1;
                bytes[0..last_idx].copy_from_slice(str_value);
                bytes[last_idx] = 0;

                self.stack.push(ptr.bytes());
            }

            Opcode::Pop64 => {
                self.stack.pop::<u64>()?;
            }

            Opcode::GetLocal64 { var, offset } => {
                let ptr = VarPointer::new_stack(Self::fp_offset(fp, var), offset);
                self.stack.push(self.stack.get_var::<u64>(ptr)?);
            }
            Opcode::SetLocal64 { var, offset } => {
                let ptr = VarPointer::new_stack(Self::fp_offset(fp, var), offset);
                let word: u64 = self.stack.pop()?;
                self.stack.set(ptr, word)?;
            }

            Opcode::Get64 { offset } => {
                let mut ptr: VarPointer = self.stack.pop()?;
                ptr.set_offset(ptr.offset().wrapping_add(offset as u32));
                self.stack.push(self.get_var::<u64>(ptr)?);
            }
            Opcode::Set64 { offset } => {
                let mut ptr: VarPointer = self.stack.pop()?;
                ptr.set_offset(ptr.offset().wrapping_add(offset as u32));
                let word = self.stack.pop::<u64>()?;
                self.set(ptr, word)?;
            }

            Opcode::AddU64 => {
                let word1: u64 = u64::from_be(self.stack.pop()?);
                let word2: u64 = u64::from_be(self.stack.pop()?);
                self.stack.push(word1.wrapping_add(word2).to_be());
            }
            Opcode::SubI64 => {
                let word1: u64 = u64::from_be(self.stack.pop()?);
                let word2: u64 = u64::from_be(self.stack.pop()?);
                self.stack.push(word1.wrapping_sub(word2).to_be());
            }
            Opcode::MulI64 => {
                let word1: u64 = u64::from_be(self.stack.pop()?);
                let word2: u64 = u64::from_be(self.stack.pop()?);
                self.stack.push(word1.wrapping_mul(word2).to_be());
            }
            Opcode::DivI64 => {
                let word1: u64 = u64::from_be(self.stack.pop()?);
                let word2: u64 = u64::from_be(self.stack.pop()?);
                self.stack.push(word1.wrapping_div(word2).to_be());
            }
            Opcode::ModI64 => {
                let word1: u64 = u64::from_be(self.stack.pop()?);
                let word2: u64 = u64::from_be(self.stack.pop()?);
                self.stack.push((word1 % word2).to_be());
            }

            Opcode::JumpIfZero64(target) => {
                let value: u64 = self.stack.pop()?;
                if value == 0 {
                    return Ok(target as i32);
                }
            }
            Opcode::JumpIfNotZero64(target) => {
                let value: u64 = self.stack.pop()?;
                if value != 0 {
                    return Ok(target as i32);
                }
            }

            Opcode::Ret => return Ok(-1),

            Opcode::AddCallstackDesc(desc) => self.callstack.push(desc),
            Opcode::RemoveCallstackDesc => self.pop_callstack()?,

            Opcode::Call(func) => {
                self.run_func(program, func as usize)?;
            }

            Opcode::Ecall(ECALL_PRINT_INT) => {
                let word: i64 = i64::from_be(self.stack.pop()?);
                write!(self.io.out(), "{}", word)
                    .map_err(|err| error!("WriteFailed", "failed to write to stdout ({})", err))?;
            }
            Opcode::Ecall(ECALL_PRINT_STR) => {
                let ptr: VarPointer = self.stack.pop()?;
                let str_bytes = if ptr.is_stack() {
                    self.stack.get_full_var_range(ptr)?
                } else {
                    self.heap.get_full_var_range(ptr)?
                };

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
            println!("{}", err.render(&program).expect("why did this fail?"));
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
            println!("{}", err.render(&program).expect("why did this fail?"));
            println!("{}", runtime.io.log().to_string());
            assert_eq!(err.short_name, "InvalidPointer");
        }
    }
}
