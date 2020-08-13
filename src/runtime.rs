use crate::errors::*;
use crate::opcodes::*;
use crate::util::*;
use core::{mem, str};
use std::io::Write;

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

#[cfg(test)]
macro_rules! map(
    { $($key:expr => $value:expr),* } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key, $value);
            )*
            m
        }
     };
);

#[derive(Debug, Clone, Copy)]
pub struct Var {
    pub idx: u32,
    pub len: u32, // len in bytes
}

#[derive(Debug, Clone, Copy)]
pub struct VarPointer {
    idx: u32,
    offset: i32,
}

impl Default for VarPointer {
    fn default() -> Self {
        VarPointer { idx: 0, offset: 0 }
    }
}

impl VarPointer {
    pub fn new_stack(idx: u32, offset: i32) -> VarPointer {
        if idx & !(1u32 << 31) != idx {
            panic!("idx is too large");
        }

        let idx = idx | (1u32 << 31);
        Self { idx, offset }
    }

    pub fn new_heap(idx: u32, offset: i32) -> VarPointer {
        if idx & !(1u32 << 31) != idx {
            panic!("idx is too large");
        }

        Self { idx, offset }
    }

    pub fn var_idx(self) -> u32 {
        self.idx & !(1u32 << 31)
    }

    pub fn is_stack(self) -> bool {
        self.idx & (1u32 << 31) != 0
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

    pub fn get_var_range_mut(&mut self, ptr: VarPointer, len: u32) -> Result<&mut [u8], IError> {
        if ptr.var_idx() == 0 {
            return err!("NullPointer", "a variable of offset 0 was used");
        }

        let var = match self.vars.get(ptr.var_idx() as usize - 1) {
            Some(x) => *x,
            None => {
                return err!(
                    "IncorrectVarOffset",
                    "a variable offset of {} is incorrect",
                    ptr.idx
                )
            }
        };

        let upper = ptr.offset + len as i32;
        if upper > var.len as i32 || ptr.offset < 0i32 {
            return err!(
                "OutOfValueBounds",
                "tried to access bytes {}..{} in an object of length {}",
                ptr.offset,
                upper,
                var.len
            );
        }

        let begin = var.idx + ptr.offset as u32;
        return Ok(&mut self.data[begin as usize..((begin + len) as usize)]);
    }

    pub fn get_var_range(&self, ptr: VarPointer, len: u32) -> Result<&[u8], IError> {
        if ptr.var_idx() == 0 {
            return err!("NullPointer", "a variable of offset 0 was used");
        }

        let var = match self.vars.get(ptr.var_idx() as usize - 1) {
            Some(x) => *x,
            None => {
                return err!(
                    "IncorrectVarOffset",
                    "a variable offset of {} is incorrect",
                    ptr.idx
                )
            }
        };

        let upper = ptr.offset + len as i32;
        if upper > var.len as i32 || ptr.offset < 0i32 {
            return err!(
                "OutOfValueBounds",
                "tried to access bytes {}..{} in an object of length {}",
                ptr.offset,
                upper,
                var.len
            );
        }

        let begin = (var.idx + ptr.offset as u32) as usize;
        return Ok(&self.data[begin..(begin + upper as usize)]);
    }

    pub fn get_full_var_range(&self, ptr: VarPointer) -> Result<&[u8], IError> {
        if ptr.var_idx() == 0 {
            return err!("NullPointer", "a variable of offset 0 was used");
        }

        let var = match self.vars.get(ptr.var_idx() as usize - 1) {
            Some(x) => *x,
            None => {
                return err!(
                    "IncorrectVarOffset",
                    "a variable offset of {} is incorrect",
                    ptr.idx
                )
            }
        };

        if ptr.offset > var.len as i32 || ptr.offset < 0i32 {
            return err!(
                "OutOfValueBounds",
                "tried to access byte {} in an object of length {}",
                ptr.offset,
                var.len
            );
        }

        return Ok(&self.data[var.idx as usize..((var.idx + var.len) as usize)]);
    }

    pub fn get_var<T: Copy>(&self, ptr: VarPointer) -> Result<T, IError> {
        let len = mem::size_of::<T>();
        if len > u32::MAX as usize {
            panic!("struct too long");
        }

        return Ok(unsafe { *(self.get_var_range(ptr, len as u32)?.as_ptr() as *const T) });
    }

    pub fn upper_bound(&self) -> u32 {
        return (self.vars.len() + 1) as u32;
    }

    pub fn add_var(&mut self, len: u32) -> u32 {
        let idx = self.data.len() as u32;
        if len > u32::MAX {
            panic!("struct too long");
        }

        let var = Var { idx, len };
        self.data.resize((idx + len) as usize, 0);
        let var_idx = self.vars.len() as u32 + 1;
        self.vars.push(var);
        return var_idx;
    }

    pub fn add_var_range(&mut self, len: u32) -> (u32, &mut [u8]) {
        let idx = self.data.len() as u32;
        if len > u32::MAX {
            panic!("struct too long");
        }

        let var = Var { idx, len };
        self.data.resize((idx + len) as usize, 0);
        let var_idx = self.vars.len() as u32 + 1;
        self.vars.push(var);
        return (
            var_idx,
            &mut self.data[idx as usize..((idx + len) as usize)],
        );
    }

    pub fn pop_var(&mut self) -> Option<Var> {
        let var = self.vars.pop();

        if let Some(var) = var {
            self.data.resize(var.idx as usize, 0);
            return Some(var);
        } else {
            return None;
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

    pub fn pop<T: Default + Copy>(&mut self) -> Result<T, IError> {
        if self.data.len() < 8 {
            return err!("StackIsEmpty", "tried to pop from stack when it is empty");
        }

        let mut out = T::default();
        let to_bytes = unsafe { any_as_u8_slice_mut(&mut out) };

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
        let len = mem::size_of::<T>();
        if len > u32::MAX as usize {
            panic!("struct too long");
        }

        let to_bytes = self.get_var_range_mut(ptr, len as u32)?;
        to_bytes.copy_from_slice(any_as_u8_slice(&t));

        return Ok(());
    }
}

pub struct Runtime<Out, Log>
where
    Out: Write,
    Log: Write,
{
    pub stack: VarBuffer,
    pub heap: VarBuffer,
    pub callstack: Vec<CallFrame>,
    pub stdout: Out,
    pub stdlog: Log,
}

impl<Out, Log> Runtime<Out, Log>
where
    Out: Write,
    Log: Write,
{
    pub fn new(stdout: Out, stdlog: Log) -> Self {
        Self {
            stack: VarBuffer::new(),
            heap: VarBuffer::new(),
            callstack: Vec::new(),
            stdout,
            stdlog,
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
            write!(self.stdlog, "op: {:?}\n", op)
                .map_err(|err| error!("LoggingFailed", "failed to log ({})", err))?;

            self.callstack.push(func_desc.into_callframe(op.line));
            pc = self.run_op(fp, pc, program, func_desc, op.op)?;
            if self.callstack.pop().is_none() {
                return err!(
                    "CallstackEmpty",
                    "tried to pop callstack when callstack was empty"
                );
            }

            write!(self.stdlog, "stack: {:?}\n", self.stack.data)
                .map_err(|err| error!("LoggingFailed", "failed to log ({})", err))?;
            write!(self.stdlog, "heap: {:?}\n\n", self.heap.data)
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
        func_desc: FuncDesc,
        opcode: Opcode,
    ) -> Result<i32, IError> {
        match opcode {
            Opcode::Func(_) => {}

            Opcode::StackAlloc(space) => {
                self.stack.add_var(space);
            }
            Opcode::StackAllocPtr(space) => {
                let var = self.stack.add_var(space);
                self.stack.push(VarPointer::new_stack(var, 0));
            }
            Opcode::Alloc(space) => {
                let var = self.heap.add_var(space);
                self.stack.push(VarPointer::new_heap(var, 0));
            }

            Opcode::MakeTempInt64(value) => self.stack.push(value.to_be()),
            Opcode::MakeTempFloat64(value) => self.stack.push(value),
            Opcode::LoadStr(idx) => {
                let str_value = program.strings[idx as usize].as_bytes();
                let str_len = str_value.len() as u32;

                let (idx, bytes) = self.heap.add_var_range(str_len + 1);
                let ptr = VarPointer::new_heap(idx, 0);

                let last_idx = bytes.len() - 1;
                bytes[0..last_idx].copy_from_slice(str_value);
                bytes[last_idx] = 0;

                self.stack.push(ptr);
            }

            Opcode::Pop64 => {
                self.stack.pop::<u64>()?;
            }

            Opcode::GetLocal64 { var, offset } => {
                let ptr = VarPointer::new_stack(Self::fp_offset(fp, var), offset as i32);
                self.stack.push(self.stack.get_var::<u64>(ptr)?);
            }
            Opcode::SetLocal64 { var, offset } => {
                let ptr = VarPointer::new_stack(Self::fp_offset(fp, var), offset as i32);
                let word: u64 = self.stack.pop()?;
                self.stack.set(ptr, word)?;
            }

            Opcode::Get64 { offset } => {
                let mut ptr: VarPointer = self.stack.pop()?;
                ptr.offset += offset;
                self.stack.push(self.get_var::<u64>(ptr)?);
            }
            Opcode::Set64 { offset } => {
                let mut ptr: VarPointer = self.stack.pop()?;
                ptr.offset += offset;
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
                write!(self.stdout, "{}", word)
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

                write!(self.stdout, "{}", str_value)
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
        files: &files,
        strings: &strings,
        functions: &functions,
        ops: &ops,
    };

    let mut out = StringWriter::new();
    let mut logs = StringWriter::new();
    let mut runtime = Runtime::new(&mut out, &mut logs);
    let result = runtime.run_program(program);
    print!("{}", logs.to_string());
    result.expect("shouldn't fail");
    assert_eq!(out.to_string(), "hello, world!\n");
}
