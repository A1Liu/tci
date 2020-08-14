use crate::runtime::*;
use crate::util::*;
use core::ops::{Deref, DerefMut};
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

pub fn render_err(
    error: &IError,
    stack_trace: &Vec<CallFrame>,
    program: &Program,
) -> Result<String, std::io::Error> {
    let mut out = StringWriter::new();
    write!(out, "{}: {}\n", error.short_name, error.message)?;
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

pub const ECALL_PRINT_INT: u32 = 0;
pub const ECALL_PRINT_STR: u32 = 1;

/// - GetLocal gets a value from the stack at a given stack and variable offset
/// - SetLocal sets a value on the stack at a given stack and variable offset to the value at the top
///   of the stack
/// - Set and Get are equivalent of GetLocal and SetLocal, but the location they access is
///   determined by popping the top of the stack first
/// - PopKeep pops keep-many bytes off the stack, then pops drop-many bytes off the stack and
///   repushes the first set of popped bytes back onto  the stack
#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    Func(FuncDesc), // Function header used for callstack manipulation

    StackAlloc(u32), // Allocates space on the stack
    StackDealloc,    // Pops a variable off of the stack
    Alloc(u32), // Allocates space on the heap, then pushes a pointer to that space onto the stack

    MakeTempInt64(i64),
    MakeTempFloat64(f64),
    LoadStr(u32),

    Pop { bytes: u32 },
    PopKeep { keep: u32, drop: u32 },

    GetLocal { var: i32, offset: u32, bytes: u32 },
    SetLocal { var: i32, offset: u32, bytes: u32 },

    Get { offset: i32, bytes: u32 },
    Set { offset: i32, bytes: u32 },

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

pub struct Runtime<IO: RuntimeIO> {
    pub memory: Memory<u32>,
    pub callstack: Vec<CallFrame>,
    pub io: IO,
}

impl<IO: RuntimeIO> DerefMut for Runtime<IO> {
    fn deref_mut(&mut self) -> &mut Memory<u32> {
        return &mut self.memory;
    }
}

impl<IO: RuntimeIO> Deref for Runtime<IO> {
    type Target = Memory<u32>;
    fn deref(&self) -> &Memory<u32> {
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
            fp - var - 1
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

    pub fn run_func(&mut self, program: &Program, pcounter: u32) -> Result<(), IError> {
        let func_desc = match program.ops[pcounter as usize].op {
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
        let ptr = self.add_stack_var(mem::size_of::<FuncDesc>() as u32, pcounter);
        self.push_stack(func_desc, pcounter);
        self.pop_stack_bytes_into(ptr, mem::size_of::<FuncDesc>() as u32, pcounter)
            .expect("why did this fail?");
        let fp = self.memory.stack_length();
        let mut pc: u32 = pcounter + 1;

        loop {
            let op = program.ops[pc as usize];
            write!(self.io.log(), "op: {:?}\n", op.op)
                .map_err(|err| error!("LoggingFailed", "failed to log ({})", err))?;

            self.callstack.push(func_desc.into_callframe(op.line));
            let new_pc = self.run_op(fp, pc, program, op.op)?;
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

            if new_pc < 0 {
                while self.memory.stack_length() > fp {
                    self.memory.pop_stack_var(pc)?;
                }
                self.memory.pop_stack_var(pc)?;
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

            pc = new_pc as u32;
        }
    }

    #[inline]
    pub fn run_op(
        &mut self,
        fp: u32,
        pc: u32,
        program: &Program,
        opcode: Opcode,
    ) -> Result<i32, IError> {
        match opcode {
            Opcode::Func(_) => {}

            Opcode::StackAlloc(space) => {
                self.add_stack_var(space, pc);
            }
            Opcode::Alloc(space) => {
                let ptr = self.add_heap_var(space, pc);
                self.push_stack(ptr, pc);
            }
            Opcode::StackDealloc => {
                self.pop_stack_var(pc)?;
            }

            Opcode::MakeTempInt64(value) => self.push_stack(value.to_be(), pc),
            Opcode::MakeTempFloat64(value) => self.push_stack(value, pc),
            Opcode::LoadStr(idx) => {
                let str_value = program.strings[idx as usize].as_bytes();
                let str_len = str_value.len() as u32; // TODO check for overflow

                let ptr = self.add_heap_var(str_len + 1, pc);
                self.write_bytes(ptr, str_value, pc)?;
                let mut end_ptr = ptr;
                end_ptr.set_offset(str_len);
                self.write_bytes(end_ptr, &vec![0], pc)?;
                self.push_stack(ptr, pc);
            }

            Opcode::Pop { bytes } => self.pop_bytes(bytes, pc)?,
            Opcode::PopKeep { keep, drop } => self.pop_keep_bytes(keep, drop, pc)?,

            Opcode::GetLocal { var, offset, bytes } => {
                let ptr = VarPointer::new_stack(Self::fp_offset(fp, var), offset);
                self.push_stack_bytes_from(ptr, bytes, pc)?;
            }
            Opcode::SetLocal { var, offset, bytes } => {
                let ptr = VarPointer::new_stack(Self::fp_offset(fp, var), offset);
                self.pop_stack_bytes_into(ptr, bytes, pc)?;
            }

            Opcode::Get { offset, bytes } => {
                let ptr: VarPointer = self.pop_stack(pc)?;
                let ptr = ptr.with_offset(ptr.offset().wrapping_add(offset as u32)); // TODO check for overflow
                self.push_stack_bytes_from(ptr, bytes, pc)?;
            }
            Opcode::Set { offset, bytes } => {
                let ptr: VarPointer = self.pop_stack(pc)?;
                let ptr = ptr.with_offset(ptr.offset().wrapping_add(offset as u32)); // TODO check for overflow
                self.pop_stack_bytes_into(ptr, bytes, pc)?;
            }

            Opcode::AddU64 => {
                let word1: u64 = u64::from_be(self.pop_stack(pc)?);
                let word2: u64 = u64::from_be(self.pop_stack(pc)?);
                self.push_stack(word1.wrapping_add(word2).to_be(), pc);
            }
            Opcode::SubI64 => {
                let word1: u64 = u64::from_be(self.pop_stack(pc)?);
                let word2: u64 = u64::from_be(self.pop_stack(pc)?);
                self.push_stack(word1.wrapping_sub(word2).to_be(), pc);
            }
            Opcode::MulI64 => {
                let word1: u64 = u64::from_be(self.pop_stack(pc)?);
                let word2: u64 = u64::from_be(self.pop_stack(pc)?);
                self.push_stack(word1.wrapping_mul(word2).to_be(), pc);
            }
            Opcode::DivI64 => {
                let word1: u64 = u64::from_be(self.pop_stack(pc)?);
                let word2: u64 = u64::from_be(self.pop_stack(pc)?);
                self.push_stack(word1.wrapping_div(word2).to_be(), pc);
            }
            Opcode::ModI64 => {
                let word1: u64 = u64::from_be(self.pop_stack(pc)?);
                let word2: u64 = u64::from_be(self.pop_stack(pc)?);
                self.push_stack((word1 % word2).to_be(), pc);
            }

            Opcode::JumpIfZero64(target) => {
                let value: u64 = self.pop_stack(pc)?;
                if value == 0 {
                    return Ok(target as i32);
                }
            }
            Opcode::JumpIfNotZero64(target) => {
                let value: u64 = self.pop_stack(pc)?;
                if value != 0 {
                    return Ok(target as i32);
                }
            }

            Opcode::Ret => return Ok(-1),

            Opcode::Call(func) => {
                self.run_func(program, func)?;
            }

            Opcode::Ecall(ECALL_PRINT_INT) => {
                let word: i64 = i64::from_be(self.pop_stack(pc)?);
                write!(self.io.out(), "{}", word)
                    .map_err(|err| error!("WriteFailed", "failed to write to stdout ({})", err))?;
            }
            Opcode::Ecall(ECALL_PRINT_STR) => {
                let ptr: VarPointer = self.pop_stack(pc)?;
                let str_bytes = self.memory.get_var_slice(ptr)?;

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

        return Ok(pc as i32 + 1);
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
        Opcode::SetLocal {
            var: 0,
            offset: 0,
            bytes: 8,
        },
        Opcode::Call(6),
        Opcode::Ret,
        Opcode::Func(FuncDesc { file: 0, name: 1 }),
        Opcode::GetLocal {
            var: -1,
            offset: 0,
            bytes: 8,
        },
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
                render_err(&err, &runtime.callstack, &program).expect("why did this fail?")
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
        Opcode::SetLocal {
            var: 0,
            offset: 0,
            bytes: 8,
        },
        Opcode::Call(6),
        Opcode::Ret,
        Opcode::Func(FuncDesc { file: 0, name: 1 }),
        Opcode::GetLocal {
            var: -1,
            offset: 0,
            bytes: 8,
        },
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
                render_err(&err, &runtime.callstack, &program).expect("why did this fail?")
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
        Opcode::SetLocal {
            var: 0,
            offset: 0,
            bytes: 8,
        },
        Opcode::Call(6),
        Opcode::Ret,
        Opcode::Func(FuncDesc { file: 0, name: 1 }),
        Opcode::GetLocal {
            var: -1,
            offset: 0,
            bytes: 8,
        },
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
                render_err(&err, &runtime.callstack, &program).expect("why did this fail?")
            );
            println!("{}", runtime.io.log().to_string());
            assert_eq!(err.short_name, "InvalidPointer");
        }
    }
}
