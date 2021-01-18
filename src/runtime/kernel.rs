use super::error::*;
use super::memory::*;
use super::types::*;
use crate::filedb::*;
use crate::util::*;
use codespan_reporting::files::Files;
use codespan_reporting::term::termcolor::WriteColor;
use core::mem;
use std::io::{Error as IOError, Write};

/// exit the program with an error code
pub const ECALL_EXIT: u32 = 0;
/// get the number of arguments in the program.
pub const ECALL_ARGC: u32 = 1;
/// get zero-indexed command line argument. Takes in a single int as a parameter,
/// and pushes a pointer to the string on the heap as the result.
pub const ECALL_ARGV: u32 = 2;
/// returns whether or not the given pointer is safe
pub const ECALL_IS_SAFE: u32 = 3;
/// returns a pointer to a buffer on the heap of the specified size.
pub const ECALL_HEAP_ALLOC: u32 = 4;
/// throws an IError object up the callstack
pub const ECALL_THROW_ERROR: u32 = 5;
/// sends a character to the screen
pub const ECALL_PRINT_STRING: u32 = 6;

pub struct Runtime {
    output: StringArray<WriteEvent>,
    memory: Memory,
    status: RuntimeStatus,
}

impl Runtime {
    pub fn new(binary: &BinaryData) -> Self {
        Self {
            output: StringArray::new(),
            memory: Memory::new(&binary),
            status: RuntimeStatus::Running,
        }
    }

    pub fn loc(&self) -> CodeLoc {
        return self.memory.loc;
    }

    pub fn run_debug(&mut self, files: &FileDb) -> i32 {
        loop {
            match self.run_op_debug(files) {
                RuntimeStatus::Exited(e) => return e,
                RuntimeStatus::Running => {}
            }
        }
    }

    pub fn run(&mut self) -> i32 {
        loop {
            match self.run_op() {
                RuntimeStatus::Exited(e) => return e,
                RuntimeStatus::Running => {}
            }
        }
    }

    pub fn run_op_count(&mut self, count: u32) -> RuntimeStatus {
        for _ in 0..count {
            match self.run_op() {
                RuntimeStatus::Running => {}
                x => return x,
            }
        }

        return self.status;
    }

    pub fn print_callstack<'a>(&mut self, files: &'a impl Files<'a, FileId = u32>) {
        let mut out = StringWriter::new();
        self.write_callstack(&mut out, files).unwrap();
        self.output
            .push(WriteEvent::StderrWrite, &out.into_string());
    }

    pub fn write_callstack<'a>(
        &self,
        out: &mut impl WriteColor,
        files: &'a impl Files<'a, FileId = u32>,
    ) -> Result<(), IOError> {
        use codespan_reporting::diagnostic::*;
        use codespan_reporting::term::*;

        let config = Config {
            display_style: DisplayStyle::Rich,
            tab_width: 4,
            styles: Styles::default(),
            chars: Chars::default(),
            start_context_lines: 3,
            end_context_lines: 1,
        };

        for frame in self.memory.callstack.iter().skip(1) {
            let diagnostic = Diagnostic::new(Severity::Void)
                .with_labels(vec![Label::primary(frame.loc.file, frame.loc)]);
            emit(out, &config, files, &diagnostic)?;
        }

        let loc = self.memory.loc;
        if loc != NO_FILE {
            let diagnostic =
                Diagnostic::new(Severity::Void).with_labels(vec![Label::primary(loc.file, loc)]);
            emit(out, &config, files, &diagnostic)?;
        }

        return Ok(());
    }

    pub fn run_op(&mut self) -> RuntimeStatus {
        match self.status {
            RuntimeStatus::Running => {}
            _ => return self.status,
        }

        let op = match self.memory.read_pc() {
            Ok(op) => op,
            Err(e) => {
                self.status = RuntimeStatus::Exited(1);
                let mut out = StringWriter::new();

                if self.output.len() > 0 {
                    let TS(t, s) = &self.output[self.output.len() - 1];
                    if !s.ends_with("\n") {
                        write!(out, "\n").unwrap();
                    }
                }

                write!(out, "{}: {}\n", e.short_name, e.message).unwrap();
                self.output
                    .push(WriteEvent::StderrWrite, &out.into_string());

                return self.status;
            }
        };

        if let Err(e) = self.run_op_internal(op) {
            self.status = RuntimeStatus::Exited(1);
            let mut out = StringWriter::new();

            if self.output.len() > 0 {
                let TS(t, s) = &self.output[self.output.len() - 1];
                if !s.ends_with("\n") {
                    write!(out, "\n").unwrap();
                }
            }

            write!(out, "{}: {}\n", e.short_name, e.message).unwrap();
            self.output
                .push(WriteEvent::StderrWrite, &out.into_string());
        }

        return self.status;
    }

    pub fn run_op_debug(&mut self, files: &FileDb) -> RuntimeStatus {
        match self.status {
            RuntimeStatus::Running => {}
            _ => return self.status,
        }

        let op = match self.memory.read_pc() {
            Ok(op) => op,
            Err(e) => {
                self.status = RuntimeStatus::Exited(1);
                let mut out = StringWriter::new();

                if self.output.len() > 0 {
                    let TS(t, s) = &self.output[self.output.len() - 1];
                    if !s.ends_with("\n") {
                        write!(out, "\n").unwrap();
                    }
                }

                write!(out, "{}: {}\n", e.short_name, e.message).unwrap();
                self.output
                    .push(WriteEvent::StderrWrite, &out.into_string());

                return self.status;
            }
        };

        if self.loc() != NO_FILE {
            debug!("{:?}\n{}", op, files.display_loc(self.loc()).unwrap());
        } else {
            debug!("{:?} NO_FILE", op);
        }

        debug!("{:?}", self.memory.expr_stack);
        let ret = self.run_op_internal(op);
        debug!("{:?}\n", self.memory.expr_stack);

        if let Err(e) = ret {
            self.status = RuntimeStatus::Exited(1);
            let mut out = StringWriter::new();

            if self.output.len() > 0 {
                let TS(t, s) = &self.output[self.output.len() - 1];
                if !s.ends_with("\n") {
                    write!(out, "\n").unwrap();
                }
            }

            write!(out, "{}: {}\n", e.short_name, e.message).unwrap();
            self.output
                .push(WriteEvent::StderrWrite, &out.into_string());
        }

        return self.status;
    }

    pub fn run_op_internal(&mut self, op: Opcode) -> Result<(), IError> {
        match op {
            Opcode::Func => {
                // this opcode is handled by Memory
                return Err(ierror!(
                    "InvalidOpcode",
                    "Found Func opcode (this is an error in TCI)"
                ));
            }
            Opcode::Loc => {
                let loc = self.memory.read_pc()?;
                self.memory.set_loc(loc);
            }
            Opcode::StackAlloc => {
                let bytes: u32 = self.memory.read_pc()?;
                self.memory.add_stack_var(bytes);
            }
            Opcode::StackDealloc => {
                self.memory.pop_stack_var()?;
            }

            Opcode::MakeI8 => {
                let val: i8 = self.memory.read_pc()?;
                self.memory.push(val);
            }
            Opcode::MakeU8 => {
                let val: u8 = self.memory.read_pc()?;
                self.memory.push(val);
            }
            Opcode::MakeI16 => {
                let val: i16 = self.memory.read_pc()?;
                self.memory.push(val);
            }
            Opcode::MakeU16 => {
                let val: u16 = self.memory.read_pc()?;
                self.memory.push(val);
            }
            Opcode::MakeI32 => {
                let val: i32 = self.memory.read_pc()?;
                self.memory.push(val);
            }
            Opcode::MakeU32 => {
                let val: u32 = self.memory.read_pc()?;
                self.memory.push(val);
            }
            Opcode::MakeI64 => {
                let val: i64 = self.memory.read_pc()?;
                self.memory.push(val);
            }
            Opcode::MakeU64 => {
                let val: u64 = self.memory.read_pc()?;
                self.memory.push(val);
            }
            Opcode::MakeF32 => {
                let val: f32 = self.memory.read_pc()?;
                self.memory.push(val);
            }
            Opcode::MakeF64 => {
                let val: f64 = self.memory.read_pc()?;
                self.memory.push(val);
            }
            Opcode::MakeSp => {
                let var_offset: i16 = self.memory.read_pc()?;
                let stack_len = self.memory.stack.len() as u16;
                let var = (stack_len as i16 + var_offset) as u16;

                self.memory.push(VarPointer::new_stack(var, 0));
            }
            Opcode::MakeFp => {
                let var_offset: i16 = self.memory.read_pc()?;
                let var = (self.memory.fp as i16 + var_offset) as u16;

                self.memory.push(VarPointer::new_stack(var, 0));
            }

            Opcode::PushUndef => {
                let bytes: u32 = self.memory.read_pc()?;
                let stack_len = self.memory.expr_stack.len();
                self.memory.expr_stack.resize(stack_len + bytes as usize, 0);
            }
            Opcode::Pop => {
                let bytes = self.memory.read_pc()?;
                self.memory.pop_bytes(bytes)?;
            }
            Opcode::Swap => {
                let top_bytes = self.memory.read_pc()?;
                let bottom_bytes = self.memory.read_pc()?;
                self.memory.swap_bytes(top_bytes, bottom_bytes)?;
            }
            Opcode::Dup => {
                let bytes = self.memory.read_pc()?;
                self.memory.dup_bytes(bytes)?;
            }
            Opcode::PushDyn => {
                let ptr: VarPointer = self.memory.pop()?;
                let size: u32 = self.memory.pop()?;
                self.memory.read_bytes_to_stack(ptr, size)?;
            }

            Opcode::I8ToF32 => {
                let n: i8 = self.memory.pop()?;
                self.memory.push(n as f32);
            }
            Opcode::U8ToF32 => {
                let n: u8 = self.memory.pop()?;
                self.memory.push(n as f32);
            }
            Opcode::I8ToF64 => {
                let n: i8 = self.memory.pop()?;
                self.memory.push(n as f64);
            }
            Opcode::U8ToF64 => {
                let n: i8 = self.memory.pop()?;
                self.memory.push(n as f64);
            }
            Opcode::I16ToF32 => {
                let n: i16 = self.memory.pop()?;
                self.memory.push(n as f32);
            }
            Opcode::U16ToF32 => {
                let n: u16 = self.memory.pop()?;
                self.memory.push(n as f32);
            }
            Opcode::I16ToF64 => {
                let n: i16 = self.memory.pop()?;
                self.memory.push(n as f64);
            }
            Opcode::U16ToF64 => {
                let n: i16 = self.memory.pop()?;
                self.memory.push(n as f64);
            }
            Opcode::I32ToF32 => {
                let n: i32 = self.memory.pop()?;
                self.memory.push(n as f32);
            }
            Opcode::U32ToF32 => {
                let n: u32 = self.memory.pop()?;
                self.memory.push(n as f32);
            }
            Opcode::I32ToF64 => {
                let n: i32 = self.memory.pop()?;
                self.memory.push(n as f64);
            }
            Opcode::U32ToF64 => {
                let n: i32 = self.memory.pop()?;
                self.memory.push(n as f64);
            }
            Opcode::I64ToF32 => {
                let n: i64 = self.memory.pop()?;
                self.memory.push(n as f32);
            }
            Opcode::U64ToF32 => {
                let n: u64 = self.memory.pop()?;
                self.memory.push(n as f32);
            }
            Opcode::I64ToF64 => {
                let n: i64 = self.memory.pop()?;
                self.memory.push(n as f64);
            }
            Opcode::U64ToF64 => {
                let n: i64 = self.memory.pop()?;
                self.memory.push(n as f64);
            }

            Opcode::F32ToI8 => {
                let f: f32 = self.memory.pop()?;
                self.memory.push(f as i8);
            }
            Opcode::F32ToU8 => {
                let f: f32 = self.memory.pop()?;
                self.memory.push(f as u8);
            }
            Opcode::F64ToI8 => {
                let f: f64 = self.memory.pop()?;
                self.memory.push(f as i8);
            }
            Opcode::F64ToU8 => {
                let f: f64 = self.memory.pop()?;
                self.memory.push(f as u8);
            }
            Opcode::F32ToI16 => {
                let f: f32 = self.memory.pop()?;
                self.memory.push(f as i16);
            }
            Opcode::F32ToU16 => {
                let f: f32 = self.memory.pop()?;
                self.memory.push(f as u16);
            }
            Opcode::F64ToI16 => {
                let f: f64 = self.memory.pop()?;
                self.memory.push(f as i16);
            }
            Opcode::F64ToU16 => {
                let f: f64 = self.memory.pop()?;
                self.memory.push(f as u16);
            }
            Opcode::F32ToI32 => {
                let f: f32 = self.memory.pop()?;
                self.memory.push(f as i32);
            }
            Opcode::F32ToU32 => {
                let f: f32 = self.memory.pop()?;
                self.memory.push(f as u32);
            }
            Opcode::F64ToI32 => {
                let f: f64 = self.memory.pop()?;
                self.memory.push(f as i32);
            }
            Opcode::F64ToU32 => {
                let f: f64 = self.memory.pop()?;
                self.memory.push(f as u32);
            }
            Opcode::F32ToI64 => {
                let f: f32 = self.memory.pop()?;
                self.memory.push(f as i64);
            }
            Opcode::F32ToU64 => {
                let f: f32 = self.memory.pop()?;
                self.memory.push(f as u64);
            }
            Opcode::F64ToI64 => {
                let f: f64 = self.memory.pop()?;
                self.memory.push(f as i64);
            }
            Opcode::F64ToU64 => {
                let f: f64 = self.memory.pop()?;
                self.memory.push(f as u64);
            }

            Opcode::F32ToF64 => {
                let f: f32 = self.memory.pop()?;
                self.memory.push(f as f64);
            }
            Opcode::F64ToF32 => {
                let f: f64 = self.memory.pop()?;
                self.memory.push(f as f32);
            }

            Opcode::SExtend8To16 => {
                let val = self.memory.pop::<i8>()?;
                self.memory.push(val as i16);
            }
            Opcode::SExtend8To32 => {
                let val = self.memory.pop::<i8>()?;
                self.memory.push(val as i32);
            }
            Opcode::SExtend8To64 => {
                let val = self.memory.pop::<i8>()?;
                self.memory.push(val as i64);
            }
            Opcode::SExtend16To32 => {
                let val: i16 = self.memory.pop()?;
                self.memory.push(val as i32);
            }
            Opcode::SExtend16To64 => {
                let val: i16 = self.memory.pop()?;
                self.memory.push(val as i64);
            }
            Opcode::SExtend32To64 => {
                let val: i32 = self.memory.pop()?;
                self.memory.push(val as i64);
            }

            Opcode::ZExtend8To16 => {
                let val = self.memory.pop::<u8>()?;
                self.memory.push(val as u16);
            }
            Opcode::ZExtend8To32 => {
                let val = self.memory.pop::<u8>()?;
                self.memory.push(val as u32);
            }
            Opcode::ZExtend8To64 => {
                let val = self.memory.pop::<u8>()?;
                self.memory.push(val as u64);
            }
            Opcode::ZExtend16To32 => {
                let val: u16 = self.memory.pop()?;
                self.memory.push(val as u32);
            }
            Opcode::ZExtend16To64 => {
                let val: u16 = self.memory.pop()?;
                self.memory.push(val as u64);
            }
            Opcode::ZExtend32To64 => {
                let val: u32 = self.memory.pop()?;
                self.memory.push(val as u64);
            }

            Opcode::Get => {
                let bytes = self.memory.read_pc()?;
                let ptr: VarPointer = self.memory.pop()?;

                self.memory.read_bytes_to_stack(ptr, bytes)?;
            }
            Opcode::Set => {
                let bytes = self.memory.read_pc()?;
                let ptr: VarPointer = self.memory.pop()?;
                self.memory.write_bytes_from_stack(ptr, bytes)?;
            }

            Opcode::BoolNorm8 => {
                let bytes: u8 = self.memory.pop()?;
                self.memory.push(if bytes != 0 { 1u8 } else { 0u8 });
            }
            Opcode::BoolNorm16 => {
                let bytes: u16 = self.memory.pop()?;
                self.memory.push(if bytes != 0 { 1u8 } else { 0u8 });
            }
            Opcode::BoolNorm32 => {
                let bytes: u32 = self.memory.pop()?;
                self.memory.push(if bytes != 0 { 1u8 } else { 0u8 });
            }
            Opcode::BoolNorm64 => {
                let bytes: u64 = self.memory.pop()?;
                self.memory.push(if bytes != 0 { 1u8 } else { 0u8 });
            }

            Opcode::BoolNot8 => {
                let bytes: u8 = self.memory.pop()?;
                self.memory.push(if bytes == 0 { 1u8 } else { 0u8 });
            }
            Opcode::BoolNot16 => {
                let bytes: u16 = self.memory.pop()?;
                self.memory.push(if bytes == 0 { 1u8 } else { 0u8 });
            }
            Opcode::BoolNot32 => {
                let bytes: u32 = self.memory.pop()?;
                self.memory.push(if bytes == 0 { 1u8 } else { 0u8 });
            }
            Opcode::BoolNot64 => {
                let bytes: u64 = self.memory.pop()?;
                self.memory.push(if bytes == 0 { 1u8 } else { 0u8 });
            }

            Opcode::CompLeqI8 => {
                let word2: i8 = self.memory.pop()?;
                let word1: i8 = self.memory.pop()?;
                self.memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLeqU8 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u8 = self.memory.pop()?;
                self.memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLeqI16 => {
                let word2: i16 = self.memory.pop()?;
                let word1: i16 = self.memory.pop()?;
                self.memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLeqU16 => {
                let word2: u16 = self.memory.pop()?;
                let word1: u16 = self.memory.pop()?;
                self.memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLeqI32 => {
                let word2: i32 = self.memory.pop()?;
                let word1: i32 = self.memory.pop()?;
                self.memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLeqU32 => {
                let word2: u32 = self.memory.pop()?;
                let word1: u32 = self.memory.pop()?;
                self.memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLeqI64 => {
                let word2: i64 = self.memory.pop()?;
                let word1: i64 = self.memory.pop()?;
                self.memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLeqU64 => {
                let word2: u64 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLeqF32 => {
                let word2: f32 = self.memory.pop()?;
                let word1: f32 = self.memory.pop()?;
                self.memory.push(if word1 < word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLeqF64 => {
                let word2: f64 = self.memory.pop()?;
                let word1: f64 = self.memory.pop()?;
                self.memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
            }

            Opcode::CompLtI8 => {
                let word2: i8 = self.memory.pop()?;
                let word1: i8 = self.memory.pop()?;
                self.memory.push(if word1 <= word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLtU8 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u8 = self.memory.pop()?;
                self.memory.push(if word1 < word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLtI16 => {
                let word2: i16 = self.memory.pop()?;
                let word1: i16 = self.memory.pop()?;
                self.memory.push(if word1 < word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLtU16 => {
                let word2: u16 = self.memory.pop()?;
                let word1: u16 = self.memory.pop()?;
                self.memory.push(if word1 < word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLtI32 => {
                let word2: i32 = self.memory.pop()?;
                let word1: i32 = self.memory.pop()?;
                self.memory.push(if word1 < word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLtU32 => {
                let word2: u32 = self.memory.pop()?;
                let word1: u32 = self.memory.pop()?;
                self.memory.push(if word1 < word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLtI64 => {
                let word2: i64 = self.memory.pop()?;
                let word1: i64 = self.memory.pop()?;
                self.memory.push(if word1 < word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLtU64 => {
                let word2: u64 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(if word1 < word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLtF32 => {
                let word2: f32 = self.memory.pop()?;
                let word1: f32 = self.memory.pop()?;
                self.memory.push(if word1 < word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompLtF64 => {
                let word2: f64 = self.memory.pop()?;
                let word1: f64 = self.memory.pop()?;
                self.memory.push(if word1 < word2 { 1u8 } else { 0u8 });
            }

            Opcode::CompEq8 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u8 = self.memory.pop()?;
                self.memory.push(if word1 == word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompEq16 => {
                let word2: u16 = self.memory.pop()?;
                let word1: u16 = self.memory.pop()?;
                self.memory.push(if word1 == word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompEq32 => {
                let word2: u32 = self.memory.pop()?;
                let word1: u32 = self.memory.pop()?;
                self.memory.push(if word1 == word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompEq64 => {
                let word2: u64 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(if word1 == word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompEqF32 => {
                let word2: f32 = self.memory.pop()?;
                let word1: f32 = self.memory.pop()?;
                self.memory.push(if word1 == word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompEqF64 => {
                let word2: f64 = self.memory.pop()?;
                let word1: f64 = self.memory.pop()?;
                self.memory.push(if word1 == word2 { 1u8 } else { 0u8 });
            }

            Opcode::CompNeq8 => {
                let word2: i8 = self.memory.pop()?;
                let word1: i8 = self.memory.pop()?;
                self.memory.push(if word1 != word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompNeq16 => {
                let word2: i16 = self.memory.pop()?;
                let word1: i16 = self.memory.pop()?;
                self.memory.push(if word1 != word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompNeq32 => {
                let word2: i32 = self.memory.pop()?;
                let word1: i32 = self.memory.pop()?;
                self.memory.push(if word1 != word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompNeq64 => {
                let word2: i64 = self.memory.pop()?;
                let word1: i64 = self.memory.pop()?;
                self.memory.push(if word1 != word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompNeqF32 => {
                let word2: f32 = self.memory.pop()?;
                let word1: f32 = self.memory.pop()?;
                self.memory.push(if word1 != word2 { 1u8 } else { 0u8 });
            }
            Opcode::CompNeqF64 => {
                let word2: f64 = self.memory.pop()?;
                let word1: f64 = self.memory.pop()?;
                self.memory.push(if word1 != word2 { 1u8 } else { 0u8 });
            }

            Opcode::Add8 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u8 = self.memory.pop()?;
                self.memory.push(word1.wrapping_add(word2));
            }
            Opcode::Add16 => {
                let word2: u16 = self.memory.pop()?;
                let word1: u16 = self.memory.pop()?;

                self.memory.push(word1.wrapping_add(word2));
            }
            Opcode::Add32 => {
                let word2: u32 = self.memory.pop()?;
                let word1: u32 = self.memory.pop()?;
                self.memory.push(word1.wrapping_add(word2));
            }
            Opcode::Add64 => {
                let word2: u64 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(word1.wrapping_add(word2));
            }
            Opcode::AddF32 => {
                let word2: f32 = self.memory.pop()?;
                let word1: f32 = self.memory.pop()?;
                self.memory.push(word1 + word2);
            }
            Opcode::AddF64 => {
                let word2: f64 = self.memory.pop()?;
                let word1: f64 = self.memory.pop()?;
                self.memory.push(word1 + word2);
            }

            Opcode::SubI8 => {
                let word2: i8 = self.memory.pop()?;
                let word1: i8 = self.memory.pop()?;
                self.memory.push(word1.wrapping_sub(word2));
            }
            Opcode::SubU8 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u8 = self.memory.pop()?;
                self.memory.push(word1.wrapping_sub(word2));
            }
            Opcode::SubI16 => {
                let word2: i16 = self.memory.pop()?;
                let word1: i16 = self.memory.pop()?;
                self.memory.push(word1.wrapping_sub(word2));
            }
            Opcode::SubU16 => {
                let word2: u16 = self.memory.pop()?;
                let word1: u16 = self.memory.pop()?;
                self.memory.push(word1.wrapping_sub(word2));
            }
            Opcode::SubI32 => {
                let word2: i32 = self.memory.pop()?;
                let word1: i32 = self.memory.pop()?;
                self.memory.push(word1.wrapping_sub(word2));
            }
            Opcode::SubU32 => {
                let word2: u32 = self.memory.pop()?;
                let word1: u32 = self.memory.pop()?;
                self.memory.push(word1.wrapping_sub(word2));
            }
            Opcode::SubI64 => {
                let word2: i64 = self.memory.pop()?;
                let word1: i64 = self.memory.pop()?;
                self.memory.push(word1.wrapping_sub(word2));
            }
            Opcode::SubU64 => {
                let word2: u64 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(word1.wrapping_sub(word2));
            }
            Opcode::SubF32 => {
                let word2: f32 = self.memory.pop()?;
                let word1: f32 = self.memory.pop()?;
                self.memory.push(word1 - word2);
            }
            Opcode::SubF64 => {
                let word2: f64 = self.memory.pop()?;
                let word1: f64 = self.memory.pop()?;
                self.memory.push(word1 - word2);
            }

            Opcode::MulU8 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u8 = self.memory.pop()?;
                self.memory.push(word1.wrapping_mul(word2));
            }
            Opcode::MulI8 => {
                let word2: i8 = self.memory.pop()?;
                let word1: i8 = self.memory.pop()?;
                self.memory.push(word1.wrapping_mul(word2));
            }
            Opcode::MulI16 => {
                let word2: i16 = self.memory.pop()?;
                let word1: i16 = self.memory.pop()?;
                self.memory.push(word1.wrapping_mul(word2));
            }
            Opcode::MulU16 => {
                let word2: u16 = self.memory.pop()?;
                let word1: u16 = self.memory.pop()?;
                self.memory.push(word1.wrapping_mul(word2));
            }
            Opcode::MulU32 => {
                let word2: u32 = self.memory.pop()?;
                let word1: u32 = self.memory.pop()?;
                self.memory.push(word1.wrapping_mul(word2));
            }
            Opcode::MulI32 => {
                let word2: i32 = self.memory.pop()?;
                let word1: i32 = self.memory.pop()?;
                self.memory.push(word1.wrapping_mul(word2));
            }
            Opcode::MulI64 => {
                let word2: i64 = self.memory.pop()?;
                let word1: i64 = self.memory.pop()?;
                self.memory.push(word1.wrapping_mul(word2));
            }
            Opcode::MulU64 => {
                let word2: u64 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(word1.wrapping_mul(word2));
            }
            Opcode::MulF32 => {
                let word2: i32 = self.memory.pop()?;
                let word1: i32 = self.memory.pop()?;
                self.memory.push(word1 * word2);
            }
            Opcode::MulF64 => {
                let word2: u64 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(word1 * word2);
            }

            Opcode::DivU8 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u8 = self.memory.pop()?;
                self.memory.push(word1.wrapping_div(word2));
            }
            Opcode::DivI8 => {
                let word2: i8 = self.memory.pop()?;
                let word1: i8 = self.memory.pop()?;
                self.memory.push(word1.wrapping_div(word2));
            }
            Opcode::DivI16 => {
                let word2: i16 = self.memory.pop()?;
                let word1: i16 = self.memory.pop()?;
                self.memory.push(word1.wrapping_div(word2));
            }
            Opcode::DivU16 => {
                let word2: u16 = self.memory.pop()?;
                let word1: u16 = self.memory.pop()?;
                self.memory.push(word1.wrapping_div(word2));
            }
            Opcode::DivU32 => {
                let word2: u32 = self.memory.pop()?;
                let word1: u32 = self.memory.pop()?;
                self.memory.push(word1.wrapping_div(word2));
            }
            Opcode::DivI32 => {
                let word2: i32 = self.memory.pop()?;
                let word1: i32 = self.memory.pop()?;
                self.memory.push(word1.wrapping_div(word2));
            }
            Opcode::DivI64 => {
                let word2: i64 = self.memory.pop()?;
                let word1: i64 = self.memory.pop()?;
                self.memory.push(word1.wrapping_div(word2));
            }
            Opcode::DivU64 => {
                let word2: u64 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(word1.wrapping_div(word2));
            }
            Opcode::DivF32 => {
                let word2: i32 = self.memory.pop()?;
                let word1: i32 = self.memory.pop()?;
                self.memory.push(word1 / word2);
            }
            Opcode::DivF64 => {
                let word2: u64 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(word1 / word2);
            }

            Opcode::ModU8 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u8 = self.memory.pop()?;
                self.memory.push(word1 % word2);
            }
            Opcode::ModI8 => {
                let word2: i8 = self.memory.pop()?;
                let word1: i8 = self.memory.pop()?;
                self.memory.push(word1 % word2);
            }
            Opcode::ModI16 => {
                let word2: i16 = self.memory.pop()?;
                let word1: i16 = self.memory.pop()?;
                self.memory.push(word1 % word2);
            }
            Opcode::ModU16 => {
                let word2: u16 = self.memory.pop()?;
                let word1: u16 = self.memory.pop()?;
                self.memory.push(word1 % word2);
            }
            Opcode::ModU32 => {
                let word2: u32 = self.memory.pop()?;
                let word1: u32 = self.memory.pop()?;
                self.memory.push(word1 % word2);
            }
            Opcode::ModI32 => {
                let word2: i32 = self.memory.pop()?;
                let word1: i32 = self.memory.pop()?;
                self.memory.push(word1 % word2);
            }
            Opcode::ModI64 => {
                let word2: i64 = self.memory.pop()?;
                let word1: i64 = self.memory.pop()?;
                self.memory.push(word1 % word2);
            }
            Opcode::ModU64 => {
                let word2: u64 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(word1 % word2);
            }
            Opcode::ModF32 => {
                let word2: i32 = self.memory.pop()?;
                let word1: i32 = self.memory.pop()?;
                self.memory.push(word1 % word2);
            }
            Opcode::ModF64 => {
                let word2: u64 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(word1 % word2);
            }

            Opcode::RShiftI8 => {
                let word2: u8 = self.memory.pop()?;
                let word1: i8 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shr(word2 as u32));
            }
            Opcode::RShiftU8 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u8 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shr(word2 as u32));
            }
            Opcode::RShiftI16 => {
                let word2: u8 = self.memory.pop()?;
                let word1: i16 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shr(word2 as u32));
            }
            Opcode::RShiftU16 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u16 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shr(word2 as u32));
            }
            Opcode::RShiftI32 => {
                let word2: u8 = self.memory.pop()?;
                let word1: i32 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shr(word2 as u32));
            }
            Opcode::RShiftU32 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u32 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shr(word2 as u32));
            }
            Opcode::RShiftI64 => {
                let word2: u8 = self.memory.pop()?;
                let word1: i64 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shr(word2 as u32));
            }
            Opcode::RShiftU64 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shr(word2 as u32));
            }

            Opcode::LShiftI8 => {
                let word2: u8 = self.memory.pop()?;
                let word1: i8 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shl(word2 as u32));
            }
            Opcode::LShiftU8 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u8 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shl(word2 as u32));
            }
            Opcode::LShiftI16 => {
                let word2: u8 = self.memory.pop()?;
                let word1: i16 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shl(word2 as u32));
            }
            Opcode::LShiftU16 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u16 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shl(word2 as u32));
            }
            Opcode::LShiftI32 => {
                let word2: u8 = self.memory.pop()?;
                let word1: i32 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shl(word2 as u32));
            }
            Opcode::LShiftU32 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u32 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shl(word2 as u32));
            }
            Opcode::LShiftI64 => {
                let word2: u8 = self.memory.pop()?;
                let word1: i64 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shl(word2 as u32));
            }
            Opcode::LShiftU64 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(word1.wrapping_shl(word2 as u32));
            }

            Opcode::BitAnd8 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u8 = self.memory.pop()?;
                self.memory.push(word1 & word2);
            }
            Opcode::BitAnd16 => {
                let word2: u16 = self.memory.pop()?;
                let word1: u16 = self.memory.pop()?;
                self.memory.push(word1 & word2);
            }
            Opcode::BitAnd32 => {
                let word2: u32 = self.memory.pop()?;
                let word1: u32 = self.memory.pop()?;
                self.memory.push(word1 & word2);
            }
            Opcode::BitAnd64 => {
                let word2: u64 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(word1 & word2);
            }

            Opcode::BitOr8 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u8 = self.memory.pop()?;
                self.memory.push(word1 | word2);
            }
            Opcode::BitOr16 => {
                let word2: u16 = self.memory.pop()?;
                let word1: u16 = self.memory.pop()?;
                self.memory.push(word1 | word2);
            }
            Opcode::BitOr32 => {
                let word2: u32 = self.memory.pop()?;
                let word1: u32 = self.memory.pop()?;
                self.memory.push(word1 | word2);
            }
            Opcode::BitOr64 => {
                let word2: u64 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(word1 | word2);
            }

            Opcode::BitXor8 => {
                let word2: u8 = self.memory.pop()?;
                let word1: u8 = self.memory.pop()?;
                self.memory.push(word1 ^ word2);
            }
            Opcode::BitXor16 => {
                let word2: u16 = self.memory.pop()?;
                let word1: u16 = self.memory.pop()?;
                self.memory.push(word1 ^ word2);
            }
            Opcode::BitXor32 => {
                let word2: u32 = self.memory.pop()?;
                let word1: u32 = self.memory.pop()?;
                self.memory.push(word1 ^ word2);
            }
            Opcode::BitXor64 => {
                let word2: u64 = self.memory.pop()?;
                let word1: u64 = self.memory.pop()?;
                self.memory.push(word1 ^ word2);
            }

            Opcode::BitNot8 => {
                let word: u8 = self.memory.pop()?;
                self.memory.push(!word);
            }
            Opcode::BitNot16 => {
                let word: u16 = self.memory.pop()?;
                self.memory.push(!word);
            }
            Opcode::BitNot32 => {
                let word: u32 = self.memory.pop()?;
                self.memory.push(!word);
            }
            Opcode::BitNot64 => {
                let word: u64 = self.memory.pop()?;
                self.memory.push(!word);
            }

            Opcode::Jump => {
                let target = self.memory.read_pc()?;
                self.memory.jump(target);
            }

            Opcode::JumpIfZero8 => {
                let target = self.memory.read_pc()?;
                let value: u8 = self.memory.pop()?;
                if value == 0 {
                    self.memory.jump(target);
                }
            }
            Opcode::JumpIfZero16 => {
                let target = self.memory.read_pc()?;
                let value: u16 = self.memory.pop()?;
                if value == 0 {
                    self.memory.jump(target);
                }
            }
            Opcode::JumpIfZero32 => {
                let target = self.memory.read_pc()?;
                let value: u32 = self.memory.pop()?;
                if value == 0 {
                    self.memory.jump(target);
                }
            }
            Opcode::JumpIfZero64 => {
                let target = self.memory.read_pc()?;
                let value: u64 = self.memory.pop()?;
                if value == 0 {
                    self.memory.jump(target);
                }
            }

            Opcode::JumpIfNotZero8 => {
                let target = self.memory.read_pc()?;
                let value: u8 = self.memory.pop()?;
                if value != 0 {
                    self.memory.jump(target);
                }
            }
            Opcode::JumpIfNotZero16 => {
                let target = self.memory.read_pc()?;
                let value: u16 = self.memory.pop()?;
                if value != 0 {
                    self.memory.jump(target);
                }
            }
            Opcode::JumpIfNotZero32 => {
                let target = self.memory.read_pc()?;
                let value: u32 = self.memory.pop()?;
                if value != 0 {
                    self.memory.jump(target);
                }
            }
            Opcode::JumpIfNotZero64 => {
                let target = self.memory.read_pc()?;
                let value: u64 = self.memory.pop()?;
                if value != 0 {
                    self.memory.jump(target);
                }
            }

            Opcode::Ret => {
                self.memory.ret()?;
            }
            Opcode::Call => {
                let func: VarPointer = self.memory.pop()?;
                self.memory.call(func)?;
            }
            Opcode::Ecall => {
                let ecall: u32 = self.memory.pop()?;
                match ecall {
                    ECALL_EXIT => {
                        let exit: i32 = self.memory.pop()?;
                        self.status = RuntimeStatus::Exited(exit);
                    }

                    ECALL_IS_SAFE => {
                        let var_pointer: VarPointer = self.memory.pop()?;
                        let result = self.memory.read::<u8>(var_pointer).is_ok();
                        self.memory.push(result as u64);
                    }
                    ECALL_THROW_ERROR => {
                        let skip_frames: u32 = self.memory.pop()?;
                        let message_ptr: VarPointer = self.memory.pop()?;
                        let name_ptr: VarPointer = self.memory.pop()?;

                        let message_bytes = self.memory.cstring_bytes(message_ptr)?;
                        let name_bytes = self.memory.cstring_bytes(name_ptr)?;

                        let mut message = String::new();
                        string_append_utf8_lossy(&mut message, message_bytes);
                        let mut name = String::new();
                        string_append_utf8_lossy(&mut name, name_bytes);

                        for _ in 0..skip_frames {
                            self.memory.ret()?;
                        }

                        return Err(IError::new(name, message));
                    }

                    ECALL_HEAP_ALLOC => {
                        let size: u64 = self.memory.pop()?;
                        let ptr = self.memory.add_heap_var(size as u32);
                        self.memory.push(ptr);
                    }

                    ECALL_PRINT_STRING => {
                        let len: u32 = self.memory.pop()?;
                        let string: VarPointer = self.memory.pop()?;

                        let bytes = self.memory.read_bytes(string, len)?;
                        let mut string = StringWriter::new();
                        write_utf8_lossy(&mut string, bytes).unwrap();
                        self.output
                            .push(WriteEvent::StdoutWrite, &string.into_string());

                        self.memory.push(0u64);
                    }

                    call => {
                        return ierr!("InvalidEnviromentCall", "invalid ecall value of {}", call)
                    }
                }
            }
        }

        return Ok(());
    }

    pub fn events(&mut self) -> StringArray<WriteEvent> {
        return mem::replace(&mut self.output, StringArray::new());
    }
}
