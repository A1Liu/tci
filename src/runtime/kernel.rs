use super::error::*;
use super::memory::*;
use super::types::*;
use crate::filedb::*;
use crate::util::*;
use codespan_reporting::files::Files;
use codespan_reporting::term::termcolor::WriteColor;
use core::mem;
use std::io::{Error as IOError, Write};
use std::ops::{BitAnd, BitOr, BitXor};

/// Exit the program with an error code
pub const ECALL_EXIT: u32 = 0;

/// Get the number of arguments in the program.
pub const ECALL_ARGC: u32 = 1;

/// Get zero-indexed command line argument. Takes in a single int as a parameter,
/// and pushes a pointer to the string on the heap as the result.
pub const ECALL_ARGV: u32 = 2;

/// Returns whether or not the given pointer is safe
pub const ECALL_IS_SAFE: u32 = 3;

/// Returns a pointer to a buffer on the heap of the specified size.
pub const ECALL_HEAP_ALLOC: u32 = 4;

/// Throws an IError object up the callstack
pub const ECALL_THROW_ERROR: u32 = 5;

/// Calls Printf
pub const ECALL_PRINTF: u32 = 6;

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
            print!("{:?}\n{}", op, files.display_loc(self.loc()).unwrap());
        } else {
            println!("{:?} NO_FILE", op);
        }

        println!("{:?}", self.memory.expr_stack);
        let ret = self.run_op_internal(op);
        println!("{:?}\n", self.memory.expr_stack);

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
            Opcode::MakeI32 => {
                let val: i32 = self.memory.read_pc()?;
                self.memory.push(val.to_le());
            }
            Opcode::MakeU32 => {
                let val: u32 = self.memory.read_pc()?;
                self.memory.push(val.to_le());
            }
            Opcode::MakeI64 => {
                let val: i64 = self.memory.read_pc()?;
                self.memory.push(val.to_le());
            }
            Opcode::MakeU64 => {
                let val: u64 = self.memory.read_pc()?;
                self.memory.push(val.to_le());
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
                let size = u32::from_le(self.memory.pop()?);
                self.memory.read_bytes_to_stack(ptr, size)?;
            }

            Opcode::SExtend8To16 => {
                let val = self.memory.pop::<i8>()?;
                self.memory.push((val as i16).to_le());
            }
            Opcode::SExtend8To32 => {
                let val = self.memory.pop::<i8>()?;
                self.memory.push((val as i32).to_le());
            }
            Opcode::SExtend8To64 => {
                let val = self.memory.pop::<i8>()?;
                self.memory.push((val as i64).to_le());
            }
            Opcode::SExtend16To32 => {
                let val = i16::from_le(self.memory.pop()?);
                self.memory.push((val as i32).to_le());
            }
            Opcode::SExtend16To64 => {
                let val = i16::from_le(self.memory.pop()?);
                self.memory.push((val as i64).to_le());
            }
            Opcode::SExtend32To64 => {
                let val = i32::from_le(self.memory.pop()?);
                self.memory.push((val as i64).to_le());
            }

            Opcode::ZExtend8To16 => {
                let val = self.memory.pop::<u8>()?;
                self.memory.push((val as u16).to_le());
            }
            Opcode::ZExtend8To32 => {
                let val = self.memory.pop::<u8>()?;
                self.memory.push((val as u32).to_le());
            }
            Opcode::ZExtend8To64 => {
                let val = self.memory.pop::<u8>()?;
                self.memory.push((val as u64).to_le());
            }
            Opcode::ZExtend16To32 => {
                let val = u16::from_le(self.memory.pop()?);
                self.memory.push((val as u32).to_le());
            }
            Opcode::ZExtend16To64 => {
                let val = u16::from_le(self.memory.pop()?);
                self.memory.push((val as u64).to_le());
            }
            Opcode::ZExtend32To64 => {
                let val = u32::from_le(self.memory.pop()?);
                self.memory.push((val as u64).to_le());
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

            Opcode::AddU32 => {
                let word2 = u32::from_le(self.memory.pop()?);
                let word1 = u32::from_le(self.memory.pop()?);
                self.memory.push(word1.wrapping_add(word2).to_le());
            }
            Opcode::SubI32 => {
                let word2 = i32::from_le(self.memory.pop()?);
                let word1 = i32::from_le(self.memory.pop()?);
                self.memory.push(word1.wrapping_sub(word2).to_le());
            }
            Opcode::MulI32 => {
                let word2 = i32::from_le(self.memory.pop()?);
                let word1 = i32::from_le(self.memory.pop()?);
                self.memory.push(word1.wrapping_mul(word2).to_le());
            }
            Opcode::DivI32 => {
                let word2 = i32::from_le(self.memory.pop()?);
                let word1 = i32::from_le(self.memory.pop()?);
                self.memory.push(word1.wrapping_div(word2).to_le());
            }
            Opcode::DivU64 => {
                let word2 = u64::from_le(self.memory.pop()?);
                let word1 = u64::from_le(self.memory.pop()?);
                self.memory.push(word1.wrapping_div(word2).to_le());
            }

            Opcode::CompLeqI32 => {
                let word2 = i32::from_le(self.memory.pop()?);
                let word1 = i32::from_le(self.memory.pop()?);
                self.memory.push((word1 <= word2) as u8);
            }
            Opcode::CompLtI32 => {
                let word2 = i32::from_le(self.memory.pop()?);
                let word1 = i32::from_le(self.memory.pop()?);
                self.memory.push((word1 < word2) as u8);
            }

            Opcode::CompLeqU64 => {
                let word2 = u64::from_le(self.memory.pop()?);
                let word1 = u64::from_le(self.memory.pop()?);
                self.memory.push((word1 <= word2) as u8);
            }
            Opcode::CompLtU64 => {
                let word2 = u64::from_le(self.memory.pop()?);
                let word1 = u64::from_le(self.memory.pop()?);
                self.memory.push((word1 < word2) as u8);
            }

            Opcode::CompEq32 => {
                let word2 = i32::from_le(self.memory.pop()?);
                let word1 = i32::from_le(self.memory.pop()?);
                self.memory.push((word1 == word2) as u8);
            }
            Opcode::CompEq64 => {
                let word2 = u64::from_le(self.memory.pop()?);
                let word1 = u64::from_le(self.memory.pop()?);
                self.memory.push((word1 == word2) as u8);
            }

            Opcode::CompNeq32 => {
                let word2 = i32::from_le(self.memory.pop()?);
                let word1 = i32::from_le(self.memory.pop()?);
                self.memory.push((word1 != word2) as u8);
            }
            Opcode::CompNeq64 => {
                let word2 = i64::from_le(self.memory.pop()?);
                let word1 = i64::from_le(self.memory.pop()?);
                self.memory.push((word1 != word2) as u8);
            }

            Opcode::AddU64 => {
                let word2 = u64::from_le(self.memory.pop()?);
                let word1 = u64::from_le(self.memory.pop()?);

                self.memory.push(word1.wrapping_add(word2).to_le());
            }
            Opcode::SubI64 => {
                let word2 = i64::from_le(self.memory.pop()?);
                let word1 = i64::from_le(self.memory.pop()?);
                self.memory.push(word1.wrapping_sub(word2).to_le());
            }
            Opcode::SubU64 => {
                let word2 = u64::from_le(self.memory.pop()?);
                let word1 = u64::from_le(self.memory.pop()?);
                self.memory.push(word1.wrapping_sub(word2).to_le());
            }
            Opcode::MulI64 => {
                let word2 = i64::from_le(self.memory.pop()?);
                let word1 = i64::from_le(self.memory.pop()?);
                self.memory.push(word1.wrapping_mul(word2).to_le());
            }
            Opcode::MulU64 => {
                let word2 = u64::from_le(self.memory.pop()?);
                let word1 = u64::from_le(self.memory.pop()?);
                self.memory.push(word1.wrapping_mul(word2).to_le());
            }
            Opcode::DivI64 => {
                let word2 = i64::from_le(self.memory.pop()?);
                let word1 = i64::from_le(self.memory.pop()?);
                self.memory.push(word1.wrapping_div(word2).to_le());
            }
            Opcode::ModI32 => {
                let word2 = u32::from_le(self.memory.pop()?);
                let word1 = u32::from_le(self.memory.pop()?);
                self.memory.push((word1 % word2).to_le());
            }
            Opcode::ModI64 => {
                let word2 = u64::from_le(self.memory.pop()?);
                let word1 = u64::from_le(self.memory.pop()?);
                self.memory.push((word1 % word2).to_le());
            }
            Opcode::RShiftI32 => {
                let word2 = u8::from_le(self.memory.pop()?);
                let word1 = i32::from_le(self.memory.pop()?);
                let result = word1.wrapping_shr(word2 as u32);
                self.memory.push(result.to_le());
            }
            Opcode::LShiftI32 => {
                let word2 = u8::from_le(self.memory.pop()?);
                let word1 = i32::from_le(self.memory.pop()?);
                let result = word1.wrapping_shl(word2 as u32);
                self.memory.push(result.to_le());
            }

            Opcode::BitAndI8 => {
                let word2 = i8::from_le(self.memory.pop()?);
                let word1 = i8::from_le(self.memory.pop()?);
                self.memory.push(word1.bitand(word2).to_le());
            }
            Opcode::BitAndI32 => {
                let word2 = i32::from_le(self.memory.pop()?);
                let word1 = i32::from_le(self.memory.pop()?);
                self.memory.push(word1.bitand(word2).to_le());
            }
            Opcode::BitOrI8 => {
                let word2 = i8::from_le(self.memory.pop()?);
                let word1 = i8::from_le(self.memory.pop()?);
                self.memory.push(word1.bitor(word2).to_le());
            }
            Opcode::BitOrI32 => {
                let word2 = i32::from_le(self.memory.pop()?);
                let word1 = i32::from_le(self.memory.pop()?);
                self.memory.push(word1.bitor(word2).to_le());
            }
            Opcode::BitXorI32 => {
                let word2 = i32::from_le(self.memory.pop()?);
                let word1 = i32::from_le(self.memory.pop()?);
                self.memory.push(word1.bitxor(word2).to_le());
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
                let ecall = u32::from_le(self.memory.pop()?);
                match ecall {
                    ECALL_EXIT => {
                        let exit = i32::from_le(self.memory.pop()?);
                        self.status = RuntimeStatus::Exited(exit);
                    }

                    ECALL_IS_SAFE => {
                        let var_pointer: VarPointer = self.memory.pop()?;
                        let result = self.memory.read::<u8>(var_pointer).is_ok();
                        self.memory.push((result as u64).to_le());
                    }
                    ECALL_THROW_ERROR => {
                        let skip_frames = u32::from_le(self.memory.pop()?);
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
                        let size = u64::from_le(self.memory.pop()?);
                        let ptr = self.memory.add_heap_var(size as u32);
                        self.memory.push(ptr);
                    }

                    ECALL_PRINTF => {
                        let format_args: VarPointer = self.memory.pop()?;
                        let format_pointer: VarPointer = self.memory.pop()?;
                        let ret = printf(
                            &mut self.memory,
                            &mut self.output,
                            format_pointer,
                            format_args.var_idx() - 1,
                        )?;
                        self.memory.push((ret as u64).to_le());
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

pub fn printf(
    sel: &Memory,
    out: &mut StringArray<WriteEvent>,
    format_ptr: VarPointer,
    args: usize,
) -> Result<i32, IError> {
    let mut string_out = StringWriter::new();
    let args = args as u16;
    let result = printf_internal(sel, format_ptr, args, &mut string_out);
    let string_out = string_out.into_string();
    let len = string_out.len() as i32; // TODO overflow

    out.push(WriteEvent::StdoutWrite, &string_out);
    result?;

    return Ok(len);
}

#[allow(unused_assignments)] // TODO remove this when we make this fully standard compliant
pub fn printf_internal(
    sel: &Memory,
    format_ptr: VarPointer,
    mut current_offset: u16,
    mut out: &mut StringWriter,
) -> Result<(), IError> {
    // OPTIMIZE This does an unnecessary linear scan
    let format_str = sel.cstring_bytes(format_ptr)?;
    let map_err = |err| ierror!("WriteFailed", "failed to write to stdout ({})", err);

    // CREDIT heavily inspired by https://github.com/mpaland/printf/blob/master/printf.c

    const FLAGS_ZEROPAD: u32 = 1;
    const FLAGS_LEFT: u32 = 2;
    const FLAGS_PLUS: u32 = 4;
    const FLAGS_SPACE: u32 = 8;
    const FLAGS_HASH: u32 = 16;
    const FLAGS_PRECISION: u32 = 32;
    const FLAGS_LONG: u32 = 64;
    const FLAGS_LONG_LONG: u32 = 128;

    let mut next_ptr = || {
        let var_ptr = VarPointer::new_stack(current_offset, 0);
        current_offset -= 1;
        return var_ptr;
    };

    let parse_int = |begin: usize| {
        let mut idx = begin;
        if format_str[idx] >= b'0' && format_str[idx] <= b'9' {
            let mut collect = 0;
            loop {
                collect *= 10;
                collect += (format_str[idx] - b'0') as usize;
                idx += 1;

                if format_str[idx] < b'0' || format_str[idx] > b'9' {
                    break;
                }
            }

            return Some((collect, idx - begin));
        }

        return None;
    };

    let mut idx = 0;
    while idx < format_str.len() {
        let mut idx2 = idx;
        while idx2 < format_str.len() && format_str[idx2] != b'%' {
            idx2 += 1;
        }

        write_utf8_lossy(&mut out, &format_str[idx..idx2]).map_err(map_err)?;

        if idx2 == format_str.len() {
            break;
        }

        // format_str[idx2] == b'%'

        idx2 += 1;
        if idx2 == format_str.len() {
            return Err(ierror!(
                "InvalidFormatString",
                "format string ends with a single '%'; to print out a '%' use '%%'"
            ));
        }

        // format specifier?  %[flags][width][.precision][length]
        let mut flags = 0;
        let mut width = 0;
        let mut precision = 0;

        loop {
            match format_str[idx2] {
                b'0' => flags |= FLAGS_ZEROPAD,
                b'-' => flags |= FLAGS_LEFT,
                b'+' => flags |= FLAGS_PLUS,
                b' ' => flags |= FLAGS_SPACE,
                b'#' => flags |= FLAGS_HASH,
                _ => break,
            }
            idx += 1;
        }

        if let Some((w, diff)) = parse_int(idx2) {
            idx2 += diff;
            width = w;
        } else if format_str[idx2] == b'*' {
            let mut next = i32::from_le(sel.read(next_ptr())?);
            if next < 0 {
                flags |= FLAGS_LEFT;
                next *= -1;
            }

            width = next as usize;
            idx2 += 1;
        }

        if format_str[idx2] == b'.' {
            flags |= FLAGS_PRECISION;
            idx2 += 1;

            if let Some((prec, diff)) = parse_int(idx2) {
                idx2 += diff;
                precision = prec;
            } else if format_str[idx2] == b'*' {
                let next = i32::from_le(sel.read(next_ptr())?);
                precision = if next > 0 { next } else { 0 } as usize;
                idx2 += 1;
            }
        }

        match format_str[idx2] {
            b'l' => {
                flags |= FLAGS_LONG;
                idx2 += 1;
                if format_str[idx2] == b'l' {
                    flags |= FLAGS_LONG_LONG;
                    idx2 += 1;
                }
            }
            _ => {}
        }

        match format_str[idx2] {
            b'u' => {
                let base = 10;
                flags &= !FLAGS_HASH;
                if (flags & FLAGS_PRECISION) != 0 {
                    flags &= !FLAGS_ZEROPAD;
                }
                flags &= !(FLAGS_PLUS | FLAGS_SPACE);

                if (flags & FLAGS_LONG_LONG) != 0 {
                    let value = u64::from_le(sel.read(next_ptr())?);
                    write!(&mut out, "{}", value).map_err(map_err)?;
                } else if (flags & FLAGS_LONG) != 0 {
                    let value = u64::from_le(sel.read(next_ptr())?);
                    write!(&mut out, "{}", value).map_err(map_err)?;
                } else {
                    let value = u32::from_le(sel.read(next_ptr())?);
                    write!(&mut out, "{}", value).map_err(map_err)?;
                }
            }
            b'i' | b'd' => {
                let base = 10;
                flags &= !FLAGS_HASH;
                if (flags & FLAGS_PRECISION) != 0 {
                    flags &= !FLAGS_ZEROPAD;
                }

                if (flags & FLAGS_LONG_LONG) != 0 {
                    let value = i64::from_le(sel.read(next_ptr())?);
                    write!(&mut out, "{}", value).map_err(map_err)?;
                } else if (flags & FLAGS_LONG) != 0 {
                    let value = i64::from_le(sel.read(next_ptr())?);
                    write!(&mut out, "{}", value).map_err(map_err)?;
                } else {
                    let value = i32::from_le(sel.read(next_ptr())?);
                    write!(&mut out, "{}", value).map_err(map_err)?;
                }
            }
            b'c' => {
                let value: u8 = sel.read(next_ptr())?;
                write!(&mut out, "{}", char::from(value)).map_err(map_err)?;
            }
            b'%' => {
                write_utf8_lossy(&mut out, &[b'%']).map_err(map_err)?;
            }
            b's' => {
                println!("hello {}", precision);
                let char_ptr = sel.read(next_ptr())?;

                if precision == 0 {
                    write_utf8_lossy(&mut out, sel.cstring_bytes(char_ptr)?).map_err(map_err)?;
                } else {
                    write_utf8_lossy(&mut out, &sel.cstring_bytes(char_ptr)?[..precision])
                        .map_err(map_err)?;
                }
            }
            byte => {
                return Err(ierror!(
                    "InvalidFormatString",
                    "got byte '{}' after '%'",
                    char::from(byte)
                ))
            }
        }

        idx = idx2 + 1;
    }

    return Ok(());
}
