use crate::filedb::*;
use crate::lexer::*;
use crate::runtime::*;
use crate::util::*;
use core::ops::{Deref, DerefMut};
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
    pub fn new(file: u32, name: u32) -> Self {
        Self { file, name }
    }

    pub fn into_callframe(self, range: Range) -> CallFrame {
        CallFrame {
            file: self.file,
            name: self.name,
            range,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CallFrame {
    pub file: u32,
    pub name: u32,
    pub range: Range,
}

pub fn render_err(error: &IError, stack_trace: &Vec<CallFrame>, program: &Program) -> String {
    use codespan_reporting::diagnostic::*;
    use codespan_reporting::term::*;

    let mut out = StringWriter::new();
    let config = Config {
        display_style: DisplayStyle::Rich,
        tab_width: 4,
        styles: Styles::default(),
        chars: Chars::default(),
        start_context_lines: 3,
        end_context_lines: 1,
    };

    write!(out, "{}: {}\n", error.short_name, error.message).expect("cannot fail");
    for frame in stack_trace {
        let diagnostic = Diagnostic::new(Severity::Void)
            .with_labels(vec![Label::primary(frame.file, frame.range)]);
        codespan_reporting::term::emit(&mut out, &config, &program.files, &diagnostic)
            .expect("why did this fail?");
    }

    return out.to_string();
}

pub const ECALL_PRINT_STR: u32 = 0;
pub const ECALL_EXIT: u32 = 1;
pub const ECALL_EXIT_WITH_CODE: u32 = 2;

pub enum Directive {
    ChangePC(u32),
    Return,
    Exit(i32),
}

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
    StackAddToTemp,  // Pops a variable off the stack, adding it to the temporary storage below
    Alloc(u32), // Allocates space on the heap, then pushes a pointer to that space onto the stack

    MakeTempInt32(i32),
    MakeTempInt64(i64),
    MakeTempFloat64(f64),
    MakeTempBinaryPtr { var: u32, offset: u32 },

    Pop { bytes: u32 },
    PopKeep { keep: u32, drop: u32 },
    PopIntoTopVar { offset: u32, bytes: u32 },

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

    GetGlobal { var: u16, offset: u32, bytes: u32 },
    SetGlobal { var: u16, offset: u32, bytes: u32 },

    GetLocal { var: i16, offset: u32, bytes: u32 },
    SetLocal { var: i16, offset: u32, bytes: u32 },

    Get { offset: i32, bytes: u32 },
    Set { offset: i32, bytes: u32 },

    AddU32,
    SubI32,

    AddU64,
    SubI64,
    MulI64,
    DivI64,
    ModI64,

    JumpIfZero64(u32),
    JumpIfNotZero64(u32),

    Ret, // Returns to caller

    Call(u32),
    LibCall(u32),
    Ecall(u32),
}

#[derive(Debug, Clone, Copy)]
pub struct TaggedOpcode {
    pub op: Opcode,
    pub range: Range,
}

#[derive(Debug, Clone, Copy)]
pub struct Program<'a> {
    pub files: FileDbRef<'a>,
    pub data: VarBufferRef<'a>,
    pub symbols: &'a [&'a str],
    pub ops: &'a [TaggedOpcode],
    pub main_idx: u32,
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

    pub fn fp_offset(fp: u16, var: i16) -> u16 {
        if var < 0 {
            // TODO make sure there's no overflow happening here
            let var = (var * -1) as u16;
            fp - var
        } else {
            fp + var as u16
        }
    }

    pub fn run_program(&mut self, program: Program) -> i32 {
        self.memory = Memory::new_with_binary(program.data);
        let ret_addr = self.add_stack_var(4, 0);
        self.set(ret_addr, 0u32, 0)
            .expect("failed to write to return address location of main");

        let _argc = self.add_stack_var(4, 0);
        let _argv = self.add_stack_var(8, 0);

        // TODO populate argc and argv

        let result = match self.run_func(&program, program.main_idx) {
            Ok(res) => res,
            Err(err) => {
                let err_str = render_err(&err, &self.callstack, &program);
                write!(self.io.err(), "{}", err_str).expect("why did this fail?");

                return 1;
            }
        };

        return result.unwrap_or(i32::from_be(self.get_var(ret_addr).unwrap()));
    }

    pub fn run_func(&mut self, program: &Program, pcounter: u32) -> Result<Option<i32>, IError> {
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

        let fp = self.memory.stack_length() + 1;
        let mut pc: u32 = pcounter + 1;

        loop {
            let op = program.ops[pc as usize];

            write!(self.io.log(), "op: {:?}\n", op.op)
                .map_err(|err| error!("WriteFailed", "failed to write to logs ({})", err))?;
            self.callstack.push(func_desc.into_callframe(op.range));
            let directive = self.run_op(fp, pc, program, op.op)?;
            write!(self.io.log(), "stack: {:0>3?}\n", self.memory.stack.data)
                .map_err(|err| error!("WriteFailed", "failed to write to logs ({})", err))?;
            write!(self.io.log(), "heap:  {:0>3?}\n\n", self.memory.heap.data)
                .map_err(|err| error!("WriteFailed", "failed to write to logs ({})", err))?;

            match directive {
                Directive::ChangePC(new_pc) => {
                    if self.callstack.pop().is_none() {
                        return err!(
                            "CallstackEmpty",
                            "tried to pop callstack when callstack was empty"
                        );
                    }

                    pc = new_pc;
                }
                Directive::Return => {
                    if self.callstack.pop().is_none() {
                        return err!(
                            "CallstackEmpty",
                            "tried to pop callstack when callstack was empty"
                        );
                    }

                    while self.memory.stack_length() >= fp {
                        self.memory.pop_stack_var(pc)?;
                    }

                    return Ok(None);
                }
                Directive::Exit(val) => {
                    return Ok(Some(val));
                }
            }
        }
    }

    #[inline]
    pub fn run_op(
        &mut self,
        fp: u16,
        pc: u32,
        program: &Program,
        opcode: Opcode,
    ) -> Result<Directive, IError> {
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
            Opcode::StackAddToTemp => {
                self.pop_stack_var_onto_stack(pc)?;
            }

            Opcode::MakeTempInt32(value) => self.push_stack(value.to_be(), pc),
            Opcode::MakeTempInt64(value) => self.push_stack(value.to_be(), pc),
            Opcode::MakeTempFloat64(value) => self.push_stack(value, pc),
            Opcode::MakeTempBinaryPtr { var, offset } => {
                let ptr = VarPointer::new_binary(var, offset);
                self.push_stack(ptr, pc);
            }

            Opcode::Pop { bytes } => self.pop_bytes(bytes, pc)?,
            Opcode::PopKeep { keep, drop } => self.pop_keep_bytes(keep, drop, pc)?,
            Opcode::PopIntoTopVar { offset, bytes } => {
                let ptr = VarPointer::new_stack(self.stack_length(), offset);
                self.pop_stack_bytes_into(ptr, bytes, pc)?;
            }

            Opcode::SExtend8To16 => {
                let val = self.pop_stack::<i8>(pc)?;
                self.push_stack(val as i16, pc);
            }
            Opcode::SExtend8To32 => {
                let val = self.pop_stack::<i8>(pc)?;
                self.push_stack(val as i32, pc);
            }
            Opcode::SExtend8To64 => {
                let val = self.pop_stack::<i8>(pc)?;
                self.push_stack(val as i64, pc);
            }
            Opcode::SExtend16To32 => {
                let val = self.pop_stack::<i16>(pc)?;
                self.push_stack(val as i32, pc);
            }
            Opcode::SExtend16To64 => {
                let val = self.pop_stack::<i16>(pc)?;
                self.push_stack(val as i64, pc);
            }
            Opcode::SExtend32To64 => {
                let val = self.pop_stack::<i32>(pc)?;
                self.push_stack(val as i64, pc);
            }

            Opcode::ZExtend8To16 => {
                let val = self.pop_stack::<u8>(pc)?;
                self.push_stack(val as u16, pc);
            }
            Opcode::ZExtend8To32 => {
                let val = self.pop_stack::<u8>(pc)?;
                self.push_stack(val as u32, pc);
            }
            Opcode::ZExtend8To64 => {
                let val = self.pop_stack::<u8>(pc)?;
                self.push_stack(val as u64, pc);
            }
            Opcode::ZExtend16To32 => {
                let val = self.pop_stack::<u16>(pc)?;
                self.push_stack(val as u32, pc);
            }
            Opcode::ZExtend16To64 => {
                let val = self.pop_stack::<u16>(pc)?;
                self.push_stack(val as u64, pc);
            }
            Opcode::ZExtend32To64 => {
                let val = self.pop_stack::<u32>(pc)?;
                self.push_stack(val as u64, pc);
            }

            Opcode::GetGlobal { var, offset, bytes } => {
                let ptr = VarPointer::new_stack(var, offset);
                self.push_stack_bytes_from(ptr, bytes, pc)?;
            }
            Opcode::SetGlobal { var, offset, bytes } => {
                let ptr = VarPointer::new_stack(var, offset);
                self.pop_stack_bytes_into(ptr, bytes, pc)?;
            }

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

            Opcode::AddU32 => {
                let word2 = u32::from_be(self.pop_stack(pc)?);
                let word1 = u32::from_be(self.pop_stack(pc)?);
                self.push_stack(word1.wrapping_add(word2).to_be(), pc);
            }

            Opcode::SubI32 => {
                let word2 = i32::from_be(self.pop_stack(pc)?);
                let word1 = i32::from_be(self.pop_stack(pc)?);
                self.push_stack(word1.wrapping_sub(word2).to_be(), pc);
            }

            Opcode::AddU64 => {
                let word2 = u64::from_be(self.pop_stack(pc)?);
                let word1 = u64::from_be(self.pop_stack(pc)?);
                self.push_stack(word1.wrapping_add(word2).to_be(), pc);
            }
            Opcode::SubI64 => {
                let word2 = u64::from_be(self.pop_stack(pc)?);
                let word1 = u64::from_be(self.pop_stack(pc)?);
                self.push_stack(word1.wrapping_sub(word2).to_be(), pc);
            }
            Opcode::MulI64 => {
                let word2 = u64::from_be(self.pop_stack(pc)?);
                let word1 = u64::from_be(self.pop_stack(pc)?);
                self.push_stack(word1.wrapping_mul(word2).to_be(), pc);
            }
            Opcode::DivI64 => {
                let word2 = u64::from_be(self.pop_stack(pc)?);
                let word1 = u64::from_be(self.pop_stack(pc)?);
                self.push_stack(word1.wrapping_div(word2).to_be(), pc);
            }
            Opcode::ModI64 => {
                let word2 = u64::from_be(self.pop_stack(pc)?);
                let word1 = u64::from_be(self.pop_stack(pc)?);
                self.push_stack((word1 % word2).to_be(), pc);
            }

            Opcode::JumpIfZero64(target) => {
                let value: u64 = self.pop_stack(pc)?;
                if value == 0 {
                    return Ok(Directive::ChangePC(target));
                }
            }
            Opcode::JumpIfNotZero64(target) => {
                let value: u64 = self.pop_stack(pc)?;
                if value != 0 {
                    return Ok(Directive::ChangePC(target));
                }
            }

            Opcode::Ret => return Ok(Directive::Return),

            Opcode::Call(func) => {
                if let Some(exit_code) = self.run_func(program, func)? {
                    return Ok(Directive::Exit(exit_code));
                }
            }
            Opcode::LibCall(func_name) => {
                if let Some(exit_code) = self.dispatch_lib_func(func_name, pc)? {
                    return Ok(Directive::Exit(exit_code));
                }
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

                let str_value = unsafe { core::str::from_utf8_unchecked(&str_bytes[0..idx]) };

                write!(self.io.out(), "{}", str_value)
                    .map_err(|err| error!("WriteFailed", "failed to write to stdout ({})", err))?;
            }
            Opcode::Ecall(ECALL_EXIT_WITH_CODE) => {
                let code = i32::from_be(self.pop_stack(pc)?);
                return Ok(Directive::Exit(code));
            }
            Opcode::Ecall(call) => {
                return err!("InvalidEnviromentCall", "invalid ecall value of {}", call);
            }
        }

        return Ok(Directive::ChangePC(pc + 1));
    }

    pub fn dispatch_lib_func(&mut self, func_name: u32, pc: u32) -> Result<Option<i32>, IError> {
        match func_name {
            PRINTF_SYMBOL => return self.printf(),
            n => {
                return Err(error!(
                    "InvalidLibraryFunction",
                    "library function symbol {} is invalid (this is a problem with tci)", n
                ))
            }
        }
    }

    pub fn printf(&mut self) -> Result<Option<i32>, IError> {
        let top_ptr_offset = self.stack_length();
        let top_ptr = VarPointer::new_stack(top_ptr_offset, 0);
        let param_len = i32::from_be(self.get_var(top_ptr)?);

        let mut current_offset = top_ptr_offset - (param_len as u16);
        let format_ptr = VarPointer::new_stack(current_offset, 0); // TODO overflow
        current_offset += 1;

        // OPTIMIZE This does an unnecessary linear scan
        let format_str = self.cstring_bytes(self.get_var(format_ptr)?)?;

        let mut out = String::new();
        let mut idx = 0;
        while idx < format_str.len() {
            let mut idx2 = idx;
            while idx2 < format_str.len() && format_str[idx2] != b'%' {
                idx2 += 1;
            }

            string_append_utf8_lossy(&mut out, &format_str[idx..idx2]);

            if idx2 == format_str.len() {
                break;
            }

            // format_str[idx2] == b'%'

            idx2 += 1;
            if idx2 == format_str.len() {
                return Err(error!(
                    "InvalidFormatString",
                    "format string ends with a single '%'; to print out a '%' use '%%'"
                ));
            }

            match format_str[idx2] {
                b'%' => string_append_utf8_lossy(&mut out, &[b'%']),
                b's' => {
                    let var_ptr = VarPointer::new_stack(current_offset, 0);
                    let char_ptr = self.get_var(var_ptr)?;
                    current_offset += 1;

                    string_append_utf8_lossy(&mut out, self.cstring_bytes(char_ptr)?);
                }
                byte => {
                    return Err(error!(
                        "InvalidFormatString",
                        "got byte '{}' after '%'",
                        char::from(byte)
                    ));
                }
            }

            idx = idx2 + 1;
        }

        let map_err = |err| error!("WriteFailed", "failed to write to stdout ({})", err);
        write!(self.io.out(), "{}", &out).map_err(map_err)?;
        return Ok(None);
    }

    pub fn cstring_bytes(&self, ptr: VarPointer) -> Result<&[u8], IError> {
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

        return Ok(&str_bytes[0..idx]);
    }
}
