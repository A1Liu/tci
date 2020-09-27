use crate::filedb::*;
use crate::runtime::*;
use crate::util::*;
use core::ops::{Deref, DerefMut};
use std::collections::HashMap;
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
pub struct CallFrame {
    pub name: u32,
    pub loc: CodeLoc,
    pub fp: u16,
    pub pc: u32,
}

impl CallFrame {
    pub fn new(name: u32, loc: CodeLoc, fp: u16, pc: u32) -> Self {
        Self { name, loc, fp, pc }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LocOrRetCode {
    Loc(CodeLoc),
    Code(i32),
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
            .with_labels(vec![Label::primary(frame.loc.file, frame.loc)]);
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
    Next,
    Call(u32),
    LibCall(u32),
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
/// - Comp compares pops t, the top of the stack, and compares it to n, the next item on the stack.
///   it pushes the byte 1 onto the stack if n < t, and the byte 0 onto the stack if n >= t.
/// - CompEq compares pops t, the top of the stack, and compares it to n, the next item on the stack.
///   it pushes the byte 1 onto the stack if n == t, and the byte 0 onto the stack if n != t.
#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    Func(u32), // Function header used for callstack manipulation

    StackAlloc(u32), // Allocates space on the stack
    StackDealloc,    // Pops a variable off of the stack
    StackAddToTemp,  // Pops a variable off the stack, adding it to the temporary storage below
    Alloc(u32), // Allocates space on the heap, then pushes a pointer to that space onto the stack

    MakeTempInt32(i32),
    MakeTempInt64(i64),
    MakeTempFloat64(f64),
    MakeTempBinaryPtr { var: u32, offset: u32 },
    MakeTempLocalStackPtr { var: i16, offset: u32 },

    Pop { bytes: u32 },
    PopKeep { keep: u32, drop: u32 },
    PushUndef { bytes: u32 }, // Push undefined bytes onto the stack
    PushDup { bytes: u32 },   // Push bytes duplicated from the top of the stack
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

    GetLocal { var: i16, offset: u32, bytes: u32 },
    SetLocal { var: i16, offset: u32, bytes: u32 },

    Get { offset: u32, bytes: u32 },
    Set { offset: u32, bytes: u32 },

    AddU32,
    AddU64,

    SubI32,
    SubI64,

    CompI32,
    CompEqI32,

    MulI64,
    DivI64,
    ModI64,

    Jump(u32),

    JumpIfZero8(u32),
    JumpIfZero16(u32),
    JumpIfZero32(u32),
    JumpIfZero64(u32),

    JumpIfNotZero8(u32),
    JumpIfNotZero16(u32),
    JumpIfNotZero32(u32),
    JumpIfNotZero64(u32),

    Ret, // Returns to caller

    Call(u32),
    LibCall(u32),
    Ecall(u32),
}

#[derive(Debug, Clone, Copy)]
pub struct TaggedOpcode {
    pub op: Opcode,
    pub loc: CodeLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct Program<'a> {
    pub files: FileDbRef<'a>,
    pub data: VarBufferRef<'a>,
    pub ops: &'a [TaggedOpcode],
    pub main_idx: u32,
}

type LibFunc<IO> = for<'a> fn(&'a mut Runtime<IO>, u32) -> Result<Option<i32>, IError>;

pub struct Runtime<IO: RuntimeIO> {
    pub memory: Memory<u32>,
    pub callstack: Vec<CallFrame>,
    pub lib_funcs: HashMap<u32, LibFunc<IO>>,
    pub program: Program<'static>,
    pub current_func: u32,
    pub ret_addr: VarPointer,
    pub fp: u16,
    pub pc: u32,
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
    pub fn new(program: Program<'static>, io: IO) -> Self {
        let mut lib_funcs: HashMap<u32, LibFunc<IO>> = HashMap::new();

        lib_funcs.insert(INIT_SYMS.translate["printf"], printf);
        lib_funcs.insert(INIT_SYMS.translate["exit"], exit);

        let main_func = match program.ops[program.main_idx as usize].op {
            Opcode::Func(name) => name,
            op => panic!("found function header {:?} (this is an error in tci)", op),
        };

        let mut memory = Memory::new_with_binary(program.data);
        let ret_addr = memory.add_stack_var(4, 0);
        #[rustfmt::skip]
        memory.set(ret_addr, 0u32, 0).expect("failed write of ret_addr of main");

        let _argc = memory.add_stack_var(4, 0);
        let _argv = memory.add_stack_var(8, 0);

        let s = Self {
            fp: memory.stack_length() + 1,
            memory,
            callstack: Vec::new(),
            pc: program.main_idx + 1,
            current_func: main_func,
            ret_addr,
            program,
            lib_funcs,
            io,
        };

        // TODO initialize _argc and _argv

        return s;
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

    pub fn run(&mut self) -> Result<i32, IError> {
        loop {
            let op = self.program.ops[self.pc as usize];

            write!(self.io.log(), "op: {:?}\n", op.op)
                .map_err(|err| error!("WriteFailed", "failed to write to logs ({})", err))?;
            self.callstack
                .push(CallFrame::new(self.current_func, op.loc, self.fp, self.pc));
            let directive = self.run_op(self.fp, self.pc, op.op)?;
            write!(self.io.log(), "stack: {:0>3?}\n", self.memory.stack.data)
                .map_err(|err| error!("WriteFailed", "failed to write to logs ({})", err))?;
            write!(self.io.log(), "heap:  {:0>3?}\n\n", self.memory.heap.data)
                .map_err(|err| error!("WriteFailed", "failed to write to logs ({})", err))?;

            match directive {
                Directive::ChangePC(new_pc) => {
                    self.callstack.pop().unwrap();
                    self.pc = new_pc;
                }
                Directive::Next => {
                    self.callstack.pop().unwrap();
                    self.pc += 1;
                }
                Directive::Return => {
                    self.callstack.pop().unwrap();

                    let frame = match self.callstack.pop() {
                        None => return Ok(i32::from_be(self.get_var(self.ret_addr).unwrap())),
                        Some(frame) => frame,
                    };
                    while self.memory.stack_length() >= self.fp {
                        self.memory.pop_stack_var(self.pc).unwrap();
                    }

                    self.current_func = frame.name;
                    self.fp = frame.fp;
                    self.pc = frame.pc + 1;
                }
                Directive::Call(func) => {
                    self.fp = self.memory.stack_length() + 1;

                    let func_name = match self.program.ops[func as usize].op {
                        Opcode::Func(name) => name,
                        op => panic!("found function header {:?} (this is an error in tci)", op),
                    };
                    self.current_func = func_name;
                    self.pc = func + 1;
                }
                Directive::LibCall(func_name) => {
                    self.dispatch_lib_func(func_name, self.pc)?;
                    self.pc += 1;
                    self.callstack.pop().unwrap();
                }
                Directive::Exit(val) => {
                    return Ok(val);
                }
            }
        }
    }

    pub fn run_until_pc(&mut self, pc: u32) -> Result<LocOrRetCode, IError> {
        loop {
            let op = self.program.ops[self.pc as usize];
            if self.pc == pc {
                return Ok(LocOrRetCode::Loc(op.loc));
            }

            write!(self.io.log(), "op: {:?}\n", op.op)
                .map_err(|err| error!("WriteFailed", "failed to write to logs ({})", err))?;
            self.callstack
                .push(CallFrame::new(self.current_func, op.loc, self.fp, self.pc));
            let directive = self.run_op(self.fp, self.pc, op.op)?;
            write!(self.io.log(), "stack: {:0>3?}\n", self.memory.stack.data)
                .map_err(|err| error!("WriteFailed", "failed to write to logs ({})", err))?;
            write!(self.io.log(), "heap:  {:0>3?}\n\n", self.memory.heap.data)
                .map_err(|err| error!("WriteFailed", "failed to write to logs ({})", err))?;

            match directive {
                Directive::ChangePC(new_pc) => {
                    self.callstack.pop().unwrap();
                    self.pc = new_pc;
                }
                Directive::Next => {
                    self.callstack.pop().unwrap();
                    self.pc += 1;
                }
                Directive::Return => {
                    self.callstack.pop().unwrap();

                    let frame = match self.callstack.pop() {
                        None => {
                            let return_code = i32::from_be(self.get_var(self.ret_addr).unwrap());
                            return Ok(LocOrRetCode::Code(return_code));
                        }
                        Some(frame) => frame,
                    };
                    while self.memory.stack_length() >= self.fp {
                        self.memory.pop_stack_var(self.pc).unwrap();
                    }

                    self.current_func = frame.name;
                    self.fp = frame.fp;
                    self.pc = frame.pc + 1;
                }
                Directive::Call(func) => {
                    self.fp = self.memory.stack_length() + 1;

                    let func_name = match self.program.ops[func as usize].op {
                        Opcode::Func(name) => name,
                        op => panic!("found function header {:?} (this is an error in tci)", op),
                    };
                    self.current_func = func_name;
                    self.pc = func + 1;
                }
                Directive::LibCall(func_name) => {
                    self.dispatch_lib_func(func_name, self.pc)?;
                    self.pc += 1;
                    self.callstack.pop().unwrap();
                }
                Directive::Exit(val) => {
                    return Ok(LocOrRetCode::Code(val));
                }
            }
        }
    }

    #[inline]
    pub fn run_op(&mut self, fp: u16, pc: u32, opcode: Opcode) -> Result<Directive, IError> {
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
            Opcode::MakeTempLocalStackPtr { var, offset } => {
                let ptr = VarPointer::new_stack(Self::fp_offset(fp, var), offset);
                self.push_stack(ptr, pc);
            }

            Opcode::Pop { bytes } => self.pop_bytes(bytes, pc)?,
            Opcode::PopKeep { keep, drop } => self.pop_keep_bytes(keep, drop, pc)?,
            Opcode::PushUndef { bytes } => {
                self.add_stack_var(bytes, pc);
                self.pop_stack_var_onto_stack(pc)
                    .expect("should never fail");
            }
            Opcode::PushDup { bytes } => {
                self.dup_top_stack_bytes(bytes, pc)?;
            }
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

            Opcode::CompI32 => {
                let word2 = i32::from_be(self.pop_stack(pc)?);
                let word1 = i32::from_be(self.pop_stack(pc)?);
                self.push_stack((word1 < word2) as u8, pc);
            }
            Opcode::CompEqI32 => {
                let word2 = i32::from_be(self.pop_stack(pc)?);
                let word1 = i32::from_be(self.pop_stack(pc)?);
                self.push_stack((word1 == word2) as u8, pc);
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

            Opcode::Jump(target) => {
                return Ok(Directive::ChangePC(target));
            }

            Opcode::JumpIfZero8(target) => {
                let value: u8 = self.pop_stack(pc)?;
                if value == 0 {
                    return Ok(Directive::ChangePC(target));
                }
            }
            Opcode::JumpIfZero16(target) => {
                let value: u16 = self.pop_stack(pc)?;
                if value == 0 {
                    return Ok(Directive::ChangePC(target));
                }
            }
            Opcode::JumpIfZero32(target) => {
                let value: u32 = self.pop_stack(pc)?;
                if value == 0 {
                    return Ok(Directive::ChangePC(target));
                }
            }
            Opcode::JumpIfZero64(target) => {
                let value: u64 = self.pop_stack(pc)?;
                if value == 0 {
                    return Ok(Directive::ChangePC(target));
                }
            }

            Opcode::JumpIfNotZero8(target) => {
                let value: u8 = self.pop_stack(pc)?;
                if value != 0 {
                    return Ok(Directive::ChangePC(target));
                }
            }
            Opcode::JumpIfNotZero16(target) => {
                let value: u16 = self.pop_stack(pc)?;
                if value != 0 {
                    return Ok(Directive::ChangePC(target));
                }
            }
            Opcode::JumpIfNotZero32(target) => {
                let value: u32 = self.pop_stack(pc)?;
                if value != 0 {
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
                return Ok(Directive::Call(func));
            }
            Opcode::LibCall(func_name) => {
                return Ok(Directive::LibCall(func_name));
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

        return Ok(Directive::Next);
    }

    pub fn dispatch_lib_func(&mut self, func_name: u32, pc: u32) -> Result<Option<i32>, IError> {
        if let Some(lib_func) = self.lib_funcs.get(&func_name) {
            return lib_func(self, pc);
        }

        return Err(error!(
            "InvalidLibraryFunction",
            "library function symbol {} is invalid (this is a problem with tci)", func_name
        ));
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

pub fn exit<IO: RuntimeIO>(sel: &mut Runtime<IO>, _pc: u32) -> Result<Option<i32>, IError> {
    let top_ptr = VarPointer::new_stack(sel.stack_length(), 0);
    let exit_code = i32::from_be(sel.memory.get_var(top_ptr)?);
    return Ok(Some(exit_code));
}

pub fn printf<IO: RuntimeIO>(sel: &mut Runtime<IO>, pc: u32) -> Result<Option<i32>, IError> {
    let top_ptr_offset = sel.stack_length();
    let top_ptr = VarPointer::new_stack(top_ptr_offset, 0);
    let param_len = i32::from_be(sel.get_var(top_ptr)?);

    let mut current_offset = top_ptr_offset - (param_len as u16);
    let return_offset = current_offset - 1;
    let format_ptr = VarPointer::new_stack(current_offset, 0); // TODO overflow
    current_offset += 1;

    // OPTIMIZE This does an unnecessary linear scan
    let format_str = sel.cstring_bytes(sel.memory.get_var(format_ptr)?)?;

    let mut out = StringWriter::new();
    let mut idx = 0;
    while idx < format_str.len() {
        let mut idx2 = idx;
        while idx2 < format_str.len() && format_str[idx2] != b'%' {
            idx2 += 1;
        }

        write_utf8_lossy(&mut out, &format_str[idx..idx2]).expect("this shouldn't fail");

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
            b'%' => {
                write_utf8_lossy(&mut out, &[b'%']).expect("this shouldn't fail");
            }
            b's' => {
                let var_ptr = VarPointer::new_stack(current_offset, 0);
                let char_ptr = sel.get_var(var_ptr)?;
                current_offset += 1;

                write_utf8_lossy(&mut out, sel.cstring_bytes(char_ptr)?)
                    .expect("this shouldn't fail");
            }
            b'd' => {
                let var_ptr = VarPointer::new_stack(current_offset, 0);
                let value = i32::from_be(sel.memory.get_var(var_ptr)?);
                current_offset += 1;

                write!(&mut out, "{}", value).expect("this shouldn't fail");
            }
            byte => {
                return Err(error!(
                    "InvalidFormatString",
                    "got byte '{}' after '%'",
                    char::from(byte)
                ))
            }
        }

        idx = idx2 + 1;
    }

    let out = out.into_string();
    let len = out.len() as i32;
    let map_err = |err| error!("WriteFailed", "failed to write to stdout ({})", err);
    write!(sel.io.out(), "{}", &out).map_err(map_err)?;

    // Return value for function
    let return_ptr = VarPointer::new_stack(return_offset, 0); // TODO overflow
    sel.set(return_ptr, len.to_be(), pc)?;

    return Ok(None);
}
