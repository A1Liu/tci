use crate::buckets::*;
use crate::filedb::*;
use crate::util::*;
use core::{fmt, mem, str};
use serde::ser::{Serialize, SerializeSeq, SerializeStruct, Serializer};
use smol::io::AsyncReadExt;
use std::collections::VecDeque;
use std::io;
use std::io::{stderr, stdout, Stderr, Stdout, Write};

pub fn render_err(error: &IError, stack_trace: &Vec<CallFrame>, files: &FileDbRef) -> String {
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

    write!(out, "{}: {}\n", error.short_name, error.message).unwrap();

    for frame in stack_trace.iter().skip(1) {
        let diagnostic = Diagnostic::new(Severity::Void)
            .with_labels(vec![Label::primary(frame.loc.file, frame.loc)]);
        codespan_reporting::term::emit(&mut out, &config, files, &diagnostic).unwrap();
    }

    return out.to_string();
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct IError {
    pub short_name: &'static str,
    pub message: String,
}

impl IError {
    pub fn new(short_name: &'static str, message: String) -> Self {
        Self {
            short_name: short_name,
            message,
        }
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

impl From<io::Error> for IError {
    fn from(err: io::Error) -> Self {
        error!("WriteFailed", "failed to write to output ({})", err)
    }
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Var {
    pub idx: usize,
    pub len: u32, // len in bytes
    pub meta: u32,
}

impl Var {
    pub fn upper(self) -> usize {
        self.idx + self.len as usize
    }

    pub fn is_valid(&self) -> bool {
        return self.meta & (1u32 << 31) == 0;
    }

    pub fn invalidate(&mut self) {
        self.meta = self.meta | (1u32 << 31);
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct VarPointerFields {
    _tid: u16,
    _idx: u16,
    _offset: u32,
}

#[derive(Clone, Copy)]
pub union VarPointer {
    fields: VarPointerFields,
    value: u64,
}

impl fmt::Display for VarPointer {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let fields = self.fields();

        return write!(
            formatter,
            "0x{:x}{:0>4x}{:0>8x}",
            u16::from_be(fields._tid),
            u16::from_be(fields._idx),
            u32::from_be(fields._offset)
        );
    }
}

impl fmt::Debug for VarPointer {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let fields = self.fields();

        return write!(
            formatter,
            "0x{:x}{:0>4x}{:0>8x}",
            u16::from_be(fields._tid),
            u16::from_be(fields._idx),
            u32::from_be(fields._offset)
        );
    }
}
impl VarPointer {
    pub const BINARY_BIT: u16 = 1u16 << 15;
    pub const STACK_BIT: u16 = 1u16 << 14;
    pub const RESERVED_BITS: u16 = Self::BINARY_BIT | Self::STACK_BIT;

    #[inline]
    pub fn fields(&self) -> VarPointerFields {
        unsafe { self.fields }
    }

    #[inline]
    pub fn fields_mut(&mut self) -> &mut VarPointerFields {
        unsafe { &mut self.fields }
    }

    pub fn new_stack(idx: u16, offset: u32) -> VarPointer {
        Self {
            fields: VarPointerFields {
                _tid: Self::STACK_BIT.to_be(),
                _idx: idx.to_be(),
                _offset: offset.to_be(),
            },
        }
    }

    pub fn new_heap(idx: u32, offset: u32) -> VarPointer {
        let tid = (idx >> 16) as u16;
        if tid & Self::RESERVED_BITS != 0 {
            panic!("idx is too large");
        }

        Self {
            fields: VarPointerFields {
                _tid: tid.to_be(),
                _idx: (idx as u16).to_be(),
                _offset: offset.to_be(),
            },
        }
    }

    pub fn new_binary(idx: u32, offset: u32) -> VarPointer {
        let tid = (idx >> 16) as u16;
        if tid & Self::RESERVED_BITS != 0 {
            panic!("idx is too large");
        }

        let tid = tid | Self::BINARY_BIT;

        Self {
            fields: VarPointerFields {
                _tid: tid.to_be(),
                _idx: (idx as u16).to_be(),
                _offset: offset.to_be(),
            },
        }
    }

    pub fn is_stack(&self) -> bool {
        return (u16::from_be(self.fields()._tid) & Self::STACK_BIT) != 0;
    }

    pub fn is_binary(&self) -> bool {
        return (u16::from_be(self.fields()._tid) & Self::BINARY_BIT) != 0;
    }

    pub fn is_heap(&self) -> bool {
        return (u16::from_be(self.fields()._tid) & Self::RESERVED_BITS) == 0;
    }

    // returns u16::MAX if not attached to a thread
    pub fn tid(&self) -> u16 {
        if self.is_stack() {
            return u16::from_be(self.fields()._tid) & !Self::RESERVED_BITS;
        }

        return u16::MAX;
    }

    pub fn var_idx(self) -> usize {
        if self.is_stack() {
            return u16::from_be(self.fields()._idx) as usize;
        }

        let top = ((u16::from_be(self.fields()._tid) & !Self::RESERVED_BITS) as u32) << 16;
        return (top | u16::from_be(self.fields()._idx) as u32) as usize;
    }

    pub fn with_offset(self, offset: u32) -> Self {
        let mut ptr = self;
        ptr.fields_mut()._offset = offset.to_be();
        return ptr;
    }

    pub fn offset(self) -> u32 {
        u32::from_be(self.fields()._offset)
    }

    pub fn set_offset(&mut self, offset: u32) {
        self.fields_mut()._offset = offset.to_be();
    }

    pub fn add(self, add: u64) -> Self {
        Self {
            value: (u64::from_be(unsafe { self.value }).wrapping_add(1)).to_be(),
        }
    }

    pub fn sub(self, add: u64) -> Self {
        Self {
            value: (u64::from_be(unsafe { self.value }).wrapping_sub(1)).to_be(),
        }
    }
}

pub fn invalid_ptr(ptr: VarPointer) -> IError {
    if ptr.is_stack() {
        return error!("InvalidPointer", "the stack pointer {} is invalid", ptr);
    } else if ptr.is_heap() {
        return error!("InvalidPointer", "the heap pointer {} is invalid", ptr);
    } else {
        return error!("InvalidPointer", "the binary pointer {} is invalid", ptr);
    }
}

pub fn invalid_offset(var: Var, ptr: VarPointer) -> IError {
    let (start, end) = (ptr.with_offset(0), ptr.with_offset(var.len));
    if ptr.is_stack() {
        return error!(
            "InvalidPointer",
            "the stack pointer {} is invalid; the nearest object is in the range {}..{}",
            ptr,
            start,
            end
        );
    } else if ptr.is_heap() {
        return error!(
            "InvalidPointer",
            "the pointer {} is invalid; the nearest object is in the range {}..{}", ptr, start, end
        );
    } else {
        return error!(
            "InvalidPointer",
            "the binary pointer {} is invalid; the nearest object is in the range {}..{}",
            ptr,
            start,
            end
        );
    }
}

#[derive(Debug, Clone, PartialEq)]
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

    pub fn load_from_ref<'a>(buffer_ref: VarBufferRef<'a>) -> Self {
        let mut buffer = Self::new();
        buffer.data.extend_from_slice(buffer_ref.data);
        buffer.vars.extend_from_slice(buffer_ref.vars);
        return buffer;
    }

    pub fn write_to_ref<'a>(&self, mut frame: Frame<'a>) -> VarBufferRef<'a> {
        let data = frame.add_slice(&self.data);
        let vars = frame.add_slice(&self.vars);
        return VarBufferRef { data, vars };
    }

    pub fn as_ref(&self) -> VarBufferRef {
        return VarBufferRef {
            data: &self.data,
            vars: &self.vars,
        };
    }

    pub fn new_from(data: Vec<u8>, vars: Vec<Var>) -> Self {
        Self { data, vars }
    }

    pub fn get_var_range(&self, ptr: VarPointer, len: u32) -> Result<(usize, usize), IError> {
        if ptr.var_idx() == 0 {
            return Err(invalid_ptr(ptr));
        }

        let var = match self.vars.get(ptr.var_idx() - 1) {
            Some(x) => *x,
            None => return Err(invalid_ptr(ptr)),
        };

        if ptr.offset() + len > var.len {
            return Err(invalid_offset(var, ptr.with_offset(ptr.offset() + len - 1)));
        }

        let start = var.idx + ptr.offset() as usize;
        return Ok((start, start + len as usize));
    }

    pub fn get_full_var_range_mut(&mut self, var: u32) -> &mut [u8] {
        if var == 0 {
            panic!("var was null");
        }

        let var = self.vars[var as usize - 1];
        return &mut self.data[var.idx..(var.idx + var.len as usize)];
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
            return Err(invalid_offset(var, ptr.with_offset(ptr.offset() + len - 1)));
        }

        let begin = var.idx + ptr.offset() as usize;
        let var_slice = &self.data[begin..(begin + len as usize)];
        let mut value = unsafe { mem::MaybeUninit::uninit().assume_init() };
        unsafe { any_as_u8_slice_mut(&mut value).copy_from_slice(var_slice) };
        return Ok(value);
    }

    pub fn add_var(&mut self, len: u32, meta: u32) -> u32 {
        let idx = self.data.len();
        self.vars.push(Var { idx, len, meta });
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
            return Err(invalid_offset(var, ptr.with_offset(ptr.offset() + len - 1)));
        }

        let begin = var.idx + ptr.offset() as usize;
        let to_bytes = &mut self.data[begin..(begin + len as usize)];
        let mut previous_value = unsafe { mem::MaybeUninit::uninit().assume_init() };
        unsafe { any_as_u8_slice_mut(&mut previous_value).copy_from_slice(to_bytes) };
        to_bytes.copy_from_slice(any_as_u8_slice(&t));
        return Ok(previous_value);
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct VarBufferRef<'a> {
    pub data: &'a [u8],
    pub vars: &'a [Var],
}

pub struct VarBufferRefIter<'a> {
    pub data: &'a [u8],
    pub vars: &'a [Var],
    pub var_idx: u32,
}

impl<'a> Iterator for VarBufferRefIter<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<Self::Item> {
        if self.var_idx as usize == self.vars.len() {
            return None;
        } else {
            let var = self.vars[self.var_idx as usize];
            let buf = &self.data[var.idx..(var.idx + var.len as usize)];
            self.var_idx += 1;
            return Some(buf);
        }
    }
}

impl<'a> fmt::Debug for VarBufferRef<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.debug_list()
            .entries(VarBufferRefIter {
                data: self.data,
                vars: self.vars,
                var_idx: 0,
            })
            .finish()
    }
}

impl<'a> Serialize for VarBufferRef<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.vars.len() + 1))?;
        seq.serialize_element::<[u8]>(&[])?;
        let iter = VarBufferRefIter {
            data: self.data,
            vars: self.vars,
            var_idx: 0,
        };

        for buf in iter {
            seq.serialize_element(buf)?;
        }
        seq.end()
    }
}

pub enum WriteEvent {
    StdoutWrite(String),
    StderrWrite(String),
    StdinWrite(String),
    Unwind(u32),
}

impl WriteEvent {
    pub fn to_string(self) -> String {
        match self {
            WriteEvent::StdoutWrite(value) => return value,
            WriteEvent::StderrWrite(value) => return value,
            WriteEvent::StdinWrite(value) => return value,
            WriteEvent::Unwind(len) => return String::new(),
        }
    }
}

pub const EVENT_STDOUT_WRITE: u32 = 1 << 31;
pub const EVENT_STDERR_WRITE: u32 = 1 << 30;
pub const EVENT_STDIN_WRITE: u32 = EVENT_STDOUT_WRITE | EVENT_STDERR_WRITE;
pub const EVENT_RESERVED_BITS: u32 = EVENT_STDIN_WRITE;
pub const EVENT_SIZE: usize = 1024 * 1024;

// bk stands for book keeping; every MAKind stores the last index its responsible
// for in historical_data for fast search during historical data resizes
#[derive(Debug, Clone, Copy)]
pub enum MAKind {
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
        meta: u32,
        var_start: usize,
        var_end_stack_start: usize,
        stack_end: usize,
    },
    AllocStackVar {
        meta: u32,
        len: u32,
        bk: usize,
    },
    AllocHeapVar {
        len: u32,
        bk: usize,
    },
    CallstackPush {
        loc: CodeLoc,
        bk: usize,
    },
    CallstackPop {
        frame: CallFrame,
        bk: usize,
    },
    ErrorCallstackPush {
        loc: CodeLoc,
        bk: usize,
    },
    SetFp {
        prev: u16,
        val: u16,
        bk: usize,
    },
    SetFunc {
        prev: u32,
        val: u32,
        bk: usize,
    },
    Jump {
        prev: u32,
        val: u32,
        bk: usize,
    },
    WriteStdout {
        start: usize,
        end: usize,
        chars: u32,
    },
    WriteStderr {
        start: usize,
        end: usize,
        chars: u32,
    },
    WriteStdin {
        start: usize,
        end: usize,
        chars: u32,
    },
    ConsumeStdin {
        start: usize,
        end: usize,
    },
    ErrorExit {
        short: &'static str,
        start: usize,
        end: usize,
    },
    Exit {
        code: i32,
        bk: usize,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct MemoryAction {
    pub kind: MAKind,
    pub tag: u32,
}

impl MemoryAction {
    pub fn data_start(&self) -> usize {
        match self.kind {
            MAKind::SetValue {
                ptr,
                value_start,
                value_end_overwrite_start,
                overwrite_end,
            } => {
                return value_start;
            }
            MAKind::PopStack {
                value_start,
                value_end,
            } => {
                return value_start;
            }
            MAKind::PushStack {
                value_start,
                value_end,
            } => {
                return value_start;
            }
            MAKind::PopStackVar {
                meta,
                var_start,
                var_end_stack_start,
                stack_end,
            } => {
                return var_start;
            }
            MAKind::AllocStackVar { meta, len, bk } => return bk,
            MAKind::AllocHeapVar { len, bk } => return bk,
            MAKind::CallstackPush { loc, bk } => return bk,
            MAKind::CallstackPop { frame, bk } => return bk,
            MAKind::ErrorCallstackPush { loc, bk } => return bk,
            MAKind::SetFp { prev, val, bk } => return bk,
            MAKind::SetFunc { prev, val, bk } => return bk,
            MAKind::Jump { prev, val, bk } => return bk,
            MAKind::WriteStderr { start, end, chars } => return start,
            MAKind::WriteStdout { start, end, chars } => return start,
            MAKind::WriteStdin { start, end, chars } => return start,
            MAKind::ConsumeStdin { start, end } => return start,
            MAKind::ErrorExit { short, start, end } => return start,
            MAKind::Exit { code, bk } => return bk,
        }
    }
}

#[derive(Debug, Clone, serde::Serialize)]
#[serde(tag = "status", content = "data")]
pub enum RuntimeStatus {
    Running,
    Exited(i32),
    ErrorExited(IError),
}

/// Abstraction for the program's replayable state (i.e. its memory + read-in randomness).
/// Note that the stack grows towards higher memory addresses in this memory implementation.
pub struct Memory {
    pub stack: VarBuffer,
    pub heap: VarBuffer,
    pub binary: VarBuffer,

    pub out_io_events: VecDeque<u32>,
    pub out_io_buf: VecDeque<u8>,
    pub in_io_buf: VecDeque<u8>,

    pub callstack: Vec<CallFrame>,
    pub current_func: u32,
    pub fp: u16,
    pub pc: u32,

    pub status: RuntimeStatus,

    pub historical_data: Vec<u8>,
    pub history: Vec<MemoryAction>,
    pub history_binary_end: usize,
    pub history_index: usize,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            stack: VarBuffer::new(),
            heap: VarBuffer::new(),
            binary: VarBuffer::new(),

            out_io_events: VecDeque::new(),
            out_io_buf: VecDeque::new(),
            in_io_buf: VecDeque::new(),

            callstack: Vec::new(),
            current_func: INIT_SYMS.translate["main"],
            fp: 1,
            pc: 0,

            status: RuntimeStatus::Running,

            historical_data: Vec::new(),
            history: Vec::new(),
            history_binary_end: 0,
            history_index: 0,
        }
    }

    pub fn current_tag(&self) -> u32 {
        if self.history_index == 0 {
            return 0; // We're at the beginning of history, i.e. at the first instruction
        }

        return self.history[self.history_index - 1].tag;
    }

    pub fn new_with_binary(binary: VarBufferRef) -> Self {
        let mut historical_data = Vec::new();
        historical_data.extend_from_slice(binary.data);
        let history_binary_end = historical_data.len();

        Self {
            stack: VarBuffer::new(),
            heap: VarBuffer::new(),
            binary: VarBuffer::load_from_ref(binary),

            out_io_events: VecDeque::new(),
            out_io_buf: VecDeque::new(),
            in_io_buf: VecDeque::new(),

            callstack: Vec::new(),
            current_func: INIT_SYMS.translate["main"],
            fp: 1,
            pc: 0,

            status: RuntimeStatus::Running,

            historical_data,
            history: Vec::new(),
            history_binary_end,
            history_index: 0,
        }
    }

    pub fn events(&mut self) -> impl Iterator<Item = WriteEvent> {
        let mut events = Vec::new();

        for event in self.out_io_events.drain(..) {
            let event_enum = event & EVENT_RESERVED_BITS;
            let event_len = event & !EVENT_RESERVED_BITS;
            if event_enum == 0 {
                events.push(WriteEvent::Unwind(event_len));
                continue;
            }

            let mut buf = Vec::new();
            for _ in 0..event_len {
                buf.push(self.out_io_buf.pop_front().unwrap());
            }

            let string = unsafe { String::from_utf8_unchecked(buf) };
            match event_enum {
                EVENT_STDOUT_WRITE => events.push(WriteEvent::StdoutWrite(string)),
                EVENT_STDERR_WRITE => events.push(WriteEvent::StderrWrite(string)),
                EVENT_STDIN_WRITE => events.push(WriteEvent::StdinWrite(string)),
                _ => unreachable!(),
            }
        }

        return events.into_iter();
    }

    #[inline]
    pub fn check_mutate(&mut self) -> Result<(), IError> {
        match &self.status {
            RuntimeStatus::Exited(_) => {
                return Err(error!(
                "AlreadyExited",
                "tried to change memory when exit had already occurred (This is an error in TCI)"
                ))
            }
            RuntimeStatus::ErrorExited(err) => {
                return Err(error!(
                "AlreadyExited",
                "tried to change memory when error exit had already occurred (This is an error in TCI)"
                ))
            }
            _ => return Ok(()),
        }
    }

    pub fn exit(&mut self, code: i32) -> Result<(), IError> {
        self.check_mutate()?;

        let bk = self.historical_data.len();
        self.push_history(MAKind::Exit { code, bk });
        self.status = RuntimeStatus::Exited(code);
        return Ok(());
    }

    pub fn ret(&mut self) -> Result<(), IError> {
        self.check_mutate()?;

        let bk = self.historical_data.len();

        let frame = self.callstack.pop().unwrap();
        self.push_history(MAKind::CallstackPop { frame, bk });

        while self.stack_length() >= self.fp {
            self.pop_stack_var().unwrap();
        }

        self.push_history(MAKind::SetFunc {
            prev: self.current_func,
            val: frame.name,
            bk,
        });
        self.current_func = frame.name;

        self.push_history(MAKind::SetFp {
            prev: self.fp,
            val: frame.fp,
            bk,
        });
        self.fp = frame.fp;

        self.push_history(MAKind::Jump {
            prev: self.pc,
            val: frame.pc + 1,
            bk,
        });
        self.pc = frame.pc + 1;

        return Ok(());
    }

    pub fn call(&mut self, func: u32, func_name: u32, loc: CodeLoc) -> Result<(), IError> {
        self.check_mutate()?;

        let bk = self.historical_data.len();

        self.callstack
            .push(CallFrame::new(self.current_func, loc, self.fp, self.pc));
        self.push_history(MAKind::CallstackPush { loc, bk });

        self.push_history(MAKind::SetFunc {
            prev: self.current_func,
            val: func_name,
            bk,
        });
        self.current_func = func_name;

        self.push_history(MAKind::SetFp {
            prev: self.fp,
            val: self.stack_length() + 1,
            bk,
        });
        self.fp = self.stack_length() + 1;

        self.push_history(MAKind::Jump {
            prev: self.pc,
            val: func,
            bk,
        });
        self.pc = func;
        return Ok(());
    }

    pub fn error_push_callstack(
        &mut self,
        error: IError,
        files: &FileDbRef,
        loc: CodeLoc,
    ) -> Result<(), IError> {
        self.check_mutate()?;

        let bk = self.historical_data.len();
        self.push_history(MAKind::ErrorCallstackPush { loc, bk });
        self.callstack
            .push(CallFrame::new(self.current_func, loc, self.fp, self.pc));

        let rendered_error = render_err(&error, &self.callstack, files);
        write!(MemoryStderr { memory: self }, "{}", rendered_error).unwrap();

        let start = self.historical_data.len();
        write!(self.historical_data, "{}", error.message).unwrap();
        let end = self.historical_data.len();
        let short = error.short_name;
        self.push_history(MAKind::ErrorExit { short, start, end });
        self.status = RuntimeStatus::ErrorExited(error);
        return Ok(());
    }

    pub fn fp_offset(&self, var: i16) -> u16 {
        if var < 0 {
            // TODO make sure there's no overflow happening here
            let var = (var * -1) as u16;
            self.fp - var
        } else {
            self.fp + var as u16
        }
    }

    pub fn sp_offset(&self, var: i16) -> u16 {
        if var < 0 {
            // TODO make sure there's no overflow happening here
            let var = (var * -1) as u16;
            self.stack_length() + 1 - var
        } else {
            self.stack_length() + 1 + var as u16
        }
    }

    pub fn jump(&mut self, pc: u32) -> Result<(), IError> {
        self.check_mutate()?;

        let bk = self.historical_data.len();

        self.push_history(MAKind::Jump {
            prev: self.pc,
            val: pc,
            bk,
        });
        self.pc = pc;

        return Ok(());
    }

    pub fn increment_pc(&mut self) -> Result<(), IError> {
        self.check_mutate()?;

        let bk = self.historical_data.len();

        self.push_history(MAKind::Jump {
            prev: self.pc,
            val: self.pc + 1,
            bk,
        });
        self.pc += 1;

        return Ok(());
    }

    pub fn push_history(&mut self, kind: MAKind) {
        if self.history.len() != self.history_index {
            let last_history_entry = self.history[self.history_index];

            // these two should always be shrinks
            self.history.resize(self.history_index, last_history_entry);
            self.historical_data
                .resize(last_history_entry.data_start(), 0);
        }

        self.history.push(MemoryAction { kind, tag: self.pc });
        self.history_index += 1;
    }

    pub async fn read_stdin(
        &mut self,
        limit: usize,
        stdin: impl AsyncReadExt + Unpin,
    ) -> Result<Vec<u8>, IError> {
        self.check_mutate()?;

        if self.in_io_buf.len() == 0 {
            smol::io::copy(stdin.take(limit as u64), MemoryStdin { memory: self }).await?;
        }

        let limit = std::cmp::min(limit, self.in_io_buf.len());
        return Ok(self.in_io_buf.drain(..limit).collect());
    }

    /// Returns a pointer to a variable matching criteria set by F, only looking
    /// in the current scope
    pub fn search_stack<F>(&self, mut f: F) -> Option<VarPointer>
    where
        F: FnMut(u32) -> bool,
    {
        for idx in self.fp..(self.stack_length() + 1) {
            if f(self.stack.vars[idx as usize].meta) {
                return Some(VarPointer::new_stack(idx, 0));
            }
        }

        return None;
    }

    #[inline]
    pub fn get_var_slice(&self, ptr: VarPointer) -> Result<&[u8], IError> {
        let buffer = if ptr.is_stack() {
            &self.stack
        } else if ptr.is_heap() {
            &self.heap
        } else {
            &self.binary
        };

        if ptr.var_idx() == 0 {
            return Err(invalid_ptr(ptr));
        }

        let var = match buffer.vars.get(ptr.var_idx() - 1) {
            Some(x) => *x,
            None => return Err(invalid_ptr(ptr)),
        };

        if ptr.offset() >= var.len {
            return Err(invalid_offset(var, ptr));
        }

        return Ok(&buffer.data[(var.idx + ptr.offset() as usize)..(var.idx + var.len as usize)]);
    }

    #[inline]
    pub fn get_var_slice_mut(&mut self, ptr: VarPointer) -> Result<&mut [u8], IError> {
        self.check_mutate()?;

        let buffer = if ptr.is_stack() {
            &mut self.stack
        } else if ptr.is_heap() {
            &mut self.heap
        } else {
            &mut self.binary
        };

        if ptr.var_idx() == 0 {
            return Err(invalid_ptr(ptr));
        }

        let var = match buffer.vars.get(ptr.var_idx() - 1) {
            Some(x) => *x,
            None => return Err(invalid_ptr(ptr)),
        };

        if ptr.offset() >= var.len {
            return Err(invalid_offset(var, ptr));
        }

        return Ok(
            &mut buffer.data[(var.idx + ptr.offset() as usize)..(var.idx + var.len as usize)]
        );
    }

    #[inline]
    pub fn get_slice(&self, ptr: VarPointer, len: u32) -> Result<&[u8], IError> {
        let buffer;
        if ptr.is_stack() {
            buffer = &self.stack;
        } else if ptr.is_heap() {
            buffer = &self.heap;
        } else {
            buffer = &self.binary;
        }

        if ptr.var_idx() == 0 {
            return Err(invalid_ptr(ptr));
        }

        let var = match buffer.vars.get(ptr.var_idx() - 1) {
            Some(x) => *x,
            None => return Err(invalid_ptr(ptr)),
        };

        if ptr.offset() >= var.len {
            return Err(invalid_offset(var, ptr));
        }

        if ptr.offset() + len > var.len {
            return Err(invalid_offset(var, ptr.with_offset(ptr.offset() + len - 1)));
        }

        return Ok(&buffer.data[(var.idx + ptr.offset() as usize)..((ptr.offset() + len) as usize)]);
    }

    #[inline]
    pub fn get_var<T: Copy>(&self, ptr: VarPointer) -> Result<T, IError> {
        if ptr.is_stack() {
            return self.stack.get_var(ptr);
        } else if ptr.is_heap() {
            return self.heap.get_var(ptr);
        } else {
            return self.binary.get_var(ptr);
        }
    }

    #[inline]
    pub fn set<T: Copy>(&mut self, ptr: VarPointer, value: T) -> Result<(), IError> {
        self.check_mutate()?;

        let value_start = self.historical_data.len();
        self.historical_data
            .extend_from_slice(any_as_u8_slice(&value));

        let previous_value;
        if ptr.is_stack() {
            previous_value = self.stack.set(ptr, value)?;
        } else if ptr.is_heap() {
            previous_value = self.heap.set(ptr, value)?;
        } else {
            previous_value = self.binary.set(ptr, value)?;
        }

        let value_end_overwrite_start = self.historical_data.len();
        self.historical_data
            .extend_from_slice(any_as_u8_slice(&previous_value));
        let overwrite_end = self.historical_data.len();
        self.push_history(MAKind::SetValue {
            ptr,
            value_start,
            value_end_overwrite_start,
            overwrite_end,
        });

        return Ok(());
    }

    #[inline]
    pub fn add_stack_var(&mut self, len: u32, meta: u32) -> Result<VarPointer, IError> {
        self.check_mutate()?;

        // TODO check for overflow
        let ptr = VarPointer::new_stack(self.stack.add_var(len, meta) as u16, 0);
        let bk = self.historical_data.len();
        self.push_history(MAKind::AllocStackVar { meta, len, bk });
        return Ok(ptr);
    }

    #[inline]
    pub fn add_heap_var(&mut self, len: u32) -> Result<VarPointer, IError> {
        self.check_mutate()?;

        let ptr = VarPointer::new_heap(self.heap.add_var(len, 0), 0);
        let bk = self.historical_data.len();
        self.push_history(MAKind::AllocHeapVar { len, bk });
        return Ok(ptr);
    }

    pub fn write_bytes(&mut self, ptr: VarPointer, bytes: &[u8]) -> Result<(), IError> {
        self.check_mutate()?;

        let value_start = self.historical_data.len();
        self.historical_data.extend_from_slice(bytes);

        let to_bytes = if ptr.is_stack() {
            let (start, end) = self.stack.get_var_range(ptr, bytes.len() as u32)?;
            &mut self.stack.data[start..end]
        } else if ptr.is_heap() {
            let (start, end) = self.heap.get_var_range(ptr, bytes.len() as u32)?;
            &mut self.heap.data[start..end]
        } else {
            let (start, end) = self.binary.get_var_range(ptr, bytes.len() as u32)?;
            &mut self.binary.data[start..end]
        };

        let value_end_overwrite_start = self.historical_data.len();
        self.historical_data.extend_from_slice(to_bytes);
        let overwrite_end = self.historical_data.len();
        to_bytes.copy_from_slice(bytes);
        self.push_history(MAKind::SetValue {
            ptr,
            value_start,
            value_end_overwrite_start,
            overwrite_end,
        });

        return Ok(());
    }

    #[inline]
    pub fn stack_length(&self) -> u16 {
        return self.stack.vars.len() as u16; // TODO check for overflow
    }

    pub fn pop_stack_var(&mut self) -> Result<Var, IError> {
        self.check_mutate()?;

        let var = self.stack.vars.pop();
        let map_err = || error!("StackIsEmpty", "tried to pop from stack when it is empty");
        let var = var.ok_or_else(map_err)?;

        let var_start = self.historical_data.len();
        let var_end_stack_start = var_start + var.len as usize;
        self.historical_data
            .extend_from_slice(&self.stack.data[var.idx..]);
        let stack_end = self.historical_data.len();

        self.stack.data.resize(var.idx, 0);
        self.push_history(MAKind::PopStackVar {
            meta: var.meta,
            var_start,
            var_end_stack_start,
            stack_end,
        });

        return Ok(var);
    }

    pub fn pop_stack_var_onto_stack(&mut self) -> Result<(), IError> {
        self.check_mutate()?;

        let var = self.stack.vars.pop();
        let map_err = || error!("StackIsEmpty", "tried to pop from stack when it is empty");
        let var = var.ok_or_else(map_err)?;

        let var_start = self.historical_data.len();
        let var_end_stack_start = var_start + var.len as usize;
        self.historical_data
            .extend_from_slice(&self.stack.data[var.idx..]);
        let stack_end = self.historical_data.len();

        self.stack.data.resize(var.idx + var.len as usize, 0);
        self.push_history(MAKind::PopStackVar {
            meta: var.meta,
            var_start,
            var_end_stack_start,
            stack_end,
        });

        self.push_history(MAKind::PushStack {
            value_start: var_start,
            value_end: var_end_stack_start,
        });

        return Ok(());
    }

    #[inline]
    pub fn push_stack<T: Copy>(&mut self, value: T) -> Result<(), IError> {
        self.check_mutate()?;

        let from_bytes = any_as_u8_slice(&value);
        let value_start = self.historical_data.len();
        self.historical_data.extend_from_slice(from_bytes);
        let value_end = self.historical_data.len();

        self.stack.data.extend_from_slice(from_bytes);
        self.push_history(MAKind::PushStack {
            value_start,
            value_end,
        });

        return Ok(());
    }

    pub fn push_stack_bytes(&mut self, from_bytes: &[u8]) -> Result<(), IError> {
        self.check_mutate()?;

        let value_start = self.historical_data.len();
        self.historical_data.extend_from_slice(from_bytes);
        let value_end = self.historical_data.len();

        self.stack.data.extend_from_slice(from_bytes);
        self.push_history(MAKind::PushStack {
            value_start,
            value_end,
        });

        return Ok(());
    }

    pub fn pop_stack_bytes_into(&mut self, ptr: VarPointer, len: u32) -> Result<(), IError> {
        self.check_mutate()?;

        if self.stack.data.len() < len as usize {
            return err!(
                "StackTooShort",
                "tried to pop {} bytes from stack when stack is only {} bytes long",
                len,
                self.stack.data.len(),
            );
        }

        let break_idx = if let Some(var) = self.stack.vars.last() {
            if self.stack.data.len() - var.upper() < len as usize {
                return err!(
                    "StackPopInvalidatesVariable",
                    "popping from the stack would invalidate a variable"
                );
            }
            var.upper()
        } else {
            0
        };

        let (to_bytes, from_bytes) = if ptr.is_stack() {
            let (start, end) = self.stack.get_var_range(ptr, len)?;
            let (stack_vars, stack) = self.stack.data.split_at_mut(break_idx);
            let pop_lower = stack.len() - len as usize;

            (&mut stack_vars[start..end], &stack[pop_lower..])
        } else if ptr.is_heap() {
            let (start, end) = self.heap.get_var_range(ptr, len)?;
            let (stack_vars, stack) = self.stack.data.split_at_mut(break_idx);
            let pop_lower = stack.len() - len as usize;

            (&mut self.heap.data[start..end], &stack[pop_lower..])
        } else {
            let (start, end) = self.binary.get_var_range(ptr, len)?;
            let (stack_vars, stack) = self.stack.data.split_at_mut(break_idx);
            let pop_lower = stack.len() - len as usize;

            (&mut self.binary.data[start..end], &stack[pop_lower..])
        };

        let value_start = self.historical_data.len();
        self.historical_data.extend_from_slice(from_bytes);
        let value_end = self.historical_data.len();
        self.historical_data.extend_from_slice(to_bytes);
        let overwrite_end = self.historical_data.len();
        to_bytes.copy_from_slice(from_bytes);
        self.push_history(MAKind::SetValue {
            ptr,
            value_start,
            value_end_overwrite_start: value_end,
            overwrite_end,
        });

        self.stack
            .data
            .resize(self.stack.data.len() - len as usize, 0);
        self.push_history(MAKind::PopStack {
            value_start,
            value_end,
        });

        return Ok(());
    }

    pub fn push_stack_bytes_from(&mut self, ptr: VarPointer, len: u32) -> Result<(), IError> {
        self.check_mutate()?;

        let break_idx = if let Some(var) = self.stack.vars.last() {
            var.upper()
        } else {
            0
        };

        let (from_bytes, stack) = if ptr.is_stack() {
            let (start, end) = match self.stack.get_var_range(ptr, len) {
                Ok((start, end)) => {
                    let data = &mut self.stack.data;
                    data.resize(data.len() + len as usize, 0);
                    (start, end)
                }
                Err(err) => return Err(err),
            };
            let (stack_vars, stack) = self.stack.data.split_at_mut(break_idx);
            (&stack_vars[start..end], stack)
        } else if ptr.is_heap() {
            let (start, end) = self.heap.get_var_range(ptr, len)?;
            let data = &mut self.stack.data;
            data.resize(data.len() + len as usize, 0);
            let (stack_vars, stack) = self.stack.data.split_at_mut(break_idx);
            (&self.heap.data[start..end], stack)
        } else {
            let (start, end) = self.binary.get_var_range(ptr, len)?;
            let data = &mut self.stack.data;
            data.resize(data.len() + len as usize, 0);
            let (stack_vars, stack) = self.stack.data.split_at_mut(break_idx);
            (&self.binary.data[start..end], stack)
        };

        let pop_lower = stack.len() - len as usize;
        let to_bytes = &mut stack[pop_lower..];

        let value_start = self.historical_data.len();
        self.historical_data.extend_from_slice(from_bytes);
        let value_end = self.historical_data.len();
        to_bytes.copy_from_slice(from_bytes);
        self.push_history(MAKind::PushStack {
            value_start,
            value_end,
        });

        return Ok(());
    }

    pub fn pop_bytes(&mut self, len: u32) -> Result<(), IError> {
        self.check_mutate()?;

        if self.stack.data.len() < len as usize {
            return err!(
                "StackTooShort",
                "tried to pop {} bytes from stack when stack is only {} bytes long",
                len,
                self.stack.data.len(),
            );
        }

        if let Some(var) = self.stack.vars.last() {
            if self.stack.data.len() - var.upper() < len as usize {
                return err!(
                    "StackPopInvalidatesVariable",
                    "popping from the stack would invalidate a variable"
                );
            }
        }

        let upper = self.stack.data.len();
        let lower = upper - len as usize;
        let from_bytes = &self.stack.data[lower..upper];

        let value_start = self.historical_data.len();
        self.historical_data.extend_from_slice(from_bytes);
        let value_end = self.historical_data.len();

        self.stack.data.resize(lower, 0);
        self.push_history(MAKind::PopStack {
            value_start,
            value_end,
        });

        return Ok(());
    }

    pub fn pop_keep_bytes(&mut self, keep: u32, pop: u32) -> Result<(), IError> {
        self.check_mutate()?;

        let len = keep + pop;
        if self.stack.data.len() < len as usize {
            return err!(
                "StackTooShort",
                "tried to pop {} bytes from stack when stack is only {} bytes long",
                len,
                self.stack.data.len(),
            );
        }

        if let Some(var) = self.stack.vars.last() {
            if self.stack.data.len() - var.upper() < len as usize {
                return err!(
                    "StackPopInvalidatesVariable",
                    "popping from the stack would invalidate a variable"
                );
            }
        }

        let keep_start = self.stack.data.len() - keep as usize;
        let pop_start = keep_start - pop as usize;
        let pop_value_start = self.historical_data.len();
        self.historical_data
            .extend_from_slice(&self.stack.data[pop_start..]);
        let pop_value_end = self.historical_data.len();
        self.historical_data
            .extend_from_slice(&self.stack.data[keep_start..]);
        let push_value_end = self.historical_data.len();

        let mutate_slice = &mut self.stack.data[pop_start..];
        for i in 0..(keep as usize) {
            mutate_slice[i] = mutate_slice[i + pop as usize];
        }
        self.stack.data.resize(pop_start + keep as usize, 0);

        self.push_history(MAKind::PopStack {
            value_start: pop_value_start,
            value_end: pop_value_end,
        });
        self.push_history(MAKind::PushStack {
            value_start: pop_value_end,
            value_end: push_value_end,
        });

        return Ok(());
    }

    pub fn dup_top_stack_bytes(&mut self, bytes: u32) -> Result<(), IError> {
        self.check_mutate()?;

        if self.stack.data.len() < bytes as usize {
            return err!(
                "StackTooShort",
                "tried to read {} bytes from stack when stack is only {} bytes long",
                bytes,
                self.stack.data.len(),
            );
        }

        if let Some(var) = self.stack.vars.last() {
            if self.stack.data.len() - var.upper() < bytes as usize {
                return err!(
                    "StackDupReadsVar",
                    "Duplicating stack data would read from a stack variable"
                );
            }
        }

        let dup_start = self.stack.data.len() - bytes as usize;
        let dup_end = self.stack.data.len();
        let value_start = self.historical_data.len();
        self.historical_data
            .extend_from_slice(&self.stack.data[dup_start..]);
        let value_end = self.historical_data.len();

        self.stack
            .data
            .resize(self.stack.data.len() + bytes as usize, 0);
        let (from_bytes, to_bytes) = self.stack.data.split_at_mut(dup_end);
        let from_bytes = &from_bytes[dup_start..];
        to_bytes.copy_from_slice(from_bytes);

        self.push_history(MAKind::PushStack {
            value_start,
            value_end,
        });

        return Ok(());
    }

    pub fn pop_stack<T: Copy>(&mut self) -> Result<T, IError> {
        self.check_mutate()?;

        let len = mem::size_of::<T>();
        if self.stack.data.len() < len {
            return err!(
                "StackTooShort",
                "tried to pop {} bytes from stack when stack is only {} bytes long",
                len,
                self.stack.data.len(),
            );
        }

        if let Some(var) = self.stack.vars.last() {
            if self.stack.data.len() - var.upper() < len {
                return err!(
                    "StackPopInvalidatesVariable",
                    "popping from the stack would invalidate a variable"
                );
            }
        }

        let upper = self.stack.data.len();
        let lower = upper - len;
        let from_bytes = &self.stack.data[lower..upper];

        let value_start = self.historical_data.len();
        self.historical_data.extend_from_slice(from_bytes);
        let value_end = self.historical_data.len();

        let mut out = unsafe { mem::MaybeUninit::uninit().assume_init() };
        unsafe { any_as_u8_slice_mut(&mut out).copy_from_slice(from_bytes) };
        self.stack.data.resize(lower, 0);
        self.push_history(MAKind::PopStack {
            value_start,
            value_end,
        });

        return Ok(out);
    }

    pub fn stdout(&mut self) -> Result<MemoryStdout, IError> {
        self.check_mutate()?;

        Ok(MemoryStdout { memory: self })
    }

    pub fn stderr(&mut self) -> Result<MemoryStderr, IError> {
        self.check_mutate()?;

        Ok(MemoryStderr { memory: self })
    }

    pub fn stdin(&mut self) -> Result<MemoryStdin, IError> {
        self.check_mutate()?;

        Ok(MemoryStdin { memory: self })
    }

    pub fn next(&mut self) -> bool {
        if self.history_index == self.history.len() {
            return false;
        }

        match self.history[self.history_index].kind {
            MAKind::SetValue {
                ptr,
                value_start,
                value_end_overwrite_start: mid,
                overwrite_end,
            } => {
                let value_bytes = &self.historical_data[value_start..mid];
                let buffer = if ptr.is_stack() {
                    &mut self.stack
                } else if ptr.is_heap() {
                    &mut self.heap
                } else {
                    &mut self.binary
                };

                let result = buffer.get_var_range(ptr, value_bytes.len() as u32);
                let (start, end) = result.expect("this should never error");
                buffer.data[start..end].copy_from_slice(value_bytes);
            }
            MAKind::PopStack {
                value_start,
                value_end,
            } => {
                let popped_len = value_end - value_start;
                let data = &mut self.stack.data;
                data.resize(data.len() - popped_len, 0);
            }
            MAKind::PushStack {
                value_start,
                value_end,
            } => {
                let popped_len = value_end - value_start;
                let data = &mut self.stack.data;
                data.extend_from_slice(&self.historical_data[value_start..value_end]);
            }
            MAKind::PopStackVar {
                meta,
                var_start,
                var_end_stack_start,
                stack_end,
            } => {
                let var = self.stack.vars.pop().unwrap();
                self.stack.data.resize(var.idx, 0);
            }
            MAKind::AllocHeapVar { len, bk } => {
                self.heap.add_var(len, 0);
            }
            MAKind::AllocStackVar { meta, len, bk } => {
                self.stack.add_var(len, meta);
            }
            MAKind::CallstackPush { loc, bk } => {
                self.callstack
                    .push(CallFrame::new(self.current_func, loc, self.fp, self.pc));
            }
            MAKind::CallstackPop { frame, bk } => {
                self.callstack.pop().unwrap();
            }
            MAKind::ErrorCallstackPush { loc, bk } => {
                self.callstack
                    .push(CallFrame::new(self.current_func, loc, self.fp, self.pc));
            }
            MAKind::SetFp { prev, val, bk } => {
                self.fp = val;
            }
            MAKind::SetFunc { prev, val, bk } => {
                self.current_func = val;
            }
            MAKind::Jump { prev, val, bk } => {
                self.pc = val;
            }
            MAKind::WriteStdout { start, end, chars } => {
                self.out_io_buf.extend(&self.historical_data[start..end]);
                let block_size = EVENT_STDOUT_WRITE | ((end - start) as u32);
                self.out_io_events.push_back(block_size);
            }
            MAKind::WriteStderr { start, end, chars } => {
                self.out_io_buf.extend(&self.historical_data[start..end]);
                let block_size = EVENT_STDERR_WRITE | ((end - start) as u32);
                self.out_io_events.push_back(block_size);
            }
            MAKind::WriteStdin { start, end, chars } => {
                self.in_io_buf.extend(&self.historical_data[start..end]);
                self.out_io_buf.extend(&self.historical_data[start..end]);
                let block_size = EVENT_STDIN_WRITE | ((end - start) as u32);
                self.out_io_events.push_back(block_size);
            }
            MAKind::ConsumeStdin { start, end } => {
                let block_size = (end - start) as u32;
                for _ in 0..block_size {
                    self.in_io_buf.pop_front().unwrap();
                }
            }
            MAKind::ErrorExit { short, start, end } => {
                #[rustfmt::skip]
                let bytes = self.historical_data[start..end].iter().map(|i| *i).collect();
                let string = unsafe { String::from_utf8_unchecked(bytes) };
                self.status = RuntimeStatus::ErrorExited(IError::new(short, string));
            }
            MAKind::Exit { code, bk } => {
                self.status = RuntimeStatus::Exited(code);
            }
        }

        self.history_index += 1;
        return true;
    }

    pub fn prev(&mut self) -> bool {
        if self.history_index == 0 {
            return false;
        }

        match self.history[self.history_index - 1].kind {
            MAKind::SetValue {
                ptr,
                value_start,
                value_end_overwrite_start: mid,
                overwrite_end,
            } => {
                let value_bytes = &self.historical_data[mid..overwrite_end];
                let buffer = if ptr.is_stack() {
                    &mut self.stack
                } else if ptr.is_heap() {
                    &mut self.heap
                } else {
                    &mut self.binary
                };

                let result = buffer.get_var_range(ptr, value_bytes.len() as u32);
                let (start, end) = result.expect("this should never error");
                buffer.data[start..end].copy_from_slice(value_bytes);
            }
            MAKind::PopStack {
                value_start,
                value_end,
            } => {
                let popped_len = value_end - value_start;
                let data = &mut self.stack.data;
                data.extend_from_slice(&self.historical_data[value_start..value_end]);
            }
            MAKind::PushStack {
                value_start,
                value_end,
            } => {
                let popped_len = value_end - value_start;
                let data = &mut self.stack.data;
                data.resize(data.len() - popped_len, 0);
            }
            MAKind::PopStackVar {
                meta,
                var_start,
                var_end_stack_start,
                stack_end,
            } => {
                let data = &mut self.stack.data;
                let idx = data.len();
                let len = (var_end_stack_start - var_start) as u32;
                data.extend_from_slice(&self.historical_data[var_start..stack_end]);
                let vars = &mut self.stack.vars;
                vars.push(Var { idx, len, meta });
            }
            MAKind::AllocHeapVar { len, bk } => {
                let var = self.heap.vars.pop().unwrap();
                self.heap.data.resize(var.idx, 0);
            }
            MAKind::AllocStackVar { meta, len, bk } => {
                let var = self.stack.vars.pop().unwrap();
                self.stack.data.resize(var.idx, 0);
            }
            MAKind::CallstackPush { loc, bk } => {
                self.callstack.pop().unwrap();
            }
            MAKind::CallstackPop { frame, bk } => {
                self.callstack.push(frame);
            }
            MAKind::ErrorCallstackPush { loc, bk } => {
                self.callstack.pop().unwrap();
            }
            MAKind::SetFp { prev, val, bk } => {
                self.fp = prev;
            }
            MAKind::SetFunc { prev, val, bk } => {
                self.current_func = prev;
            }
            MAKind::Jump { prev, val, bk } => {
                self.pc = prev;
            }
            MAKind::WriteStdout { start, end, chars } => {
                self.out_io_events.push_back(chars);
            }
            MAKind::WriteStderr { start, end, chars } => {
                self.out_io_events.push_back(chars);
            }
            MAKind::WriteStdin { start, end, chars } => {
                let block_size = (end - start) as u32;
                for _ in 0..block_size {
                    self.in_io_buf.pop_back().unwrap();
                }
                self.out_io_events.push_back(chars);
            }
            MAKind::ConsumeStdin { start, end } => {
                let block_size = (end - start) as u32;
                for byte in self.historical_data[start..end].iter().rev() {
                    self.in_io_buf.push_front(*byte);
                }
            }
            MAKind::ErrorExit { short, start, end } => {
                self.status = RuntimeStatus::Running;
            }
            MAKind::Exit { code, bk } => {
                self.status = RuntimeStatus::Running;
            }
        }

        self.history_index -= 1;
        return true;
    }

    pub fn snapshot(&self) -> MemorySnapshot {
        MemorySnapshot {
            stack: self.stack.clone(),
            heap: self.heap.clone(),
            binary: self.binary.clone(),
        }
    }
}

pub struct MemoryStdout<'a> {
    pub memory: &'a mut Memory,
}

pub struct MemoryStderr<'a> {
    pub memory: &'a mut Memory,
}

pub struct MemoryStdin<'a> {
    pub memory: &'a mut Memory,
}

impl<'a> Write for MemoryStdin<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let map_err = |err| io::Error::new(io::ErrorKind::InvalidInput, err);
        let buf = core::str::from_utf8(buf).map_err(map_err)?;
        self.memory.out_io_buf.extend(buf.as_bytes());

        let mut force_flush = |start: usize, end: usize, chars: u32| {
            let hist_start = self.memory.historical_data.len();
            let memory = &mut self.memory;
            #[rustfmt::skip]
            memory.historical_data.extend(&buf.as_bytes()[start..end]);
            let (start, end) = (hist_start, memory.historical_data.len());
            memory.push_history(MAKind::WriteStdin { start, end, chars });
        };

        let mut try_flush = |start: usize, end: usize, chars: u32| -> bool {
            if end - start < EVENT_SIZE {
                return false;
            }

            force_flush(start, end, chars);
            return true;
        };

        let (mut start, mut end, mut chars) = (0, 0, 0);
        for c in buf.chars() {
            let block_size = end - start;
            if try_flush(start, end, chars) {
                start = end;
                chars = 0;
                continue;
            }

            end += c.len_utf8();
            chars += 1;
        }

        force_flush(start, end, chars);

        return Ok(buf.len());
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

use core::pin::Pin;
use core::task::{Context, Poll};
use smol::io::AsyncWrite;
impl<'a> AsyncWrite for MemoryStdin<'a> {
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, io::Error>> {
        return Poll::Ready(self.write(buf));
    }
    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), io::Error>> {
        return Poll::Ready(Ok(()));
    }
    fn poll_close(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), io::Error>> {
        return Poll::Ready(Ok(()));
    }
}

impl<'a> io::Write for MemoryStderr<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let map_err = |err| io::Error::new(io::ErrorKind::InvalidInput, err);
        let buf = core::str::from_utf8(buf).map_err(map_err)?;
        self.memory.out_io_buf.extend(buf.as_bytes());

        let mut force_flush = |start: usize, end: usize, chars: u32| {
            let block_size = end - start;
            let hist_start = self.memory.historical_data.len();
            let memory = &mut self.memory;
            #[rustfmt::skip]
            memory.out_io_events.push_back(EVENT_STDERR_WRITE | (block_size as u32));
            memory.historical_data.extend(&buf.as_bytes()[start..end]);
            let (start, end) = (hist_start, memory.historical_data.len());
            memory.push_history(MAKind::WriteStderr { start, end, chars });
        };

        let mut try_flush = |start: usize, end: usize, chars: u32| -> bool {
            if end - start < EVENT_SIZE {
                return false;
            }

            force_flush(start, end, chars);
            return true;
        };

        let (mut start, mut end, mut chars) = (0, 0, 0);
        for c in buf.chars() {
            let block_size = end - start;
            if try_flush(start, end, chars) {
                start = end;
                chars = 0;
                continue;
            }

            end += c.len_utf8();
            chars += 1;
        }

        force_flush(start, end, chars);

        return Ok(buf.len());
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl<'a> io::Write for MemoryStdout<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let map_err = |err| io::Error::new(io::ErrorKind::InvalidInput, err);
        let buf = core::str::from_utf8(buf).map_err(map_err)?;
        self.memory.out_io_buf.extend(buf.as_bytes());

        let mut force_flush = |start: usize, end: usize, chars: u32| {
            let block_size = end - start;
            let hist_start = self.memory.historical_data.len();
            let memory = &mut self.memory;
            #[rustfmt::skip]
            memory.out_io_events.push_back(EVENT_STDOUT_WRITE | (block_size as u32));
            memory.historical_data.extend(&buf.as_bytes()[start..end]);
            let (start, end) = (hist_start, memory.historical_data.len());
            memory.push_history(MAKind::WriteStdout { start, end, chars });
        };

        let mut try_flush = |start: usize, end: usize, chars: u32| -> bool {
            if end - start < EVENT_SIZE {
                return false;
            }

            force_flush(start, end, chars);
            return true;
        };

        let (mut start, mut end, mut chars) = (0, 0, 0);
        for c in buf.chars() {
            let block_size = end - start;
            if try_flush(start, end, chars) {
                start = end;
                chars = 0;
                continue;
            }

            end += c.len_utf8();
            chars += 1;
        }

        force_flush(start, end, chars);

        return Ok(buf.len());
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct MemorySnapshot {
    pub stack: VarBuffer,
    pub heap: VarBuffer,
    pub binary: VarBuffer,
}

impl Serialize for MemorySnapshot {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("MemorySnapshot", 3)?;
        state.serialize_field("stack", &self.stack.as_ref())?;
        state.serialize_field("heap", &self.heap.as_ref())?;
        state.serialize_field("binary", &self.binary.as_ref())?;
        state.end()
    }
}

#[test]
fn test_memory_walker() {
    let mut memory = Memory::new();
    memory.add_stack_var(12, 0).unwrap();
    memory.push_stack(12u64.to_be()).unwrap();
    memory.push_stack(4u32.to_be()).unwrap();
    memory
        .pop_stack_bytes_into(VarPointer::new_stack(1, 0), 12)
        .expect("should not fail");

    println!("history: {:?}", memory.history);

    let stack_expected = VarBuffer::new_from(
        vec![0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 4],
        vec![Var {
            idx: 0,
            len: 12,
            meta: 0,
        }],
    );

    assert_eq!(memory.stack, stack_expected);

    memory.prev();
    memory.prev();

    let stack_expected_2 = VarBuffer::new_from(
        vec![
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 4,
        ],
        vec![Var {
            idx: 0,
            len: 12,
            meta: 0,
        }],
    );

    assert_eq!(memory.stack, stack_expected_2);
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

impl RuntimeIO for &mut InMemoryIO {
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

pub struct DefaultIO {
    pub out: Stdout,
    pub log: StringWriter,
    pub err: Stderr,
}

pub struct TestIO {
    pub writer: RecordingWriter<Stderr>,
}

impl DefaultIO {
    pub fn new() -> Self {
        Self {
            out: stdout(),
            log: StringWriter::new(),
            err: stderr(),
        }
    }
}

impl RuntimeIO for DefaultIO {
    type Out = Stdout;
    type Log = StringWriter;
    type Err = Stderr;

    fn out(&mut self) -> &mut Stdout {
        return &mut self.out;
    }
    fn log(&mut self) -> &mut StringWriter {
        return &mut self.log;
    }
    fn err(&mut self) -> &mut Stderr {
        return &mut self.err;
    }
}

impl RuntimeIO for &mut DefaultIO {
    type Out = Stdout;
    type Log = StringWriter;
    type Err = Stderr;

    fn out(&mut self) -> &mut Stdout {
        return &mut self.out;
    }
    fn log(&mut self) -> &mut StringWriter {
        return &mut self.log;
    }
    fn err(&mut self) -> &mut Stderr {
        return &mut self.err;
    }
}

impl TestIO {
    pub fn new() -> Self {
        Self {
            writer: RecordingWriter::new(stderr()),
        }
    }
}

impl RuntimeIO for &mut TestIO {
    type Out = RecordingWriter<Stderr>;
    type Log = RecordingWriter<Stderr>;
    type Err = RecordingWriter<Stderr>;

    fn out(&mut self) -> &mut Self::Out {
        return &mut self.writer;
    }
    fn log(&mut self) -> &mut Self::Log {
        return &mut self.writer;
    }
    fn err(&mut self) -> &mut Self::Err {
        return &mut self.writer;
    }
}

impl RuntimeIO for TestIO {
    type Out = RecordingWriter<Stderr>;
    type Log = RecordingWriter<Stderr>;
    type Err = RecordingWriter<Stderr>;

    fn out(&mut self) -> &mut Self::Out {
        return &mut self.writer;
    }
    fn log(&mut self) -> &mut Self::Log {
        return &mut self.writer;
    }
    fn err(&mut self) -> &mut Self::Err {
        return &mut self.writer;
    }
}

impl RuntimeIO for Void {
    type Out = Void;
    type Log = Void;
    type Err = Void;

    fn out(&mut self) -> &mut Void {
        return self;
    }
    fn log(&mut self) -> &mut Void {
        return self;
    }
    fn err(&mut self) -> &mut Void {
        return self;
    }
}
