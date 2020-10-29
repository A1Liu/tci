use crate::buckets::*;
use crate::util::*;
use core::{fmt, mem, str};
use serde::Serialize;
use std::io;
use std::io::{stderr, stdout, Stderr, Stdout, Write};

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

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
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

    pub fn is_valid(&self) -> bool {
        return self.meta & (1u32 << 31) == 0;
    }

    pub fn invalidate(&mut self) {
        self.meta = self.meta | (1u32 << 31);
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct VarPointer {
    _tid: u16,
    _idx: u16,
    _offset: u32,
}

impl fmt::Display for VarPointer {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        return write!(
            formatter,
            "0x{:x}{:0>4x}{:0>8x}",
            u16::from_be(self._tid),
            u16::from_be(self._idx),
            u32::from_be(self._offset)
        );
    }
}

impl fmt::Debug for VarPointer {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        return write!(
            formatter,
            "0x{:x}{:0>4x}{:0>8x}",
            u16::from_be(self._tid),
            u16::from_be(self._idx),
            u32::from_be(self._offset)
        );
    }
}
impl VarPointer {
    pub const HEAP_BIT: u16 = 1u16 << 15;
    pub const STACK_BIT: u16 = 1u16 << 14;
    pub const RESERVED_BITS: u16 = Self::HEAP_BIT | Self::STACK_BIT;

    pub fn new_stack(idx: u16, offset: u32) -> VarPointer {
        Self {
            _tid: Self::STACK_BIT.to_be(),
            _idx: idx.to_be(),
            _offset: offset.to_be(),
        }
    }

    pub fn new_heap(idx: u32, offset: u32) -> VarPointer {
        let tid = (idx >> 16) as u16;
        if tid & Self::RESERVED_BITS != 0 {
            panic!("idx is too large");
        }

        let tid = Self::HEAP_BIT | tid;
        let idx = idx as u16;

        Self {
            _tid: tid.to_be(),
            _idx: idx.to_be(),
            _offset: offset.to_be(),
        }
    }

    pub fn new_binary(idx: u32, offset: u32) -> VarPointer {
        let tid = (idx >> 16) as u16;
        if tid & Self::RESERVED_BITS != 0 {
            panic!("idx is too large");
        }

        Self {
            _tid: tid.to_be(),
            _idx: (idx as u16).to_be(),
            _offset: offset.to_be(),
        }
    }

    pub fn is_stack(&self) -> bool {
        return (u16::from_be(self._tid) & Self::STACK_BIT) != 0;
    }

    pub fn is_heap(&self) -> bool {
        return (u16::from_be(self._tid) & Self::HEAP_BIT) != 0;
    }

    pub fn is_binary(&self) -> bool {
        return (u16::from_be(self._tid) & Self::RESERVED_BITS) == 0;
    }

    // returns u16::MAX if not attached to a thread
    pub fn tid(&self) -> u16 {
        if self.is_stack() {
            return u16::from_be(self._tid) & !Self::RESERVED_BITS;
        }

        return u16::MAX;
    }

    pub fn var_idx(self) -> usize {
        if self.is_stack() {
            return u16::from_be(self._idx) as usize;
        }

        let top = ((u16::from_be(self._tid) & !Self::RESERVED_BITS) as u32) << 16;
        return (top | u16::from_be(self._idx) as u32) as usize;
    }

    pub fn with_offset(self, offset: u32) -> Self {
        let mut ptr = self;
        ptr._offset = offset.to_be();
        return ptr;
    }

    pub fn offset(self) -> u32 {
        u32::from_be(self._offset)
    }

    pub fn set_offset(&mut self, offset: u32) {
        self._offset = offset.to_be();
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
            "the heap pointer {} is invalid; the nearest object is in the range {}..{}",
            ptr,
            start,
            end
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

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub struct VarBufferRef<'a> {
    pub data: &'a [u8],
    pub vars: &'a [Var],
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
        var_start: usize,
        var_end_stack_start: usize,
        stack_end: usize,
    },
    AllocStackVar {
        len: u32,
        book_keeping: usize,
    },
    AllocHeapVar {
        len: u32,
        book_keeping: usize,
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
                var_start,
                var_end_stack_start,
                stack_end,
            } => {
                return var_start;
            }
            MAKind::AllocStackVar { len, book_keeping } => return book_keeping,
            MAKind::AllocHeapVar { len, book_keeping } => return book_keeping,
        }
    }
}

pub struct Memory {
    pub stack: VarBuffer,
    pub heap: VarBuffer,
    pub binary: VarBuffer,

    pub callstack: Vec<CallFrame>,
    pub fp: u16,
    pub pc: u32,

    pub historical_data: Vec<u8>,
    pub history: Vec<MemoryAction>,
    pub history_binary_end: usize,
    pub history_index: usize,
}

impl Memory {
    pub fn new(pc: u32) -> Self {
        Self {
            stack: VarBuffer::new(),
            heap: VarBuffer::new(),
            binary: VarBuffer::new(),

            callstack: Vec::new(),
            fp: 0,
            pc,

            historical_data: Vec::new(),
            history: Vec::new(),
            history_binary_end: 0,
            history_index: 0,
        }
    }

    pub fn current_tag(&self) -> u32 {
        return self.history[self.history_index - 1].tag;
    }

    pub fn new_with_binary(pc: u32, binary: VarBufferRef) -> Self {
        let mut historical_data = Vec::new();
        historical_data.extend_from_slice(binary.data);
        let history_binary_end = historical_data.len();

        Self {
            stack: VarBuffer::new(),
            heap: VarBuffer::new(),
            binary: VarBuffer::load_from_ref(binary),

            callstack: Vec::new(),
            fp: 0,
            pc,

            historical_data,
            history: Vec::new(),
            history_binary_end,
            history_index: 0,
        }
    }

    pub fn push_callstack(&mut self, callframe: CallFrame) {
        self.callstack.push(callframe);
    }

    pub fn pop_callstack(&mut self) -> Option<CallFrame> {
        return self.callstack.pop();
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

    pub fn set_fp(&mut self, fp: u16) {
        self.fp = fp;
    }

    pub fn jump(&mut self, pc: u32) {
        self.pc = pc;
    }

    pub fn increment_pc(&mut self) {
        self.pc += 1;
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
    pub fn add_stack_var(&mut self, len: u32) -> VarPointer {
        // TODO check for overflow
        let ptr = VarPointer::new_stack(self.stack.add_var(len) as u16, 0);
        let book_keeping = self.historical_data.len();
        self.push_history(MAKind::AllocStackVar { len, book_keeping });
        return ptr;
    }

    #[inline]
    pub fn add_heap_var(&mut self, len: u32) -> VarPointer {
        let ptr = VarPointer::new_heap(self.heap.add_var(len), 0);
        let book_keeping = self.historical_data.len();
        self.push_history(MAKind::AllocHeapVar { len, book_keeping });
        return ptr;
    }

    pub fn write_bytes(&mut self, ptr: VarPointer, bytes: &[u8]) -> Result<(), IError> {
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
            var_start,
            var_end_stack_start,
            stack_end,
        });

        return Ok(var);
    }

    pub fn pop_stack_var_onto_stack(&mut self) -> Result<(), IError> {
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
    pub fn push_stack<T: Copy>(&mut self, value: T) {
        let from_bytes = any_as_u8_slice(&value);
        let value_start = self.historical_data.len();
        self.historical_data.extend_from_slice(from_bytes);
        let value_end = self.historical_data.len();

        self.stack.data.extend_from_slice(from_bytes);
        self.push_history(MAKind::PushStack {
            value_start,
            value_end,
        });
    }

    pub fn push_stack_bytes(&mut self, from_bytes: &[u8]) {
        let value_start = self.historical_data.len();
        self.historical_data.extend_from_slice(from_bytes);
        let value_end = self.historical_data.len();

        self.stack.data.extend_from_slice(from_bytes);
        self.push_history(MAKind::PushStack {
            value_start,
            value_end,
        });
    }

    pub fn pop_stack_bytes_into(&mut self, ptr: VarPointer, len: u32) -> Result<(), IError> {
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
        let break_idx = if let Some(var) = self.stack.vars.last() {
            var.upper()
        } else {
            0
        };

        let data = &mut self.stack.data;
        data.resize(data.len() + len as usize, 0);

        let (from_bytes, stack) = if ptr.is_stack() {
            let (start, end) = self.stack.get_var_range(ptr, len)?;
            let (stack_vars, stack) = self.stack.data.split_at_mut(break_idx);
            (&stack_vars[start..end], stack)
        } else if ptr.is_heap() {
            let (start, end) = self.heap.get_var_range(ptr, len)?;
            let (stack_vars, stack) = self.stack.data.split_at_mut(break_idx);
            (&self.heap.data[start..end], stack)
        } else {
            let (start, end) = self.binary.get_var_range(ptr, len)?;
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
                var_start,
                var_end_stack_start,
                stack_end,
            } => {
                let var = self.stack.vars.pop().unwrap();
                self.stack.data.resize(var.idx, 0);
            }
            MAKind::AllocHeapVar { len, book_keeping } => {
                self.heap.add_var(len);
            }
            MAKind::AllocStackVar { len, book_keeping } => {
                self.stack.add_var(len);
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
                var_start,
                var_end_stack_start,
                stack_end,
            } => {
                let data = &mut self.stack.data;
                let idx = data.len();
                let len = (var_end_stack_start - var_start) as u32;
                data.extend_from_slice(&self.historical_data[var_start..stack_end]);
                let vars = &mut self.stack.vars;
                vars.push(Var { idx, len, meta: 0 });
            }
            MAKind::AllocHeapVar { len, book_keeping } => {
                let var = self.heap.vars.pop().unwrap();
                self.heap.data.resize(var.idx, 0);
            }
            MAKind::AllocStackVar { len, book_keeping } => {
                let var = self.stack.vars.pop().unwrap();
                self.stack.data.resize(var.idx, 0);
            }
        }

        self.history_index -= 1;
        return true;
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MemorySnapshot<'a> {
    pub stack_data: &'a [u8],
    pub stack_vars: &'a [Var],
    pub heap_data: &'a [u8],
    pub heap_vars: &'a [Var],
    pub binary_data: &'a [u8],
    pub binary_vars: &'a [Var],
}

#[derive(Debug, Clone)]
struct MockMemory {
    stack: VarBuffer,
    heap: VarBuffer,
    binary: VarBuffer,
}

impl MockMemory {
    pub fn new() -> Self {
        Self {
            stack: VarBuffer::new(),
            heap: VarBuffer::new(),
            binary: VarBuffer::new(),
        }
    }

    pub fn new_from(stack: VarBuffer, heap: VarBuffer, binary: VarBuffer) -> Self {
        Self {
            stack,
            heap,
            binary,
        }
    }

    pub fn var_buffer(&mut self, ptr: VarPointer) -> &mut VarBuffer {
        if ptr.is_stack() {
            &mut self.stack
        } else if ptr.is_heap() {
            &mut self.heap
        } else {
            &mut self.binary
        }
    }

    pub fn snapshot(&self) -> MemorySnapshot {
        MemorySnapshot {
            stack_data: &self.stack.data,
            stack_vars: &self.stack.vars,
            heap_data: &self.heap.data,
            heap_vars: &self.heap.vars,
            binary_data: &self.binary.data,
            binary_vars: &self.binary.vars,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MemorySnapshotWalker<'a> {
    memory: MockMemory,
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
            match self.history[self.index - 1].kind {
                MAKind::SetValue {
                    ptr,
                    value_start,
                    value_end_overwrite_start,
                    overwrite_end,
                } => {
                    let value_bytes = &self.historical_data[value_start..value_end_overwrite_start];
                    let buffer = self.memory.var_buffer(ptr);
                    let result = buffer.get_var_range(ptr, value_bytes.len() as u32);
                    let (start, end) = result.expect("this should never error");
                    buffer.data[start..end].copy_from_slice(value_bytes);
                }
                MAKind::PopStack {
                    value_start,
                    value_end,
                } => {
                    let popped_len = value_end - value_start;
                    let data = &mut self.memory.stack.data;
                    data.resize(data.len() - popped_len, 0);
                }
                MAKind::PushStack {
                    value_start,
                    value_end,
                } => {
                    let popped_len = value_end - value_start;
                    let data = &mut self.memory.stack.data;
                    data.extend_from_slice(&self.historical_data[value_start..value_end]);
                }
                MAKind::PopStackVar {
                    var_start,
                    var_end_stack_start,
                    stack_end,
                } => {
                    let var = self.memory.stack.vars.pop().unwrap();
                    self.memory.stack.data.resize(var.idx, 0);
                }
                MAKind::AllocHeapVar { len, book_keeping } => {
                    self.memory.heap.add_var(len);
                }
                MAKind::AllocStackVar { len, book_keeping } => {
                    self.memory.stack.add_var(len);
                }
            }
        }
        self.index += 1;

        Some(self.memory.snapshot())
    }

    pub fn prev(&mut self) -> Option<MemorySnapshot> {
        if self.index == 0 {
            return None;
        }

        if self.index <= self.history.len() {
            match self.history[self.index - 1].kind {
                MAKind::SetValue {
                    ptr,
                    value_start,
                    value_end_overwrite_start,
                    overwrite_end,
                } => {
                    let value_bytes =
                        &self.historical_data[value_end_overwrite_start..overwrite_end];
                    let buffer = self.memory.var_buffer(ptr);
                    let result = buffer.get_var_range(ptr, value_bytes.len() as u32);
                    let (start, end) = result.expect("this should never error");
                    buffer.data[start..end].copy_from_slice(value_bytes);
                }
                MAKind::PopStack {
                    value_start,
                    value_end,
                } => {
                    let popped_len = value_end - value_start;
                    let data = &mut self.memory.stack.data;
                    data.extend_from_slice(&self.historical_data[value_start..value_end]);
                }
                MAKind::PushStack {
                    value_start,
                    value_end,
                } => {
                    let popped_len = value_end - value_start;
                    let data = &mut self.memory.stack.data;
                    data.resize(data.len() - popped_len, 0);
                }
                MAKind::PopStackVar {
                    var_start,
                    var_end_stack_start,
                    stack_end,
                } => {
                    let data = &mut self.memory.stack.data;
                    let idx = data.len();
                    let len = (var_end_stack_start - var_start) as u32;
                    data.extend_from_slice(&self.historical_data[var_start..stack_end]);
                    let vars = &mut self.memory.stack.vars;
                    vars.push(Var { idx, len, meta: 0 });
                }
                MAKind::AllocHeapVar { len, book_keeping } => {
                    let var = self.memory.heap.vars.pop().unwrap();
                    self.memory.heap.data.resize(var.idx, 0);
                }
                MAKind::AllocStackVar { len, book_keeping } => {
                    let var = self.memory.stack.vars.pop().unwrap();
                    self.memory.stack.data.resize(var.idx, 0);
                }
            }
        }

        self.index -= 1;

        Some(self.memory.snapshot())
    }
}

impl Memory {
    pub fn forwards_walker(&self) -> MemorySnapshotWalker {
        let memory = MockMemory::new_from(
            VarBuffer::new(),
            VarBuffer::new(),
            VarBuffer::new_from(
                self.historical_data[..self.history_binary_end]
                    .iter()
                    .map(|u| *u)
                    .collect(),
                self.binary.vars.clone(),
            ),
        );

        MemorySnapshotWalker {
            memory,
            historical_data: &self.historical_data,
            history: &self.history,
            index: 0,
        }
    }

    pub fn backwards_walker(&self) -> MemorySnapshotWalker {
        MemorySnapshotWalker {
            memory: MockMemory::new_from(
                self.stack.clone(),
                self.heap.clone(),
                self.binary.clone(),
            ),
            historical_data: &self.historical_data,
            history: &self.history,
            index: self.history.len() + 1,
        }
    }
}

#[test]
fn test_memory_walker() {
    let mut memory = Memory::new(0);
    memory.add_stack_var(12);
    memory.push_stack(12u64.to_be());
    memory.push_stack(4u32.to_be());
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

    memory.next();
    memory.next();

    assert_eq!(memory.stack, stack_expected);
}

#[test]
fn test_walker() {
    let mut memory = Memory::new(0);
    memory.add_stack_var(12);
    memory.push_stack(12u64.to_be());
    memory.push_stack(4u32.to_be());
    memory
        .pop_stack_bytes_into(VarPointer::new_stack(1, 0), 12)
        .expect("should not fail");

    println!("history: {:?}", memory.history);

    let mut walker = memory.backwards_walker();

    let expected = MockMemory::new_from(
        VarBuffer::new_from(
            vec![0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 4],
            vec![Var {
                idx: 0,
                len: 12,
                meta: 0,
            }],
        ),
        VarBuffer::new(),
        VarBuffer::new(),
    );
    assert_eq!(walker.prev().unwrap(), expected.snapshot());

    let expected = MockMemory::new_from(
        VarBuffer::new_from(
            vec![
                0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 4,
            ],
            vec![Var {
                idx: 0,
                len: 12,
                meta: 0,
            }],
        ),
        VarBuffer::new(),
        VarBuffer::new(),
    );
    assert_eq!(walker.prev().unwrap(), expected.snapshot());

    let expected = MockMemory::new_from(
        VarBuffer::new_from(
            vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 4,
            ],
            vec![Var {
                idx: 0,
                len: 12,
                meta: 0,
            }],
        ),
        VarBuffer::new(),
        VarBuffer::new(),
    );
    assert_eq!(walker.prev().unwrap(), expected.snapshot());
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

pub struct DebugSW(pub StringWriter);

impl Write for DebugSW {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        // let len = stdout().write(buf)?;
        // return self.0.write(&buf[0..len]);
        return self.0.write(buf);
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

pub struct DebugIO {
    pub out: DebugSW,
    pub log: DebugSW,
    pub err: DebugSW,
}

impl DebugIO {
    pub fn new() -> Self {
        Self {
            out: DebugSW(StringWriter::new()),
            log: DebugSW(StringWriter::new()),
            err: DebugSW(StringWriter::new()),
        }
    }
}

impl RuntimeIO for &mut DebugIO {
    type Out = DebugSW;
    type Log = DebugSW;
    type Err = DebugSW;

    fn out(&mut self) -> &mut Self::Out {
        return &mut self.out;
    }
    fn err(&mut self) -> &mut Self::Log {
        return &mut self.err;
    }
    fn log(&mut self) -> &mut Self::Err {
        return &mut self.log;
    }
}

impl RuntimeIO for DebugIO {
    type Out = DebugSW;
    type Log = DebugSW;
    type Err = DebugSW;

    fn out(&mut self) -> &mut Self::Out {
        return &mut self.out;
    }
    fn err(&mut self) -> &mut Self::Log {
        return &mut self.err;
    }
    fn log(&mut self) -> &mut Self::Err {
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
