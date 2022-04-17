use super::error::*;
use super::types::*;
use crate::util::*;
use core::mem;

#[derive(Debug)]
pub struct AllocInfo {
    pub alloc_loc: CodeLoc,
    pub free_loc: CodeLoc,
    pub len: n32,
}

impl AllocInfo {
    pub fn new(alloc_loc: CodeLoc) -> Self {
        Self {
            alloc_loc,
            free_loc: NO_FILE,
            len: n32::NULL,
        }
    }
}

#[derive(Debug)]
pub struct Memory {
    shared_data: Pod<u8>,
    ranges: Pod<AllocTracker>,
    freed: usize,
    binary_var_len: u32,
    binary_data_len: u32,

    // Per thread
    pub expr_stack: Vec<u8>,
    stack_data: Vec<u8>,
    pub stack: Vec<Var<()>>,
    pub callstack: Vec<CallFrame>,
    pub current_func: LinkName,
    pub fp: u16,
    pub pc: VarPointer,
    pub loc: CodeLoc,
}

impl Memory {
    pub fn new(binary: &BinaryData) -> Self {
        let binary_var_len = binary.vars.len() as u32;
        let binary_data_len = binary.data.len() as u32;

        Self {
            shared_data: binary.data.clone(),
            ranges: binary.vars.clone(),
            freed: 0,
            binary_var_len,
            binary_data_len,

            expr_stack: Vec::new(),
            stack_data: Vec::new(),
            stack: Vec::new(),

            callstack: Vec::new(),
            current_func: LinkName::new(!0),
            loc: NO_FILE,
            fp: 1,
            pc: VarPointer::new(1, 0),
        }
    }

    pub fn ret(&mut self) -> Result<(), IError> {
        let or_else = || ierror!("InvalidReturn", "returned when not in a function");
        let frame = self.callstack.pop().ok_or_else(or_else)?;

        self.current_func = frame.name;
        self.fp = frame.fp;
        self.pc = frame.pc;
        self.loc = frame.loc;

        return Ok(());
    }

    pub fn call(&mut self, new_pc: VarPointer) -> Result<(), IError> {
        if self.callstack.len() > 1000 {
            return Err(ierror!("StackOverflow", "maximum number of calls reached"));
        }

        self.callstack.push(CallFrame::new(
            self.current_func,
            self.loc,
            self.fp,
            self.pc,
        ));

        self.pc = new_pc;
        let func: Opcode = self.read_pc()?;
        if func != Opcode::Func {
            return Err(ierror!(
                "InvalidCall",
                "called {} which is not a function (this is an error in TCI)",
                new_pc
            ));
        }

        self.current_func = self.read_pc()?;
        self.loc = self.read_pc()?;
        self.fp = self.stack.len() as u16 + 1;

        return Ok(());
    }

    pub fn jump(&mut self, pc: VarPointer) {
        self.pc = pc;
    }

    pub fn set_loc(&mut self, loc: CodeLoc) {
        self.loc = loc;
    }

    pub fn read_pc<T: Copy>(&mut self) -> Result<T, IError> {
        if self.pc.var_idx() == 0 {
            return Err(invalid_ptr(self.pc));
        }

        let var_idx = self.pc.var_idx() - 1;
        let or_else = || invalid_ptr(self.pc);

        if self.pc.is_stack() {
            return Err(ierror!(
                "PermissionDenied",
                "tried to execute memory outside of functions"
            ));
        }
        let alloc = self.ranges.get(var_idx).ok_or_else(or_else)?;
        let from_bytes = match *alloc {
            AllocTracker::Exe { start, len } => &self.shared_data[r(start, start + len)],

            _ => {
                return Err(ierror!(
                    "PermissionDenied",
                    "tried to execute memory outside of functions"
                ));
            }
        };

        let (len, from_len, ptr) = (mem::size_of::<T>(), from_bytes.len() as u32, self.pc);
        let range = (self.pc.offset() as usize)..(ptr.offset() as usize + len);
        let or_else = move || invalid_offset(from_len, ptr, len as u32);
        let from_bytes = from_bytes.get(range).ok_or_else(or_else)?;

        self.pc = self.pc.add(len as u64);
        let mut out = mem::MaybeUninit::uninit();
        unsafe { any_as_u8_slice_mut(&mut out).copy_from_slice(from_bytes) };
        return Ok(unsafe { out.assume_init() });
    }

    pub fn add_stack_var(&mut self, len: u32) -> Result<VarPointer, IError> {
        let stack_len = self.stack_data.len();
        let new_len = stack_len + len as usize;
        if new_len > 1024 * 8 {
            return Err(ierror!(
                "StackOverflow",
                "stack size would be over 8KB after this declaration"
            ));
        }

        if self.stack.len() > 4000 {
            return Err(ierror!(
                "StackOverflow",
                "stack would have over 4000 variables after this declaration"
            ));
        }

        self.stack_data.resize(new_len, 0);
        self.stack.push(Var::new(stack_len, ()));
        return Ok(VarPointer::new_stack(self.stack.len() as u16, 0));
    }

    pub fn frame_loc(&self, skip_frames: u32) -> Result<CodeLoc, IError> {
        let skip_frames = skip_frames as usize;
        if skip_frames == 0 {
            return Ok(self.loc);
        }

        if skip_frames > self.callstack.len() {
            return Err(ierror!(
                "SkippedTooManyFrames",
                "tried to skip more stack frames than the stack holds"
            ));
        }

        return Ok(self.callstack[self.callstack.len() - skip_frames].loc);
    }

    pub fn add_heap_var(&mut self, len: u32, skip_frames: u32) -> Result<VarPointer, IError> {
        let data_len = self.shared_data.len() as u32;

        if data_len + len > 1024 * 1024 * 16 {
            return Err(ierror!(
                "HeapTooLarge",
                "memory size would be over 16MB after this allocation"
            ));
        }

        // if self.heap.len() >= 10_000 {
        //     return Err(ierror!(
        //         "TooManyAllocations",
        //         "allocated over 10,000 items on the heap"
        //     ));
        // }

        let loc = self.frame_loc(skip_frames)?;
        self.shared_data.push_repeat(!0, len as usize);

        self.ranges.push(AllocTracker::HeapLive {
            loc,
            start: data_len,
            len,
        });

        return Ok(VarPointer::new(self.ranges.len() as u32, 0));
    }

    pub fn free(&mut self, ptr: VarPointer, skip_frames: u32) -> Result<(), IError> {
        if ptr.var_idx() == 0 {
            return Err(invalid_ptr(ptr));
        }

        let var_idx = ptr.var_idx() - 1;
        let or_else = || invalid_ptr(ptr);

        if ptr.is_stack() {
            return Err(ierror!(
                "InvalidFreeTarget",
                "tried to free a pointer from the stack"
            ));
        }

        let free_loc = self.frame_loc(skip_frames)?;

        let alloc = self.ranges.get_mut(var_idx).ok_or_else(or_else)?;

        let (loc, len) = match *alloc {
            AllocTracker::HeapLive { loc, start, len } => (loc, len),
            AllocTracker::HeapDead { loc, free_loc } => {
                return Err(ierror!(
                    "DoubleFree",
                    "tried to free something that has already been freed"
                ));
            }
            _ => {
                return Err(ierror!(
                    "InvalidFreeTarget",
                    "tried to free a pointer that isn't from the heap"
                ));
            }
        };

        *alloc = AllocTracker::HeapDead { loc, free_loc };
        self.freed += len as usize;

        if self.freed * 2 >= self.shared_data.capacity() {
            self.coallesce_heap();
        }

        return Ok(());
    }

    pub fn coallesce_heap(&mut self) {
        let mut write_to = self.binary_data_len;

        let range_len = self.ranges.len() as u32;

        for tracker in &mut self.ranges[r(self.binary_var_len, range_len)] {
            let (start, len) = match tracker {
                AllocTracker::StackLive { loc, start, len } => (start, *len),
                AllocTracker::HeapLive { loc, start, len } => (start, *len),

                _ => continue,
            };

            let write_from = *start;

            debug_assert!(write_to <= write_from);

            if write_to == write_from {
                continue;
            }

            if write_to + len > write_from {
                let src = &self.shared_data[write_from] as *const u8;
                let dest = &mut self.shared_data[write_to] as *mut u8;

                unsafe { core::ptr::copy(src, dest, len as usize) };
            } else {
                let src = &self.shared_data[write_from] as *const u8;
                let dest = &mut self.shared_data[write_to] as *mut u8;

                unsafe { core::ptr::copy_nonoverlapping(src, dest, len as usize) };
            }

            *start = write_to;
            write_to += len;
        }

        unsafe {
            // should never increase the size of the buffer
            self.shared_data.set_len(write_to as usize);
        }
    }

    pub fn pop_stack_var(&mut self) -> Result<(), IError> {
        let or_else = || empty_stack();
        let var = self.stack.pop().ok_or_else(or_else)?;
        self.stack_data.resize(var.idx, 0);

        return Ok(());
    }

    pub fn upper_bound(&self, ptr: VarPointer) -> Option<VarPointer> {
        if ptr.var_idx() == 0 {
            return None;
        }

        let var_idx = ptr.var_idx() - 1;
        let offset = if ptr.is_stack() {
            let lower = self.stack.get(var_idx)?.idx;
            let upper = self.stack.get(var_idx + 1).map(|a| a.idx);
            let upper = upper.unwrap_or(self.stack_data.len());

            upper - lower
        } else {
            let alloc = self.ranges.get(var_idx)?;
            let (lower, upper) = alloc.range()?;

            (upper - lower) as usize
        };

        return Some(ptr.with_offset(offset as u32));
    }

    pub fn read_bytes(&self, ptr: VarPointer, len: u32) -> Result<&[u8], IError> {
        if ptr.var_idx() == 0 {
            return Err(invalid_ptr(ptr));
        }

        let var_idx = ptr.var_idx() - 1;
        let or_else = || invalid_ptr(ptr);

        let from_bytes = if ptr.is_stack() {
            let lower = self.stack.get(var_idx).ok_or_else(or_else)?.idx;
            let upper = self.stack.get(var_idx + 1).map(|a| a.idx);
            let upper = upper.unwrap_or(self.stack_data.len());

            &self.stack_data[lower..upper]
        } else {
            let alloc = self.ranges.get(var_idx).ok_or_else(or_else)?;
            let (lower, upper) = alloc.dereference(ptr, Const)?;

            &self.shared_data[r(lower, upper)]
        };

        let from_len = from_bytes.len() as u32;
        let range = (ptr.offset() as usize)..(ptr.offset() as usize + len as usize);
        let or_else = move || invalid_offset(from_len, ptr, len);
        let from_bytes = from_bytes.get(range).ok_or_else(or_else)?;
        return Ok(from_bytes);
    }

    pub fn read<T: Copy>(&self, ptr: VarPointer) -> Result<T, IError> {
        let len = mem::size_of::<T>() as u32;
        let from_bytes = self.read_bytes(ptr, len)?;

        let mut out = mem::MaybeUninit::uninit();
        unsafe { any_as_u8_slice_mut(&mut out).copy_from_slice(from_bytes) };
        return Ok(unsafe { out.assume_init() });
    }

    pub fn write_bytes(&mut self, ptr: VarPointer, buffer: &[u8]) -> Result<(), IError> {
        if ptr.var_idx() == 0 {
            return Err(invalid_ptr(ptr));
        }

        let var_idx = ptr.var_idx() - 1;
        let or_else = || invalid_ptr(ptr);

        let to_bytes = if ptr.is_stack() {
            let lower = self.stack.get(var_idx).ok_or_else(or_else)?.idx;
            let upper = self.stack.get(var_idx + 1).map(|a| a.idx);
            let upper = upper.unwrap_or(self.stack_data.len());

            &mut self.stack_data[lower..upper]
        } else {
            let alloc = self.ranges.get(var_idx).ok_or_else(or_else)?;
            let (lower, upper) = alloc.dereference(ptr, Mut)?;

            &mut self.shared_data[r(lower, upper)]
        };

        let to_len = to_bytes.len() as u32;
        let range = (ptr.offset() as usize)..(ptr.offset() as usize + buffer.len());
        let or_else = move || invalid_offset(to_len, ptr, buffer.len() as u32);
        let to_bytes = to_bytes.get_mut(range).ok_or_else(or_else)?;
        to_bytes.copy_from_slice(buffer);
        return Ok(());
    }

    pub fn cstring_bytes(&self, ptr: VarPointer) -> Result<&[u8], IError> {
        if ptr.var_idx() == 0 {
            return Err(invalid_ptr(ptr));
        }

        let var_idx = ptr.var_idx() - 1;
        let or_else = || invalid_ptr(ptr);

        let from_bytes = if ptr.is_stack() {
            let lower = self.stack.get(var_idx).ok_or_else(or_else)?.idx;
            let upper = self.stack.get(var_idx + 1).map(|a| a.idx);
            let upper = upper.unwrap_or(self.stack_data.len());

            &self.stack_data[lower..upper]
        } else {
            let alloc = self.ranges.get(var_idx).ok_or_else(or_else)?;
            let (lower, upper) = alloc.dereference(ptr, Const)?;

            &self.shared_data[r(lower, upper)]
        };

        let (from_len, range) = (from_bytes.len() as u32, (ptr.offset() as usize)..);
        let or_else = move || invalid_ptr(ptr);
        let from_bytes = from_bytes.get(range).ok_or_else(or_else)?;

        let mut idx = from_bytes.len();
        for (idx_, byte) in from_bytes.iter().enumerate() {
            if *byte == 0 {
                idx = idx_;
                break;
            }
        }

        if idx == from_bytes.len() {
            return ierr!(
                "MissingNullTerminator",
                "valid C strings should end in '\\0' character"
            );
        }

        return Ok(&from_bytes[..idx]);
    }

    pub fn read_bytes_to_stack(&mut self, ptr: VarPointer, len: u32) -> Result<(), IError> {
        if ptr.var_idx() == 0 {
            return Err(invalid_ptr(ptr));
        }

        let var_idx = ptr.var_idx() - 1;
        let or_else = || invalid_ptr(ptr);

        let from_bytes = if ptr.is_stack() {
            let lower = self.stack.get(var_idx).ok_or_else(or_else)?.idx;
            let upper = self.stack.get(var_idx + 1).map(|a| a.idx);
            let upper = upper.unwrap_or(self.stack_data.len());

            &self.stack_data[lower..upper]
        } else {
            let alloc = self.ranges.get(var_idx).ok_or_else(or_else)?;
            let (lower, upper) = alloc.dereference(ptr, Const)?;

            &self.shared_data[r(lower, upper)]
        };

        let from_len = from_bytes.len() as u32;
        let range = (ptr.offset() as usize)..(ptr.offset() as usize + len as usize);
        let or_else = move || invalid_offset(from_len, ptr, len);
        let from_bytes = from_bytes.get(range).ok_or_else(or_else)?;
        self.expr_stack.extend_from_slice(from_bytes);
        return Ok(());
    }

    pub fn write_bytes_from_stack(&mut self, ptr: VarPointer, len: u32) -> Result<(), IError> {
        if ptr.var_idx() == 0 {
            return Err(invalid_ptr(ptr));
        }

        let var_idx = ptr.var_idx() - 1;
        let or_else = || invalid_ptr(ptr);

        let to_bytes = if ptr.is_stack() {
            let lower = self.stack.get(var_idx).ok_or_else(or_else)?.idx;
            let upper = self.stack.get(var_idx + 1).map(|a| a.idx);
            let upper = upper.unwrap_or(self.stack_data.len());

            &mut self.stack_data[lower..upper]
        } else {
            let alloc = self.ranges.get(var_idx).ok_or_else(or_else)?;
            let (lower, upper) = alloc.dereference(ptr, Mut)?;

            &mut self.shared_data[r(lower, upper)]
        };

        let to_len = to_bytes.len();
        let range = (ptr.offset() as usize)..(ptr.offset() as usize + len as usize);
        let or_else = move || invalid_offset(to_len as u32, ptr, len);
        let to_bytes = to_bytes.get_mut(range).ok_or_else(or_else)?;

        let stack_len = self.expr_stack.len();
        if len as usize > stack_len {
            return Err(expr_stack_too_short(stack_len, len as usize));
        }
        let new_stack_len = stack_len - len as usize;
        let from_bytes = &self.expr_stack[new_stack_len..];
        to_bytes.copy_from_slice(from_bytes);

        self.expr_stack.resize(new_stack_len, 0);
        return Ok(());
    }

    pub fn pop_bytes(&mut self, len: u32) -> Result<(), IError> {
        let (len, stack_len) = (len as usize, self.expr_stack.len());
        if stack_len < len {
            return Err(expr_stack_too_short(stack_len, len));
        }

        self.expr_stack.resize(stack_len - len, 0);
        return Ok(());
    }

    pub fn swap_bytes(&mut self, top_bytes: u32, bottom_bytes: u32) -> Result<(), IError> {
        let (top_bytes, bottom_bytes) = (top_bytes as usize, bottom_bytes as usize);
        let (len, stack_len) = (top_bytes + bottom_bytes, self.expr_stack.len());
        if len > stack_len {
            return Err(expr_stack_too_short(stack_len, len));
        }

        let slice_o = self.expr_stack.get_mut((stack_len - len)..);
        slice_o.unwrap().rotate_left(bottom_bytes);
        return Ok(());
    }

    pub fn dup_bytes(&mut self, bytes: u32) -> Result<(), IError> {
        let bytes = bytes as usize;
        let (stack_len, new_len) = (self.expr_stack.len(), self.expr_stack.len() + bytes);

        if bytes > self.expr_stack.len() {
            return Err(expr_stack_too_short(stack_len, bytes));
        }
        self.expr_stack.resize(new_len, 0);

        let slice = &mut self.expr_stack[(new_len - 2 * bytes)..];
        let (from, to) = slice.split_at_mut(bytes);
        to.copy_from_slice(from);

        return Ok(());
    }

    pub fn pop<T: Copy>(&mut self) -> Result<T, IError> {
        let (len, stack_len) = (mem::size_of::<T>(), self.expr_stack.len());
        if len > stack_len {
            return Err(expr_stack_too_short(stack_len, len));
        }

        let from_bytes = &self.expr_stack[(stack_len - len)..];

        let mut out = mem::MaybeUninit::uninit();
        unsafe { any_as_u8_slice_mut(&mut out).copy_from_slice(from_bytes) };
        self.expr_stack.resize(stack_len - len, 0);
        return Ok(unsafe { out.assume_init() });
    }

    pub fn push<T: Copy>(&mut self, t: T) {
        let from_bytes = any_as_u8_slice(&t);
        self.expr_stack.extend_from_slice(from_bytes);
    }
}

pub fn invalid_ptr(ptr: VarPointer) -> IError {
    return ierror!(
        "InvalidPointer",
        "the pointer {} doesn't point to valid memory",
        ptr
    );
}

pub fn freed_ptr(ptr: VarPointer) -> IError {
    return ierror!(
        "InvalidPointer",
        "the pointer {} points to freed memory",
        ptr
    );
}

pub fn invalid_offset(valid_len: u32, ptr: VarPointer, len: u32) -> IError {
    let (start, end) = (ptr.with_offset(0), ptr.with_offset(valid_len));
    return ierror!(
        "InvalidPointer",
        "the pointer {} with size {} doesn't point to valid memory; the nearest object is in the range {}..{}",
        ptr,
        len,
        start,
        end
    );
}

pub fn expr_stack_too_short(stack_len: usize, popped_len: usize) -> IError {
    return ierror!(
        "StackTooShort",
        "tried to read {} bytes from stack buffer that is {} bytes long (this is an error in TCI)",
        popped_len,
        stack_len,
    );
}

pub fn empty_stack() -> IError {
    return ierror!(
        "StackTooShort",
        "tried to pop from an empty stack (this is an error in TCI)"
    );
}
