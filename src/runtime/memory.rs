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
    freed: usize,
    binary_var_len: u32,
    binary_data_len: u32,

    data: Pod<u8>,
    ranges: Pod<AllocTracker>,
    expr_stack: Pod<u8>,
    pub stack: Pod<u32>,
    pub callstack: Pod<CallFrame>,
    pub frame: CallFrame,
}

impl Memory {
    pub fn new(binary: &BinaryData) -> Self {
        let binary_var_len = binary.vars.len() as u32;
        let binary_data_len = binary.data.len() as u32;

        Self {
            data: binary.data.clone(),
            ranges: binary.vars.clone(),
            freed: 0,
            binary_var_len,
            binary_data_len,

            expr_stack: Pod::new(),
            stack: Pod::new(),

            callstack: Pod::new(),
            frame: CallFrame {
                name: LinkName::new(!0),
                loc: NO_FILE,
                fp: 0,
                pc: VarPointer::new(1, 0),
            },
        }
    }

    pub fn ret(&mut self) -> Result<(), IError> {
        let or_else = || ierror!("InvalidReturn", "returned when not in a function");
        self.frame = self.callstack.pop().ok_or_else(or_else)?;

        return Ok(());
    }

    pub fn call(&mut self, new_pc: VarPointer) -> Result<(), IError> {
        if self.callstack.len() > 1000 {
            return Err(ierror!("StackOverflow", "maximum number of calls reached"));
        }

        self.callstack.push(self.frame);

        self.frame.pc = new_pc;
        let func: Opcode = self.read_pc()?;
        if func != Opcode::Func {
            return Err(ierror!(
                "InvalidCall",
                "called {} which is not a function (this is an error in TCI)",
                new_pc
            ));
        }

        self.frame.name = self.read_pc()?;
        self.frame.loc = self.read_pc()?;
        self.frame.fp = self.stack.len() as u16;

        return Ok(());
    }

    pub fn jump(&mut self, pc: VarPointer) {
        self.frame.pc = pc;
    }

    pub fn set_loc(&mut self, loc: CodeLoc) {
        self.frame.loc = loc;
    }

    pub fn read_pc<T: Copy>(&mut self) -> Result<T, IError> {
        let len = mem::size_of::<T>();

        let from_bytes = self.read_pc_bytes(len)?;

        let mut out = mem::MaybeUninit::<T>::uninit();
        unsafe { any_as_u8_slice_mut(&mut out).copy_from_slice(from_bytes) };
        return Ok(unsafe { out.assume_init() });
    }

    pub fn read_pc_bytes(&mut self, len: usize) -> Result<&[u8], IError> {
        let pc = self.frame.pc;
        if pc.var_idx() == 0 {
            return Err(invalid_ptr(pc));
        }

        let var_idx = pc.var_idx() - 1;
        let or_else = || invalid_ptr(pc);

        if pc.is_stack() {
            return Err(ierror!(
                "PermissionDenied",
                "tried to execute memory outside of functions"
            ));
        }
        let alloc = self.ranges.get(var_idx).ok_or_else(or_else)?;
        let from_bytes = match *alloc {
            AllocTracker::Exe { start, len } => &self.data[r(start, start + len)],

            _ => {
                return Err(ierror!(
                    "PermissionDenied",
                    "tried to execute memory outside of functions"
                ));
            }
        };

        let (from_len, ptr) = (from_bytes.len() as u32, pc);
        let range = (pc.offset() as usize)..(ptr.offset() as usize + len);
        let or_else = move || invalid_offset(from_len, ptr, len as u32);
        let from_bytes = from_bytes.get(range).ok_or_else(or_else)?;

        self.frame.pc = pc.add(len as u64);

        return Ok(from_bytes);
    }

    pub fn add_stack_var(&mut self, len: u32) -> Result<VarPointer, IError> {
        // let new_len = stack_len + len as usize;
        // if new_len > 1024 * 8 {
        //     return Err(ierror!(
        //         "StackOverflow",
        //         "stack size would be over 8KB after this declaration"
        //     ));
        // }

        // if self.stack.len() > 4000 {
        //     return Err(ierror!(
        //         "StackOverflow",
        //         "stack would have over 4000 variables after this declaration"
        //     ));
        // }

        let start = self.data.len() as u32;

        self.data.push_repeat(0, len as usize);

        self.ranges.push(AllocTracker::StackLive {
            loc: self.frame.loc,
            start,
            len,
        });

        let range_id = self.ranges.len() as u32;

        self.stack.push(range_id);

        return Ok(VarPointer::new(range_id, 0));
    }

    pub fn frame_loc(&self, skip_frames: u32) -> Result<CodeLoc, IError> {
        let skip_frames = skip_frames as usize;
        if skip_frames == 0 {
            return Ok(self.frame.loc);
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
        let data_len = self.data.len() as u32;

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
        self.data.push_repeat(!0, len as usize);

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

        if self.freed * 2 >= self.data.capacity() {
            self.coallesce_heap();
            self.freed = 0;
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

            let src = &self.data[write_from] as *const u8;
            let dest = &mut self.data[write_to] as *mut u8;

            unsafe { core::ptr::copy(src, dest, len as usize) };

            *start = write_to;
            write_to += len;
        }

        self.data.truncate(write_to as usize);
    }

    pub fn pop_stack_var(&mut self) -> Result<(), IError> {
        let or_else = || empty_stack();
        let var = self.stack.pop().ok_or_else(or_else)?;
        let (loc, len) = match self.ranges[var - 1] {
            AllocTracker::StackLive { loc, len, .. } => (loc, len),
            _ => unreachable!("what the hell"),
        };

        self.ranges[var - 1] = AllocTracker::StackDead { loc };
        self.freed += len as usize;

        return Ok(());
    }

    pub fn upper_bound(&self, ptr: VarPointer) -> Option<VarPointer> {
        if ptr.var_idx() == 0 {
            return None;
        }

        let var_idx = ptr.var_idx() - 1;
        let alloc = self.ranges.get(var_idx)?;
        let (lower, upper) = alloc.range()?;

        let offset = (upper - lower) as usize;

        return Some(ptr.with_offset(offset as u32));
    }

    pub fn read_bytes(&self, ptr: VarPointer, len: u32) -> Result<&[u8], IError> {
        if ptr.var_idx() == 0 {
            return Err(invalid_ptr(ptr));
        }

        let var_idx = ptr.var_idx() - 1;
        let or_else = || invalid_ptr(ptr);

        let alloc = self.ranges.get(var_idx).ok_or_else(or_else)?;
        let (lower, upper) = alloc.dereference(ptr, Const)?;

        let from_bytes = &self.data[r(lower, upper)];

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

        let alloc = self.ranges.get(var_idx).ok_or_else(or_else)?;
        let (lower, upper) = alloc.dereference(ptr, Mut)?;

        let to_bytes = &mut self.data[r(lower, upper)];

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

        let alloc = self.ranges.get(var_idx).ok_or_else(or_else)?;
        let (lower, upper) = alloc.dereference(ptr, Const)?;

        let from_bytes = &self.data[r(lower, upper)];

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

        let alloc = self.ranges.get(var_idx).ok_or_else(or_else)?;
        let (lower, upper) = alloc.dereference(ptr, Const)?;

        let from_bytes = &self.data[r(lower, upper)];

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

        let alloc = self.ranges.get(var_idx).ok_or_else(or_else)?;
        let (lower, upper) = alloc.dereference(ptr, Mut)?;

        let to_bytes = &mut self.data[r(lower, upper)];

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

        self.expr_stack.truncate(new_stack_len);
        return Ok(());
    }

    pub fn pop_bytes(&mut self, len: u32) -> Result<(), IError> {
        let (len, stack_len) = (len as usize, self.expr_stack.len());
        if stack_len < len {
            return Err(expr_stack_too_short(stack_len, len));
        }

        self.expr_stack.truncate(stack_len - len);
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
        let stack_len = self.expr_stack.len();

        if bytes > self.expr_stack.len() {
            return Err(expr_stack_too_short(stack_len, bytes));
        }

        let (from, to) = self.expr_stack.extend_uninit(bytes);
        to.copy_from_slice(&from[(stack_len - bytes)..]);

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
        self.expr_stack.truncate(stack_len - len);
        return Ok(unsafe { out.assume_init() });
    }

    pub fn push<T: Copy>(&mut self, t: T) {
        let from_bytes = any_as_u8_slice(&t);
        self.expr_stack.extend_from_slice(from_bytes);
    }

    pub fn push_undef(&mut self, bytes: u32) {
        self.expr_stack.push_repeat(!0, bytes as usize);
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
