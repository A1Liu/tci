use super::error::*;
use super::types::*;
use crate::util::*;
use core::mem;

#[derive(Debug)]
pub struct Memory {
    pub shared_data: Vec<u8>,
    pub binary: Vec<Var<()>>,
    pub heap: Vec<Var<()>>,

    pub expr_stack: Vec<u8>,
    pub stack_data: Vec<u8>,
    pub stack: Vec<Var<()>>,

    pub callstack: Vec<CallFrame>,
    pub current_func: LinkName,
    pub fp: u16,
    pub pc: VarPointer,
    pub loc: CodeLoc,
}

impl Memory {
    pub fn new(binary: &BinaryData) -> Self {
        Self {
            shared_data: binary.data.clone(),
            binary: binary.vars.clone(),
            heap: Vec::new(),

            expr_stack: Vec::new(),
            stack_data: Vec::new(),
            stack: Vec::new(),

            callstack: Vec::new(),
            current_func: LinkName::new(!0),
            loc: NO_FILE,
            fp: 1,
            pc: VarPointer::new_binary(1, 0),
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

        let from_bytes = if self.pc.is_binary() {
            let lower = self.binary.get(var_idx).ok_or_else(or_else)?.idx;
            let upper = self.binary.get(var_idx + 1).map(|a| a.idx);
            let heap_lower = self.heap.get(0).map(|a| a.idx);
            let upper = upper.or(heap_lower).unwrap_or(self.shared_data.len());

            &self.shared_data[lower..upper]
        } else {
            return Err(ierror!(
                "PermissionDenied",
                "tried to execute memory outside of functions"
            ));
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

    pub fn add_stack_var(&mut self, len: u32) -> VarPointer {
        let stack_len = self.stack_data.len();
        self.stack_data.resize(stack_len + len as usize, 0);
        self.stack.push(Var::new(stack_len, ()));
        return VarPointer::new_stack(self.stack.len() as u16, 0);
    }

    pub fn add_heap_var(&mut self, len: u32) -> VarPointer {
        let data_len = self.shared_data.len();
        self.shared_data.resize(data_len + len as usize, 0);
        self.heap.push(Var::new(data_len, ()));
        return VarPointer::new_heap(self.heap.len() as u32, 0);
    }

    pub fn pop_stack_var(&mut self) -> Result<(), IError> {
        let or_else = || empty_stack();
        let var = self.stack.pop().ok_or_else(or_else)?;
        self.stack_data.resize(var.idx, 0);

        return Ok(());
    }

    pub fn read<T: Copy>(&self, ptr: VarPointer) -> Result<T, IError> {
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
        } else if ptr.is_heap() {
            let lower = self.heap.get(var_idx).ok_or_else(or_else)?.idx;
            let upper = self.heap.get(var_idx + 1).map(|a| a.idx);
            let upper = upper.unwrap_or(self.shared_data.len());

            &self.shared_data[lower..upper]
        } else {
            let lower = self.binary.get(var_idx).ok_or_else(or_else)?.idx;
            let upper = self.binary.get(var_idx + 1).map(|a| a.idx);
            let heap_lower = self.heap.get(0).map(|a| a.idx);
            let upper = upper.or(heap_lower).unwrap_or(self.shared_data.len());

            &self.shared_data[lower..upper]
        };

        let (len, from_len) = (mem::size_of::<T>() as u32, from_bytes.len() as u32);
        let range = (ptr.offset() as usize)..(ptr.offset() as usize + len as usize);
        let or_else = move || invalid_offset(from_len, ptr, len);
        let from_bytes = from_bytes.get(range).ok_or_else(or_else)?;

        let mut out = mem::MaybeUninit::uninit();
        unsafe { any_as_u8_slice_mut(&mut out).copy_from_slice(from_bytes) };
        return Ok(unsafe { out.assume_init() });
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
        } else if ptr.is_heap() {
            let lower = self.heap.get(var_idx).ok_or_else(or_else)?.idx;
            let upper = self.heap.get(var_idx + 1).map(|a| a.idx);
            let upper = upper.unwrap_or(self.shared_data.len());

            &self.shared_data[lower..upper]
        } else {
            let lower = self.binary.get(var_idx).ok_or_else(or_else)?.idx;
            let upper = self.binary.get(var_idx + 1).map(|a| a.idx);
            let heap_lower = self.heap.get(0).map(|a| a.idx);
            let upper = upper.or(heap_lower).unwrap_or(self.shared_data.len());

            &self.shared_data[lower..upper]
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
            return ierr!("MissingNullTerminator", "string missing null terminator");
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
        } else if ptr.is_heap() {
            let lower = self.heap.get(var_idx).ok_or_else(or_else)?.idx;
            let upper = self.heap.get(var_idx + 1).map(|a| a.idx);
            let upper = upper.unwrap_or(self.shared_data.len());

            &self.shared_data[lower..upper]
        } else {
            let lower = self.binary.get(var_idx).ok_or_else(or_else)?.idx;
            let upper = self.binary.get(var_idx + 1).map(|a| a.idx);
            let heap_lower = self.heap.get(0).map(|a| a.idx);
            let upper = upper.or(heap_lower).unwrap_or(self.shared_data.len());

            &self.shared_data[lower..upper]
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
        } else if ptr.is_heap() {
            let lower = self.heap.get(var_idx).ok_or_else(or_else)?.idx;
            let upper = self.heap.get(var_idx + 1).map(|a| a.idx);
            let upper = upper.unwrap_or(self.shared_data.len());

            &mut self.shared_data[lower..upper]
        } else {
            let lower = self.binary.get(var_idx).ok_or_else(or_else)?.idx;
            let upper = self.binary.get(var_idx + 1).map(|a| a.idx);
            let heap_lower = self.heap.get(0).map(|a| a.idx);
            let upper = upper.or(heap_lower).unwrap_or(self.shared_data.len());

            &mut self.shared_data[lower..upper]
        };

        let to_len = to_bytes.len();
        let range = (ptr.offset() as usize)..(ptr.offset() as usize + len as usize);
        let or_else = move || invalid_offset(to_len as u32, ptr, len);
        let to_bytes = to_bytes.get_mut(range).ok_or_else(or_else)?;

        let stack_len = self.expr_stack.len();
        let new_stack_len = stack_len - len as usize;
        let or_else = move || expr_stack_too_short(stack_len, len as usize);
        let from_bytes = self.expr_stack.get(new_stack_len..).ok_or_else(or_else)?;
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
