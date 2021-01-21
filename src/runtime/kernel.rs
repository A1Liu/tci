use super::error::*;
use super::files::*;
use super::interpreter::*;
use super::memory::*;
use super::types::*;
use crate::filedb::*;
use crate::util::*;
use core::mem;

pub struct Runtime {
    pub files: FileSystem,
    pub status: RuntimeStatus,

    // per process
    pub output: StringArray<WriteEvent>,
    pub memory: Memory,
    pub fd_list: Vec<usize>,
}

impl Runtime {
    pub fn new(binary: &BinaryData) -> Self {
        Self {
            output: StringArray::new(),
            files: FileSystem::new(),
            memory: Memory::new(&binary),
            status: RuntimeStatus::Running,
            fd_list: Vec::new(),
        }
    }

    pub fn run_debug(&mut self, files: &FileDb) -> Result<i32, IError> {
        if let RuntimeStatus::Exited(code) = self.status {
            return Ok(code);
        }

        loop {
            if self.memory.loc != NO_FILE {
                println!("{}", files.loc_to_string(self.memory.loc));
            }
            println!("{:?}", self.memory.expr_stack);
            let res = run_op(&mut self.memory);
            println!("{:?}\n", self.memory.expr_stack);

            if let Some(e) = res? {
                if let Some(exit) = self.ecall(e)? {
                    self.status = RuntimeStatus::Exited(exit);
                    return Ok(exit);
                }
            }
        }
    }

    pub fn run(&mut self) -> Result<i32, IError> {
        if let RuntimeStatus::Exited(code) = self.status {
            return Ok(code);
        }

        loop {
            let res = run_op(&mut self.memory);
            if let Some(e) = res? {
                if let Some(exit) = self.ecall(e)? {
                    self.status = RuntimeStatus::Exited(exit);
                    return Ok(exit);
                }
            }
        }
    }

    pub fn run_op_count(&mut self, count: u32) -> Result<RuntimeStatus, IError> {
        if let RuntimeStatus::Exited(_) = self.status {
            return Ok(self.status);
        }

        for _ in 0..count {
            if let Some(e) = run_op(&mut self.memory)? {
                if let Some(exit) = self.ecall(e)? {
                    self.status = RuntimeStatus::Exited(exit);
                    return Ok(self.status);
                }
            }
        }

        return Ok(self.status);
    }

    pub fn ecall(&mut self, req: Ecall) -> Result<Option<i32>, IError> {
        match req {
            Ecall::Exit => {
                let exit: i32 = self.memory.pop()?;
                return Ok(Some(exit));
            }

            Ecall::OpenFd => {
                let name: VarPointer = self.memory.pop()?;
                let open_mode: OpenMode = self.memory.pop()?;
                let name = self.memory.cstring_bytes(name)?;
                let id = match open_mode {
                    OpenMode::Read => self.files.open(name),
                    OpenMode::Create => self.files.open_create(name),
                    OpenMode::CreateClear => self.files.open_create_clear(name),
                };

                let err = match id {
                    Err(e) => e,
                    Ok(idx) => {
                        self.memory.push(idx as u64);
                        return Ok(None);
                    }
                };

                self.memory.push((err as u32 as u64) << 32);
            }
            Ecall::ReadFd => {
                let len: u32 = self.memory.pop()?;
                let buf: VarPointer = self.memory.pop()?;
                let begin: u32 = self.memory.pop()?;
                let fd: u32 = self.memory.pop()?;

                let file_buffer = match self.files.read_file_range(fd, begin, len) {
                    Ok(buf) => buf,
                    Err(e) => {
                        self.memory.push((e as u32 as u64) << 32);
                        return Ok(None);
                    }
                };

                self.memory.write_bytes(buf, file_buffer)?;
                self.memory.push(file_buffer.len() as u32);
            }
            Ecall::WriteFd => {
                let len: u32 = self.memory.pop()?;
                let buf: VarPointer = self.memory.pop()?;
                let begin: u32 = self.memory.pop()?;
                let fd: u32 = self.memory.pop()?;

                let buffer = self.memory.read_bytes(buf, len)?;

                let e = match self.files.write_to_file_range(fd, begin, buffer) {
                    Ok(len) => {
                        self.memory.push(len as u64);
                        return Ok(None);
                    }
                    Err(err) => err,
                };

                self.memory.push((e as u32 as u64) << 32);
            }
            Ecall::AppendFd => {
                let len: u32 = self.memory.pop()?;
                let buf: VarPointer = self.memory.pop()?;
                let fd: u32 = self.memory.pop()?;

                let buffer = self.memory.read_bytes(buf, len)?;

                let e = match self.files.append_to_file(fd, buffer) {
                    Ok(len) => {
                        self.memory.push(len as u64);
                        return Ok(None);
                    }
                    Err(err) => err,
                };

                self.memory.push((e as u32 as u64) << 32);
            }
            Ecall::FdLen => {
                let fd: u32 = self.memory.pop()?;

                let e = match self.files.len(fd) {
                    Ok(len) => {
                        self.memory.push(len as u64);
                        return Ok(None);
                    }
                    Err(err) => err,
                };

                self.memory.push((e as u32 as u64) << 32);
            }

            Ecall::PrintString => {
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
                return ierr!(
                    "InvalidEnviromentCall",
                    "invalid ecall value of {}",
                    call as u32
                )
            }
        }

        return Ok(None);
    }

    pub fn events(&mut self) -> StringArray<WriteEvent> {
        return mem::replace(&mut self.output, StringArray::new());
    }
}
