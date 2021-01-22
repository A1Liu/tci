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
                let open_mode: OpenMode = self.memory.pop()?;
                let name: VarPointer = self.memory.pop()?;

                let name = self.memory.cstring_bytes(name)?;
                let id = match open_mode {
                    OpenMode::Read => self.files.open(name),
                    OpenMode::Create => self.files.open_create(name),
                    OpenMode::CreateClear => self.files.open_create_clear(name),
                };

                let err = match id {
                    Err(e) => e,
                    Ok(idx) => {
                        self.memory.push(idx as u64 + 4);
                        return Ok(None);
                    }
                };

                self.memory.push(err.to_u64());
            }
            Ecall::ReadFd => {
                let len: u32 = self.memory.pop()?;
                let buf: VarPointer = self.memory.pop()?;
                let begin: u32 = self.memory.pop()?;
                let fd: u32 = self.memory.pop()?;

                let to_ret = match fd {
                    0 => {
                        unimplemented!("stdin")
                    }
                    1 => EcallError::ReadStdout.to_u64(),
                    2 => EcallError::ReadStderr.to_u64(),
                    3 => (EcallError::ReadStdlog as u32 as u64) << 32,
                    fd => match self.files.read_file_range(fd - 4, begin, len) {
                        Ok(file_buffer) => {
                            self.memory.write_bytes(buf, file_buffer)?;
                            file_buffer.len() as u64
                        }
                        Err(e) => e.to_u64(),
                    },
                };

                self.memory.push(to_ret);
            }
            Ecall::WriteFd => {
                let len: u32 = self.memory.pop()?;
                let buf: VarPointer = self.memory.pop()?;
                let begin: u32 = self.memory.pop()?;
                let fd: u32 = self.memory.pop()?;

                let buffer = self.memory.read_bytes(buf, len)?;

                let write_event = match fd {
                    0 => {
                        self.memory.push(EcallError::WriteStdin.to_u64());
                        return Ok(None);
                    }
                    1 => WriteEvent::StdoutWrite,
                    2 => WriteEvent::StderrWrite,
                    3 => WriteEvent::StdlogWrite,
                    fd => match self.files.write_to_file_range(fd - 4, begin, buffer) {
                        Ok(len) => {
                            self.memory.push(len as u64);
                            return Ok(None);
                        }
                        Err(err) => {
                            self.memory.push((err as u32 as u64) << 32);
                            return Ok(None);
                        }
                    },
                };

                let bytes = self.memory.read_bytes(buf, len)?;

                let mut string = StringWriter::new();
                write_utf8_lossy(&mut string, bytes).unwrap();

                self.output.push(write_event, &string.into_string());

                self.memory.push(0u64);
                return Ok(None);
            }
            Ecall::AppendFd => {
                let len: u32 = self.memory.pop()?;
                let buf: VarPointer = self.memory.pop()?;
                let fd: u32 = self.memory.pop()?;

                let buffer = self.memory.read_bytes(buf, len)?;

                let write_event = match fd {
                    0 => {
                        self.memory.push(EcallError::WriteStdin.to_u64());
                        return Ok(None);
                    }
                    1 => WriteEvent::StdoutWrite,
                    2 => WriteEvent::StderrWrite,
                    3 => WriteEvent::StdlogWrite,
                    fd => match self.files.append_to_file(fd - 4, buffer) {
                        Ok(len) => {
                            self.memory.push(len as u64);
                            return Ok(None);
                        }
                        Err(err) => {
                            self.memory.push(err.to_u64());
                            return Ok(None);
                        }
                    },
                };

                let bytes = self.memory.read_bytes(buf, len)?;

                let mut string = StringWriter::new();
                write_utf8_lossy(&mut string, bytes).unwrap();

                self.output.push(write_event, &string.into_string());

                self.memory.push(0u64);
                return Ok(None);
            }
            Ecall::FdLen => {
                let fd: u32 = self.memory.pop()?;

                if fd < 4 {
                    self.memory.push(EcallError::StreamLen.to_u64());
                    return Ok(None);
                }

                let val = match self.files.len(fd - 4) {
                    Ok(len) => len as u64,
                    Err(err) => err.to_u64(),
                };

                self.memory.push(val);
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
