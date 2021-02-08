use super::error::*;
use super::fs::*;
use super::interpreter::*;
use super::memory::*;
use super::types::*;
use crate::filedb::*;
use crate::util::*;
use core::mem;

pub struct Kernel {
    pub files: FileSystem,

    // per process
    pub output: StringArray<WriteEvent>,
    pub memory: Memory,
    pub status: IRtStat,
}

impl Kernel {
    pub fn new(binary: &BinaryData) -> Self {
        Self {
            files: FileSystem::new(),

            output: StringArray::new(),
            memory: Memory::new(&binary),
            status: IRtStat::Running,
        }
    }

    pub fn loc(&self) -> CodeLoc {
        self.memory.loc
    }

    pub fn run_debug(&mut self, files: &FileDb) -> Result<i32, IError> {
        if let IRtStat::Exited(code) = self.status {
            return Ok(code);
        }

        loop {
            if self.memory.loc != NO_FILE {
                println!("{}", files.loc_to_string(self.memory.loc));
            }

            println!("{:?}", self.memory.expr_stack);
            let res = run_op_count(&mut self.memory, !0);
            println!("{:?}\n", self.memory.expr_stack);

            if let Some(ecall) = res? {
                if let RuntimeStatus::Exited(e) = self.ecall(ecall)? {
                    return Ok(e);
                }
            }
        }
    }

    pub fn run(&mut self) -> Result<i32, IError> {
        if let IRtStat::Exited(code) = self.status {
            return Ok(code);
        }

        loop {
            if let Some(ecall) = run_op_count(&mut self.memory, !0)? {
                if let RuntimeStatus::Exited(e) = self.ecall(ecall)? {
                    return Ok(e);
                }
            }
        }
    }

    pub fn run_op_count(&mut self, count: u32) -> Result<RuntimeStatus, IError> {
        match self.status {
            IRtStat::Running => {}
            IRtStat::Blocked => {
                return Err(ierror!(
                    "RanDeadProcess",
                    "tried to run dead process (this is a bug in TCI)"
                ))
            }
            IRtStat::Exited(_) => {
                return Err(ierror!(
                    "RanDeadProcess",
                    "tried to run dead process (this is a bug in TCI)"
                ))
            }
        }

        match run_op_count(&mut self.memory, count)? {
            Some(ecall) => return self.ecall(ecall),
            None => return Ok(RuntimeStatus::Running),
        }
    }

    pub fn resolve_result(&mut self, res: EcallResult) -> Result<(), IError> {
        return Ok(());
    }

    pub fn ecall(&mut self, req: EcallExt) -> Result<RuntimeStatus, IError> {
        match req {
            EcallExt::Exit(exit) => {
                self.status = IRtStat::Exited(exit);
                return Ok(RuntimeStatus::Exited(exit));
            }

            EcallExt::OpenFd { name, open_mode } => {
                let id = match open_mode {
                    OpenMode::Read => self.files.open(&name),
                    OpenMode::Create => self.files.open_create(&name),
                    OpenMode::CreateClear => self.files.open_create_clear(&name),
                };

                let err = match id {
                    Err(e) => e,
                    Ok(idx) => {
                        self.memory.push(idx as u64 + 4);
                        return Ok(RuntimeStatus::Blocked(EcallExt::OpenFd { name, open_mode }));
                    }
                };

                self.memory.push(err.to_u64());
                return Ok(RuntimeStatus::Running);
            }

            EcallExt::ReadFd {
                len,
                buf,
                begin,
                fd,
            } => {
                let to_ret = match fd {
                    0 => unimplemented!("stdin"),
                    1 => EcallError::ReadStdout.to_u64(),
                    2 => EcallError::ReadStderr.to_u64(),
                    3 => EcallError::ReadStdlog.to_u64(),
                    fd => match self.files.read_file_range(fd - 4, begin, len) {
                        Ok(file_buffer) => {
                            self.memory.write_bytes(buf, file_buffer)?;
                            file_buffer.len() as u64
                        }
                        Err(e) => e.to_u64(),
                    },
                };

                self.memory.push(to_ret);
                return Ok(RuntimeStatus::Running);
            }
            EcallExt::WriteFd { buf, begin, fd } => {
                let write_event = match fd {
                    0 => {
                        self.memory.push(EcallError::WriteStdin.to_u64());
                        return Ok(RuntimeStatus::Running);
                    }
                    1 => WriteEvent::StdoutWrite,
                    2 => WriteEvent::StderrWrite,
                    3 => WriteEvent::StdlogWrite,
                    fd => {
                        let fd = fd - 4;
                        self.files
                            .write_to_file_range(fd, begin, &buf)
                            .map(|len| self.memory.push(len as u64))
                            .unwrap_or_else(|err| self.memory.push(err.to_u64()));
                        return Ok(RuntimeStatus::Blocked(EcallExt::WriteFd { buf, begin, fd }));
                    }
                };

                let mut string = StringWriter::new();
                write_utf8_lossy(&mut string, &buf).unwrap();

                self.output.push(write_event, &string.into_string());

                self.memory.push(0u64);
                return Ok(RuntimeStatus::Running);
            }
            EcallExt::AppendFd { buf, fd } => {
                let write_event = match fd {
                    0 => {
                        self.memory.push(EcallError::WriteStdin.to_u64());
                        return Ok(RuntimeStatus::Running);
                    }
                    1 => WriteEvent::StdoutWrite,
                    2 => WriteEvent::StderrWrite,
                    3 => WriteEvent::StdlogWrite,
                    fd => {
                        let fd = fd - 4;
                        self.files
                            .append_to_file(fd, &buf)
                            .map(|len| self.memory.push(len as u64))
                            .unwrap_or_else(|err| self.memory.push(err.to_u64()));
                        return Ok(RuntimeStatus::Blocked(EcallExt::AppendFd { buf, fd }));
                    }
                };

                let mut string = StringWriter::new();
                write_utf8_lossy(&mut string, &buf).unwrap();

                self.output.push(write_event, &string.into_string());

                self.memory.push(0u64);
                return Ok(RuntimeStatus::Running);
            }

            _ => unimplemented!(),
        }
    }

    pub fn events(&mut self) -> StringArray<WriteEvent> {
        return mem::replace(&mut self.output, StringArray::new());
    }
}
