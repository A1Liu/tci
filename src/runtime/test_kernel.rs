use super::error::*;
use super::fs::*;
use super::interpreter::*;
use super::memory::*;
use super::types::*;
use crate::filedb::*;
use crate::util::*;
use core::mem;

pub struct TestKernel {
    pub files: FileSystem,

    // per process
    pub output: StringArray<WriteEvent>,
    pub memory: Memory,
    pub status: IRtStat,
}

impl TestKernel {
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

            if let Some(e) = res? {
                if let Some(exit) = self.ecall(e)? {
                    self.status = IRtStat::Exited(exit);
                    return Ok(exit);
                }
            }
        }
    }

    pub fn run(&mut self) -> Result<i32, IError> {
        if let IRtStat::Exited(code) = self.status {
            return Ok(code);
        }

        loop {
            let res = run_op_count(&mut self.memory, !0);
            if let Some(e) = res? {
                if let Some(exit) = self.ecall(e)? {
                    self.status = IRtStat::Exited(exit);
                    return Ok(exit);
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

        let ecall = match run_op_count(&mut self.memory, count)? {
            Some(ecall) => ecall,
            None => return Ok(RuntimeStatus::Running),
        };

        if let Some(exit) = self.ecall(ecall)? {
            self.status = IRtStat::Exited(exit);
            return Ok(RuntimeStatus::Exited(exit));
        }

        return Ok(RuntimeStatus::Running);
    }

    pub fn ecall(&mut self, req: EcallExt) -> Result<Option<i32>, IError> {
        match req {
            EcallExt::Exit(exit) => return Ok(Some(exit)),

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
                        return Ok(None);
                    }
                };

                self.memory.push(err.to_u64());
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
            }
            EcallExt::WriteFd { buf, begin, fd } => {
                let write_event = match fd {
                    0 => {
                        self.memory.push(EcallError::WriteStdin.to_u64());
                        return Ok(None);
                    }
                    1 => WriteEvent::StdoutWrite,
                    2 => WriteEvent::StderrWrite,
                    3 => WriteEvent::StdlogWrite,
                    fd => {
                        self.files
                            .write_to_file_range(fd - 4, begin, &buf)
                            .map(|len| self.memory.push(len as u64))
                            .unwrap_or_else(|err| self.memory.push(err.to_u64()));
                        return Ok(None);
                    }
                };

                let mut string = StringWriter::new();
                write_utf8_lossy(&mut string, &buf).unwrap();

                self.output.push(write_event, &string.into_string());

                self.memory.push(0u64);
                return Ok(None);
            }
            EcallExt::AppendFd { buf, fd } => {
                let write_event = match fd {
                    0 => {
                        self.memory.push(EcallError::WriteStdin.to_u64());
                        return Ok(None);
                    }
                    1 => WriteEvent::StdoutWrite,
                    2 => WriteEvent::StderrWrite,
                    3 => WriteEvent::StdlogWrite,
                    fd => {
                        self.files
                            .append_to_file(fd - 4, &buf)
                            .map(|len| self.memory.push(len as u64))
                            .unwrap_or_else(|err| self.memory.push(err.to_u64()));
                        return Ok(None);
                    }
                };

                let mut string = StringWriter::new();
                write_utf8_lossy(&mut string, &buf).unwrap();

                self.output.push(write_event, &string.into_string());

                self.memory.push(0u64);
                return Ok(None);
            }

            _ => unimplemented!(),
        }

        return Ok(None);
    }

    pub fn events(&mut self) -> StringArray<WriteEvent> {
        return mem::replace(&mut self.output, StringArray::new());
    }
}
