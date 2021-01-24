use super::error::*;
use super::interpreter::*;
use super::memory::*;
use super::types::*;
use crate::util::*;
use core::mem;

#[derive(Debug, Clone, Copy)]
enum IRtStat {
    // internal runtime status
    Running,
    Blocked,
    Exited(i32),
}

// Yeah this technically isn't what a kernel does, but like, idk it's what it is.
pub struct Kernel {
    // per process
    pub memory: Memory,
    status: IRtStat,
    pub output: StringArray<WriteEvent>,
}

impl Kernel {
    pub fn new(binary: &BinaryData) -> Self {
        Self {
            memory: Memory::new(&binary),
            status: IRtStat::Running,
            output: StringArray::new(),
        }
    }

    pub fn loc(&self) -> CodeLoc {
        self.memory.loc
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

        match ecall {
            EcallExt::Exit(code) => {
                self.status = IRtStat::Exited(code);
                return Ok(RuntimeStatus::Exited(code));
            }

            EcallExt::ReadFd {
                len,
                buf,
                begin,
                fd,
            } => {
                let to_ret = match fd {
                    0 => unimplemented!("stdin"),
                    1 => EcallExt::Error(EcallError::ReadStdout),
                    2 => EcallExt::Error(EcallError::ReadStderr),
                    3 => EcallExt::Error(EcallError::ReadStdlog),
                    fd => EcallExt::ReadFd {
                        len,
                        buf,
                        begin,
                        fd: fd - 4,
                    },
                };

                return Ok(RuntimeStatus::Blocked(to_ret));
            }

            EcallExt::WriteFd { buf, begin, fd } => {
                let write_event = match fd {
                    0 => {
                        return Ok(RuntimeStatus::Blocked(EcallExt::Error(
                            EcallError::WriteStdin,
                        )))
                    }
                    1 => WriteEvent::StdoutWrite,
                    2 => WriteEvent::StderrWrite,
                    3 => WriteEvent::StdlogWrite,
                    fd => {
                        return Ok(RuntimeStatus::Blocked(EcallExt::WriteFd {
                            buf,
                            begin,
                            fd: fd - 4,
                        }))
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
                        return Ok(RuntimeStatus::Blocked(EcallExt::Error(
                            EcallError::WriteStdin,
                        )))
                    }
                    1 => WriteEvent::StdoutWrite,
                    2 => WriteEvent::StderrWrite,
                    3 => WriteEvent::StdlogWrite,
                    fd => {
                        return Ok(RuntimeStatus::Blocked(EcallExt::AppendFd {
                            buf,
                            fd: fd - 4,
                        }))
                    }
                };

                let mut string = StringWriter::new();
                write_utf8_lossy(&mut string, &buf).unwrap();

                self.output.push(write_event, &string.into_string());

                self.memory.push(0u64);
                return Ok(RuntimeStatus::Running);
            }

            ecall => return Ok(RuntimeStatus::Blocked(ecall)),
        }
    }

    pub fn resolve_result(&mut self, result: EcallResult) -> Result<(), IError> {
        match result {
            EcallResult::None => self.memory.push(0u64),
            EcallResult::Error(err) => self.memory.push(err.to_u64()),
            EcallResult::AppendFd { position } => self.memory.push(position as u64),
            EcallResult::ReadFd { buf, content } => self.memory.write_bytes(buf, &content)?,
        }

        return Ok(());
    }

    pub fn events(&mut self) -> StringArray<WriteEvent> {
        return mem::replace(&mut self.output, StringArray::new());
    }
}
