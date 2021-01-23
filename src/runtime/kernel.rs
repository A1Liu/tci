use super::error::*;
use super::interpreter::*;
use super::memory::*;
use super::types::*;
use crate::util::*;
use core::mem;
use std::collections::VecDeque;

#[derive(Debug, Clone, Copy)]
enum IRtStat {
    // internal runtime status
    Running,
    Blocked,
    Exited(i32),
}

// Yeah this technically isn't what a kernel does, but like, idk it's what it is.
pub struct Kernel {
    pub requests: VecDeque<EcallExt>,

    // per process
    pub memory: Memory,
    status: IRtStat,
    pub output: StringArray<WriteEvent>,
}

impl Kernel {
    pub fn new(binary: &BinaryData) -> Self {
        Self {
            requests: VecDeque::new(),

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

        match run_op_count(&mut self.memory, count)? {
            Some(EcallExt::Exit(code)) => {
                self.status = IRtStat::Exited(code);
                return Ok(RuntimeStatus::Exited(code));
            }
            Some(ecall) => return Ok(RuntimeStatus::Blocked(ecall)),
            None => return Ok(RuntimeStatus::Running),
        }
    }

    pub fn events(&mut self) -> StringArray<WriteEvent> {
        return mem::replace(&mut self.output, StringArray::new());
    }
}
