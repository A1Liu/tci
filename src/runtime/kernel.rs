use super::error::*;
use super::interpreter::*;
use super::memory::*;
use super::types::*;
use crate::filedb::*;
use crate::util::*;
use core::mem;

pub struct Runtime {
    pub output: StringArray<WriteEvent>,
    pub memory: Memory,
    pub status: RuntimeStatus,
}

impl Runtime {
    pub fn new(binary: &BinaryData) -> Self {
        Self {
            output: StringArray::new(),
            memory: Memory::new(&binary),
            status: RuntimeStatus::Running,
        }
    }

    pub fn loc(&self) -> CodeLoc {
        return self.memory.loc;
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

            Ecall::PrintString => {
                println!(self.memory.expr_stack);
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
