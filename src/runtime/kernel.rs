use super::error::*;
use super::fs::*;
use super::interpreter::*;
use super::memory::*;
use super::types::*;
use crate::util::*;
use core::mem;

#[derive(Debug, Clone, Copy)]
pub enum IRtStat {
    // internal runtime status
    Running,
    Blocked,
    Exited(i32),
}

#[derive(Debug, Clone, Copy)]
pub enum KernStat {
    Running,
    Errored(u32),
}

pub struct Process {
    pub memory: Memory,
    pub status: IRtStat,
    pub op_count: u32,
}

impl Process {
    pub fn new(binary: &BinaryData) -> Self {
        Self {
            memory: Memory::new(binary),
            status: IRtStat::Running,
            op_count: 0,
        }
    }
}

pub struct Kernel {
    pub files: FileSystem,
    pub in_begin: usize,
    pub input: Vec<u8>,
    pub output: TaggedMultiArray<WriteEvt, u8>,

    pub process: Option<Process>,
    pub fd: Pod<FdKind>,
}

const PROC_MAX_OP_COUNT: u32 = 5000;

impl Kernel {
    pub fn new(files: Vec<(String, u32, Vec<u8>)>) -> Self {
        Self {
            files: FileSystem::new(files),
            in_begin: 0,
            input: Vec::new(),
            output: TaggedMultiArray::new(),

            process: None,
            fd: Pod::new(),
        }
    }

    pub fn loc(&self) -> CodeLoc {
        let tag = match &self.process {
            None => return NO_FILE,
            Some(p) => p,
        };

        return tag.memory.frame.loc;
    }

    pub fn cur_mem(&self) -> Option<&Memory> {
        let tag = self.process.as_ref()?;

        return Some(&tag.memory);
    }

    pub fn load_term_program(&mut self, binary: &BinaryData) {
        let (i, o, proc) = (FdKind::TermIn, FdKind::TermOut, Process::new(binary));
        self.in_begin = 0;
        self.input.clear();
        mem::drop(mem::replace(&mut self.output, TaggedMultiArray::new()));
        self.process.replace(proc);

        self.fd.clear();
        self.fd.reserve(4);
        self.fd.push(i);
        self.fd.push(o);
        self.fd.push(o);
        self.fd.push(o);
    }

    pub fn run(&mut self, binary: &BinaryData) -> Result<i32, IError> {
        self.load_term_program(binary);

        loop {
            let proc = self.process.as_mut().unwrap();
            if let IRtStat::Exited(c) = proc.status {
                return Ok(c);
            }

            self.run_op_count(!0)?;
        }
    }

    pub fn run_debug(&mut self, binary: &BinaryData) -> Result<i32, IError> {
        self.load_term_program(binary);
        let mut out = String::new();

        loop {
            let proc = self.process.as_mut().unwrap();
            if let IRtStat::Exited(c) = proc.status {
                return Ok(c);
            }

            self.run_op_count(!0)?;

            for TE(tag, s) in &self.output {
                write_utf8_lossy(&mut out, s).unwrap();
            }

            println!(out);
            out.clear();
        }
    }

    pub fn run_op_count(&mut self, count: u32) -> Result<(), IError> {
        let proc = match &mut self.process {
            Some(p) => p,
            None => {
                return Err(ierror!(
                    "NoProcesses",
                    "tried to run kernel with no processes (this is a bug in TCI)"
                ))
            }
        };

        match proc.status {
            IRtStat::Running => {}
            _ => {
                return Ok(());
            }
        }

        let ops_allowed = count;
        let (ran_count, res) = run_op_count(&mut proc.memory, ops_allowed);
        proc.op_count += ran_count;

        match res {
            Err(e) => {
                proc.status = IRtStat::Exited(1);

                return Err(e);
            }

            Ok(Some(ecall)) => {
                let res = self.ecall(ecall);
                let mut proc = self.process.as_mut().unwrap();

                match res {
                    Ok(IRtStat::Blocked) => {
                        proc.status = IRtStat::Blocked;
                    }
                    Ok(IRtStat::Exited(exit)) => {
                        proc.status = IRtStat::Exited(exit);
                    }
                    Ok(IRtStat::Running) => {}

                    Err(e) => {
                        proc.status = IRtStat::Exited(1);
                        return Err(e);
                    }
                }

                return Ok(());
            }

            Ok(None) => {}
        }

        return Ok(());
    }

    #[inline]
    pub fn ecall(&mut self, req: EcallExt) -> Result<IRtStat, IError> {
        let proc = self.process.as_mut().unwrap();

        match req {
            EcallExt::Exit(exit) => return Ok(IRtStat::Exited(exit)),

            EcallExt::OpenFd { name, open_mode } => {
                let bytes = proc.memory.cstring_bytes(name)?;
                let name_len = bytes.len() as u32;
                let id = match open_mode {
                    OpenMode::Read => self.files.open(bytes),
                    OpenMode::Create => self.files.open_create(bytes),
                    OpenMode::CreateClear => self.files.open_create_clear(bytes),
                };

                match id {
                    Ok(fd) => {
                        let len = self.fd.len() as u64;
                        proc.memory.push(len);
                        self.fd.push(FdKind::FileSys(fd));

                        match open_mode {
                            OpenMode::Read => {}
                            OpenMode::Create => {
                                let bytes = proc.memory.read_bytes(name, name_len)?;
                                self.output.push_from(WriteEvt::CreateFile { fd }, bytes)
                            }
                            OpenMode::CreateClear => {
                                let bytes = proc.memory.read_bytes(name, name_len)?;
                                let name = proc.memory.cstring_bytes(name)?;
                                self.output.push_from(WriteEvt::CreateFile { fd }, bytes);
                                self.output.push_from(WriteEvt::ClearFd { fd }, &[]);
                            }
                        }
                    }
                    Err(e) => proc.memory.push(e.to_u64()),
                }

                return Ok(IRtStat::Running);
            }

            EcallExt::ReadFd {
                len,
                buf,
                begin,
                fd,
            } => {
                let fd_info = self.fd.get(fd as usize);
                let to_ret = match fd_info {
                    None => EcallError::DoesntExist.to_u64(),
                    Some(FdKind::TermIn) => {
                        let end = core::cmp::min(self.input.len(), len as usize);
                        let bytes = &self.input[(self.in_begin as usize)..end];
                        proc.memory.write_bytes(buf, bytes)?;

                        let begin = self.in_begin as usize;
                        self.in_begin = if end == self.input.len() {
                            self.input.clear();
                            0
                        } else {
                            end
                        };

                        (end - begin) as u64
                    }
                    Some(FdKind::TermOut) => EcallError::ReadTermOut.to_u64(),
                    Some(FdKind::TermErr) => EcallError::ReadTermErr.to_u64(),
                    Some(FdKind::TermLog) => EcallError::ReadTermLog.to_u64(),
                    Some(FdKind::FileSys(fd)) => {
                        match self.files.read_file_range(*fd, begin, len) {
                            Ok(file_buffer) => {
                                proc.memory.write_bytes(buf, file_buffer)?;
                                file_buffer.len() as u64
                            }
                            Err(e) => e.to_u64(),
                        }
                    }
                    _ => unimplemented!(),
                };

                proc.memory.push(to_ret);
                return Ok(IRtStat::Running);
            }

            EcallExt::WriteFd {
                buf,
                len,
                begin,
                fd,
            } => {
                match self.fd.get(fd as usize).map(|a| *a) {
                    None => {
                        proc.memory.push(EcallError::DoesntExist.to_u64());
                    }
                    Some(FdKind::TermIn) => {
                        proc.memory.push(EcallError::WriteTermIn.to_u64());
                    }
                    Some(FdKind::TermOut) => {
                        let buf = proc.memory.read_bytes(buf, len)?;
                        self.output.push_from(WriteEvt::StdoutWrite, buf);
                        proc.memory.push(0u64);
                    }
                    Some(FdKind::TermErr) => {
                        let buf = proc.memory.read_bytes(buf, len)?;
                        self.output.push_from(WriteEvt::StderrWrite, buf);
                        proc.memory.push(0u64);
                    }
                    Some(FdKind::TermLog) => {
                        let buf = proc.memory.read_bytes(buf, len)?;
                        self.output.push_from(WriteEvt::StdlogWrite, buf);
                        proc.memory.push(0u64);
                    }
                    Some(FdKind::FileSys(fd)) => {
                        let buffer = proc.memory.read_bytes(buf, len)?;
                        match self.files.write_to_file_range(fd, begin, buffer) {
                            Ok(len) => proc.memory.push(len as u64),
                            Err(err) => proc.memory.push(err.to_u64()),
                        }

                        let buf = proc.memory.read_bytes(buf, len)?;
                        self.output.push_from(WriteEvt::WriteFd { begin, fd }, buf);
                    }
                    _ => unimplemented!(),
                }

                return Ok(IRtStat::Running);
            }

            EcallExt::AppendFd { buf, len, fd } => {
                match self.fd.get(fd as usize).map(|a| *a) {
                    None => proc.memory.push(EcallError::DoesntExist.to_u64()),
                    Some(FdKind::TermIn) => {
                        proc.memory.push(EcallError::WriteTermIn.to_u64());
                    }
                    Some(FdKind::TermOut) => {
                        let buf = proc.memory.read_bytes(buf, len)?;
                        self.output.push_from(WriteEvt::StdoutWrite, &buf);
                        proc.memory.push(0u64);
                    }
                    Some(FdKind::TermErr) => {
                        let buf = proc.memory.read_bytes(buf, len)?;
                        self.output.push_from(WriteEvt::StderrWrite, &buf);
                        proc.memory.push(0u64);
                    }
                    Some(FdKind::TermLog) => {
                        let buf = proc.memory.read_bytes(buf, len)?;
                        self.output.push_from(WriteEvt::StdlogWrite, &buf);
                        proc.memory.push(0u64);
                    }
                    Some(FdKind::FileSys(fd)) => {
                        let buffer = proc.memory.read_bytes(buf, len)?;
                        match self.files.append_to_file(fd, buffer) {
                            Ok(len) => proc.memory.push(len as u64),
                            Err(err) => proc.memory.push(err.to_u64()),
                        }

                        let buf = proc.memory.read_bytes(buf, len)?;
                        self.output.push_from(WriteEvt::AppendFd { fd }, buf);
                    }
                    _ => unimplemented!(),
                }

                return Ok(IRtStat::Running);
            }
        }
    }

    pub fn events(&mut self) -> TaggedMultiArray<WriteEvt, u8> {
        return mem::replace(&mut self.output, TaggedMultiArray::new());
    }

    pub fn term_out(&mut self) -> String {
        let mut out = String::new();

        for TE(tag, s) in &self.output {
            match tag {
                WriteEvt::WriteFd { .. } => continue,
                WriteEvt::AppendFd { .. } => continue,
                WriteEvt::CreateFile { .. } => continue,
                WriteEvt::ClearFd { .. } => continue,
                _ => {}
            }

            write_utf8_lossy(&mut out, s).unwrap();
        }

        mem::drop(mem::replace(&mut self.output, TaggedMultiArray::new()));

        return out;
    }

    fn write(&mut self, s: &[u8]) -> core::fmt::Result {
        self.input.extend(s);

        self.output.push_from(WriteEvt::StdinWrite, s);
        return Ok(());
    }
}

impl Write for Kernel {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        return self.write(s.as_bytes());
    }
}
