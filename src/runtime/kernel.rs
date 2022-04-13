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
}

impl Process {
    pub fn new(binary: &BinaryData) -> Self {
        Self {
            memory: Memory::new(binary),
            status: IRtStat::Running,
        }
    }
}

pub struct Kernel {
    pub files: FileSystem,
    pub in_begin: usize,
    pub input: Vec<u8>,
    pub output: TaggedMultiArray<WriteEvt, u8>,

    // pub pipes: TaggedMultiVec<usize, u8>,
    pub processes: TaggedMultiVec<Process, FdKind>,
    pub current_proc: u32,
}

const PROC_MAX_OP_COUNT: u32 = 5000;

impl Kernel {
    pub fn new(files: Vec<(String, u32, Vec<u8>)>) -> Self {
        Self {
            files: FileSystem::new(files),
            in_begin: 0,
            input: Vec::new(),
            output: TaggedMultiArray::new(),

            processes: TaggedMultiVec::new(),
            current_proc: !0,
        }
    }

    pub fn loc(&self) -> CodeLoc {
        if self.current_proc == !0 {
            return NO_FILE;
        }

        let tag = self.processes.get(self.current_proc as usize).unwrap().tag;
        return tag.memory.loc;
    }

    pub fn cur_mem(&self) -> Option<&Memory> {
        if self.current_proc == !0 {
            return None;
        }

        let tag = &self.processes.get(self.current_proc as usize).unwrap().tag;
        return Some(&tag.memory);
    }

    pub fn load_term_program(&mut self, binary: &BinaryData) -> u32 {
        if self.current_proc != !0 {
            let mut prev = self.processes.get_mut(self.current_proc as usize).unwrap();
            match &mut prev.tag_mut().status {
                IRtStat::Exited(_) => {}
                x => *x = IRtStat::Exited(1),
            }
        }

        self.processes = TaggedMultiVec::new();
        self.current_proc = self.processes.len() as u32;

        let (i, o, proc) = (FdKind::TermIn, FdKind::TermOut, Process::new(binary));
        self.in_begin = 0;
        self.input.clear();
        mem::drop(mem::replace(&mut self.output, TaggedMultiArray::new()));
        self.processes.push(proc, vec![i, o, o, o]);
        return self.current_proc;
    }

    pub fn run(&mut self, binary: &BinaryData) -> Result<i32, IError> {
        let proc_id = self.load_term_program(binary);

        loop {
            let proc = self.processes.get_mut(proc_id as usize).unwrap();
            if let IRtStat::Exited(c) = proc.tag().status {
                return Ok(c);
            }

            self.run_op_count(!0)?;
        }
    }

    pub fn run_debug(&mut self, binary: &BinaryData) -> Result<i32, IError> {
        let proc_id = self.load_term_program(binary);
        let mut out = StringWriter::new();

        loop {
            let proc = self.processes.get_mut(proc_id as usize).unwrap();
            if let IRtStat::Exited(c) = proc.tag().status {
                return Ok(c);
            }

            self.run_op_count(!0)?;

            for TE(tag, s) in &self.output {
                write_utf8_lossy(&mut out, s).unwrap();
            }

            println!(out.flush_string());
        }
    }

    pub fn run_op_count(&mut self, count: u32) -> Result<(), IError> {
        let mut proc = match self.processes.get_mut(self.current_proc as usize) {
            Some(p) => p,
            None => {
                return Err(ierror!(
                    "NoProcesses",
                    "tried to run kernel with no processes (this is a bug in TCI)"
                ))
            }
        };

        match proc.tag().status {
            IRtStat::Running => {}
            _ => {
                self.current_proc = !0;

                return Ok(());
            }
        }

        let ops_allowed = count;
        let (ran_count, res) = run_op_count(&mut proc.tag_mut().memory, ops_allowed);

        match res {
            Err(e) => {
                proc.tag_mut().status = IRtStat::Exited(1);
                self.current_proc = !0;

                return Err(e);
            }

            Ok(Some(ecall)) => {
                let res = self.ecall(self.current_proc, ecall);
                let mut proc = self.processes.get_mut(self.current_proc as usize).unwrap();

                match res {
                    Ok(IRtStat::Blocked) => {
                        proc.tag_mut().status = IRtStat::Blocked;
                    }
                    Ok(IRtStat::Exited(exit)) => {
                        self.current_proc = !0;

                        proc.tag_mut().status = IRtStat::Exited(exit);
                    }
                    Ok(IRtStat::Running) => {}

                    Err(e) => {
                        self.current_proc = !0;

                        let mut proc = self.processes.get_mut(self.current_proc as usize).unwrap();
                        proc.tag_mut().status = IRtStat::Exited(1);
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
    pub fn ecall(&mut self, proc: u32, req: EcallExt) -> Result<IRtStat, IError> {
        let mut proc = self.processes.get_mut(proc as usize).unwrap();

        match req {
            EcallExt::Exit(exit) => return Ok(IRtStat::Exited(exit)),

            EcallExt::OpenFd { name, open_mode } => {
                let bytes = proc.tag().memory.cstring_bytes(name)?;
                let name_len = bytes.len() as u32;
                let id = match open_mode {
                    OpenMode::Read => self.files.open(bytes),
                    OpenMode::Create => self.files.open_create(bytes),
                    OpenMode::CreateClear => self.files.open_create_clear(bytes),
                };

                match id {
                    Ok(fd) => {
                        let len = proc.len() as u64;
                        proc.tag_mut().memory.push(len);
                        proc.push(FdKind::FileSys(fd));

                        match open_mode {
                            OpenMode::Read => {}
                            OpenMode::Create => {
                                let bytes = proc.tag().memory.read_bytes(name, name_len)?;
                                self.output.push_from(WriteEvt::CreateFile { fd }, bytes)
                            }
                            OpenMode::CreateClear => {
                                let bytes = proc.tag().memory.read_bytes(name, name_len)?;
                                let name = proc.tag().memory.cstring_bytes(name)?;
                                self.output.push_from(WriteEvt::CreateFile { fd }, bytes);
                                self.output.push_from(WriteEvt::ClearFd { fd }, &[]);
                            }
                        }
                    }
                    Err(e) => proc.tag_mut().memory.push(e.to_u64()),
                }

                return Ok(IRtStat::Running);
            }

            EcallExt::ReadFd {
                len,
                buf,
                begin,
                fd,
            } => {
                let fd_info = proc.get(fd as usize);
                let to_ret = match fd_info {
                    None => EcallError::DoesntExist.to_u64(),
                    Some(FdKind::TermIn) => {
                        let end = core::cmp::min(self.input.len(), len as usize);
                        let bytes = &self.input[(self.in_begin as usize)..end];
                        proc.tag_mut().memory.write_bytes(buf, bytes)?;

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
                                proc.tag_mut().memory.write_bytes(buf, file_buffer)?;
                                file_buffer.len() as u64
                            }
                            Err(e) => e.to_u64(),
                        }
                    }
                    _ => unimplemented!(),
                };

                proc.tag_mut().memory.push(to_ret);
                return Ok(IRtStat::Running);
            }

            EcallExt::WriteFd {
                buf,
                len,
                begin,
                fd,
            } => {
                match proc.get(fd as usize).map(|a| *a) {
                    None => {
                        proc.tag_mut().memory.push(EcallError::DoesntExist.to_u64());
                    }
                    Some(FdKind::TermIn) => {
                        proc.tag_mut().memory.push(EcallError::WriteTermIn.to_u64());
                    }
                    Some(FdKind::TermOut) => {
                        let buf = proc.tag().memory.read_bytes(buf, len)?;
                        self.output.push_from(WriteEvt::StdoutWrite, buf);
                        proc.tag_mut().memory.push(0u64);
                    }
                    Some(FdKind::TermErr) => {
                        let buf = proc.tag().memory.read_bytes(buf, len)?;
                        self.output.push_from(WriteEvt::StderrWrite, buf);
                        proc.tag_mut().memory.push(0u64);
                    }
                    Some(FdKind::TermLog) => {
                        let buf = proc.tag().memory.read_bytes(buf, len)?;
                        self.output.push_from(WriteEvt::StdlogWrite, buf);
                        proc.tag_mut().memory.push(0u64);
                    }
                    Some(FdKind::FileSys(fd)) => {
                        let buffer = proc.tag().memory.read_bytes(buf, len)?;
                        match self.files.write_to_file_range(fd, begin, buffer) {
                            Ok(len) => proc.tag_mut().memory.push(len as u64),
                            Err(err) => proc.tag_mut().memory.push(err.to_u64()),
                        }

                        let buf = proc.tag().memory.read_bytes(buf, len)?;
                        self.output.push_from(WriteEvt::WriteFd { begin, fd }, buf);
                    }
                    _ => unimplemented!(),
                }

                return Ok(IRtStat::Running);
            }

            EcallExt::AppendFd { buf, len, fd } => {
                match proc.get(fd as usize).map(|a| *a) {
                    None => proc.tag_mut().memory.push(EcallError::DoesntExist.to_u64()),
                    Some(FdKind::TermIn) => {
                        proc.tag_mut().memory.push(EcallError::WriteTermIn.to_u64());
                    }
                    Some(FdKind::TermOut) => {
                        let buf = proc.tag().memory.read_bytes(buf, len)?;
                        self.output.push_from(WriteEvt::StdoutWrite, &buf);
                        proc.tag_mut().memory.push(0u64);
                    }
                    Some(FdKind::TermErr) => {
                        let buf = proc.tag().memory.read_bytes(buf, len)?;
                        self.output.push_from(WriteEvt::StderrWrite, &buf);
                        proc.tag_mut().memory.push(0u64);
                    }
                    Some(FdKind::TermLog) => {
                        let buf = proc.tag().memory.read_bytes(buf, len)?;
                        self.output.push_from(WriteEvt::StdlogWrite, &buf);
                        proc.tag_mut().memory.push(0u64);
                    }
                    Some(FdKind::FileSys(fd)) => {
                        let buffer = proc.tag().memory.read_bytes(buf, len)?;
                        match self.files.append_to_file(fd, buffer) {
                            Ok(len) => proc.tag_mut().memory.push(len as u64),
                            Err(err) => proc.tag_mut().memory.push(err.to_u64()),
                        }

                        let buf = proc.tag().memory.read_bytes(buf, len)?;
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
        let mut out = StringWriter::new();

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

        return out.into_string();
    }

    fn write(&mut self, s: &[u8]) -> core::fmt::Result {
        if self.current_proc != !0 {
            self.input.extend(s);
        }

        self.output.push_from(WriteEvt::StdinWrite, s);
        return Ok(());
    }
}

impl Write for Kernel {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        return self.write(s.as_bytes());
    }
}
