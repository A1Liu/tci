use crate::filedb::*;
use crate::interpreter::Program;
use crate::interpreter::*;
use crate::runtime::*;
use crate::*;
use serde::{Deserialize, Serialize};
use strum_macros::IntoStaticStr;

#[derive(Debug, Deserialize, Serialize, IntoStaticStr)]
#[serde(tag = "command", content = "data")]
pub enum Command {
    AddFile {
        path: String,
        data: String,
    },
    Compile,
    RunUntilScopedPC(u32),
    RunOp,
    RunCount(u32),
    RunCountOrUntil {
        count: u32,
        stack_size: u16,
        pc: u32,
    },
    Snapshot,
    Back(u32),
    Forwards(u32),
}

#[derive(Serialize)]
#[serde(tag = "response", content = "data")]
pub enum CommandResult {
    Confirm(&'static str),
    Compiled(Program<'static>),
    InvalidCommand,
    IOError(String),
    Stdout(String),
    Stderr(String),
    Unwind(u32),
    Snapshot(MemorySnapshot),
    CompileError(String),
    RuntimeError(String),
    Status(RuntimeDiagnostic),
    StatusRet { status: RuntimeDiagnostic, ret: i32 },
}

impl From<WriteEvent> for CommandResult {
    fn from(event: WriteEvent) -> CommandResult {
        match event {
            WriteEvent::StderrWrite(value) => return CommandResult::Stderr(value),
            WriteEvent::StdoutWrite(value) => return CommandResult::Stdout(value),
            WriteEvent::Unwind(len) => return CommandResult::Unwind(len),
        }
    }
}

pub enum WSState {
    Files(FileDbSlim),
    Running(Runtime),
}

impl Drop for WSState {
    fn drop(&mut self) {
        match self {
            WSState::Files(db) => {}
            WSState::Running(runtime) => {
                let mut buckets = runtime.program.buckets;
                while let Some(b) = unsafe { buckets.dealloc() } {
                    buckets = b;
                }
            }
        }
    }
}

impl Default for WSState {
    fn default() -> Self {
        Self::Files(FileDbSlim::new())
    }
}

impl WSState {
    pub fn run_command(&mut self, command: Command) -> Vec<CommandResult> {
        let mut messages = Vec::new();
        macro_rules! ret {
            ($expr:expr) => {{
                messages.push($expr);
                return messages;
            }};
        }

        if let Self::Files(files) = self {
            if let Command::AddFile { path, data } = &command {
                files.add(path, data);
            } else if let Command::Compile = &command {
                let mut db = files.file_db();
                let program = match compile(&mut db) {
                    Ok(prog) => prog,
                    Err(err) => {
                        let mut writer = StringWriter::new();
                        emit_err(&err, &mut db, &mut writer);
                        ret!(CommandResult::CompileError(writer.into_string()));
                    }
                };

                *self = Self::Running(Runtime::new(program, StringArray::new()));
                ret!(CommandResult::Compiled(program));
            } else {
                ret!(CommandResult::InvalidCommand);
            }

            ret!(CommandResult::Confirm(command.into()));
        }

        if let Self::Running(runtime) = self {
            match command {
                Command::RunOp => {
                    let ret = match runtime.run_op() {
                        Ok(ret) => ret,
                        Err(err) => {
                            let err = render_err(&err, &runtime.memory.callstack, &runtime.program);
                            ret!(CommandResult::RuntimeError(err));
                        }
                    };

                    for event in runtime.memory.events() {
                        messages.push(event.into());
                    }

                    if let Some(ret) = ret {
                        ret!(CommandResult::StatusRet {
                            status: runtime.diagnostic(),
                            ret,
                        });
                    }

                    ret!(CommandResult::Status(runtime.diagnostic()));
                }
                Command::RunCount(count) => {
                    let ret = match runtime.run_op_count(count) {
                        Ok(prog) => prog,
                        Err(err) => {
                            let err = render_err(&err, &runtime.memory.callstack, &runtime.program);
                            ret!(CommandResult::RuntimeError(err));
                        }
                    };

                    for event in runtime.memory.events() {
                        messages.push(event.into());
                    }

                    if let Some(ret) = ret {
                        ret!(CommandResult::StatusRet {
                            status: runtime.diagnostic(),
                            ret,
                        });
                    }

                    ret!(CommandResult::Status(runtime.diagnostic()));
                }
                Command::RunCountOrUntil {
                    count,
                    pc,
                    stack_size,
                } => {
                    let ret = match runtime.run_count_or_until(count, pc, stack_size) {
                        Ok(ret) => ret,
                        Err(err) => {
                            let err = render_err(&err, &runtime.memory.callstack, &runtime.program);
                            ret!(CommandResult::RuntimeError(err));
                        }
                    };

                    for event in runtime.memory.events() {
                        messages.push(event.into());
                    }

                    if let Some(ret) = ret {
                        ret!(CommandResult::StatusRet {
                            status: runtime.diagnostic(),
                            ret,
                        });
                    }

                    ret!(CommandResult::Status(runtime.diagnostic()));
                }
                Command::Snapshot => {
                    ret!(CommandResult::Snapshot(runtime.memory.snapshot()));
                }
                Command::Forwards(count) => {
                    for _ in 0..count {
                        let tag = runtime.memory.current_tag();
                        while runtime.memory.current_tag() == tag && runtime.memory.next() {}

                        if runtime.memory.current_tag() == tag {
                            break;
                        }
                    }

                    for event in runtime.memory.events() {
                        messages.push(event.into());
                    }

                    ret!(CommandResult::Status(runtime.diagnostic()));
                }
                Command::Back(count) => {
                    for _ in 0..count {
                        let tag = runtime.memory.current_tag();
                        while runtime.memory.current_tag() == tag && runtime.memory.prev() {}

                        if runtime.memory.current_tag() == tag {
                            break;
                        }
                    }

                    for event in runtime.memory.events() {
                        messages.push(event.into());
                    }

                    ret!(CommandResult::Status(runtime.diagnostic()));
                }
                _ => ret!(CommandResult::InvalidCommand),
            }
        }

        ret!(CommandResult::Confirm(command.into()));
    }
}
