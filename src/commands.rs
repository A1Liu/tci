use crate::interpreter::Program;
use crate::interpreter::*;
use crate::runtime::*;
use crate::*;
use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
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
}

#[derive(Serialize)]
#[serde(tag = "response", content = "data")]
pub enum CommandResult {
    Confirm(Command),
    Compiled(Program<'static>),
    InvalidCommand,
    IOError(String),
    CompileError(String),
    RuntimeError(String),
    Status(RuntimeDiagnostic),
    StatusRet { status: RuntimeDiagnostic, ret: i32 },
}

pub enum WSState<'a> {
    Files(FileDb<'a>),
    Running(Runtime<InMemoryIO>),
}

impl<'a> Default for WSState<'a> {
    fn default() -> Self {
        Self::Files(FileDb::new(false))
    }
}

impl<'a> WSState<'a> {
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
                match files.add(path, data) {
                    Ok(_) => {}
                    Err(err) => {
                        ret!(CommandResult::IOError(format!("{}", err)));
                    }
                }
            } else if let Command::Compile = &command {
                let program = match compile(files) {
                    Ok(prog) => prog,
                    Err(err) => {
                        let mut writer = StringWriter::new();
                        emit_err(&err, files, &mut writer);
                        ret!(CommandResult::CompileError(writer.into_string()));
                    }
                };

                *self = Self::Running(Runtime::new(program, InMemoryIO::new(), StringArray::new()));
                ret!(CommandResult::Compiled(program));
            } else {
                ret!(CommandResult::InvalidCommand);
            }

            ret!(CommandResult::Confirm(command));
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

                    if let Some(ret) = ret {
                        return vec![CommandResult::StatusRet {
                            status: runtime.diagnostic(),
                            ret,
                        }];
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

                    if let Some(ret) = ret {
                        ret!(CommandResult::StatusRet {
                            status: runtime.diagnostic(),
                            ret,
                        });
                    }

                    ret!(CommandResult::Status(runtime.diagnostic()));
                }
                _ => ret!(CommandResult::InvalidCommand),
            }
        }

        ret!(CommandResult::Confirm(command));
    }
}
