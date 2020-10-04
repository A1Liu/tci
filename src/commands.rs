use crate::interpreter::Program;
use crate::interpreter::*;
use crate::runtime::*;
use crate::*;
use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
#[serde(tag = "command", content = "data")]
pub enum Command {
    AddFile(String),
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
pub enum CommandError {
    IO(String),
    Compile(String),
    Runtime(String),
    InvalidCommand,
}

impl From<std::io::Error> for CommandError {
    fn from(err: std::io::Error) -> Self {
        return Self::IO(format!("{:?}", err));
    }
}

#[derive(Serialize)]
#[serde(tag = "response", content = "data")]
pub enum CommandResult {
    None,
    Confirm(Command),
    Compiled(Program<'static>),
    Status(RuntimeDiagnostic),
    StatusRet { status: RuntimeDiagnostic, ret: i32 },
}

pub enum WSRuntime<'a> {
    Files(FileDb<'a>),
    Running(Runtime<InMemoryIO>),
}

impl<'a> Default for WSRuntime<'a> {
    fn default() -> Self {
        Self::Files(FileDb::new())
    }
}

impl<'a> WSRuntime<'a> {
    pub fn run_command(&mut self, command: Command) -> Result<CommandResult, CommandError> {
        if let Self::Files(files) = self {
            if let Command::AddFile(file) = &command {
                files.add(file)?;
            } else if let Command::Compile = &command {
                let program = match compile(files) {
                    Ok(prog) => prog,
                    Err(err) => {
                        let mut writer = StringWriter::new();
                        emit_err(&err, files, &mut writer);
                        return Err(CommandError::Compile(writer.into_string()));
                    }
                };
                *self = Self::Running(Runtime::new(program, InMemoryIO::new()));
                return Ok(CommandResult::Compiled(program));
            } else {
                return Err(CommandError::InvalidCommand);
            }

            return Ok(CommandResult::Confirm(command));
        }

        if let Self::Running(runtime) = self {
            match command {
                Command::RunOp => {
                    let ret = match runtime.run_op() {
                        Ok(ret) => ret,
                        Err(err) => {
                            let err = render_err(&err, &runtime.callstack, &runtime.program);
                            return Err(CommandError::Runtime(err));
                        }
                    };

                    if let Some(ret) = ret {
                        return Ok(CommandResult::StatusRet {
                            status: runtime.diagnostic(),
                            ret,
                        });
                    }

                    return Ok(CommandResult::Status(runtime.diagnostic()));
                }
                Command::RunCount(count) => {
                    let ret = match runtime.run_op_count(count) {
                        Ok(prog) => prog,
                        Err(err) => {
                            let err = render_err(&err, &runtime.callstack, &runtime.program);
                            return Err(CommandError::Runtime(err));
                        }
                    };

                    if let Some(ret) = ret {
                        return Ok(CommandResult::StatusRet {
                            status: runtime.diagnostic(),
                            ret,
                        });
                    }

                    return Ok(CommandResult::Status(runtime.diagnostic()));
                }
                Command::RunCountOrUntil {
                    count,
                    pc,
                    stack_size,
                } => {
                    let ret = match runtime.run_count_or_until(count, pc, stack_size) {
                        Ok(ret) => ret,
                        Err(err) => {
                            let err = render_err(&err, &runtime.callstack, &runtime.program);
                            return Err(CommandError::Runtime(err));
                        }
                    };

                    if let Some(ret) = ret {
                        return Ok(CommandResult::StatusRet {
                            status: runtime.diagnostic(),
                            ret,
                        });
                    }

                    return Ok(CommandResult::Status(runtime.diagnostic()));
                }
                _ => return Err(CommandError::InvalidCommand),
            }
        }

        return Ok(CommandResult::Confirm(command));
    }
}
