use crate::filedb::*;
use crate::interpreter::*;
use crate::runtime::*;
use crate::*;
use serde::{Deserialize, Serialize};
use std::path::Path;
use strum::IntoStaticStr;

#[derive(Debug, Deserialize, Serialize, IntoStaticStr)]
#[serde(tag = "command", content = "data")]
pub enum Command {
    AddFile { path: String, data: String },
    RemoveFile(u32),
    Compile,
    RunCount(u32),
    RunLine,
    Snapshot,
    Back(u32),
    Forwards(u32),
    BackLine(u32),
    ForwardsLine(u32),
    WriteStdin(String),
}

#[derive(Debug, Serialize)]
#[serde(tag = "response", content = "data")]
pub enum CommandResult {
    Confirm(&'static str),
    Compiled,
    InvalidCommand {
        state: &'static str,
        command: &'static str,
    },
    DeserializationError(String),
    IOError(String),
    Stdout(String),
    Stderr(String),
    Stdin(String),
    Unwind(u32),
    Snapshot(MemorySnapshot),
    CompileError {
        rendered: String,
        error: Vec<Error>,
    },
    Status(RuntimeDiagnostic),
    FileId {
        path: String,
        file_id: u32,
    },
}

impl From<WriteEvent> for CommandResult {
    fn from(event: WriteEvent) -> CommandResult {
        match event {
            WriteEvent::StderrWrite(value) => return CommandResult::Stderr(value),
            WriteEvent::StdoutWrite(value) => return CommandResult::Stdout(value),
            WriteEvent::StdinWrite(value) => return CommandResult::Stdin(value),
            WriteEvent::Unwind(len) => return CommandResult::Unwind(len),
        }
    }
}

#[derive(IntoStaticStr)] // LMAO this name
pub enum CommandEngineState<Stdin: IStdin> {
    Blocked {
        runtime: Runtime<Stdin>,
        continuation: IFuture,
    },
    Running(Runtime<Stdin>),
    NotRunning,
}

pub struct CommandEngine<Stdin: IStdin> {
    state: CommandEngineState<Stdin>,
    files: FileDbSlim,
}

impl<Stdin: IStdin> CommandEngine<Stdin> {
    pub fn new() -> Self {
        Self {
            state: CommandEngineState::NotRunning,
            files: FileDbSlim::new(),
        }
    }

    pub async fn run_command(&mut self, command: Command, stdin: &mut Stdin) -> Vec<CommandResult> {
        let mut messages = Vec::new();
        macro_rules! ret {
            ($expr:expr) => {{
                messages.push($expr);
                return messages;
            }};
        }

        match &command {
            Command::AddFile { path, data } => {
                let file_id = if Path::new(path).is_relative() {
                    let real_path = Path::new("/").join(path);
                    let path = real_path.to_str().unwrap();
                    self.files.add(&path, data)
                } else {
                    self.files.add(path, data)
                };

                messages.push(CommandResult::FileId {
                    path: path.to_string(),
                    file_id,
                });
                ret!(CommandResult::Confirm(command.into()));
            }
            Command::RemoveFile(id) => {
                self.files.remove_id(*id);
                ret!(CommandResult::Confirm(command.into()));
            }
            Command::Compile => {
                let mut db = self.files.file_db();
                let program = match compile(&mut db) {
                    Ok(prog) => prog,
                    Err(err) => {
                        let mut writer = StringWriter::new();
                        emit_err(&err, &mut db, &mut writer);
                        ret!(CommandResult::CompileError {
                            rendered: writer.into_string(),
                            error: err
                        });
                    }
                };

                let runtime = Runtime::new(program, StringArray::new());
                self.state = CommandEngineState::Running(runtime);
                messages.push(CommandResult::Compiled);
                ret!(CommandResult::Confirm(command.into()));
            }
            _ => {}
        }

        if let CommandEngineState::Running(runtime) = &mut self.state {
            match command {
                Command::RunLine => {
                    let mut count: u8 = 0;
                    let loc = runtime.program.ops[runtime.pc() as usize].loc;
                    let line = if let Some(line) = self.files.line_index(loc) {
                        line
                    } else {
                        ret!(CommandResult::IOError("problem loading file".to_string()));
                    };

                    loop {
                        count += 1;
                        if count > 100 {
                            break;
                        }

                        let ret = runtime.run_op(stdin).await;

                        for event in runtime.memory.events() {
                            messages.push(event.into());
                        }

                        let current_loc = runtime.program.ops[runtime.pc() as usize].loc;
                        let current_line = if let Some(line) = self.files.line_index(current_loc) {
                            line
                        } else {
                            ret!(CommandResult::IOError("problem loading file".to_string()));
                        };

                        if current_loc.file != loc.file || line != current_line {
                            break;
                        }

                        if let RuntimeStatus::Running = ret {
                            continue;
                        }

                        ret!(CommandResult::Status(runtime.diagnostic()));
                    }

                    ret!(CommandResult::Status(runtime.diagnostic()));
                }
                Command::RunCount(count) => {
                    let ret = runtime.run_op_count(count, stdin).await;

                    for event in runtime.memory.events() {
                        messages.push(event.into());
                    }

                    ret!(CommandResult::Status(ret));
                }
                Command::Snapshot => {
                    ret!(CommandResult::Snapshot(runtime.memory.snapshot()));
                }
                Command::Forwards(count) => {
                    for _ in 0..count {
                        let loc = runtime.program.ops[runtime.pc() as usize].loc;
                        if !runtime.next() {
                            break;
                        }

                        while runtime.program.ops[runtime.pc() as usize].loc == loc
                            && runtime.next()
                        {}
                    }

                    for event in runtime.memory.events() {
                        messages.push(event.into());
                    }

                    ret!(CommandResult::Status(runtime.diagnostic()));
                }
                Command::ForwardsLine(count) => {
                    for _ in 0..count {
                        let loc = runtime.program.ops[runtime.pc() as usize].loc;
                        let line = if let Some(line) = self.files.line_index(loc) {
                            line
                        } else {
                            ret!(CommandResult::IOError("problem loading file".to_string()));
                        };

                        let loc = runtime.program.ops[runtime.pc() as usize].loc;
                        if !runtime.next() {
                            break;
                        }

                        loop {
                            let current_loc = runtime.program.ops[runtime.pc() as usize].loc;
                            let current_line = if let Some(line) =
                                self.files.line_index(current_loc)
                            {
                                line
                            } else {
                                ret!(CommandResult::IOError("problem loading file".to_string()));
                            };

                            if current_loc.file != loc.file || line != current_line {
                                break;
                            }

                            if !runtime.next() {
                                break;
                            }
                        }
                    }

                    for event in runtime.memory.events() {
                        messages.push(event.into());
                    }

                    ret!(CommandResult::Status(runtime.diagnostic()));
                }
                Command::Back(count) => {
                    for _ in 0..count {
                        let loc = runtime.program.ops[runtime.pc() as usize].loc;
                        if !runtime.prev() {
                            break;
                        }

                        while runtime.program.ops[runtime.pc() as usize].loc == loc
                            && runtime.prev()
                        {}
                    }

                    for event in runtime.memory.events() {
                        messages.push(event.into());
                    }

                    ret!(CommandResult::Status(runtime.diagnostic()));
                }
                Command::BackLine(count) => {
                    for _ in 0..count {
                        let loc = runtime.program.ops[runtime.pc() as usize].loc;
                        let line = if let Some(line) = self.files.line_index(loc) {
                            line
                        } else {
                            ret!(CommandResult::IOError("problem loading file".to_string()));
                        };

                        let loc = runtime.program.ops[runtime.pc() as usize].loc;
                        if !runtime.prev() {
                            break;
                        }

                        loop {
                            let current_loc = runtime.program.ops[runtime.pc() as usize].loc;
                            let current_line = if let Some(line) =
                                self.files.line_index(current_loc)
                            {
                                line
                            } else {
                                ret!(CommandResult::IOError("problem loading file".to_string()));
                            };

                            if current_loc.file != loc.file || line != current_line {
                                break;
                            }

                            if !runtime.prev() {
                                break;
                            }
                        }
                    }

                    for event in runtime.memory.events() {
                        messages.push(event.into());
                    }

                    ret!(CommandResult::Status(runtime.diagnostic()));
                }
                _ => ret!(CommandResult::InvalidCommand {
                    state: (&self.state).into(),
                    command: command.into()
                }),
            }
        }

        ret!(CommandResult::InvalidCommand {
            state: (&self.state).into(),
            command: command.into()
        });
    }
}
