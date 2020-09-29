use crate::interpreter::Program;
use crate::*;
use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
#[serde(tag = "command", content = "data")]
pub enum Command {
    AddFile(String),
    Compile,
}

#[derive(Serialize)]
#[serde(tag = "response", content = "data")]
pub enum CommandError {
    IO(String),
    Compile(String),
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
}

pub enum WSRuntime<'a> {
    Files(FileDb<'a>),
    Compiled(Program<'static>),
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
                *self = Self::Compiled(program);
            } else {
                return Err(CommandError::InvalidCommand);
            }

            return Ok(CommandResult::Confirm(command));
        }

        if let Self::Compiled(program) = self {
            match command {
                _ => return Err(CommandError::InvalidCommand),
            }
        }

        return Ok(CommandResult::Confirm(command));
    }
}
