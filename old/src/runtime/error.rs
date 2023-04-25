use super::types::*;
use crate::filedb::*;
use crate::util::*;
use core::fmt;

pub fn render_err(error: &IError, stack_trace: &Vec<CallFrame>, files: &FileDb) -> String {
    let mut out = String::new();

    write!(out, "{}: {}\n", error.short_name, error.message).unwrap();

    for frame in stack_trace.iter().skip(1) {
        files.write_loc(&mut out, frame.loc).unwrap();
        write!(out, "\n").unwrap();
        files.display_loc(&mut out, frame.loc).unwrap();
    }

    return out;
}

#[derive(Debug, Clone)]
pub struct IError {
    pub short_name: String,
    pub message: String,
}

impl IError {
    pub fn new(short_name: String, message: String) -> Self {
        Self {
            short_name,
            message,
        }
    }
}

#[allow(unused_macros)]
macro_rules! ierror {
    ($arg1:tt,$($arg:tt)*) => {
        IError::new($arg1.to_string(), format!($($arg)*))
    };
}

#[allow(unused_macros)]
macro_rules! ierr {
    ($arg1:tt,$($arg:tt)*) => {
        Err(IError::new($arg1.to_string(), format!($($arg)*)))
    };
}

impl From<fmt::Error> for IError {
    fn from(err: fmt::Error) -> Self {
        ierror!("WriteFailed", "failed to write to output ({})", err)
    }
}
