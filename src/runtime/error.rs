use super::types::*;
use crate::util::*;
use codespan_reporting::files::Files;
use core::fmt;

pub fn render_err<'a>(
    error: &IError,
    stack_trace: &Vec<CallFrame>,
    files: &'a impl Files<'a, FileId = u32>,
) -> String {
    use codespan_reporting::diagnostic::*;

    let mut out = StringWriter::new();

    write!(out, "{}: {}\n", error.short_name, error.message).unwrap();

    for frame in stack_trace.iter().skip(1) {
        let diagnostic = Diagnostic::new(Severity::Void)
            .with_labels(vec![Label::primary(frame.loc.file, frame.loc)]);
        codespan_reporting::term::emit(&mut out, files, &diagnostic).unwrap();
    }

    return out.to_string();
}

#[derive(Debug, Clone, serde::Serialize)]
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
