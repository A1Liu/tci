#[macro_use]
pub mod error;

pub mod fs;
pub mod interpreter;
pub mod kernel;
pub mod memory;
pub mod types;

pub use error::*;
pub use fs::*;
pub use interpreter::*;
pub use kernel::*;
pub use memory::*;
pub use types::*;

use crate::filedb::FileDb;
use alloc::string::String;

pub fn print_error(err: &IError, memory: &Memory, files: &FileDb) -> String {
    use crate::util::*;
    use codespan_reporting::diagnostic::*;
    use codespan_reporting::term::*;

    let mut out = StringWriter::new();

    write!(out, "{}: {}\n", err.short_name, err.message).unwrap();

    for frame in memory.callstack.iter().skip(1) {
        let diagnostic = Diagnostic::new(Severity::Void)
            .with_labels(vec![Label::primary(frame.loc.file, frame.loc)]);
        emit(&mut out, files, &diagnostic).unwrap();
    }

    let loc = memory.loc;
    if loc != NO_FILE {
        let diagnostic =
            Diagnostic::new(Severity::Void).with_labels(vec![Label::primary(loc.file, loc)]);
        emit(&mut out, files, &diagnostic).unwrap();
    }

    return out.into_string();
}
